# Set up -----
library(plumber)
library(jsonlite)
library(dplyr)
library(workflows)
library(rlang)
library(vctrs)

limit_threads <- function() {
  # BLAS/OpenMP families
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1",
    XGBOOST_NUM_THREADS = "1"
  )
}
limit_threads()

# ---------------------------------------------------------------------
# Lazy model loader
# ---------------------------------------------------------------------
.load_env <- new.env(parent = emptyenv())

get_fit <- function(key, path) {
  if (!exists(key, envir = .load_env, inherits = FALSE)) {
    assign(key, readRDS(path), envir = .load_env)
  }
  get(key, envir = .load_env, inherits = FALSE)
}

is_loaded <- function(key) exists(key, envir = .load_env, inherits = FALSE)

# Model paths
V1_PATH <- "models/v1_model.rds"
V2_PATH <- "models/v2_model.rds"
V3_PATH <- "models/v3_model.rds"
V4_PATH <- "models/v4_model.rds"
V5_PATH <- "models/v5_model.rds"

v1_meta <- readRDS("models/v1_meta.rds")
v2_meta <- readRDS("models/v2_meta.rds")
v3_meta <- readRDS("models/v3_meta.rds")
v4_meta <- readRDS("models/v4_meta.rds")
v5_meta <- readRDS("models/v5_meta.rds")

# ---------------------------------------------------------------------
# Defaults / knobs (used if meta doesn't define them)
# ---------------------------------------------------------------------
DEF_THR_V1        <- 0.025
DEF_THR_V2        <- 0.20
DEF_MARGIN        <- 0.05
DEF_VETO_SOFT     <- 0.10
DEF_V1_STRONG     <- 0.20
DEF_LOW_V2_RESCUE <- 0.01

get_num <- function(x, default) {
  xnum <- suppressWarnings(as.numeric(x))
  if (length(xnum) == 0L || is.na(xnum)) default else xnum
}

get_required_outcomes <- function(obj) {
  wf <- if (inherits(obj, "workflow")) obj else obj$wf
  mold <- workflows::extract_mold(wf)
  mold$blueprint$ptypes$outcomes
}

get_required_ids <- function(obj) {
  wf <- if (inherits(obj, "workflow")) obj else obj$wf
  mold <- workflows::extract_mold(wf)
  ids_tbl <- try(mold$extras$roles$id, silent = TRUE)
  if (inherits(ids_tbl, "try-error") || is.null(ids_tbl)) return(list())
  # Build a prototype list for each id column (usually just "label")
  out <- list()
  for (nm in names(ids_tbl)) {
    out[[nm]] <- vctrs::vec_ptype(ids_tbl[[nm]])
  }
  out
}

get_required_case_wts <- function(obj) {
  wf <- if (inherits(obj, "workflow")) obj else obj$wf
  mold <- workflows::extract_mold(wf)
  cw_tbl <- try(mold$extras$roles$case_weights, silent = TRUE)
  if (inherits(cw_tbl, "try-error") || is.null(cw_tbl)) return(list())
  out <- list()
  for (nm in names(cw_tbl)) {
    out[[nm]] <- vctrs::vec_ptype(cw_tbl[[nm]])
  }
  out
}

add_role_stubs <- function(obj, df) {
  n <- nrow(df)

  # Outcomes (e.g., SFI_bin_severe)
  out_pt <- try(get_required_outcomes(obj), silent = TRUE)
  if (!inherits(out_pt, "try-error") && length(out_pt)) {
    for (nm in names(out_pt)) {
      if (!nm %in% names(df)) {
        df[[nm]] <- vctrs::vec_init(out_pt[[nm]], n)
        attr(df, "schema_added_outcomes") <- unique(c(attr(df, "schema_added_outcomes"), nm))
      }
    }
  }

  # ID role (e.g., label)
  id_pt <- get_required_ids(obj)
  if (length(id_pt)) {
    for (nm in names(id_pt)) {
      if (!nm %in% names(df)) {
        df[[nm]] <- vctrs::vec_init(id_pt[[nm]], n)
        attr(df, "schema_added_ids") <- unique(c(attr(df, "schema_added_ids"), nm))
      }
    }
  }

  # Case weights role (rarely needed at predict, but make it safe)
  cw_pt <- get_required_case_wts(obj)
  if (length(cw_pt)) {
    for (nm in names(cw_pt)) {
      if (!nm %in% names(df)) {
        df[[nm]] <- vctrs::vec_init(cw_pt[[nm]], n)
        attr(df, "schema_added_case_wts") <- unique(c(attr(df, "schema_added_case_wts"), nm))
      }
    }
  }

  df
}

get_thr <- function(fit, meta, default = 0.5) {
  if (!is.null(meta$threshold)) return(as.numeric(meta$threshold))
  if (!is.null(fit$operating_point$threshold)) return(as.numeric(fit$operating_point$threshold))
  default
}

eff_thr_v1        <- get_num(v1_meta$threshold,        DEF_THR_V1)
eff_thr_v2        <- get_num(v2_meta$threshold,        DEF_THR_V2)
eff_margin        <- get_num(v1_meta$margin,           DEF_MARGIN)
eff_veto_soft     <- get_num(v2_meta$veto_soft,        DEF_VETO_SOFT)
eff_v1_strong     <- get_num(v1_meta$v1_strong,        DEF_V1_STRONG)
eff_low_v2_rescue <- get_num(v2_meta$low_v2_rescue,    DEF_LOW_V2_RESCUE)

# ---------------------------------------------------------------------
# Workflow helpers
# ---------------------------------------------------------------------
suppressPackageStartupMessages({
  library(recipes)
  library(Matrix)
  library(xgboost)
})

is_trained_wf <- function(x) {
  inherits(x, "workflow") && tryCatch(workflows::is_trained_workflow(x), error = function(e) FALSE)
}

predict_probs <- function(obj, new_data) {
  if (is_trained_wf(obj)) {
    return(predict(obj, new_data = new_data, type = "prob"))
  }
  if (!is.null(obj$wf) && workflows::is_trained_workflow(obj$wf)) {
    return(predict(obj$wf, new_data = new_data, type = "prob"))
  }
  stop("Model object is not a trained workflow.")
}

predict_probs_safe <- function(obj, new_data) {
  tryCatch(
    predict_probs(obj, new_data),
    error = function(e) {
      attr(e, "is_predict_error") <- TRUE
      stop(e)
    }
  )
}

apply_calibrator <- function(p_raw, calib, eps = 1e-6) {
  if (is.null(calib) || is.null(calib$ok) || !isTRUE(calib$ok)) return(p_raw)
  p_safe <- pmin(1 - eps, pmax(eps, p_raw))
  b0 <- calib$coef[["(Intercept)"]]
  b1 <- calib$coef[["qlogis(p)"]]
  plogis(b0 + b1 * qlogis(p_safe))
}

cast_to_ptypes <- function(df, ptypes) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  n <- nrow(df)
  # Add missing predictors
  miss <- setdiff(names(ptypes), names(df))
  if (length(miss)) for (nm in miss) df[[nm]] <- vctrs::vec_init(ptypes[[nm]], n)

  # Cast existing to prototype classes
  to_bin_char <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) return(ifelse(isTRUE(x), "1", "0"))
    x <- trimws(tolower(as.character(x)))
    ifelse(x %in% c("1","true","t","yes","y","m","male"), "1",
      ifelse(x %in% c("0","false","f","no","n","female","fem"), "0", x))
  }

  for (nm in names(ptypes)) {
    proto <- ptypes[[nm]]
    x <- df[[nm]]
    if (is.factor(proto)) {
      lv <- levels(proto)
      if (is.numeric(x) || is.integer(x) || is.logical(x)) x <- to_bin_char(x)
      x <- trimws(as.character(x))
      if (length(lv) == 2L) {
        low <- lv[1]; high <- lv[2]
        if (all(c("0","1") %in% lv)) {
          x <- ifelse(x %in% c("0","1"), x, NA_character_)
        } else {
          x <- ifelse(x == "0", low, ifelse(x == "1", high, x))
        }
      }
      df[[nm]] <- factor(ifelse(x %in% lv, x, NA_character_), levels = lv)
    } else if (is.numeric(proto)) {
      if (!is.numeric(x)) df[[nm]] <- suppressWarnings(as.numeric(x))
    } else if (is.integer(proto)) {
      if (!is.integer(x)) df[[nm]] <- suppressWarnings(as.integer(x))
    } else if (is.logical(proto)) {
      if (!is.logical(x)) {
        xl <- tolower(as.character(x))
        df[[nm]] <- xl %in% c("1","true","t","yes","y")
      }
    } else if (is.character(proto)) {
      if (!is.character(x)) df[[nm]] <- as.character(x)
    }
  }
  df
}

predict_s2_probs <- function(fit, new_data, calibrated = TRUE) {
  stopifnot(!is.null(fit$prep), !is.null(fit$bst))

  # 1) Cast known predictor ptypes but DO NOT drop other cols
  if (!is.null(fit$predictor_ptypes)) {
    new_data <- cast_to_ptypes(new_data, fit$predictor_ptypes)
  } else {
    new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)
  }

  # 2) Ensure ALL variables referenced by the recipe exist
  #    (recipes needs them even if later removed)
  needed <- unique(summary(fit$prep)$variable)

  # Build a prototype pool from what we saved in the bundle
  proto_pool <- list()
  if (!is.null(fit$predictor_ptypes))    proto_pool <- c(proto_pool, fit$predictor_ptypes)
  if (!is.null(fit$id_role_ptypes))      proto_pool <- c(proto_pool, fit$id_role_ptypes)
  if (!is.null(fit$outcome_ptypes))      proto_pool <- c(proto_pool, fit$outcome_ptypes)

  miss <- setdiff(needed, names(new_data))

if (length(miss)) {
  # short log of what's being added
  suffix <- if (length(miss) > 12) " … (more)" else ""
  .log(
    "s2_infer: adding missing vars -> ",
    paste(utils::head(miss, 12), collapse = ", "),
    suffix
  )

  n <- nrow(new_data)
  for (nm in miss) {
    proto <- proto_pool[[nm]]
    if (!is.null(proto)) {
      new_data[[nm]] <- vctrs::vec_init(proto, n)
    } else {
      # Fallback heuristics: ids as character, flag/dummy-ish as integer, else numeric
      if (grepl("^(label|site|ipdopd)$", nm)) {
        new_data[[nm]] <- rep(NA_character_, n)
      } else if (grepl("^(na[_]|na_ind_|.*(_X0|_X1|_unknown|_new)$)", nm)) {
        new_data[[nm]] <- rep(NA_integer_, n)
      } else {
        new_data[[nm]] <- rep(NA_real_, n)
      }
    }
  }
}

  # 3) Now bake safely
  baked <- bake(fit$prep, new_data = new_data)

  # 4) Align to training feature set (add any missing with 0)
  feats <- fit$features
  miss_feats <- setdiff(feats, names(baked))
  if (length(miss_feats)) for (mc in miss_feats) baked[[mc]] <- 0
  baked <- baked[, feats, drop = FALSE]

  # 5) Build sparse design without dense copy
  fml <- as.formula(paste("~", paste(sprintf("`%s`", feats), collapse = " + "), "- 1"))
  X <- Matrix::sparse.model.matrix(fml, data = baked)
  rm(baked); gc(verbose = FALSE)

  # 6) Predict with one thread
  p_raw <- xgboost::predict(fit$bst, newdata = X, nthread = 1)
  rm(X); gc(verbose = FALSE)

  if (isTRUE(calibrated)) apply_calibrator(p_raw, fit$calibrator) else p_raw
}


prop_excess <- function(p, thr, eps = 1e-6) pmax(0, (p - thr) / pmax(thr, eps))

route_s2 <- function(p3, p4, p5, thr3, thr4, thr5) {
  f3 <- as.integer(p3 >= thr3)
  f4 <- as.integer(p4 >= thr4)
  f5 <- as.integer(p5 >= thr5)

  m3 <- prop_excess(p3, thr3)
  m4 <- prop_excess(p4, thr4)
  m5 <- prop_excess(p5, thr5)

  key <- paste0(f3, f4, f5)
  out <- character(length(key))

  # Direct mappings
  out[key == "000"] <- "NonSevere"
  out[key == "100"] <- "Severe"
  out[key == "010"] <- "ProbSevere"
  out[key == "001"] <- "ProbNonSevere"

  # Tie patterns with proportional margins
  idx <- which(key %in% c("110","111"))  # Severe vs ProbSevere
  if (length(idx)) {
    choose <- ifelse(m3[idx] > m4[idx], "Severe",
              ifelse(m4[idx] > m3[idx], "ProbSevere", "ProbSevere")) # tie -> conservative
    out[idx] <- choose
  }

  idx <- which(key == "101")             # Severe vs ProbNonSevere
  if (length(idx)) {
    choose <- ifelse(m3[idx] > m5[idx], "Severe",
              ifelse(m5[idx] > m3[idx], "ProbNonSevere", "ProbNonSevere"))
    out[idx] <- choose
  }

  idx <- which(key == "011")             # ProbSevere vs ProbNonSevere
  if (length(idx)) {
    choose <- ifelse(m4[idx] > m5[idx], "ProbSevere",
              ifelse(m5[idx] > m4[idx], "ProbNonSevere", "ProbNonSevere"))
    out[idx] <- choose
  }

  factor(out, levels = c("NonSevere","ProbNonSevere","ProbSevere","Severe"))
}


# ---------------------------------------------------------------------
# Schema guard: align request to each workflow's expected predictor ptypes
# ---------------------------------------------------------------------
get_required_ptypes <- function(obj) {
  wf <- NULL
  if (inherits(obj, "workflow")) {
    wf <- obj
  } else if (is.list(obj) && !is.null(obj$wf) && inherits(obj$wf, "workflow")) {
    wf <- obj$wf
  } else {
    stop("Object is not a trained workflow or wrapper containing $wf.")
  }
  mold <- workflows::extract_mold(wf)
  mold$blueprint$ptypes$predictors
}

union_required_ptypes <- function(models) {
  reqs <- lapply(models, get_required_ptypes)
  out <- list()
  for (pt in reqs) {
    for (nm in names(pt)) out[[nm]] <- pt[[nm]]
  }
  out
}

ensure_predictor_schema <- function(models, new_data) {
  if (!is.list(models)) models <- list(models)

  # ---- Collect predictor ptypes (last wins on name clashes)
  req_pred_list <- lapply(models, get_required_ptypes)
  req_pred <- list(); for (pt in req_pred_list) for (nm in names(pt)) req_pred[[nm]] <- pt[[nm]]

  # ---- Collect outcome ptypes (e.g., 'label') if any
  req_out_list <- lapply(models, function(m) {
    out <- try(get_required_outcomes(m), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) list() else out
  })
  req_out <- list(); for (pt in req_out_list) for (nm in names(pt)) req_out[[nm]] <- pt[[nm]]

  n <- nrow(new_data)
  added_pred  <- character()
  added_out   <- character()
  casted_pred <- character()

  # Ensure all expected predictor cols exist
  miss_pred <- setdiff(names(req_pred), names(new_data))
  if (length(miss_pred)) {
    for (nm in miss_pred) new_data[[nm]] <- vctrs::vec_init(req_pred[[nm]], n = n)
    added_pred <- miss_pred
  }

  # Ensure all expected outcome cols (e.g., 'label') exist — set to NA prototype
  miss_out <- setdiff(names(req_out), names(new_data))
  if (length(miss_out)) {
    for (nm in miss_out) new_data[[nm]] <- vctrs::vec_init(req_out[[nm]], n = n)
    added_out <- miss_out
  }

  # Order predictors to match ptypes (nice-to-have)
  new_data <- new_data[, unique(c(names(req_pred), names(new_data))), drop = FALSE]

  # Helper to normalize many binary encodings to "0"/"1"
  to_bin_char <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) return(ifelse(isTRUE(x), "1", "0"))
    x <- trimws(tolower(as.character(x)))
    ifelse(x %in% c("1","true","t","yes","y","m","male"), "1",
      ifelse(x %in% c("0","false","f","no","n","female","fem"), "0", x))
  }

  # Cast predictors to prototype classes/levels
  for (nm in names(req_pred)) {
    proto <- req_pred[[nm]]
    x     <- new_data[[nm]]

    if (is.factor(proto)) {
      lv <- levels(proto)
      if (is.numeric(x) || is.integer(x) || is.logical(x)) x <- to_bin_char(x)
      x <- trimws(as.character(x))

      if (length(lv) == 2L) {
        low <- lv[1]; high <- lv[2]
        if (all(c("0","1") %in% lv)) {
          x <- ifelse(x %in% c("0","1"), x, NA_character_)
        } else {
          x <- ifelse(x == "0", low, ifelse(x == "1", high, x))
        }
      }
      new_x <- factor(ifelse(x %in% lv, x, NA_character_), levels = lv)
      if (!identical(class(new_data[[nm]]), class(new_x))) casted_pred <- c(casted_pred, nm)
      new_data[[nm]] <- new_x

    } else if (is.numeric(proto)) {
      if (!is.numeric(x)) { new_data[[nm]] <- suppressWarnings(as.numeric(x)); casted_pred <- c(casted_pred, nm) }

    } else if (is.integer(proto)) {
      if (!is.integer(x)) { new_data[[nm]] <- suppressWarnings(as.integer(x)); casted_pred <- c(casted_pred, nm) }

    } else if (is.logical(proto)) {
      if (!is.logical(x)) {
        xl <- tolower(as.character(x))
        new_data[[nm]] <- xl %in% c("1","true","t","yes","y")
        casted_pred <- c(casted_pred, nm)
      }

    } else if (is.character(proto)) {
      if (!is.character(x)) { new_data[[nm]] <- as.character(x); casted_pred <- c(casted_pred, nm) }
    }
  }

  attr(new_data, "schema_added")            <- added_pred
  attr(new_data, "schema_added_outcomes")   <- added_out
  attr(new_data, "schema_casted")           <- unique(casted_pred)
  new_data
}

# ---------------------------------------------------------------------
# Decision logic (margin + soft veto + strong V1 + low-V2 rescue)
# ---------------------------------------------------------------------
combine_margin <- function(p1, p2,
                           thr_v1        = eff_thr_v1,
                           thr_v2        = eff_thr_v2,
                           margin        = eff_margin,
                           veto_soft     = eff_veto_soft,
                           v1_strong     = eff_v1_strong,
                           low_v2_rescue = eff_low_v2_rescue) {
  n <- length(p1)
  out  <- rep("Other", n)
  rule <- rep("other_default", n)

  # 1) NOTSevere by V2 threshold
  idx_notsev <- p2 >= thr_v2
  out[idx_notsev]  <- "NOTSevere"
  rule[idx_notsev] <- "v2>=thr_v2"

  # 2) Severe by V1 + margin + veto/override (only where not already NOTSevere)
  idx_cand <- !idx_notsev &
    (p1 >= thr_v1) &
    ((p1 - p2) >= margin) &
    ( (p2 < veto_soft) | (p1 >= v1_strong) | (p2 < low_v2_rescue) )

  out[idx_cand]  <- "Severe"
  rule[idx_cand] <- dplyr::case_when(
    p2[idx_cand] < low_v2_rescue ~ "severe_low_v2_rescue",
    p1[idx_cand] >= v1_strong    ~ "severe_v1_strong",
    p2[idx_cand] < veto_soft     ~ "severe_v2_soft_veto",
    TRUE                         ~ "severe_margin"
  )

  list(
    decision = factor(out, levels = c("Other","Severe","NOTSevere")),
    rule     = rule
  )
}

# ---------------------------------------------------------------------
# CORS
# ---------------------------------------------------------------------
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  forward()
}

# --- simple tracing helper ---
.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

.log <- function(...) { cat(sprintf("[%s] ", .now()), ..., "\n"); flush.console() }

#* @post /s2_warmup
function(req, res) {
  limit_threads()
  .log("S2 warmup: loading v3...")
  v3 <- get_fit("v3", V3_PATH); invisible(v3$bst); rm(v3); gc()
  Sys.sleep(0.3)

  .log("S2 warmup: loading v4...")
  v4 <- get_fit("v4", V4_PATH); invisible(v4$bst); rm(v4); gc()
  Sys.sleep(0.3)

  .log("S2 warmup: loading v5...")
  v5 <- get_fit("v5", V5_PATH); invisible(v5$bst); rm(v5); gc()
  Sys.sleep(0.3)

  .log("S2 warmup: done.")
  list(ok = TRUE, warmed = c("v3","v4","v5"))
}

#* @get /s2_trace
function() {
  list(
    models_loaded = list(v3 = is_loaded("v3"), v4 = is_loaded("v4"), v5 = is_loaded("v5")),
    mem = as.list(gc()[, c("used","gc trigger","max used")])
  )
}

# ---------------------------------------------------------------------
# Health / schema
# ---------------------------------------------------------------------
#* @get /healthz
function() list(
  ok = TRUE,
  service = "S1+S2",
  defaults = list(
    thr_v1        = eff_thr_v1,
    thr_v2        = eff_thr_v2,
    margin        = eff_margin,
    veto_soft     = eff_veto_soft,
    v1_strong     = eff_v1_strong,
    low_v2_rescue = eff_low_v2_rescue,
    # S2 thresholds from meta only (won't load models):
    thr_v3        = if (!is.null(v3_meta$threshold)) as.numeric(v3_meta$threshold) else NA_real_,
    thr_v4        = if (!is.null(v4_meta$threshold)) as.numeric(v4_meta$threshold) else NA_real_,
    thr_v5        = if (!is.null(v5_meta$threshold)) as.numeric(v5_meta$threshold) else NA_real_
  ),
  s2_models_loaded = list(
    v3 = is_loaded("v3"),
    v4 = is_loaded("v4"),
    v5 = is_loaded("v5")
  )
)

#* @get /schema
function() {
  # S1: lightweight summary from metas only
  s1 <- list(
    v1 = list(threshold = eff_thr_v1),
    v2 = list(threshold = eff_thr_v2)
  )
  # S2: list the feature names from meta if present (or NULL)
  s2 <- list(
    v3_threshold = if (!is.null(v3_meta$threshold)) as.numeric(v3_meta$threshold) else NA_real_,
    v4_threshold = if (!is.null(v4_meta$threshold)) as.numeric(v4_meta$threshold) else NA_real_,
    v5_threshold = if (!is.null(v5_meta$threshold)) as.numeric(v5_meta$threshold) else NA_real_,
    v3_features = v3_meta$features %||% NULL,
    v4_features = v4_meta$features %||% NULL,
    v5_features = v5_meta$features %||% NULL
  )
  list(S1 = s1, S2 = s2)
}

# ---------------------------------------------------------------------
# Inference
# ---------------------------------------------------------------------
#* @post /s1_infer
function(req, res) {
  # Lazy-load S1 models
  v1_obj <- get_fit("v1", V1_PATH)
  v2_obj <- get_fit("v2", V2_PATH)
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_in <- as.data.frame(body$features, stringsAsFactors = FALSE)

  # Ensure at least a 1-row frame
  if (!nrow(feats_in)) feats_in <- feats_in[NA, , drop = FALSE]

  # Align request schema to both workflows (adds missing cols, casts types)
  feats <- ensure_predictor_schema(list(v1_obj, v2_obj), feats_in)
  feats <- add_role_stubs(v1_obj, feats)
  feats <- add_role_stubs(v2_obj, feats)

  # --- Predict ---
  p1 <- try(predict_probs_safe(v1_obj, feats), silent = TRUE)
  if (inherits(p1, "try-error")) {
    res$status <- 400
    return(list(
      error = "V1 prediction failed",
      message = as.character(p1),
      received_schema = lapply(feats, function(x) c(class = paste(class(x), collapse=","), example = utils::head(x,1))),
      missing_predictors_filled_with_NA = unname(attr(feats, "schema_added")),
      predictors_casted_to_expected_type = unname(attr(feats, "schema_casted"))
    ))
  }

  p2 <- try(predict_probs_safe(v2_obj, feats), silent = TRUE)
  if (inherits(p2, "try-error")) {
    res$status <- 400
    return(list(
      error = "V2 prediction failed",
      message = as.character(p2),
      received_schema = lapply(feats, function(x) c(class = paste(class(x), collapse=","), example = utils::head(x,1))),
      missing_predictors_filled_with_NA = unname(attr(feats, "schema_added")),
      predictors_casted_to_expected_type = unname(attr(feats, "schema_casted"))
    ))
  }

  # Extract prob columns
  if (!(".pred_Severe" %in% names(p1))) { res$status <- 500; return(list(error = "v1 prob column .pred_Severe missing")) }
  if (!(".pred_NOTSevere" %in% names(p2))) { res$status <- 500; return(list(error = "v2 prob column .pred_NOTSevere missing")) }

  v1_prob <- p1$.pred_Severe
  v2_prob <- p2$.pred_NOTSevere

  # Decision
  cmb <- combine_margin(
    p1 = v1_prob, p2 = v2_prob,
    thr_v1 = eff_thr_v1, thr_v2 = eff_thr_v2,
    margin = eff_margin, veto_soft = eff_veto_soft,
    v1_strong = eff_v1_strong, low_v2_rescue = eff_low_v2_rescue
  )

  # Minimal info sheet (first row only, for demo)
  sheet <- list(
    sheet_version = 1L,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    patient = list(anon_id = paste0(as.hexmode(sample(0:255, 8, TRUE)), collapse="")),
    context = list(),
    features = list(clinical = body$features, labs = list()),
    s1 = list(
      v1 = list(prob = unname(v1_prob[1]), thr = eff_thr_v1),
      v2 = list(prob = unname(v2_prob[1]), thr = eff_thr_v2),
      decision = as.character(cmb$decision[1]),
      rule = cmb$rule[1],
      params = list(
        margin = eff_margin, veto_soft = eff_veto_soft,
        v1_strong = eff_v1_strong, low_v2_rescue = eff_low_v2_rescue
      ),
      model_hash = "sha256:s1"
    ),
    notes = list()
  )

  resp <- list(
  v1 = list(prob = unname(v1_prob[1]), thr = eff_thr_v1),
  v2 = list(prob = unname(v2_prob[1]), thr = eff_thr_v2),
  s1_decision = as.character(cmb$decision[1]),
  rule = cmb$rule[1],
  warnings = list(
    missing_predictors_filled_with_NA = unname(attr(feats, "schema_added")),
    predictors_casted_to_expected_type = unname(attr(feats, "schema_casted")),
    schema_added_outcomes  = unname(attr(feats, "schema_added_outcomes")),
    schema_added_ids       = unname(attr(feats, "schema_added_ids")),
    schema_added_case_wts  = unname(attr(feats, "schema_added_case_wts"))
  ),
  current_info_sheet = sheet,
  hash = "sha256:s1",
  timing_ms = 0L
)
return(resp)  
}

# ---------------------------------------------------------------------
# S2 inference (v3/v4/v5 -> 4-class with margin-normalised tie-breaks)
# ---------------------------------------------------------------------
#* @post /s2_infer
function(req, res) {
  limit_threads()  # ensure per-request too
  .log("s2_infer: start")

  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_in <- as.data.frame(body$features, stringsAsFactors = FALSE)
  if (!nrow(feats_in)) feats_in <- feats_in[NA, , drop = FALSE]
  if (!"label" %in% names(feats_in)) feats_in$label <- sprintf("row_%s", seq_len(nrow(feats_in)))
  use_cal <- isTRUE(body$apply_calibration) || is.null(body$apply_calibration)

  # ---------- v3 ----------
  .log("s2_infer: loading v3")
  v3_fit <- get_fit("v3", V3_PATH)
  thr3   <- get_thr(v3_fit, v3_meta, 0.50)
  .log("s2_infer: predicting v3")
  p3 <- try(predict_s2_probs(v3_fit, feats_in, calibrated = use_cal), silent = TRUE)
  if (inherits(p3, "try-error")) {
    res$status <- 400
    .log("s2_infer: v3 prediction failed -> ", substr(as.character(p3), 1, 200))
  return(list(error = "v3 prediction failed", message = as.character(p3)))
  }
  .log("s2_infer: v3 done")
  rm(v3_fit); gc(verbose = FALSE)

  # ---------- v4 ----------
  .log("s2_infer: loading v4")
  v4_fit <- get_fit("v4", V4_PATH)
  thr4   <- get_thr(v4_fit, v4_meta, 0.50)
  .log("s2_infer: predicting v4")
  p4 <- try(predict_s2_probs(v4_fit, feats_in, calibrated = use_cal), silent = TRUE)
  if (inherits(p4, "try-error")) {
    res$status <- 400
    .log("s2_infer: v4 prediction failed -> ", substr(as.character(p4), 1, 200))
    return(list(error = "v4 prediction failed", message = as.character(p4)))
  }
  .log("s2_infer: v4 done")
  rm(v4_fit); gc(verbose = FALSE)

  # ---------- v5 ----------
  .log("s2_infer: loading v5")
  v5_fit <- get_fit("v5", V5_PATH)
  thr5   <- get_thr(v5_fit, v5_meta, 0.50)
  .log("s2_infer: predicting v5")
  p5 <- try(predict_s2_probs(v5_fit, feats_in, calibrated = use_cal), silent = TRUE)
  if (inherits(p5, "try-error")) {
  res$status <- 400
  .log("s2_infer: v5 prediction failed -> ", substr(as.character(p5), 1, 200))
  return(list(error = "v5 prediction failed", message = as.character(p5)))
  }
  .log("s2_infer: v5 done")
  rm(v5_fit); gc(verbose = FALSE)

  # ---------- routing ----------
  .log("s2_infer: routing results")
  call <- route_s2(p3, p4, p5, thr3, thr4, thr5)
  f3 <- as.integer(p3 >= thr3); f4 <- as.integer(p4 >= thr4); f5 <- as.integer(p5 >= thr5)
  bit_key <- paste0(f3, f4, f5)

  resp <- tibble::tibble(
    label = feats_in$label,
    p_v3 = p3, p_v4 = p4, p_v5 = p5,
    thr_v3 = thr3, thr_v4 = thr4, thr_v5 = thr5,
    f_v3 = f3, f_v4 = f4, f_v5 = f5,
    bit_key = bit_key,
    call = as.character(call)
  )

  .log("s2_infer: finished")
  jsonlite::toJSON(resp, dataframe = "rows", auto_unbox = TRUE, na = "null")
}


