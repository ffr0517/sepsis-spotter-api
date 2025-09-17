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

# Control model caching for low-RAM instances
CACHE_MODELS <- tolower(Sys.getenv("CACHE_MODELS", "true")) %in% c("1","true","t","yes","y")

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- simple tracing helper ---
.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.log <- function(...) { cat(sprintf("[%s] ", .now()), ..., "\n"); flush.console() }


# ---------------------------------------------------------------------
# Global Hooks & Filters
# ---------------------------------------------------------------------

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  plumber::forward()
}

#* @plumber
function(pr) {
  pr %>%
    plumber::pr_hook("exit", function() {
      gc()
      .log("Global Hook: Garbage collection triggered after request.")
    })
}


# S2 predictors (clinical + labs + meta from s1) ----
S2_PREDICTORS <- c(
  # Clinical predictors
  "age.months","sex","bgcombyn","adm.recent","wfaz","waste","stunt","cidysymp",
  "prior.care","travel.time.bin","diarrhoeal","pneumo","sev.pneumo","ensapro",
  "vomit.all","seiz","pfacleth","not.alert","danger.sign","hr.all","rr.all",
  "oxy.ra","envhtemp","crt.long","parenteral_screen","SIRS_num",
  
  # Biomarkers / lab assays
  "ANG1","ANG2","CHI3L","CRP","CXCl10","IL1ra","IL6","IL8","IL10","PROC",
  "TNFR1","STREM1","VEGFR1","supar","lblac","lbglu","enescbchb1",
  
  # Meta from S1
  "v1_pred_Severe", "v1_pred_Other", "v2_pred_NOTSevere", "v2_pred_Other"
)

S2_REQUIRED_STUB_COLS <- c(
  "label","w_final",".case_w",
  "SFI_bin_severe_vs_other","SFI_bin_probSev_vs_other","SFI_bin_probNS_vs_other",
  "site","ipdopd","outcome.binary","infection","weight","weight_bin",
  "strata_var","n_stratum","p_stratum","SFI_5cat",
  # NEW (from error log)
  "urti","lrti","neuro","auf","LqSOFA","syndrome.resp","syndrome.nonresp",
  "prop_handoff","ipw"
)

# ---------------------------------------------------------------------
# Lazy model loader
# ---------------------------------------------------------------------
.load_env <- new.env(parent = emptyenv())

get_fit <- function(key, path, cache = CACHE_MODELS) {
  if (cache && exists(key, envir = .load_env, inherits = FALSE)) {
    return(get(key, envir = .load_env, inherits = FALSE))
  }
  obj <- readRDS(path)
  if (cache) assign(key, obj, envir = .load_env)
  obj
}

release_fit <- function(key) {
  if (exists(key, envir = .load_env, inherits = FALSE)) {
    rm(list = key, envir = .load_env)
  }
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
  library(tibble)
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
  if (length(miss)) {
    for (nm in miss) df[[nm]] <- vctrs::vec_init(ptypes[[nm]], n)
  }

  # Normalize many binary encodings to "0"/"1"
  to_bin_char <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) return(ifelse(isTRUE(x), "1", "0"))
    x <- trimws(tolower(as.character(x)))
    ifelse(x %in% c("1","true","t","yes","y","m","male"), "1",
           ifelse(x %in% c("0","false","f","no","n","female","fem"), "0", x))
  }

  # Cast existing to prototype classes
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


# --- Shared bake-once + sparse builders (top-level) --------------------
bake_once_with <- function(fit, new_data) {
  # Cast predictor types if saved
  if (!is.null(fit$predictor_ptypes)) {
    new_data <- cast_to_ptypes(new_data, fit$predictor_ptypes)
  } else {
    new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)
  }

  # Ensure all recipe-referenced vars exist
  needed <- unique(summary(fit$prep)$variable)
  proto_pool <- list()
  if (!is.null(fit$predictor_ptypes)) proto_pool <- c(proto_pool, fit$predictor_ptypes)
  if (!is.null(fit$id_role_ptypes))   proto_pool <- c(proto_pool, fit$id_role_ptypes)
  if (!is.null(fit$outcome_ptypes))   proto_pool <- c(proto_pool, fit$outcome_ptypes)

  miss <- setdiff(needed, names(new_data))
  if (length(miss)) {
    n <- nrow(new_data)
    for (nm in miss) {
      proto <- proto_pool[[nm]]
      if (!is.null(proto)) {
        new_data[[nm]] <- vctrs::vec_init(proto, n)
      } else if (grepl("^(label|site|ipdopd)$", nm)) {
        new_data[[nm]] <- rep(NA_character_, n)
      } else if (grepl("^(urti|lrti|neuro|auf|prop_handoff)$", nm)) {
        new_data[[nm]] <- rep(NA_integer_, n)
      } else if (grepl("^syndrome\\.(resp|nonresp)$", nm)) {
        new_data[[nm]] <- rep(NA_integer_, n)
      } else if (grepl("^(ipw|LqSOFA)$", nm)) {
        new_data[[nm]] <- rep(NA_real_, n)
      } else if (grepl("^(na[_]|na_ind_|.*(_X0|_X1|_unknown|_new)$)", nm)) {
        new_data[[nm]] <- rep(NA_integer_, n)
      } else {
        new_data[[nm]] <- rep(NA_real_, n)
      }
    }
  }

  # Smart factorize (binary to {0,1} factors)
  bin_cat <- c(
    "bgcombyn","adm.recent","waste","stunt","prior.care","travel.time.bin",
    "diarrhoeal","ensapro","vomit.all","seiz","pfacleth","crt.long",
    "not.alert","danger.sign","syndrome.resp","syndrome.nonresp","pneumo",
    "sev.pneumo","infection","parenteral_screen"
  )
  if (length(intersect(bin_cat, names(new_data)))) {
    for (nm in intersect(bin_cat, names(new_data))) {
      x <- new_data[[nm]]
      if (is.character(x)) {
        x <- trimws(x); x[x == ""] <- NA
        xi <- suppressWarnings(as.integer(x))
      } else if (is.logical(x)) {
        xi <- as.integer(x)
      } else {
        xi <- suppressWarnings(as.integer(x))
      }
      xi[is.na(xi)] <- 0L
      xi[!(xi %in% c(0L,1L))] <- 0L
      new_data[[nm]] <- factor(xi, levels = c(0L,1L))
    }
  }

  # Bake once using the fit's recipe
  bake(fit$prep, new_data = new_data)
}

build_dense_for <- function(baked, features) {
  # Ensure all expected features exist
  miss_feats <- setdiff(features, names(baked))
  if (length(miss_feats)) for (mc in miss_feats) baked[[mc]] <- 0

  # Subset & coerce every column to numeric without model.matrix()
  df <- baked[, features, drop = FALSE]
  num_df <- as.data.frame(lapply(df, function(x) {
    if (is.numeric(x)) return(x)
    if (is.integer(x)) return(as.numeric(x))
    if (is.logical(x)) return(as.numeric(x))
    if (is.factor(x))  return(suppressWarnings(as.numeric(as.character(x))))
    suppressWarnings(as.numeric(x))
  }), stringsAsFactors = FALSE)

  mm <- as.matrix(num_df)
  colnames(mm) <- features
  mm
}


# ---------------------------------------------------------------------
# Decision logic & Other Helpers
# ---------------------------------------------------------------------

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

.align_for_booster_matrix <- function(M, booster, fallback_names = NULL) {
  feat_obj <- booster$feature_names
  if (is.null(feat_obj) || !length(feat_obj)) feat_obj <- fallback_names %||% colnames(M)

  missing <- setdiff(feat_obj, colnames(M))
  extra   <- setdiff(colnames(M), feat_obj)
  if (length(missing) || length(extra)) {
    stop(sprintf("feature_name_mismatch: missing=%s; extra=%s",
                 paste(utils::head(missing, 10), collapse=","),
                 paste(utils::head(extra, 10), collapse=",")))
  }

  M <- M[, match(feat_obj, colnames(M)), drop = FALSE]
  colnames(M) <- feat_obj
  M
}


# ---------------------------------------------------------------------
# Endpoints
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
    thr_v3        = if (!is.null(v3_meta$threshold)) as.numeric(v3_meta$threshold) else NA_real_,
    thr_v4        = if (!is.null(v4_meta$threshold)) as.numeric(v4_meta$threshold) else NA_real_,
    thr_v5        = if (!is.null(v5_meta$threshold)) as.numeric(v5_meta$threshold) else NA_real_
  ),
  models_loaded = list(
    v1 = is_loaded("v1"), v2 = is_loaded("v2"),
    v3 = is_loaded("v3"), v4 = is_loaded("v4"), v5 = is_loaded("v5")
  )
)

#* @get /schema
function() {
  s1 <- list( v1 = list(threshold = eff_thr_v1), v2 = list(threshold = eff_thr_v2) )
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


#* @post /s1_infer
function(req, res) {
  on.exit({
    if (!CACHE_MODELS) {
      release_fit("v1")
      release_fit("v2")
    }
  }, add = TRUE)

  # Lazy-load S1 models
  v1_obj <- get_fit("v1", V1_PATH)
  v2_obj <- get_fit("v2", V2_PATH)
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_in <- as.data.frame(body$features, stringsAsFactors = FALSE)

  if (!nrow(feats_in)) feats_in <- feats_in[NA, , drop = FALSE]

  # Align request schema to both workflows
  feats <- ensure_predictor_schema(list(v1_obj, v2_obj), feats_in)
  feats <- add_role_stubs(v1_obj, feats)
  feats <- add_role_stubs(v2_obj, feats)

  # --- Predict ---
  p1 <- try(predict_probs_safe(v1_obj, feats), silent = TRUE)
  if (inherits(p1, "try-error")) {
    res$status <- 400
    return(list( error = "V1 prediction failed", message = as.character(p1) ))
  }

  p2 <- try(predict_probs_safe(v2_obj, feats), silent = TRUE)
  if (inherits(p2, "try-error")) {
    res$status <- 400
    return(list( error = "V2 prediction failed", message = as.character(p2) ))
  }

  if (!(".pred_Severe" %in% names(p1))) { res$status <- 500; return(list(error = "v1 prob column .pred_Severe missing")) }
  if (!(".pred_NOTSevere" %in% names(p2))) { res$status <- 500; return(list(error = "v2 prob column .pred_NOTSevere missing")) }

  v1_prob <- p1$.pred_Severe
  v2_prob <- p2$.pred_NOTSevere

  # Decision
  cmb <- combine_margin(p1 = v1_prob, p2 = v2_prob)

  # Minimal info sheet
  sheet <- list(
    sheet_version = 1L,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    patient = list(anon_id = paste0(as.hexmode(sample(0:255, 8, TRUE)), collapse="")),
    features = list(clinical = body$features, labs = list()),
    s1 = list(
      v1 = list(prob = unname(v1_prob[1]), thr = eff_thr_v1),
      v2 = list(prob = unname(v2_prob[1]), thr = eff_thr_v2),
      decision = as.character(cmb$decision[1]),
      rule = cmb$rule[1]
    )
  )

  resp <- list(
    v1 = list(prob = unname(v1_prob[1]), thr = eff_thr_v1),
    v2 = list(prob = unname(v2_prob[1]), thr = eff_thr_v2),
    s1_decision = as.character(cmb$decision[1]),
    rule = cmb$rule[1],
    current_info_sheet = sheet
  )
  return(resp)
}


#* @post /s2_infer
function(req, res) {
  limit_threads()
  .log("s2_infer: start")

  on.exit({
    if (!CACHE_MODELS) {
      release_fit("v3")
      release_fit("v4")
      release_fit("v5")
    }
  }, add = TRUE)

  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_in <- tryCatch(
    tibble::as_tibble_row(body$features),
    error = function(e) as.data.frame(body$features, stringsAsFactors = FALSE)
  )
  if (!nrow(feats_in)) feats_in <- feats_in[NA, , drop = FALSE]
  if (!"label" %in% names(feats_in)) feats_in$label <- sprintf("row_%s", seq_len(nrow(feats_in)))
  use_cal <- isTRUE(body$apply_calibration) || is.null(body$apply_calibration)

  n <- nrow(feats_in)

  add_if_missing <- function(df, nm, val) { if (!nm %in% names(df)) df[[nm]] <- val; df }
  feats_in <- add_if_missing(feats_in, "w_final", rep(1, n))
  feats_in <- add_if_missing(feats_in, ".case_w", rep(1, n))

  # Early exit if too sparse
  LAB_VARS <- c("ANG1","ANG2","CHI3L","CRP","CXCl10","IL1ra","IL6","IL8","IL10",
                "PROC","TNFR1","STREM1","VEGFR1","supar","lblac","lbglu","enescbchb1")
  labs_present <- intersect(LAB_VARS, names(feats_in))
  have_labs_n <- if (length(labs_present)) sum(!is.na(unlist(feats_in[, labs_present, drop = FALSE]))) else 0L

  na_oxy  <- (!"oxy.ra"   %in% names(feats_in))  || isTRUE(is.na(feats_in$oxy.ra[1]))
  na_sirs <- (!"SIRS_num" %in% names(feats_in)) || isTRUE(is.na(feats_in$SIRS_num[1]))

  if (!isTRUE(body$allow_heavy_impute) && have_labs_n < 1 && (na_oxy && na_sirs)) {
    res$status <- 422
    .log("s2_infer: aborted early (too sparse: no labs + oxy/SIRS missing)")
    return(list( error = "S2 requires more signal", reason = "no_labs_and_no_oxy_or_SIRS" ))
  }

  for (nm in S2_REQUIRED_STUB_COLS) if (!nm %in% names(feats_in)) feats_in[[nm]] <- NA_real_
  miss_pred <- setdiff(S2_PREDICTORS, names(feats_in))
  if (length(miss_pred)) for (nm in miss_pred) feats_in[[nm]] <- NA

  # ---------- Bake once ----------
  .log("s2_infer: loading v3 for baking")
  v3_fit <- get_fit("v3", V3_PATH); thr3 <- get_thr(v3_fit, v3_meta, 0.50)
  .log("s2_infer: baking with v3 recipe")
  baked_once <- bake_once_with(v3_fit, feats_in)

  # ---------- Predict v3 ----------
  .log("s2_infer: predicting v3")
  X3 <- build_dense_for(baked_once, v3_fit$features)
  if (all(X3 == 0)) {
    res$status <- 422; return(list(error = "all_zero_feature_vector_v3"))
  }
  X3 <- .align_for_booster_matrix(X3, v3_fit$bst, fallback_names = v3_fit$features)
  p3_raw <- predict(v3_fit$bst, newdata = X3, nthread = 1)
  p3 <- if (use_cal) apply_calibrator(p3_raw, v3_fit$calibrator) else p3_raw
  rm(v3_fit, X3, p3_raw)

  # ---------- Predict v4 ----------
  .log("s2_infer: loading v4")
  v4_fit <- get_fit("v4", V4_PATH); thr4 <- get_thr(v4_fit, v4_meta, 0.50)
  .log("s2_infer: predicting v4")
  X4 <- build_dense_for(baked_once, v4_fit$features)
  if (all(X4 == 0)) {
    res$status <- 422; return(list(error = "all_zero_feature_vector_v4"))
  }
  X4 <- .align_for_booster_matrix(X4, v4_fit$bst, fallback_names = v4_fit$features)
  p4_raw <- predict(v4_fit$bst, newdata = X4, nthread = 1)
  p4 <- if (use_cal) apply_calibrator(p4_raw, v4_fit$calibrator) else p4_raw
  rm(v4_fit, X4, p4_raw)

  # ---------- Predict v5 ----------
  .log("s2_infer: loading v5")
  v5_fit <- get_fit("v5", V5_PATH); thr5 <- get_thr(v5_fit, v5_meta, 0.50)
  .log("s2_infer: predicting v5")
  X5 <- build_dense_for(baked_once, v5_fit$features)
  if (all(X5 == 0)) {
    res$status <- 422; return(list(error = "all_zero_feature_vector_v5"))
  }
  X5 <- .align_for_booster_matrix(X5, v5_fit$bst, fallback_names = v5_fit$features)
  p5_raw <- predict(v5_fit$bst, newdata = X5, nthread = 1)
  p5 <- if (use_cal) apply_calibrator(p5_raw, v5_fit$calibrator) else p5_raw
  rm(v5_fit, X5, p5_raw, baked_once)

  # ---------- Routing ----------
  .log("s2_infer: routing results")
  call <- route_s2(p3, p4, p5, thr3, thr4, thr5)

  resp <- tibble::tibble(
    label = feats_in$label,
    p_v3 = p3, p_v4 = p4, p_v5 = p5,
    thr_v3 = thr3, thr_v4 = thr4, thr_v5 = thr5,
    call = as.character(call)
  )

  .log("s2_infer: finished")
  return(resp)
}






