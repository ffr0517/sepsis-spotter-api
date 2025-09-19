# Set up -----
library(plumber)
library(jsonlite)
library(dplyr)
library(workflows)
library(rlang)
library(vctrs)

# Control model caching for low-RAM instances
CACHE_MODELS <- tolower(Sys.getenv("CACHE_MODELS", "false")) %in% c("1","true","t","yes","y")

`%||%` <- function(x, y) if (is.null(x)) y else x

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
    invisible(gc())
  }
}

is_loaded <- function(key) exists(key, envir = .load_env, inherits = FALSE)

# Meta lazy loader
.meta_env <- new.env(parent = emptyenv())
get_meta <- function(key, path) {
  if (exists(key, envir = .meta_env, inherits = FALSE)) {
    return(get(key, envir = .meta_env, inherits = FALSE))
  }
  obj <- readRDS(path)
  assign(key, obj, envir = .meta_env)
  obj
}

# Model paths
V1_PATH <- "models/v1_model.rds"
V2_PATH <- "models/v2_model.rds"
V3_PATH <- "models/v3_model.rds"
V4_PATH <- "models/v4_model.rds"
V5_PATH <- "models/v5_model.rds"

v1_meta <- get_meta("v1_meta", "models/v1_meta.rds")
v2_meta <- get_meta("v2_meta", "models/v2_meta.rds")
v3_meta <- get_meta("v3_meta", "models/v3_meta.rds")
v4_meta <- get_meta("v4_meta", "models/v4_meta.rds")
v5_meta <- get_meta("v5_meta", "models/v5_meta.rds")

# ---------------------------------------------------------------------
# Defaults / knobs (used if meta doesn't define them)
# ---------------------------------------------------------------------
DEF_THR_V1        <- 0.025
DEF_THR_V2        <- 0.20

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
# Re-usable helpers so we don't duplicate inside predict_s2_probs()
# and so /s2_infer can reuse a single baked frame across v3/v4/v5.
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


predict_s2_probs <- function(fit, new_data, calibrated = TRUE) {
  stopifnot(!is.null(fit$prep), !is.null(fit$bst))

  # 1) Cast known predictor ptypes but DO NOT drop other cols
  if (!is.null(fit$predictor_ptypes)) {
    new_data <- cast_to_ptypes(new_data, fit$predictor_ptypes)
  } else {
    new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)
  }

  # 2) Ensure ALL variables referenced by the recipe exist
  needed <- unique(summary(fit$prep)$variable)

  proto_pool <- list()
  if (!is.null(fit$predictor_ptypes)) proto_pool <- c(proto_pool, fit$predictor_ptypes)
  if (!is.null(fit$id_role_ptypes))   proto_pool <- c(proto_pool, fit$id_role_ptypes)
  if (!is.null(fit$outcome_ptypes))   proto_pool <- c(proto_pool, fit$outcome_ptypes)

  miss <- setdiff(needed, names(new_data))
  if (length(miss)) {
    suffix <- if (length(miss) > 12) " … (more)" else ""
    .log("s2_infer: adding missing vars -> ", paste(utils::head(miss, 12), collapse = ", "), suffix)
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

    # ---- SMART IMPUTE + FACTORIZE (single pass!) --------------------------
  bin_cat <- c(
    "bgcombyn","adm.recent","waste","stunt","prior.care","travel.time.bin",
    "diarrhoeal","ensapro","vomit.all","seiz","pfacleth","crt.long",
    "not.alert","danger.sign","syndrome.resp","syndrome.nonresp","pneumo",
    "sev.pneumo","infection","parenteral_screen"
  )

  if (length(intersect(bin_cat, names(new_data)))) {
    for (nm in intersect(bin_cat, names(new_data))) {
      x <- new_data[[nm]]
      # NA / "" -> 0; coerce to integer; clamp to {0,1}; THEN factor with levels 0,1
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
  # ----------------------------------------------------------------------

  # 3) Bake safely
  baked <- bake(fit$prep, new_data = new_data)

  # 4) Align to training feature set (add any missing with 0)
  feats <- fit$features
  miss_feats <- setdiff(feats, names(baked))
  if (length(miss_feats)) {
    for (mc in miss_feats) baked[[mc]] <- 0
  }
  baked <- baked[, feats, drop = FALSE]

  # 5) Build sparse design without dense copy
  fml <- as.formula(paste("~", paste(sprintf("`%s`", feats), collapse = " + "), "- 1"))
  X <- Matrix::sparse.model.matrix(fml, data = baked)
  rm(baked); gc(verbose = FALSE)

  # Fallback if design ended up empty for any reason
  if (is.null(dim(X)) || ncol(X) == 0L) {
    .log("s2_infer: sparse matrix had 0 cols; injecting bias col")
    X <- Matrix::Matrix(0, nrow = nrow(new_data), ncol = 1, sparse = TRUE)
    colnames(X) <- ".bias0"
  }

  # 6) Predict with one thread
  p_raw <- predict(fit$bst, newdata = as.matrix(X), nthread = 1)
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
combine_thresholds <- function(p1, p2,
                               thr_v1 = eff_thr_v1,
                               thr_v2 = eff_thr_v2) {
  n <- length(p1)
  call_severe <- !is.na(p1) & (p1 >= thr_v1)
  call_notsev <- !is.na(p2) & (p2 >= thr_v2)

  # Priority:
  # - Severe if only v1 crosses
  # - NOTSevere if only v2 crosses
  # - "Other" if both cross (conflict) OR neither crosses
  decision <- ifelse(call_severe & !call_notsev, "Severe",
              ifelse(!call_severe & call_notsev, "NOTSevere", "Other"))

  rule <- ifelse(call_severe & !call_notsev, "v1>=thr_v1",
          ifelse(!call_severe & call_notsev, "v2>=thr_v2",
          ifelse(call_severe & call_notsev, "conflict_both", "neither_meet")))

  list(
    decision = factor(decision, levels = c("Other","Severe","NOTSevere")),
    rule = rule
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
  if (!CACHE_MODELS) {
    .log("S2 warmup skipped (CACHE_MODELS=FALSE).")
    return(list(ok = TRUE, warmed = character()))
  }
  .log("S2 warmup: loading v3..."); v3 <- get_fit("v3", V3_PATH); invisible(v3$bst)
  .log("S2 warmup: loading v4..."); v4 <- get_fit("v4", V4_PATH); invisible(v4$bst)
  .log("S2 warmup: loading v5..."); v5 <- get_fit("v5", V5_PATH); invisible(v5$bst)
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

#* @post /admin/release_all
function() {
  for (k in c("v1","v2","v3","v4","v5")) release_fit(k)
  invisible(gc())
  list(ok = TRUE, released = c("v1","v2","v3","v4","v5"))
}

# ---------------------------------------------------------------------
# Health / schema
# ---------------------------------------------------------------------
#* @get /healthz
function() list(
  ok = TRUE,
  service = "S1+S2",
  defaults = list(
    thr_v1 = eff_thr_v1,
    thr_v2 = eff_thr_v2,
    thr_v3 = if (!is.null(v3_meta$threshold)) as.numeric(v3_meta$threshold) else NA_real_,
    thr_v4 = if (!is.null(v4_meta$threshold)) as.numeric(v4_meta$threshold) else NA_real_,
    thr_v5 = if (!is.null(v5_meta$threshold)) as.numeric(v5_meta$threshold) else NA_real_
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
  on.exit({
    if (!CACHE_MODELS) {
      release_fit("v1")
      release_fit("v2")
    }
    gc(verbose = FALSE) # Force garbage collection
    .log("s1_infer: Cleaned up S1 models.")
  }, add = TRUE)

  # Lazy-load S1 models
  v1_obj <- get_fit("v1", V1_PATH)
  v2_obj <- get_fit("v2", V2_PATH)
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_in <- as.data.frame(body$features, stringsAsFactors = FALSE)

  # Ensure at least a 1-row frame
  if (!nrow(feats_in)) feats_in <- feats_in[NA, , drop = FALSE]

  # Align request schema to both workflows
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
  cmb <- combine_thresholds(
  p1 = v1_prob, p2 = v2_prob,
  thr_v1 = eff_thr_v1, thr_v2 = eff_thr_v2
)

  # Minimal info sheet
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
      params = list(mode = "thr_only_conflict=Other"),
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
# S2 inference (v3/v4/v5 -> 4-class with margin-normalised tie-breaks)
# ---------------------------------------------------------------------
#* @post /s2_infer
function(req, res) {
  .log("s2_infer: start")

  # ---- helper: load -> predict -> free (point B) --------------------
  score_one_version <- function(key, path, meta_obj, new_data, calibrated, res) {
    .log("s2_infer: loading ", key)
    fit <- get_fit(key, path)                  # cache-aware loader
    thr <- get_thr(fit, meta_obj, 0.50)

    .log("s2_infer: predicting ", key)
    p <- try(predict_s2_probs(fit, new_data, calibrated = calibrated), silent = TRUE)

    # Drop the booster + prep immediately, then GC; optionally release cache
    rm(fit); gc(verbose = FALSE)
    if (!CACHE_MODELS) release_fit(key)
    gc(FALSE); gc(TRUE)

    if (inherits(p, "try-error")) {
      res$status <- 400
      .log("s2_infer: ", key, " prediction failed -> ", substr(as.character(p), 1, 240))
      return(list(failed = TRUE,
                  payload = list(error = paste(key, "prediction failed"),
                                 message = as.character(p))))
    }
    list(failed = FALSE, thr = thr, p = p)
  }

  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_in <- tryCatch(
    tibble::as_tibble_row(body$features),
    error = function(e) as.data.frame(body$features, stringsAsFactors = FALSE)
    )
  if (!nrow(feats_in)) feats_in <- feats_in[NA, , drop = FALSE]
  if (!"label" %in% names(feats_in)) feats_in$label <- sprintf("row_%s", seq_len(nrow(feats_in)))
  use_cal <- isTRUE(body$apply_calibration) || is.null(body$apply_calibration)

  on.exit({
    if (!CACHE_MODELS) { release_fit("v3"); release_fit("v4"); release_fit("v5") }
    invisible(gc())
  }, add = TRUE)

  # ---------- schema padding (critical) ----------
  n <- nrow(feats_in)

  # Ensure training-remembered basics
  add_if_missing <- function(df, nm, val) { if (!nm %in% names(df)) df[[nm]] <- val; df }
  feats_in <- add_if_missing(feats_in, "w_final", rep(1, n))
  feats_in <- add_if_missing(feats_in, ".case_w", rep(1, n))

    LAB_VARS <- c("ANG1","ANG2","CHI3L","CRP","CXCl10","IL1ra","IL6","IL8","IL10",
                "PROC","TNFR1","STREM1","VEGFR1","supar","lblac","lbglu","enescbchb1")

  labs_present <- intersect(LAB_VARS, names(feats_in))
  have_labs_n <- if (length(labs_present)) sum(!is.na(unlist(feats_in[, labs_present, drop = FALSE]))) else 0L

  # Robust checks even if columns are missing entirely
  na_oxy  <- (!"oxy.ra"   %in% names(feats_in))  || isTRUE(is.na(feats_in$oxy.ra[1]))
  na_sirs <- (!"SIRS_num" %in% names(feats_in)) || isTRUE(is.na(feats_in$SIRS_num[1]))
  na_both_oxy_sirs <- na_oxy && na_sirs

  allow_heavy_impute <- isTRUE(body$allow_heavy_impute)

  if (!allow_heavy_impute && have_labs_n < 1 && na_both_oxy_sirs) {
    res$status <- 422
    .log("s2_infer: aborted early (too sparse: no labs + oxy/SIRS missing)")
    return(list(
      error = "S2 requires more signal",
      reason = "no_labs_and_no_oxy_or_SIRS",
      needed = list(any_lab = LAB_VARS, or = c("oxy.ra", "SIRS_num"))
    ))
  }

  # Outcomes remembered by the recipe; give factor levels to be safe
  lvl_sev  <- c("Other","Severe")
  lvl_psev <- c("Other","Probable Severe")
  lvl_pns  <- c("Other","Probable Non-Severe")
  if (!"SFI_bin_severe_vs_other" %in% names(feats_in))
    feats_in$SFI_bin_severe_vs_other <- factor(rep("Other", n), levels = lvl_sev)
  if (!"SFI_bin_probSev_vs_other" %in% names(feats_in))
    feats_in$SFI_bin_probSev_vs_other <- factor(rep("Other", n), levels = lvl_psev)
  if (!"SFI_bin_probNS_vs_other" %in% names(feats_in))
    feats_in$SFI_bin_probNS_vs_other  <- factor(rep("Other", n), levels = lvl_pns)

  # >>> PLACE THE NEW STUB LOOP HERE <<<
  # Stub any remembered training columns the recipe references
  for (nm in S2_REQUIRED_STUB_COLS) {
    if (!nm %in% names(feats_in)) {
      # type doesn't matter; step_rm() will drop them
      feats_in[[nm]] <- NA_real_
    }
  }

  factorish <- c("site","ipdopd","sex","bgcombyn","adm.recent","waste","stunt",
                 "prior.care","travel.time.bin","urti","lrti","diarrhoeal","neuro",
                 "auf","ensapro","vomit.all","seiz","pfacleth","parenteral_screen","SFI_5cat")
  for (nm in intersect(factorish, names(feats_in))) {
    if (!is.factor(feats_in[[nm]])) feats_in[[nm]] <- factor(feats_in[[nm]])
  }

  # 2b) Pad ALL model predictors so downstream casting doesn’t fail
  miss_pred <- setdiff(S2_PREDICTORS, names(feats_in))
  if (length(miss_pred)) for (nm in miss_pred) feats_in[[nm]] <- NA

  # ---------- v3 load (for baking) ----------
.log("s2_infer: loading v3 (baseline bake)")
v3_fit <- get_fit("v3", V3_PATH)  # keep object to bake
thr3   <- get_thr(v3_fit, v3_meta, 0.50)

# Bake ONCE with v3 recipe (covers indicate_na, bagged impute, dummies, etc.)
.log("s2_infer: baking once via v3 recipe")
baked_once <- bake_once_with(v3_fit, feats_in)

# DEBUG STEP (TEMPORARY)
.log("DEBUG START: Feature Name Comparison for v3")
baked_names <- names(baked_once)
expected_names <- v3_fit$features
in_both <- intersect(baked_names, expected_names)
only_in_baked <- setdiff(baked_names, expected_names)
only_in_expected <- setdiff(expected_names, baked_names)

.log("DEBUG: Features present in BOTH (first 10): ", paste(head(in_both, 10), collapse=", "))
.log("DEBUG: Features ONLY in recipe output (first 10): ", paste(head(only_in_baked, 10), collapse=", "))
.log("DEBUG: Features ONLY in model's list (first 10): ", paste(head(only_in_expected, 10), collapse=", "))
.log("DEBUG END: Feature Name Comparison for v3")

# Which meta-probs does v3 actually use?
meta_in_v3 <- intersect(v3_fit$features,
                        c("v1_pred_Severe","v1_pred_Other","v2_pred_NOTSevere","v2_pred_Other"))
.log(paste("s2_infer: v3 uses meta probs ->", paste(meta_in_v3, collapse=", ")))

# Do we have a 1 in the not.alert dummy?
.log(paste("s2_infer: has not.alert_X1 in baked:", "not.alert_X1" %in% names(baked_once)))
if ("not.alert_X1" %in% names(baked_once)) {
  .log(paste("s2_infer: not.alert_X1 value =", baked_once$not.alert_X1[1]))
}

# Sanity check after bake
.log(sprintf("s2_infer: nrow feats_in=%s, nrow baked=%s, ncol baked=%s",
             nrow(feats_in), nrow(baked_once), ncol(baked_once)))

if (nrow(baked_once) == 0L) {
  res$status <- 400
  return(list(
    error = "empty_baked_frame",
    note  = "Recipe returned 0 rows; cannot score.",
    nrow_feats_in   = nrow(feats_in),
    names_in_request = names(feats_in),
    ncol_baked      = ncol(baked_once),
    baked_colnames  = names(baked_once),
    need_features   = head(v3_fit$features, 20L)
  ))
}

# Predict v3 using reused baked data
.log("s2_infer: predicting v3")
X3 <- build_dense_for(baked_once, v3_fit$features)
if (all(X3 == 0)) {
  res$status <- 422
  return(list(
    error = "all_zero_feature_vector",
    note  = "After recipe + alignment to v3 features, no non-zero entries.",
    hint  = "Provide at least one non-zero predictor used by v3 (a lab, a meta-prob, or a binary=1 flag that maps to a v3 dummy)."
  ))
}

# align names/order to booster, then densify for predict
X3 <- .align_for_booster_matrix(X3, v3_fit$bst, fallback_names = v3_fit$features)
p3_raw <- predict(v3_fit$bst, newdata = as.matrix(X3), nthread = 1)
rm(X3); gc(FALSE)
p3 <- if (isTRUE(use_cal)) apply_calibrator(p3_raw, v3_fit$calibrator) else p3_raw
rm(p3_raw); if (!CACHE_MODELS) release_fit("v3"); rm(v3_fit); gc()

# ---------- v4 ----------
.log("s2_infer: loading v4")
v4_fit <- get_fit("v4", V4_PATH); thr4 <- get_thr(v4_fit, v4_meta, 0.50)
.log("s2_infer: predicting v4 (reuse baked)")
X4 <- build_dense_for(baked_once, v4_fit$features)
if (all(X4 == 0)) {
  res$status <- 422
  return(list(
    error = "all_zero_feature_vector",
    note  = "After recipe + alignment to v4 features, no non-zero entries.",
    hint  = "Provide at least one non-zero predictor used by v4 (a lab, a meta-prob, or a binary=1 flag that maps to a v3 dummy)."
  ))
}

# align names/order to booster, then densify for predict
X4 <- .align_for_booster_matrix(X4, v4_fit$bst, fallback_names = v4_fit$features)
p4_raw <- predict(v4_fit$bst, newdata = as.matrix(X4), nthread = 1)
rm(X4); gc(FALSE)
p4 <- if (isTRUE(use_cal)) apply_calibrator(p4_raw, v4_fit$calibrator) else p4_raw
rm(p4_raw); if (!CACHE_MODELS) release_fit("v4"); rm(v4_fit); gc()

# ---------- v5 ----------
.log("s2_infer: loading v5")
v5_fit <- get_fit("v5", V5_PATH); thr5 <- get_thr(v5_fit, v5_meta, 0.50)
.log("s2_infer: predicting v5 (reuse baked)")
X5 <- build_dense_for(baked_once, v5_fit$features)
if (all(X5 == 0)) {
  res$status <- 522
  return(list(
    error = "all_zero_feature_vector",
    note  = "After recipe + alignment to v5 features, no non-zero entries.",
    hint  = "Provide at least one non-zero predictor used by v5 (a lab, a meta-prob, or a binary=1 flag that maps to a v3 dummy)."
  ))
}

# align names/order to booster, then densify for predict
X5 <- .align_for_booster_matrix(X5, v5_fit$bst, fallback_names = v5_fit$features)
p5_raw <- predict(v5_fit$bst, newdata = as.matrix(X5), nthread = 1)
rm(X5); gc(FALSE)
p5 <- if (isTRUE(use_cal)) apply_calibrator(p5_raw, v5_fit$calibrator) else p5_raw
rm(p5_raw); if (!CACHE_MODELS) release_fit("v5"); rm(v5_fit); gc()
rm(baked_once); gc(FALSE); gc(TRUE)

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
  return(resp)
}






