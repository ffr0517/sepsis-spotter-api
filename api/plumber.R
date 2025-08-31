# Set up -----
library(plumber)
library(jsonlite)
library(dplyr)
library(workflows)
library(rlang)
library(vctrs)

# ---------------------------------------------------------------------
# Load models + metas
# ---------------------------------------------------------------------
v1_obj  <- readRDS("models/v1_model.rds")
v1_meta <- readRDS("models/v1_meta.rds")
v2_obj  <- readRDS("models/v2_model.rds")
v2_meta <- readRDS("models/v2_meta.rds")

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

# Pull effective knobs from metas (fallback to defaults)
eff_thr_v1        <- get_num(v1_meta$threshold,        DEF_THR_V1)
eff_thr_v2        <- get_num(v2_meta$threshold,        DEF_THR_V2)
eff_margin        <- get_num(v1_meta$margin,           DEF_MARGIN)
eff_veto_soft     <- get_num(v2_meta$veto_soft,        DEF_VETO_SOFT)
eff_v1_strong     <- get_num(v1_meta$v1_strong,        DEF_V1_STRONG)
eff_low_v2_rescue <- get_num(v2_meta$low_v2_rescue,    DEF_LOW_V2_RESCUE)

# ---------------------------------------------------------------------
# Workflow helpers
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# Health / schema
# ---------------------------------------------------------------------
#* @get /healthz
function() list(
  ok = TRUE,
  service = "S1",
  defaults = list(
    thr_v1        = eff_thr_v1,
    thr_v2        = eff_thr_v2,
    margin        = eff_margin,
    veto_soft     = eff_veto_soft,
    v1_strong     = eff_v1_strong,
    low_v2_rescue = eff_low_v2_rescue
  )
)

#* @get /schema
function() {
  fmt <- function(obj) {
    ptp <- get_required_ptypes(obj)
    out <- try(get_required_outcomes(obj), silent = TRUE)
    list(
      predictors = data.frame(name = names(ptp), type = vapply(ptp, function(x) paste(class(x), collapse="/"), "")),
      outcomes   = if (inherits(out, "try-error") || is.null(out)) NULL else
        data.frame(name = names(out), type = vapply(out, function(x) paste(class(x), collapse="/"), ""))
    )
  }
  list(v1 = fmt(v1_obj), v2 = fmt(v2_obj))
}

# ---------------------------------------------------------------------
# Inference
# ---------------------------------------------------------------------
#* @post /s1_infer
function(req, res) {
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


