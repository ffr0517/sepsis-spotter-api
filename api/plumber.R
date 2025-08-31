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

# Pull effective knobs from metas (fallback to defaults)
eff_thr_v1        <- suppressWarnings(as.numeric(v1_meta$threshold));        if (is.na(eff_thr_v1))        eff_thr_v1        <- DEF_THR_V1
eff_thr_v2        <- suppressWarnings(as.numeric(v2_meta$threshold));        if (is.na(eff_thr_v2))        eff_thr_v2        <- DEF_THR_V2
eff_margin        <- suppressWarnings(as.numeric(v1_meta$margin));           if (is.na(eff_margin))        eff_margin        <- DEF_MARGIN
eff_veto_soft     <- suppressWarnings(as.numeric(v2_meta$veto_soft));        if (is.na(eff_veto_soft))     eff_veto_soft     <- DEF_VETO_SOFT
eff_v1_strong     <- suppressWarnings(as.numeric(v1_meta$v1_strong));        if (is.na(eff_v1_strong))     eff_v1_strong     <- DEF_V1_STRONG
eff_low_v2_rescue <- suppressWarnings(as.numeric(v2_meta$low_v2_rescue));    if (is.na(eff_low_v2_rescue)) eff_low_v2_rescue <- DEF_LOW_V2_RESCUE

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
get_required_ptypes <- function(wf) {
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
  req <- union_required_ptypes(models)
  n <- nrow(new_data)
  added <- character()
  casted <- character()

  for (nm in names(req)) {
    ptype <- req[[nm]]
    if (!nm %in% names(new_data)) {
      new_data[[nm]] <- vctrs::vec_init(ptype, n = n)
      added <- c(added, nm)
    } else {
      # Try casting; if it fails, leave as-is (recipe may still handle)
      ok <- try({
        new_data[[nm]] <- vctrs::vec_cast(new_data[[nm]], ptype, x_arg = nm, to_arg = paste(class(ptype), collapse = "/"))
        casted <- c(casted, nm)
      }, silent = TRUE)
      if (inherits(ok, "try-error")) {
        # no-op; we still carry the column as sent
      }
    }
  }

  attr(new_data, "schema_added")  <- added
  attr(new_data, "schema_casted") <- casted
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
  fmt <- function(wf) {
    pt <- get_required_ptypes(wf)
    data.frame(
      name = names(pt),
      type = vapply(pt, function(x) paste(class(x), collapse = "/"), ""),
      stringsAsFactors = FALSE
    )
  }
  list(v1_predictors = fmt(v1_obj),
       v2_predictors = fmt(v2_obj))
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

  jsonlite::toJSON(list(
    v1 = list(prob = unname(v1_prob[1]), thr = eff_thr_v1),
    v2 = list(prob = unname(v2_prob[1]), thr = eff_thr_v2),
    s1_decision = as.character(cmb$decision[1]),
    rule = cmb$rule[1],
    warnings = list(
      missing_predictors_filled_with_NA = unname(attr(feats, "schema_added")),
      predictors_casted_to_expected_type = unname(attr(feats, "schema_casted"))
    ),
    current_info_sheet = sheet,
    hash = "sha256:s1",
    timing_ms = 0L
  ), auto_unbox = TRUE)
}


