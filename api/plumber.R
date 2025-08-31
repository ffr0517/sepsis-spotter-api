# Set up -----
library(plumber)
library(jsonlite)
library(dplyr)
library(workflows)
library(rlang)

# Models
v1_obj  <- readRDS("models/v1_model.rds")
v1_meta <- readRDS("models/v1_meta.rds")
v2_obj  <- readRDS("models/v2_model.rds")
v2_meta <- readRDS("models/v2_meta.rds")

# ---------- helpers ----------
as_num <- function(x) suppressWarnings(as.numeric(x))

coerce_schema <- function(feats) {
  # Ensure all expected columns exist
  needed <- c(
    "age.months","sex","bgcombyn","adm.recent","wfaz","waste","stunt","cidysymp",
    "prior.care","travel.time.bin","diarrhoeal","pneumo","sev.pneumo","ensapro",
    "vomit.all","seiz","pfacleth","not.alert","danger.sign","hr.all","rr.all",
    "oxy.ra","envhtemp","crt.long","parenteral_screen","SIRS_num"
  )
  for (nm in setdiff(needed, names(feats))) feats[[nm]] <- NA

  # Coerce numeric-ish
  num_cols <- c("age.months","bgcombyn","adm.recent","wfaz","waste","stunt","cidysymp",
                "prior.care","diarrhoeal","pneumo","sev.pneumo","ensapro","vomit.all",
                "seiz","pfacleth","not.alert","danger.sign","hr.all","rr.all","oxy.ra",
                "envhtemp","crt.long","parenteral_screen","SIRS_num")
  for (nm in intersect(num_cols, names(feats))) feats[[nm]] <- as_num(feats[[nm]])

  # Coerce categoricals commonly seen in training
  # If your recipe already handles character->factor, this is still safe.
  feats$sex <- factor(feats$sex, levels = c("M","F"))
  feats$travel.time.bin <- factor(
    feats$travel.time.bin,
    levels = c("0-30","31-60","61-120",">120") # adjust if your training used different bins
  )

  # Return a plain data.frame
  tibble::as_tibble(feats)
}

predict_probs_safe <- function(obj, new_data) {
  tryCatch({
    predict(obj, new_data = new_data, type = "prob")
  }, error = function(e) {
    attr(e, "is_predict_error") <- TRUE
    stop(e)
  })
}

# Default thresholds from meta (fallbacks if missing)
def_thr_v1 <- suppressWarnings(as.numeric(v1_meta$threshold)); if (is.na(def_thr_v1)) def_thr_v1 <- 0.025
def_thr_v2 <- suppressWarnings(as.numeric(v2_meta$threshold)); if (is.na(def_thr_v2)) def_thr_v2 <- 0.20

# Default margin logic knobs (your chosen settings)
def_margin      <- 0.05
def_veto_soft   <- 0.10
def_v1_strong   <- 0.20
def_low_v2_rescue <- 0.01

# Trained workflow?
is_trained_wf <- function(x) {
  inherits(x, "workflow") && tryCatch(workflows::is_trained_workflow(x), error = function(e) FALSE)
}

# Probability prediction
predict_probs <- function(obj, new_data) {
  if (is_trained_wf(obj)) {
    return(predict(obj, new_data = new_data, type = "prob"))
  }
  if (!is.null(obj$wf) && workflows::is_trained_workflow(obj$wf)) {
    return(predict(obj$wf, new_data = new_data, type = "prob"))
  }
  stop("Model object is not a trained workflow.")
}

# Margin-aware S1 combiner (vectorised)
# Order:
#   1) V2 clear to NOTSevere if p2 >= thr_v2
#   2) Otherwise Severe if: (p1 >= thr_v1) & (p1 - p2 >= margin) &
#      (p2 < veto_soft  OR p1 >= v1_strong  OR p2 < low_v2_rescue)
#   3) Else Other
combine_margin <- function(p1, p2,
                           thr_v1    = def_thr_v1,
                           thr_v2    = def_thr_v2,
                           margin    = def_margin,
                           veto_soft = def_veto_soft,
                           v1_strong = def_v1_strong,
                           low_v2_rescue = def_low_v2_rescue) {
  n <- length(p1)
  out  <- rep("Other", n)
  rule <- rep("other_default", n)

  # 1) NOTSevere by V2 threshold
  idx_notsev <- p2 >= thr_v2
  out[idx_notsev]  <- "NOTSevere"
  rule[idx_notsev] <- "v2>=thr_v2"

  # 2) Severe by V1 with margin & veto/override logic, only where not already NOTSevere
  idx_cand <- !idx_notsev &
    (p1 >= thr_v1) &
    ((p1 - p2) >= margin) &
    ( (p2 < veto_soft) | (p1 >= v1_strong) | (p2 < low_v2_rescue) )

  out[idx_cand]  <- "Severe"
  # Label which clause triggered (helpful for debugging)
  rule[idx_cand] <- dplyr::case_when(
    p2[idx_cand] < low_v2_rescue ~ "severe_low_v2_rescue",
    p1[idx_cand] >= v1_strong    ~ "severe_v1_strong",
    p2[idx_cand] < veto_soft     ~ "severe_v2_soft_veto",
    TRUE                         ~ "severe_margin"
  )

  data.frame(
    decision = factor(out, levels = c("Other","Severe","NOTSevere")),
    rule     = rule,
    stringsAsFactors = FALSE
  )
}

# CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  forward()
}

#* @get /healthz
function() list(
  ok = TRUE, service = "S1",
  defaults = list(
    thr_v1 = def_thr_v1, thr_v2 = def_thr_v2,
    margin = def_margin, veto_soft = def_veto_soft,
    v1_strong = def_v1_strong, low_v2_rescue = def_low_v2_rescue
  )
)

# Helper to coerce/complete features
coerce_features <- function(x) {
  feats <- as.data.frame(x, stringsAsFactors = FALSE)

  needed <- c(
    "age.months","sex","bgcombyn","adm.recent","wfaz","waste","stunt","cidysymp",
    "prior.care","travel.time.bin","diarrhoeal","pneumo","sev.pneumo","ensapro",
    "vomit.all","seiz","pfacleth","not.alert","danger.sign","hr.all","rr.all",
    "oxy.ra","envhtemp","crt.long","parenteral_screen","SIRS_num"
  )
  for (nm in setdiff(needed, names(feats))) feats[[nm]] <- NA
  feats
}

#* @post /s1_infer
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats_raw <- as.data.frame(body$features, stringsAsFactors = FALSE)
  feats <- coerce_schema(feats_raw)

  # Try predicting; if anything fails, return a 400 with details.
  p1 <- try(predict_probs_safe(v1_obj, feats), silent = TRUE)
  if (inherits(p1, "try-error")) {
    res$status <- 400
    return(list(
      error = "V1 prediction failed",
      message = as.character(p1),
      received_schema = lapply(feats, function(x) c(class = paste(class(x), collapse=","), example = utils::head(x,1)))
    ))
  }

  p2 <- try(predict_probs_safe(v2_obj, feats), silent = TRUE)
  if (inherits(p2, "try-error")) {
    res$status <- 400
    return(list(
      error = "V2 prediction failed",
      message = as.character(p2),
      received_schema = lapply(feats, function(x) c(class = paste(class(x), collapse=","), example = utils::head(x,1)))
    ))
  }

  v1_prob <- if (".pred_Severe" %in% names(p1)) p1$.pred_Severe else {
    res$status <- 500; return(list(error = "v1 prob column .pred_Severe missing"))
  }
  v2_prob <- if (".pred_NOTSevere" %in% names(p2)) p2$.pred_NOTSevere else {
    res$status <- 500; return(list(error = "v2 prob column .pred_NOTSevere missing"))
  }

  # Pull decision defaults from meta or use baked-in defaults
  thr_v1    <- as.numeric(v1_meta$threshold %||% 0.025)
  thr_v2    <- as.numeric(v2_meta$threshold %||% 0.20)
  margin    <- as.numeric(v1_meta$margin    %||% 0.05)
  veto_soft <- as.numeric(v2_meta$veto_soft %||% 0.10)
  v1_strong <- as.numeric(v1_meta$v1_strong %||% 0.20)
  low_v2_rescue <- as.numeric(v2_meta$low_v2_rescue %||% 0.01)

  # Margin-aware combiner with low-V2 rescue
  combine_v1_v2_margin <- function(v1_p, v2_p) {
    out <- rep("Other", length(v1_p))
    idx_notsev <- v2_p >= thr_v2
    out[idx_notsev] <- "NOTSevere"

    idx_sev <- (v1_p >= thr_v1) &
      ((v1_p - v2_p) >= margin) &
      ((v2_p < veto_soft) | (v1_p >= v1_strong)) &
      !idx_notsev
    out[idx_sev] <- "Severe"

    # rescue: definitely-not-notSevere + above thr_v1
    idx_rescue <- (v2_p < low_v2_rescue) & (v1_p >= thr_v1) & !idx_notsev
    out[idx_rescue] <- "Severe"

    factor(out, levels = c("Other","Severe","NOTSevere"))
  }

  s1_dec <- combine_v1_v2_margin(v1_prob, v2_prob)

  jsonlite::toJSON(list(
    v1 = list(prob = unname(v1_prob[1]), thr = thr_v1),
    v2 = list(prob = unname(v2_prob[1]), thr = thr_v2),
    decision_params = list(margin = margin, veto_soft = veto_soft, v1_strong = v1_strong, low_v2_rescue = low_v2_rescue),
    s1_decision = as.character(s1_dec[1])
  ), auto_unbox = TRUE)
}

