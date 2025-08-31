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

  # Optional per-request overrides (body$params)
  p <- body$params %||% list()
  thr_v1    <- as.numeric(p$thr_v1    %||% def_thr_v1)
  thr_v2    <- as.numeric(p$thr_v2    %||% def_thr_v2)
  margin    <- as.numeric(p$margin    %||% def_margin)
  veto_soft <- as.numeric(p$veto_soft %||% def_veto_soft)
  v1_strong <- as.numeric(p$v1_strong %||% def_v1_strong)
  low_v2_rescue <- as.numeric(p$low_v2_rescue %||% def_low_v2_rescue)

  feats <- coerce_features(body$features)

  # Predict probabilities
  p1 <- predict_probs(v1_obj, feats) # expect .pred_Severe, .pred_Other
  p2 <- predict_probs(v2_obj, feats) # expect .pred_NOTSevere, .pred_Other

  if (!all(c(".pred_Severe", ".pred_Other") %in% names(p1)))
    stop("v1 prob columns missing (.pred_Severe/.pred_Other)")
  if (!all(c(".pred_NOTSevere", ".pred_Other") %in% names(p2)))
    stop("v2 prob columns missing (.pred_NOTSevere/.pred_Other)")

  v1_prob <- p1$.pred_Severe
  v2_prob <- p2$.pred_NOTSevere
  delta   <- v1_prob - v2_prob

  s1 <- combine_margin(
    p1 = v1_prob, p2 = v2_prob,
    thr_v1 = thr_v1, thr_v2 = thr_v2,
    margin = margin, veto_soft = veto_soft,
    v1_strong = v1_strong, low_v2_rescue = low_v2_rescue
  )

  # Build response row-wise
  out <- lapply(seq_len(nrow(feats)), function(i) {
    list(
      v1 = list(prob = unname(v1_prob[i]), thr = thr_v1),
      v2 = list(prob = unname(v2_prob[i]), thr = thr_v2),
      deltas = list(v1_minus_v2 = unname(delta[i])),
      params = list(
        margin = margin, veto_soft = veto_soft,
        v1_strong = v1_strong, low_v2_rescue = low_v2_rescue
      ),
      s1_decision = unname(as.character(s1$decision[i])),
      rule_fired  = unname(s1$rule[i])
    )
  })

  jsonlite::toJSON(list(
    results = out,
    meta = list(
      sheet_version = 1L,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      model_hash = "sha256:s1_margin_v1",
      defaults = list(
        thr_v1 = def_thr_v1, thr_v2 = def_thr_v2,
        margin = def_margin, veto_soft = def_veto_soft,
        v1_strong = def_v1_strong, low_v2_rescue = def_low_v2_rescue
      )
    )
  ), auto_unbox = TRUE)
}

