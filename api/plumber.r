# Set up ----
library(plumber)
library(jsonlite)
library(dplyr)
library(workflows)

# Models
v1_obj  <- readRDS("models/v1_model.rds")
v1_meta <- readRDS("models/v1_meta.rds")
v2_obj  <- readRDS("models/v2_model.rds")
v2_meta <- readRDS("models/v2_meta.rds")

thr_v1 <- as.numeric(v1_meta$threshold)
thr_v2 <- as.numeric(v2_meta$threshold)

# Trained workflow
is_trained_wf <- function(x) {
  inherits(x, "workflow") && tryCatch(workflows::is_trained_workflow(x), error = function(e) FALSE)
}

# Probability prediction
predict_probs <- function(obj, new_data, severe_col = ".pred_Severe", notsev_col = ".pred_NOTSevere") {
  if (is_trained_wf(obj)) {
    p <- predict(obj, new_data = new_data, type = "prob")
    return(p)
  }
  # fallback 
  if (!is.null(obj$wf) && workflows::is_trained_workflow(obj$wf)) {
    return(predict(obj$wf, new_data = new_data, type = "prob"))
  }
  stop("Model object is not a trained workflow.")
}

combine_v1_v2 <- function(v1_call, v2_call) {
  dplyr::case_when(
    v1_call == "Severe"    & v2_call == "Other"     ~ "Severe",
    v1_call == "Severe"    & v2_call == "NOTSevere" ~ "Severe",
    v1_call == "Other"     & v2_call == "NOTSevere" ~ "NOTSevere",
    v1_call == "Other"     & v2_call == "Other"     ~ "Other",
    TRUE ~ "Other"
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
function() list(ok = TRUE, service = "S1", thr_v1 = thr_v1, thr_v2 = thr_v2)

#* @post /s1_infer
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  feats <- as.data.frame(body$features, stringsAsFactors = FALSE)

  # Ensure expected columns exist (NA if missing)
  needed <- c(
    "age.months","sex","bgcombyn","adm.recent","wfaz","waste","stunt","cidysymp",
    "prior.care","travel.time.bin","diarrhoeal","pneumo","sev.pneumo","ensapro",
    "vomit.all","seiz","pfacleth","not.alert","danger.sign","hr.all","rr.all",
    "oxy.ra","envhtemp","crt.long","parenteral_screen","SIRS_num"
  )
  for (nm in setdiff(needed, names(feats))) feats[[nm]] <- NA

  p1 <- predict_probs(v1_obj, feats)
  p2 <- predict_probs(v2_obj, feats)

  # expect columns like .pred_Severe, .pred_Other, .pred_NOTSevere
  v1_prob <- if (".pred_Severe" %in% names(p1)) p1$.pred_Severe else stop("v1 prob column .pred_Severe missing")
  v2_prob <- if (".pred_NOTSevere" %in% names(p2)) p2$.pred_NOTSevere else stop("v2 prob column .pred_NOTSevere missing")

  v1_call <- ifelse(v1_prob >= thr_v1, "Severe", "Other")
  v2_call <- ifelse(v2_prob >= thr_v2, "NOTSevere", "Other")
  s1_dec  <- combine_v1_v2(v1_call, v2_call)

  sheet <- list(
    sheet_version = 1L,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    patient = list(anon_id = paste0(as.hexmode(sample(0:255, 8, TRUE)), collapse="")),
    context = list(),
    features = list(clinical = body$features, labs = list()),
    s1 = list(
      v1 = list(prob = unname(v1_prob[1]), thr = thr_v1, call = v1_call[1]),
      v2 = list(prob = unname(v2_prob[1]), thr = thr_v2, call = v2_call[1]),
      decision = s1_dec[1],
      model_hash = "sha256:s1"
    ),
    notes = list()
  )

  jsonlite::toJSON(list(
    v1 = list(prob = unname(v1_prob[1]), thr = thr_v1, call = v1_call[1]),
    v2 = list(prob = unname(v2_prob[1]), thr = thr_v2, call = v2_call[1]),
    s1_decision = s1_dec[1],
    explanations = list(),
    warnings = list(),
    current_info_sheet = sheet,
    hash = "sha256:s1",
    timing_ms = 0L
  ), auto_unbox = TRUE)
}
