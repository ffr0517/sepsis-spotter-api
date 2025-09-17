# Set up ----
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

BASE_URL <- Sys.getenv("SEPSIS_SPOTTER_URL", unset = "https://sepsis-spotter-beta.onrender.com")

# http helpers ----
`%||%` <- function(x, y) if (is.null(x)) y else x

post_json <- function(
    url,
    payload,
    retries    = 100,
    wait       = 2,
    timeout_s  = 60,
    is_success = function(resp) {
      status <- httr2::resp_status(resp)
      status >= 200 && status < 300
    }
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Hard cap on retries
  retries <- min(as.integer(retries %||% 100L), 100L)
  
  last_err_msg <- NULL
  transient_statuses <- c(408, 425, 429, 500, 502, 503, 504)
  
  for (i in seq_len(retries)) {
    resp <- try({
      req <- httr2::request(url) |>
        httr2::req_body_json(payload, auto_unbox = TRUE) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "Accept"       = "application/json"
        ) |>
        httr2::req_timeout(seconds = timeout_s) |>
        httr2::req_retry(max_tries = 1)  # manual retry loop
      httr2::req_perform(req)
    }, silent = TRUE)
    
    if (inherits(resp, "try-error")) {
      last_err_msg <- conditionMessage(attr(resp, "condition") %||% simpleError("request failed"))
      if (i < retries) Sys.sleep(wait)
      next
    }
    
    status <- httr2::resp_status(resp)
    
    # Fail immediately on non-transient 4xx/5xx
    if (status >= 400 && !(status %in% transient_statuses)) {
      cat("Server error body:\n", httr2::resp_body_string(resp), "\n")
      stop(sprintf("HTTP %s %s", status, httr2::resp_status_desc(resp)))
    }
    
    # Exit immediately if response is “good”
    if (is_success(resp)) {
      return(httr2::resp_body_json(resp, simplifyVector = TRUE))
    }
    
    # Otherwise, retry if we can
    message("Attempt ", i, " -> HTTP ", status, " (", httr2::resp_status_desc(resp), ")")
    if (i < retries) Sys.sleep(wait)
  }
  
  stop("Failed after retries: ", last_err_msg %||% "unknown")
}


get_json <- function(url) {
  resp <- httr2::request(url) |> httr2::req_timeout(15) |> httr2::req_perform()
  if (httr2::resp_status(resp) >= 400) {
    cat("Server error body:\n", httr2::resp_body_string(resp), "\n")
    stop(sprintf("HTTP %s %s", httr2::resp_status(resp), httr2::resp_status_desc(resp)))
  }
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

num <- function(x) suppressWarnings(as.numeric(x))

# API Warm-up Routine ----
tic()
cat("Warming up API at", BASE_URL, "(this may take up to 5 minutes)...\n")
max_wait_s <- 300      # 5 minutes total
poll_interval_s <- 5   # Check every 5 seconds
max_attempts <- max_wait_s / poll_interval_s
api_ready <- FALSE

for (i in seq_len(max_attempts)) {
  # Gracefully handle connection errors while the server is starting
  resp <- try(
    httr2::request(paste0(BASE_URL, "/healthz")) |>
      httr2::req_timeout(seconds = poll_interval_s) |>
      httr2::req_perform(),
    silent = TRUE
  )
  
  if (!inherits(resp, "try-error") && httr2::resp_status(resp) == 200) {
    cat("\nAPI is live!\n")
    api_ready <- TRUE
    break # Exit the loop on success
  }
  
  cat(".") # Print a dot to show progress
  Sys.sleep(poll_interval_s)
}

# If the loop finished without the API becoming ready, stop the script.
if (!api_ready) {
  stop("API did not become available after ", max_wait_s, " seconds.")
}
toc()

# health check ----
tic()
cat("Health check @", BASE_URL, "...\n")
print(get_json(paste0(BASE_URL, "/healthz")))
toc()

# Mock patient details (S1 clinical features only) ----
s1_features <- list(
  age.months        = 24,
  sex               = 0,   # 1 = male, 0 = female  (server schema guard will coerce)
  bgcombyn          = 0,
  adm.recent        = 0,
  wfaz              = -1.1,
  waste             = 0,
  stunt             = 0,
  cidysymp          = 2,   # illness duration in days
  prior.care        = 0,
  travel.time.bin   = 0,
  diarrhoeal        = 0,
  pneumo            = 0,
  sev.pneumo        = 0,
  ensapro           = 0,
  vomit.all         = 0,
  seiz              = 0,
  pfacleth          = 0,
  not.alert         = 0,
  danger.sign       = 0,
  hr.all            = 120,
  rr.all            = 28,
  oxy.ra            = 98,
  envhtemp          = 37.8,
  crt.long          = 0,
  parenteral_screen = 0,
  SIRS_num          = 1
)

# call s1 ----
tic()
cat("\nCalling /s1_infer ...\n")
s1_out <- post_json(paste0(BASE_URL, "/s1_infer"), list(features = s1_features))
# Extract S1 meta-probs for S2
v1_sev <- num(s1_out$v1$prob)             # .pred_Severe
v2_not <- num(s1_out$v2$prob)             # .pred_NOTSevere
v1_oth <- 1 - v1_sev
v2_oth <- 1 - v2_not

cat("\nS1 result (first row):\n")
print(list(
  v1_prob_Severe = v1_sev,
  v2_prob_NOTSevere = v2_not,
  s1_decision = s1_out$s1_decision,
  rule = s1_out$rule
))
toc()

# build S2 features = S1 clinical + biomarkers + S1 meta-probs ----
## NOTE: send only raw fields; the saved recipe handles NA indicators, imputation, dummies.
s2_features <- modifyList(
  s1_features,
  list(
    # Biomarkers (mock values; set to NA_real_ if unknown)
    ANG1       = 14.2,
    ANG2       = 2.6,
    CHI3L      = 0.8,
    CRP        = 20.5,
    CXCl10     = 325,
    IL1ra      = 120,
    IL6        = 15.3,
    IL8        = 22.1,
    IL10       = 4.7,
    PROC       = 0.85,
    TNFR1      = 1700,
    STREM1     = 2.1,
    VEGFR1     = 60,
    supar      = 3.2,
    lblac      = 1.6,
    lbglu      = 4.9,
    enescbchb1 = 0.0,
    
    # S1 meta-probs (required by S2)
    v1_pred_Severe    = v1_sev,
    v1_pred_Other     = v1_oth,
    v2_pred_NOTSevere = v2_not,
    v2_pred_Other     = v2_oth,
    
    # Optional label to track the row
    label = "demo-patient-001"
  )
)

# calling s2 (calibrated) ----
tic()
cat("\nCalling /s2_infer (calibrated=TRUE) ...\n")
s2_out_cal <- post_json(paste0(BASE_URL, "/s2_infer"),
                        list(features = s2_features, apply_calibration = TRUE),
                        timeout_s = 180)

cat("\nS2 (calibrated) result:\n")
print(s2_out_cal)
toc()

# S2 call in isolation with minimal inputs ----
s2_features <- list(
  # Core clinical anchors
  sex        = 0,
  age.months        = 24,
  wfaz              = -1.2,
  hr.all            = 118,
  rr.all            = 30,
  not.alert         = 1,   # 1 = not alert, 0 = alert
  crt.long          = 0,   # 1 = prolonged CRT, 0 = normal
  adm.recent        = 0,   # 1 = yes, 0 = no recent admission
  
  # Three chosen biomarkers (example values, replace with real ones)
  CRP    = 22.5,
  IL6    = 14.8,
  supar  = 2.9,
  oxy.ra = 98,
  
  # All other biomarkers left missing
  ANG1       = NA_real_,
  ANG2       = NA_real_,
  CHI3L      = NA_real_,
  CXCl10     = NA_real_,
  IL1ra      = NA_real_,
  IL8        = NA_real_,
  IL10       = NA_real_,
  PROC       = NA_real_,
  TNFR1      = NA_real_,
  STREM1     = NA_real_,
  VEGFR1     = NA_real_,
  lblac      = NA_real_,
  lbglu      = NA_real_,
  enescbchb1 = NA_real_,
  
  # Binary / flag-like predictors defaulted to “absent”
  bgcombyn          = 0L,
  waste             = 0L,
  stunt             = 0L,
  prior.care        = 0L,
  travel.time.bin   = 0L,
  diarrhoeal        = 0L,
  pneumo            = 0L,
  sev.pneumo        = 0L,
  ensapro           = 0L,
  vomit.all         = 0L,
  seiz              = 0L,
  pfacleth          = 0L,
  danger.sign       = 0L,
  parenteral_screen = 0L,
  syndrome.resp     = 0L,
  syndrome.nonresp  = 0L,
  infection         = 0L,
  
  # Optional continuous fields left missing
  envhtemp = NA_real_,
  
  # S1 meta-probs (must come from /s1_infer output above)
  v1_pred_Severe    = v1_sev,
  v1_pred_Other     = v1_oth,
  v2_pred_NOTSevere = v2_not,
  v2_pred_Other     = v2_oth,
  
  # Label for traceability
  label = "demo-patient-001"
)

tic()
cat("\nCalling /s2_infer (calibrated=TRUE) ...\n")
s2_out_cal <- post_json(paste0(BASE_URL, "/s2_infer"),
                        list(features = s2_features, apply_calibration = TRUE),
                        timeout_s = 180)

cat("\nS2 (calibrated) result:\n")
print(s2_out_cal)
toc()
