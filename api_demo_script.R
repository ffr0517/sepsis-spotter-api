# Set up ----
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

BASE_URL <- Sys.getenv("SEPSIS_SPOTTER_URL", unset = "https://sepsis-spotter.onrender.com")

# http helpers ----
`%||%` <- function(x, y) if (is.null(x)) y else x

post_json <- function(url, payload, retries = 6, wait = 2) {
  last_err_msg <- NULL
  for (i in seq_len(retries)) {
    resp <- try({
      req <- httr2::request(url) |>
        httr2::req_timeout(30) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "User-Agent"   = "sepsis-spotter-test/1.0"
        ) |>
        httr2::req_body_json(payload, auto_unbox = TRUE)
      httr2::req_perform(req)
    }, silent = TRUE)
    
    if (inherits(resp, "try-error")) {
      last_err_msg <- conditionMessage(attr(resp, "condition") %||% simpleError("request failed"))
      Sys.sleep(wait * (1.5^(i - 1)) + runif(1, 0, 0.5))  # exp backoff + jitter
      next
    }
    
    status <- httr2::resp_status(resp)
    if (status %in% c(408,409,425,429) || status >= 500) {
      message("Attempt ", i, " -> HTTP ", status, " (", httr2::resp_status_desc(resp), ")")
      Sys.sleep(wait * (1.5^(i - 1)) + runif(1, 0, 0.5))
      next
    }
    if (status >= 400) {
      cat("Server error body:\n", httr2::resp_body_string(resp), "\n")
      stop(sprintf("HTTP %s %s", status, httr2::resp_status_desc(resp)))
    }
    return(httr2::resp_body_json(resp, simplifyVector = TRUE))
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

# health check ----
cat("Health check @", BASE_URL, "...\n")
print(get_json(paste0(BASE_URL, "/healthz")))

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

# calling s2 (calibrated and raw) ----
cat("\nCalling /s2_infer (calibrated=TRUE) ...\n")
s2_out_cal <- post_json(
  paste0(BASE_URL, "/s2_infer"),
  list(features = s2_features, apply_calibration = TRUE)
)

cat("\nS2 (calibrated) result:\n")
print(s2_out_cal)

cat("\nCalling /s2_infer (calibrated=FALSE) ...\n")
s2_out_raw <- post_json(
  paste0(BASE_URL, "/s2_infer"),
  list(features = s2_features, apply_calibration = FALSE)
)

cat("\nS2 (raw) result:\n")
print(s2_out_raw)

# pretty summary ----
cat("\n--- Summary ---\n")
fmt_row <- function(x) {
  if (is.character(x) && length(x) == 1L && grepl("^\\s*([\\[{])", x)) {
    x <- jsonlite::fromJSON(x, simplifyVector = TRUE)
  }
  
  if (is.data.frame(x)) {
    x <- as.list(x[1, , drop = FALSE])
  } else if (is.list(x) && is.null(names(x)) && length(x) >= 1L && is.list(x[[1]])) {
    x <- x[[1]]
  }
  
  # Safety getters
  gnum <- function(nm) suppressWarnings(as.numeric(if (is.null(x[[nm]])) NA_real_ else x[[nm]]))
  gchr <- function(nm) as.character(if (is.null(x[[nm]])) "" else x[[nm]])
  
  sprintf(
    "ID=%s | v3=%.3f (thr=%.3f) | v4=%.3f (thr=%.3f) | v5=%.3f (thr=%.3f) | bits=%s | call=%s",
    gchr("label"),
    gnum("p_v3"), gnum("thr_v3"),
    gnum("p_v4"), gnum("thr_v4"),
    gnum("p_v5"), gnum("thr_v5"),
    gchr("bit_key"), gchr("call")
  )
}
cat("Calibrated: ", fmt_row(s2_out_cal), "\n", sep = "")
cat("Raw:        ", fmt_row(s2_out_raw), "\n", sep = "")

cat("\nDone.\n")
