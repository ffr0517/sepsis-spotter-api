# Load the httr2 package
library(httr2)

# Define the payload (the information about the patient) as a nested list.
payload <- list(
  features = list(
    "age.months"       = 24,     # Patient age in months
    "sex"              = 0,      # Sex (1 = male, 0 = female)
    "bgcombyn"         = 1,      # Comorbidity present (1 = yes, 0 = no)
    "adm.recent"       = 0,      # Overnight hospitalisation in last 6 months (1 = yes, 0 = no)
    "wfaz"             = -1.1,   # Weight-for-age z-score
    "waste"            = 0,      # Wasting (1 = yes, 0 = no)
    "stunt"            = 0,      # Stunting (1 = yes, 0 = no)
    "cidysymp"         = 0,      # Duration of illness in days
    "prior.care"       = 0,      # Sought care prior to presentation? (1 = yes, 0 = no)
    "travel.time.bin"  = 0,      # Travel time to site (1 = ≤1h, 0 = >1h)
    "diarrhoeal"       = 0,      # Diarrhoeal syndrome (1 = yes, 0 = no)
    "pneumo"           = 0,      # WHO pneumonia (1 = yes, 0 = no)
    "sev.pneumo"       = 0,      # WHO severe pneumonia (1 = yes, 0 = no)
    "ensapro"          = 0,      # Prostration (1 = yes, 0 = no)
    "vomit.all"        = 0,      # Intractable vomiting (1 = yes, 0 = no)
    "seiz"             = 0,      # Convulsions (1 = yes, 0 = no)
    "pfacleth"         = 0,      # Lethargy (1 = yes, 0 = no)
    "not.alert"        = 0,      # Not alert (AVPU < A; 1 = yes, 0 = no)
    "danger.sign"      = 0,      # WHO danger sign present? (1 = yes, 0 = no)
    "hr.all"           = 120,    # Heart rate (bpm)
    "rr.all"           = 28,     # Respiratory rate (bpm)
    "oxy.ra"           = 98,     # Oxygen saturation in %
    "envhtemp"         = 27.0,   # Axillary temperature in °C
    "crt.long"         = 0,      # Capillary refill >2s (1 = yes, 0 = no)
    "parenteral_screen"= 0,      # Received parenteral treatment before enrolment? (1 = yes, 0 = no)
    "SIRS_num"         = 1       # SIRS score (numeric count)
  )
)


# Build the HTTP POST request to the API endpoint
req <- request("https://sepsis-spotter-beta.onrender.com/s1_infer") |>
  req_body_json(payload) |>
  req_headers("Content-Type" = "application/json")

# Perform the request (send it to the server)
resp <- req_perform(req)

# Parse the server's response as JSON
resp_body_json(resp)
