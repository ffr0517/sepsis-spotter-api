# Sepsis-Spotter API

Sepsis-Spotter is a **REST API** that delivers predictions of pediatric sepsis severity using machine learning workflows trained on the Spot Sepsis study dataset. The API is implemented in **R (via plumber)**, containerized with **Docker**, and designed for research and evaluation use only.  

---

## Overview

Sepsis-Spotter exposes endpoints that allow users to submit structured clinical and contextual data for febrile children, and to receive:

- Probability estimates of **Severe sepsis** (v1 model).  
- Probability estimates of **Not-severe sepsis** (v2 model).  
- A combined decision output (`Severe`, `NOTSevere`, or `Other`) based on thresholds, margin checks, veto rules, and override logic.  
- Diagnostic metadata including the thresholds applied, which rule fired, schema adjustments, timing, and hash checksums.  

⚠️ **Important:** This project is a research tool. It is not a certified medical device and must not be used to guide clinical care.

---

## Key Features

- **Machine Learning Models**  
  - `v1_model.rds` — trained to identify children at risk of severe sepsis.  
  - `v2_model.rds` — trained to identify children confidently not at risk of severe sepsis.  

- **Schema Normalization**  
  Automatically adjusts submitted JSON payloads to match the expected feature schema (e.g., adding missing predictors with `NA`, type casting).  

- **Decision Logic**  
  Uses `combine_margin` to merge v1 and v2 outputs into a single classification, applying margin thresholds, veto rules, strong-overrides, and low-probability rescues.  

- **Lightweight API**  
  Deployed with plumber, containerized for portability, and exposes endpoints via port `8000`.  

---

## Endpoints

| Endpoint       | Method | Description                                                                 |
|----------------|--------|-----------------------------------------------------------------------------|
| `/healthz`     | GET    | Health check. Confirms API is running.                                      |
| `/schema`      | GET    | Returns the expected feature schema for inference requests.                 |
| `/s1_infer`    | POST   | Main prediction endpoint. Accepts a JSON payload of predictors and returns model probabilities, combined decision, and metadata. |

---

## Example Request

```bash
curl -X POST http://localhost:8000/s1_infer \
  -H "Content-Type: application/json" \
  -d '{
        "features": {
          "age.months": 24,
          "sex": 0,
          "bgcombyn": 1,
          "adm.recent": 0,
          "wfaz": -1.1,
          "waste": 0,
          "stunt": 0,
          "cidysymp": 0,
          "prior.care": 0,
          "travel.time.bin": 0,
          "diarrhoeal": 0,
          "pneumo": 0,
          "sev.pneumo": 0,
          "ensapro": 0,
          "vomit.all": 0,
          "seiz": 0,
          "pfacleth": 0,
          "not.alert": 0,
          "danger.sign": 0,
          "hr.all": 120,
          "rr.all": 28,
          "oxy.ra": 98,
          "envhtemp": 27.0,
          "crt.long": 0,
          "parenteral_screen": 0,
          "SIRS_num": 1
        }
      }'
  }
}


