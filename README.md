**Sepsis Spotter Project Repository Quick Links**

[Build](https://github.com/ffr0517/sepsis-spotter-build)
[API](https://github.com/ffr0517/sepsis-spotter-api)
[UI](https://github.com/ffr0517/sepsis-spotter-ui)
[Manuscript](https://github.com/ffr0517/sepsis-spotter-manuscript)

# Sepsis Spotter API Repository

The Sepsis Spotter API is a **research-focused REST service** that provides machine learning–based predictions of pediatric sepsis severity.  
It is implemented in **R (via Plumber)**, containerized with **Docker**, and currently distributed for **evaluation, validation, and experimental use only** - not for clinical deployment.

---

## Current Implementation (Legacy-Compatible)

This API currently serves the **previous generation of Spot Sepsis models** (`v1`–`v5`) and remains compatible with the earlier research pipeline.  
It will be replaced in a later phase with the updated models and calibration workflows developed under the new `spot-sepsis-build` repository.

### Active Models (Legacy)

| Model | Stage | Description |
|-------|-------|-------------|
| `v1_model.rds` | S1 | Severe vs Other voter |
| `v2_model.rds` | S1 | Non-Severe vs Other voter |
| `v3_model.rds` | S2 | Severe vs Other |
| `v4_model.rds` | S2 | Probable Severe vs Other |
| `v5_model.rds` | S2 | Probable Non-Severe vs Other |

Each model has a paired metadata file (`*_meta.rds`) storing schema, thresholds, and calibration parameters.

**Note:** These models are legacy research artefacts. Future versions will draw from the unified training pipeline and isotonic calibration methods described in the project proposal.

---

## API Overview

The API exposes endpoints allowing clients to submit structured JSON payloads for febrile pediatric cases.  
It returns probabilistic severity estimates, decision classifications, and metadata describing model behaviour and schema adjustments.

### Endpoints

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/healthz` | GET | Returns service status, default thresholds, and cache state. |
| `/schema` | GET | Provides the expected schema metadata captured during model training. |
| `/s1_infer` | POST | Executes Stage 1 models (v1/v2) and returns calibrated probabilities and classification. |
| `/s2_infer` | POST | Executes Stage 2 models (v3–v5) using S1 meta-probabilities and available labs. |
| `/s2_warmup` | POST | Pre-loads S2 models into memory (effective when `CACHE_MODELS=true`). |
| `/admin/release_all` | POST | Clears cached model objects (helpful on constrained hosts). |

---

## Example Request (Stage 1)

```bash
curl -X POST http://localhost:8000/s1_infer \
  -H "Content-Type: application/json" \
  -d '{
        "features": {
          "age.months": 24,
          "sex": 0,
          "bgcombyn": 0,
          "adm.recent": 0,
          "wfaz": -1.1,
          "waste": 0,
          "stunt": 0,
          "cidysymp": 2,
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
          "envhtemp": 37.5,
          "crt.long": 0,
          "parenteral_screen": 0,
          "SIRS_num": 1
        }
      }'
```

---

## Service Architecture

- **Core router:** `api/plumber.R` — defines endpoints, schema guards, CORS policies, decision logic, and structured logging.
- **Models:** `api/models/` — pre-trained `.rds` model objects and associated metadata (legacy artefacts).
- **Dockerfile:** `api/Dockerfile` — provisions R 4.3.x on rocker/r-ver, installs dependencies, and pins CPU threading for reproducibility.
- **OpenAPI spec:** `sepsis-spotter-api.yaml` — captures the `/s1_infer` contract (S2 documentation is pending refresh).
- **Demo script:** `api_demo_script.R` — scripted warm-up, health check, and staged inference sequence for local or remote testing.

---

## Planned Upgrades (Next Major Integration Phase)

This API will be updated after the `spot-sepsis-build` repository finalizes its new Stage 1/2 models and calibration framework.

| Area | Planned Enhancement | Target Outcome |
|------|---------------------|----------------|
| Model Artefacts | Replace legacy `.rds` files with retrained isotonic-calibrated models | Improved calibration and PR-AUC |
| Inference Logic | Harmonize thresholds, bit-key rules, and meta-probability weighting with the new pipeline | Consistent classification rules across stages |
| Security & Privacy | Strengthen CORS controls, add optional token-based authentication | Controlled research access |
| Continuous Benchmarking | Introduce “live model” evaluation option for anonymised incoming cases | Dynamic benchmarking under opt-in consent |
| Schema & Metadata | Synchronize schema with updated `spot-sepsis-build` variable definitions | Prevent drift between training and API |
| Monitoring | Add structured logging, error telemetry, and latency metrics | Better transparency and debugging |

---

## Privacy and Data Handling

- All data are processed transiently in memory — no storage, persistence, or analytics logging.
- The API does not attempt to identify users or link submissions across sessions.
- Deployed instances should be hosted behind institution-managed security layers (e.g., HTTPS reverse proxy, authentication).
- See `privacy_policy.md` for full details on data handling, encryption expectations, and deployer responsibilities.

---

## Deployment

### Local

```bash
docker build -t spot-sepsis-api ./api
docker run -p 8000:8000 spot-sepsis-api
```

After startup, visit `http://localhost:8000/__swagger__/` for interactive docs (served by plumber).

### Remote

Integrate the Docker image (or direct R runtime) into your preferred cloud provider or container registry.  
Environment variables such as `CACHE_MODELS`, `MODEL_PATH`, and `OMP_NUM_THREADS` can be set to tune memory and throughput behaviour.

---

## Repository Structure

```
spot-sepsis-api/
├── README.md
├── api/
│   ├── plumber.R          # Core API definition
│   ├── Dockerfile         # Containerized runtime
│   └── models/            # Model artefacts (legacy)
├── api_demo_script.R      # Example interaction script
├── sepsis-spotter-api.yaml# OpenAPI 3.1 specification
└── privacy_policy.md      # Data and privacy documentation
```

---

## Integration Roadmap

| Phase | Repository | Description |
|-------|------------|-------------|
| 1 | `spot-sepsis-build` | Core model training, calibration, and evaluation |
| 2 | `spot-sepsis-api` | Serve trained models through updated inference endpoints |
| 3 | `spot-sepsis-ui` | Replace legacy inference logic with unified endpoints |
| 4 | `spot-sepsis-manuscript` | Reference updated API outputs for results and figures |

---

## Disclaimer

This API is provided for research purposes only.  
It is not a certified clinical product, and no inferences from this system should be used for patient care or clinical decision-making.  
All contributors and deployers must ensure appropriate data governance and ethical approvals before use.
