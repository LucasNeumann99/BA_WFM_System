
# BA WFM System — Forecasting, Erlang C & Staffing Optimization

Denne mappe er **den officielle BA-projektmappe**. `main` er stabil; `glm` bruges til GLM-eksperimenter (lag-features, mm.).

## Target (y)
- Y er “Real Offered Calls” (`real_offered_call`) aggregeret pr. team/time (`pipeline/03_aggregate_hourly.R`).
- Kunde nr. (customer_code) C370 fjernes for Team NO på call-niveau før aggregation.

## Pipeline
- 01–09: ingestion → cleaning → hourly aggregation → features → lag-features → forecast horizon.
- 10–12: GLM baseline og GLM med lags på test (Poisson/NegBin).
- 13: sammenligner baseline vs. lag (uplift/R2).
- 14: træner endelig NegBin pr. team (lag eller ej).
- 15: evaluerer endelig model, gemmer plots og metrics.

### Endelig modelvalg (branch `glm`)
- Lag-model (`GLM_NegBin_lags`): Team DK 1, Team FI 1, Team NO 1, Team SE 1.
- Baseline uden lag (`GLM_NegBin_baseline`): Team SE 2.
- Forecasts: `results/final/glm/fc_final_glm_negbin.rds`
- Metrics: `output/diagnostics/metrics_final_glm.csv` og `results/final/glm/metrics_final_glm.rds`
- Plots: `figures/final/glm/<team>/`

### Aktiv GLM-model (main)
- Default er de endelige GLM NegBin-modeller (mix af lag/baseline) fra ovenstående valg.
- Aktiv model styres af `models/active_glm_model.txt` (initielt `final_glm_negbin_by_team.rds`). Skift fx til baseline: `echo "baseline_glm_by_team.rds" > models/active_glm_model.txt`.
- Hjælpefunktioner ligger i `model_functions/model_registry.R`:
  - `load_active_glm_models()` læser den aktive model-Liste.
  - `list_available_glm_models()` viser hvad der kan vælges.
  - `set_active_glm_model("<filnavn>.rds")` sætter/validerer ny aktiv model.

### Branches
- `main`: stabil kodebase med aktiv final GLM-modeller (kan udskiftes via `models/active_glm_model.txt`).
- `glm`: GLM-eksperimenter og endelige GLM-valg (lag vs. baseline) før de lægges på `main`.

## Mappestruktur
- data_raw/                → Rå CSV-filer
- data_processed/          → RDS-filer klar til modeller
- models/                  → Trænede modeller
- model_functions/         → FE, modelling, LP, Erlang
- pipeline/                → Scripts der kører workflowet
- shiny_app/               → Shiny-dashboard
- output/diagnostics/      → Diagnoseplots og metrics
- output/forecasts/        → Forecasts til Erlang
- output/staffing/         → Optimerede bemandingsplaner

## Kørsel (lokalt)
- Kør hele GLM-workflowet fra ren start:  
  `for s in pipeline/0{1..15}*.R; do Rscript "$s"; done`
- Alternativt kun final GLM: kør 10–15.
