
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
- Volumen-scenarier: fra 2026-05-01 reduceres Team DK 1 med faktor 0,7 (justeres i `pipeline/14_train_final_glm.R`).

## V2 lane (backtest → ops forecast → scenarier → Erlang)
- Configs:
  - `config/forecast_v2.json`: backtest-folds, horisont, tidszone.
  - `config/volume_shocks.csv`: breakpoints med start/evtl. slut, multiplier, label, priority.
- Scripts:
  - `pipeline/16_backtest_rolling_final_glm.R`: rolling backtests, ingen shocks. Output til `results/v2/backtests/`, metrics i `output/v2/diagnostics/`.
  - `pipeline/17_operational_forecast_v2.R`: rå forecasts (y_hat_raw) for operational horisont. Output: `results/v2/operational/fc_operational_raw_v2.rds`.
  - `pipeline/18_apply_volume_shocks_v2.R`: anvender volume_shocks til y_hat_raw → y_hat. Output: `results/v2/scenarios/fc_operational_scenario_v2.rds`.
  - `pipeline/19_export_erlang_input_v2.R`: eksporterer justeret y_hat som volume til `output/v2/erlang/erlang_input_v2.csv`.
- Brug:
  1) Kør 16 (backtest, valgfrit), 17 (rå forecast), 18 (scenarier), 19 (Erlang-export).
  2) Rå forecast = y_hat_raw. Scenarie = y_hat_raw * multiplier (via config). Erlang bruger y_hat.
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
