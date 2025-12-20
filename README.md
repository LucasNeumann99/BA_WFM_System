
# BA WFM System — Forecasting, Erlang C & Staffing Optimization

Denne mappe er **den officielle BA-projektmappe**. `main` er stabil; `glm` bruges til GLM-eksperimenter (lag-features, mm.).

## Target (y)
- Y er “Real Offered Calls” (`real_offered_call`) aggregeret pr. team/time (`pipeline/03_aggregate_hourly.R`).

## Pipeline
- 01–09: ingestion → cleaning → hourly aggregation → features → lag-features → forecast horizon.
- 10–12: GLM baseline og GLM med lags på test (Poisson/NegBin).
- 13: sammenligner baseline vs. lag (uplift/R2).
- 14: træner endelig NegBin pr. team (lag eller ej).
- 15: evaluerer endelig model, gemmer plots og metrics.
- 16: rolling backtest (baseline for alle teams).
- 17: operational forecast (baseline for alle teams, bruger future features).
- 18: anvender volumen-shocks.
- 19–21: Erlang C + staffing pr. team.

### Endelig modelvalg (branch `glm`)
- Forecasts: `<results_base>/final/glm/fc_final_glm_negbin.rds`
- Metrics (samlet): `<results_base>/final/glm/metrics_final_glm.rds`
- Plots: `figures/final/glm/<team>/`

### Aktiv v2 pipeline (baseline for alle teams)
- Operational forecast bruger kun baseline (ingen lag/rekursion) for alle teams.
- Output pr. team: `<output_base>/baseline_glm/<team>/`
  - diagnostics: `.../diagnostics/metrics_final_glm.csv` + `model_summary_final_glm.csv`
  - erlang: `.../erlang/erlang_input_v2.csv` + `erlang_output_v2.csv`
  - staffing: `.../staffing/shift_plan_optimized_v2.csv` + `hourly_coverage_vs_required_v2.csv`

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

## Output paths
- Output gemmes uden for repoet; results bliver i repoet.
- Justeres i `config/paths.json`:
  - `output_base` (fx `../../BA_WFM_Output/output`)
  - `results_base` (fx `results`)

## Mappestruktur
- data_raw/                → Rå CSV-filer
- data_processed/          → RDS-filer klar til modeller
- models/                  → Trænede modeller
- model_functions/         → FE, modelling, LP, Erlang
- pipeline/                → Scripts der kører workflowet
- shiny_app/               → Shiny-dashboard
- <output_base>/baseline_glm/<team>/diagnostics/ → Team-metrics og modelsummary
- <output_base>/baseline_glm/<team>/erlang/      → Erlang input/output pr. team
- <output_base>/baseline_glm/<team>/staffing/    → Optimerede bemandingsplaner pr. team

