
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
- Plots: `<output_base>/baseline_glm/<team>/diagnostics/`

### Aktiv v2 pipeline (operative model)
- Operational forecast bruger kun baseline (ingen lag/rekursion) for alle teams.
- Output pr. team:
  - diagnostics: `<output_base>/diagnostics/<team>/`
  - erlang: `<output_base>/capacity/erlang/<team>/`
  - staffing: `<output_base>/capacity/staffing/<team>/`

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
- Output og artefakter gemmes uden for repoet (CSV/PNG/RDS).
- Justeres i `config/paths.json`:
  - `output_base` (fx `../../BA_WFM_Output`)
  - `results_base` (fx `results`)

## Artefakter & versionering
- `figures/`, `results/`, `output/` er **ikke versioneret**.
- Kun scripts, konfiguration og kode ligger i git.
- Kør pipelines for at regenerere plots/metrics lokalt.

## Mappestruktur
- data_raw/                → Rå CSV-filer
- data_processed/          → RDS-filer klar til modeller
- models/                  → Trænede modeller
- model_functions/         → FE, modelling, LP, Erlang
- pipeline/                → Scripts der kører workflowet
- analysis_extra/          → Ad hoc analyse-scripts (skriver til `<output_base>/analysis_extra/`)
- shiny_app/               → Shiny-dashboard
- <output_base>/diagnostics/<team>/ → Operative metrics/plots pr. team
- <output_base>/capacity/erlang/<team>/ → Erlang input/output pr. team
- <output_base>/capacity/staffing/<team>/ → Optimerede bemandingsplaner pr. team
