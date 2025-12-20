#!/usr/bin/env python3
# ============================================================
# 20_erlang_c_v2.py
# ------------------------------------------------------------
# Formål: Beregn bemandingsbehov med en ren Python Erlang C-implementering.
# Ingen afhængighed til pyworkforce. Numerisk stabilitet via lgamma + log-sum-exp
# i stedet for factorial, så vi undgår overflow for store n.
# Input:  <output_base>/v2/erlang/erlang_input_v2.csv
# Output: <output_base>/v2/erlang/erlang_output_v2.csv
# Køres med: python pipeline/20_erlang_c_v2.py
# ============================================================

import json
import math
import sys
from pathlib import Path

import pandas as pd

ROOT_DIR = Path(__file__).resolve().parents[1]


def _load_paths():
    cfg_path = ROOT_DIR / "config" / "paths.json"
    output_base = "output"
    results_base = "results"
    if cfg_path.exists():
        with cfg_path.open(encoding="utf-8") as handle:
            cfg = json.load(handle)
        output_base = cfg.get("output_base", output_base)
        results_base = cfg.get("results_base", results_base)
    return (ROOT_DIR / output_base).resolve(), (ROOT_DIR / results_base).resolve()


OUTPUT_BASE, RESULTS_BASE = _load_paths()
BASELINE_DIR = OUTPUT_BASE / "baseline_glm"
INTERVAL_SECONDS = 3600  # antager 1 times intervaller
MAX_AGENTS = 200


def _logsumexp(values):
    """Stabil log(sum(exp(values))) for at undgå overflow."""
    m = max(values)
    if math.isinf(m):
        return m
    return m + math.log(sum(math.exp(v - m) for v in values))


class ErlangC:
    """
    Klassisk Erlang C med numerisk stabile sandsynligheder.
    _prob_wait bruger lgamma og log-sum-exp til at undgå factorial-overflow.
    service_level clips altid til [0, 1].
    """

    def __init__(self, arrival_rate, service_rate, target_wait_time):
        if service_rate <= 0:
            raise ValueError("service_rate skal være > 0")
        self.arrival_rate = arrival_rate
        self.service_rate = service_rate
        self.target_wait_time = target_wait_time
        self.traffic = arrival_rate / service_rate  # erlangs

    def _prob_wait(self, agents):
        """P(wait > 0) via log-sum-exp; stabilt for større agent-tal."""
        traffic = self.traffic
        if traffic <= 0:
            return 0.0
        if agents <= traffic:
            return 1.0

        log_traffic = math.log(traffic)
        # log(traffic^k / k!) for k = 0..agents-1
        log_terms = [k * log_traffic - math.lgamma(k + 1) for k in range(agents)]
        log_numer = (
            agents * log_traffic
            - math.lgamma(agents + 1)
            + math.log(agents / (agents - traffic))
        )
        log_denom = _logsumexp(log_terms + [log_numer])
        return math.exp(log_numer - log_denom)

    def service_level(self, agents):
        """P(wait <= T). Clips resultat til [0, 1]."""
        traffic = self.traffic
        if agents <= 0:
            return 0.0
        if traffic <= 0:
            return 1.0
        if agents <= traffic:
            return 0.0

        p_wait = self._prob_wait(agents)
        decay = (agents - traffic) * self.service_rate * self.target_wait_time
        sl = 1.0 - p_wait * math.exp(-decay)
        return max(0.0, min(1.0, sl))


def compute_staffing(row):
    calls = row["calls"]
    aht_sec = row["aht_sec"]
    target_sl = row["target_sl"]
    threshold_sec = row["threshold_sec"]
    shrinkage = row["shrinkage"]

    # Hvis calls mangler/<=0 → triviel løsning
    if pd.isna(calls) or calls <= 0:
        return {
            "traffic_erlangs": 0.0,
            "agents_required": 0,
            "agents_with_shrinkage": 0,
            "achieved_sl": 1.0,
        }

    # Manglende SLA- eller AHT-parametre → ingen beregning
    if (
        pd.isna(aht_sec)
        or pd.isna(target_sl)
        or pd.isna(threshold_sec)
        or aht_sec <= 0
        or target_sl <= 0
        or threshold_sec <= 0
    ):
        return {
            "traffic_erlangs": math.nan,
            "agents_required": None,
            "agents_with_shrinkage": None,
            "achieved_sl": None,
        }

    arrival_rate = calls / INTERVAL_SECONDS
    service_rate = 1.0 / aht_sec
    traffic = arrival_rate / service_rate  # = calls * aht / 3600

    erlang = ErlangC(
        arrival_rate=arrival_rate,
        service_rate=service_rate,
        target_wait_time=threshold_sec,
    )

    agents_req = None
    achieved = None
    for n in range(1, MAX_AGENTS + 1):
        sla = erlang.service_level(n)
        if sla >= target_sl:
            agents_req = n
            achieved = sla
            break

    if agents_req is None:
        agents_req = MAX_AGENTS
        achieved = erlang.service_level(MAX_AGENTS)

    if pd.isna(shrinkage) or shrinkage >= 1:
        agents_shr = None
    else:
        agents_shr = int(math.ceil(agents_req / (1 - shrinkage)))

    return {
        "traffic_erlangs": traffic,
        "agents_required": agents_req,
        "agents_with_shrinkage": agents_shr,
        "achieved_sl": achieved,
    }


def main():
    if not BASELINE_DIR.exists():
        sys.stderr.write(f"Baseline dir findes ikke: {BASELINE_DIR}\n")
        sys.exit(1)

    input_files = sorted(BASELINE_DIR.glob("*/erlang/erlang_input_v2.csv"))
    if not input_files:
        sys.stderr.write(f"Ingen input CSV fundet i: {BASELINE_DIR}\n")
        sys.exit(1)

    required_cols = [
        "team",
        "ds",
        "calls",
        "aht_sec",
        "target_sl",
        "threshold_sec",
        "shrinkage",
        "model_used",
        "scenario_label",
        "run_id",
    ]

    for input_csv in input_files:
        df = pd.read_csv(input_csv)
        missing = [c for c in required_cols if c not in df.columns]
        if missing:
            sys.stderr.write(
                f"Mangler kolonner i input ({input_csv}): {missing}\n"
            )
            sys.exit(1)

        results = df.apply(compute_staffing, axis=1, result_type="expand")

        out_df = pd.concat(
            [
                df[
                    [
                        "team",
                        "ds",
                        "calls",
                        "aht_sec",
                        "target_sl",
                        "threshold_sec",
                        "shrinkage",
                        "model_used",
                        "scenario_label",
                        "run_id",
                    ]
                ],
                results,
            ],
            axis=1,
        )

        output_csv = input_csv.parent / "erlang_output_v2.csv"
        output_csv.parent.mkdir(parents=True, exist_ok=True)
        out_df.to_csv(output_csv, index=False)
        print(f"✔ Erlang C output gemt: {output_csv}")


if __name__ == "__main__":
    main()
