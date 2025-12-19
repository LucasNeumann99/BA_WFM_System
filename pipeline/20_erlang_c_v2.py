#!/usr/bin/env python3
# ============================================================
# 20_erlang_c_v2.py
# ------------------------------------------------------------
# Formål: beregn bemandingsbehov med Erlang C fra scenarie-justerede forecasts.
# Bruger et eksisterende Erlang C-bibliotek (pyworkforce) – ingen egen formel.
# Input:  output/v2/erlang/erlang_input_v2.csv (calls, AHT, SLA-parametre)
# Output: output/v2/erlang/erlang_output_v2.csv (agents, SLA, metadata)
# ============================================================

import math
import sys
from pathlib import Path

import pandas as pd

class _FallbackErlangC:
    """
    Simpel Erlang C fallback hvis pyworkforce ikke er installeret.
    Bruger standardformel for service level:
      P(wait>0) = ErlangC probability
      P(wait<=t) = 1 - P(wait>0) * exp(-(n - a) * mu * t)
    Hvor a = traffic (erlangs), mu = service_rate.
    """

    def __init__(self, arrival_rate, service_rate, target_wait_time):
        self.arrival_rate = arrival_rate
        self.service_rate = service_rate
        self.target_wait_time = target_wait_time

    def _erlang_c_prob(self, n_agents):
        a = self.arrival_rate / self.service_rate  # erlangs
        if n_agents <= a:
            return 1.0
        numerator = (a ** n_agents) / (math.factorial(n_agents) * (n_agents - a))
        denom = sum((a ** k) / math.factorial(k) for k in range(n_agents)) + numerator
        return numerator / denom

    def service_level(self, n_agents):
        a = self.arrival_rate / self.service_rate
        p_wait = self._erlang_c_prob(n_agents)
        pw_t = p_wait * math.exp(-(n_agents - a) * self.service_rate * self.target_wait_time)
        return 1 - pw_t


try:
    from pyworkforce.queuing import ErlangC
    ErlangCImpl = ErlangC
except ImportError:
    sys.stderr.write(
        "pyworkforce mangler; bruger fallback Erlang C-implementation.\n"
        "Installer evt.: pip install pyworkforce\n"
    )
    ErlangCImpl = _FallbackErlangC


INPUT_CSV = Path("output/v2/erlang/erlang_input_v2.csv")
OUTPUT_CSV = Path("output/v2/erlang/erlang_output_v2.csv")
N_MAX = 200


def compute_staffing(row):
    """
    Beregn staffing med Erlang C for én række.
    Antager pyworkforce.ErlangC med metode service_level(n_agents).
    """
    calls = row.get("calls")
    if pd.isna(calls) or calls <= 0:
        return {
            "traffic_erlangs": 0.0,
            "agents_required": 0,
            "agents_with_shrinkage": 0,
            "achieved_sl": 1.0,
        }

    aht_sec = row["aht_sec"]
    target_sl = row["target_sl"]
    threshold_sec = row["threshold_sec"]
    shrinkage = row["shrinkage"]

    arrival_rate = calls / 3600.0  # pr. sekund
    service_rate = 1.0 / aht_sec
    traffic = calls * aht_sec / 3600.0

    erlang = ErlangCImpl(
        arrival_rate=arrival_rate,
        service_rate=service_rate,
        target_wait_time=threshold_sec,
    )

    agents_req = None
    achieved = None
    for agents in range(1, N_MAX + 1):
        sla = erlang.service_level(agents)
        if sla >= target_sl:
            agents_req = agents
            achieved = sla
            break

    if agents_req is None:
        return {
            "traffic_erlangs": traffic,
            "agents_required": None,
            "agents_with_shrinkage": None,
            "achieved_sl": None,
        }

    agents_shr = math.ceil(agents_req / (1 - shrinkage))

    return {
        "traffic_erlangs": traffic,
        "agents_required": agents_req,
        "agents_with_shrinkage": agents_shr,
        "achieved_sl": achieved,
    }


def main():
    if not INPUT_CSV.exists():
        sys.stderr.write(f"Input CSV findes ikke: {INPUT_CSV}\n")
        sys.exit(1)

    df = pd.read_csv(INPUT_CSV, parse_dates=["ds"])

    # Backwards-compat: hvis kolonnen hedder volume, mapper vi til calls.
    if "calls" not in df.columns and "volume" in df.columns:
        df["calls"] = df["volume"]

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
    missing = [c for c in required_cols if c not in df.columns]
    if missing:
        sys.stderr.write(f"Mangler kolonner i input: {missing}\n")
        sys.exit(1)

    results = df.apply(compute_staffing, axis=1, result_type="expand")

    out_df = pd.concat([df[["team", "ds", "calls", "model_used", "scenario_label", "run_id"]], results], axis=1)

    OUTPUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(OUTPUT_CSV, index=False)
    print(f"✔ Erlang C output gemt: {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
