# Federal Reserve Board — FRB/US Model

**Agency:** Board of Governors of the Federal Reserve System
**FOIA Request Number:** FOIA-2026-00790
**Date of Final Response:** August 26, 2026 (granted in full)

## Description

FRB/US is the Federal Reserve Board's large-scale estimated general-equilibrium macroeconometric model of the U.S. economy, used at the Board since 1996 for forecasting, analysis of policy options, and simulation. In response to this request the Board granted it in full by pointing to its public FRB/US distribution page. The Board's official download packages are vendored here as received.

## Contents

The Board's official FRB/US packages, as published at https://www.federalreserve.gov/econres/us-models-about.htm:

- `pyfrbus.zip` — the Python implementation (PyFRB/US): the model equations (`pyfrbus/models/model.xml`) and the full solver library (`frbus.py` plus ~20 modules — equations, Jacobian, Newton solver, stochastic simulation, symbolic handling, time-series data), with example programs and HTML documentation.
- `frbus_package.zip` — the EViews implementation: the model equations (`mods/model.xml`), the model libraries (`subs/master_library.prg`, `subs/mce_solve_library.prg`), example programs (`ocpolicy`, `stochsim`, `pings`, …), and documentation.
- `data_only_package.zip` — the accompanying historical/illustrative dataset. The Board notes these trajectories are not FRB/US forecasts.

The Board also publishes LINVER, an RE-solver, and a state-space supply-side package from the same page; those are not included here but are freely downloadable.

## License

Work of the Board of Governors of the Federal Reserve System, published openly by the Board and not subject to domestic copyright protection under 17 U.S.C. § 105. See the model disclaimer linked from https://www.federalreserve.gov/econres/us-models-about.htm.
