# DOI / Bureau of Reclamation — CRMMS 24-Month Study Model

**Agency:** U.S. Department of the Interior, Bureau of Reclamation
**Agency FOIA number:** Not stated — the production included no cover or response letter.

## Description

The 24-Month Study is the Bureau of Reclamation's operational projection of Colorado River system reservoir conditions (including Lake Powell and Lake Mead) over a rolling 24-month horizon. It is produced with the Colorado River Mid-term Modeling System (CRMMS), a RiverWare-based model, and informs annual operating decisions and reservoir release determinations under the Colorado River operating criteria.

This production is the complete native CRMMS package for the May 2026 (MAY26) ensemble streamflow prediction (ESP) run — sufficient to run and inspect the model.

## Contents

- `RW Files/` — the RiverWare model itself: `CRMMS_v4.17_202605_ESP.mdl.gz` plus MAX / MIN / MOST scenario variants (gzip-compressed RiverWare `.mdl` files), and a basin outline image
- `SCTs/` — RiverWare System Control Table configurations (reservoir input/status, flood-control and summary reports)
- `Inflow Forecasts/` — ensemble streamflow prediction (ESP) inflow traces, the CBRFC ensemble-forecast workbook, and a download script
- `Input Data/` — model run control file
- `Output Data/` — CRMMS-to-CRSS hand-off workbooks and ensemble output
- `rdfOutput/` — RiverWare RDF output datasets (reservoir, streamflow, diversion, EIS, flags)
- `Supporting 24MS Docs/` — 24-Month Study procedure, input workbooks, schedules, and Mead/Powell projection PDFs
- `202605_CRMMS ESP Model Assumptions.pdf`, `CRMMS Operational Reference Guide.pdf` — model assumptions and operational reference
- `0.ReadMe.txt`, `1.SetCrmmsDirectory.bat` — agency-provided setup notes and directory script

Note: RiverWare (the application required to open `.mdl` files) is a third-party product of CADSWES at the University of Colorado Boulder and is not included here.

## License

U.S. Government work. Public domain under 17 U.S.C. § 105.
