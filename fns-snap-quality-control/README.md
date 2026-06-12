# USDA FNS — SNAP Quality Control Minimodels

**Agency:** U.S. Department of Agriculture, Food and Nutrition Service (FNS)
**FOIA Request Number:** 2025-FNS-08918-F
**Date of Final Response:** 2025

## Description

Source code for the SNAP (Supplemental Nutrition Assistance Program) Quality Control (QC) minimodels used by FNS to process and tabulate QC review data. The codebase is written primarily in Fortran 90 and SAS.

## Contents

- `common/` — Shared Fortran modules used across QC components (FSTAMP, SUPER)
- `qc/data_processing/` — SAS parameter file for data processing
- `qc/fstamp/` — Fortran source for the QC FSTAMP component
- `qc/minimodel/` — Fortran and SAS code for the QC minimodel runner and tally programs
- `List of FY 2023 QC Minimodel programs.pdf` — Index of programs included in the release

## License

U.S. Government work. Public domain under 17 U.S.C. § 105.
