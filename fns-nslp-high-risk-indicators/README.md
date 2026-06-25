# USDA FNS — NSLP High-Risk Indicators of Certification Error Model

**Agency:** U.S. Department of Agriculture, Food and Nutrition Service (FNS)
**FOIA Request Number:** 2026-FNS-04833-F
**Date of Final Response:** January 2026

## Description

Source code and documentation for the model used to identify school food authorities (SFAs) at high risk of certification error in the National School Lunch Program (NSLP). The model was developed by Mathematica Policy Research in 2011 using data from school years 2004–2008.

The model consists of a series of Tobit regressions that predict certification error rates at the SFA (school district) level across six error types — free-certified/not-eligible, free-certified/reduced-price-eligible, reduced-price-certified/free-eligible, reduced-price-certified/not-eligible, not-certified/free-eligible, and not-certified/reduced-price-eligible — as well as aggregate overall certification error. Model parameters were used to generate certification error risk scores for all SFAs reporting to the FNS-742 Verification Summary Report (VSR), which in turn informed a monitoring tool used by FNS.

FNS notes that the model is no longer in active use.

## Contents

- `programs/data-processing/` — SAS programs that read and merge raw source data (VSR, CCD, LAUS, SAIPE) into linked analysis files
- `programs/analysis/apec/` — SAS and Stata programs that estimate and validate Tobit models for APEC overall certification error and generate SFA-level risk scores
- `programs/analysis/macro/` — Shared SAS macros for hot-deck imputation and serpentine sorting used in data processing
- `programs/analysis/rora/` — SAS and Stata programs for the RORA administrative error model
- `programs/analysis/vsr/` — SAS programs that construct the VSR analysis file used for model validation and risk score application
- `docs/` — Model documentation including variable lists, analysis file descriptions, final model coefficient tables, and methodology briefings

## License

U.S. Government work. Public domain under 17 U.S.C. § 105.
