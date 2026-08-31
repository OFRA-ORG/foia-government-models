# Postal Regulatory Commission — Workshare Cost Avoidance Models

**Agency:** Postal Regulatory Commission (PRC)
**FOIA Request Number:** PRC FOIA No. 26-56
**Date of Final Response:** August 14, 2026

## Description

The workshare cost-avoidance models are the spreadsheet cost models used in the PRC's Annual Compliance Determination to estimate USPS avoided costs and set workshare-discount passthroughs across mail categories (First-Class Mail, Marketing Mail, Periodicals, Package Services, and delivery/collection costs). The models are prepared by USPS and filed with the PRC as FY25 library references; the workbooks' logic lives in their Excel formulas.

In response to the request, PRC stated it does not hold the requested "source code" as a filed record, but that the models are publicly available in its filing library, and attached a directory of links (included here). The referenced workbooks were retrieved from the PRC filings. One package, USPS-FY25-19 (UDCModel25), additionally ships the SAS source used to build its delivery-cost inputs.

## Contents

- `Workshare Cost Model Directory.pdf` — PRC's directory mapping each FY25 cost model to its library reference, filing number, and Excel file (the "attached spreadsheet" from the response).
- Model workbooks (native Excel, formula-driven), by library reference:
  - `USPS-FY25-10.FCM.Letters` / `.MM.Letters` — FCM and Marketing Mail Presort Letters mail-processing models
  - `USPS-FY25-11.FCM.Flats` / `.MM_CR.Flats` / `.POC.Flats` — FCM, Marketing Mail, and Periodicals Presort Flats models
  - `USPS-FY25-12` — Marketing Mail Parcels mail-processing model
  - `USPS-FY25-13.MKTG` / `.PER` — Marketing Mail and Periodicals Destination Entry models
  - `USPS-FY25-15.BPM` — Bound Printed Matter mail-processing model
  - `USPS-FY25-21 BRM_QBRM` — Business Reply Mail model
- `USPS-FY25-19/` — the Delivery Costs by Shape package (UDCModel25): the `UDCModel25.xlsx` and `UDCInputs25.xlsx` workbooks, the KL-table workbooks, and the SAS programs, input files, and run logs used to construct the delivery-cost inputs.

Filenames retain PRC's own revision labels (e.g., `_MODS_Update_2026.02.06`) as received.

## License

Public regulatory records filed with and released by the Postal Regulatory Commission; publicly available in the PRC filing library. Prepared by the U.S. Postal Service and filed with the PRC.
