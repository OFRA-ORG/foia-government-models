# DOI / BOEM — Market Simulation Model (MarketSim)

**Agency:** U.S. Department of the Interior, Bureau of Ocean Energy Management (BOEM)
**FOIA Request Number:** DOI-2026-008186
**Date of Final Response:** 2026

## Description

MarketSim is BOEM's economic model for estimating the market effects of Outer Continental Shelf (OCS) oil and gas leasing and development. It simulates supply, demand, prices, and consumer/producer surplus across energy markets (oil, gas, electricity, coal) to support BOEM's net-benefits and environmental/economic analyses. The model is implemented in Microsoft Excel: its logic lives in the workbook formulas, so the workbooks below are both the model artifacts and, in native form, its source.

BOEM released all responsive records in full — 7 documentation PDFs (238 pages) and 5 Excel workbooks — with no redactions or exemptions.

## Contents

**Model workbooks (the model logic, as spreadsheet formulas):**
- `MarketSim_AEO2023_FOIA 2026-008186.xlsx` — the MarketSim model workbook (48 sheets): oil/gas/electricity/coal supply and demand, consumer surplus, prices, elasticities, parameters, and a model-run interface, implemented as tens of thousands of live cross-sheet formulas
- `MS_OECM_E&D_Inputs Template v2025wb_Generic CI and GOA.xlsx` — the exploration-and-development (E&D) cost input model workbook (63 sheets), also formula-driven

**Model input data (static, third-party):**
- `aeo2023_boem_ref2023.xlsx`, `aeo2023_boem_highprice.xlsx`, `aeo2023_boem_lowprice..xlsx` — Annual Energy Outlook 2023 (AEO2023) reference / high-price / low-price projections used as model inputs. These are U.S. Energy Information Administration (EIA) data; they contain no formulas.

**Documentation:**
- `Market-Sim-Model_2015-054.pdf`, `Market-Sim-Model_2017-039.pdf`, `Market-Sim-Model_2021-072.pdf`, `Market-Sim-Model_2023-055.pdf` — dated BOEM OCS Study model-description reports (framework, assumptions, and a de facto version history; 2023-055 is a ~94-page formal model description)
- `MarketSim-Elasticities-Tables.pdf` — model elasticity tables
- `Expert-Elasticity-Summary-Memo-merged-Appended-Expert-Discussion-Notes.pdf` — expert elasticity summary memo with appended discussion notes
- `Implications of Changes to MarketSim Baseline.pdf` — memo on baseline (AEO) input changes

## Provenance and license

The MarketSim model and its OCS Study documentation were prepared for BOEM by its contractor, Industrial Economics, Inc., and released by BOEM as agency records; the OCS Study reports are BOEM publications. The `aeo2023_boem_*` input workbooks are third-party EIA data (Annual Energy Outlook 2023), included as necessary model inputs. These are U.S. Government records released in full via FOIA and are, to the best of our understanding, not subject to copyright restriction in the United States; the EIA data are likewise U.S. Government works. Consult BOEM for any authoritative terms.
