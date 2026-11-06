# DOE / NETL — Coal Baseline Lifecycle Model

**Agency:** U.S. Department of Energy, National Energy Technology Laboratory (NETL)

**FOIA:** PPT to DOE/NETL, request NETL-2026-02282-F; final response October 29, 2026 ("publicly posted").

## Description

The NETL Coal Baseline Lifecycle Model is a life-cycle assessment (LCA) model for U.S. coal mining and delivery, published by NETL as "LCA Baseline for U.S. Coal Mining and Delivery Data Products." It is distributed as an openLCA model and an Excel implementation, with supporting basin / coal-type / mine-type and transportation inventory data.

## Artifacts as received

These are the model files exactly as published by NETL, retrieved on 2026-11-06 from NETL's Energy Data Exchange (EDX), the repository the FOIA response and the OSTI record point to:

| File | What it is |
| --- | --- |
| `NETLCoalBaselineModel.xlsm` | The Excel implementation of the Coal Baseline Lifecycle Model |
| `netl_coal_delivery_lca_baseline_model_v4_1226.zip` | The openLCA model package (v4) |
| `Basin-CoalType-MineTypeInventories.xlsx` | Basin / coal-type / mine-type inventory data |
| `Transportation_Inventories_02262025.xlsx` | Transportation inventory data |

## Source

- **OSTI record (DOI):** https://doi.org/10.18141/2560300 (OSTI ID 2560300) — "LCA Baseline for U.S. Coal Mining and Delivery Data Products" (Dataset), NETL, 2025.
- **EDX dataset:** https://edx.netl.doe.gov/dataset/lca-baseline-for-us-coal-mining-and-delivery-data-products

The OSTI record resolves to the public EDX dataset page, which serves the files above via direct resource-download links (no account required). This snapshot is a point-in-time capture (retrieved 2026-11-06).

## Verification

Verified locally with the harness `legibility` check: every artifact opens as its declared type (the `.xlsm`/`.xlsx` files are valid OOXML workbooks; the openLCA package is a valid archive). A Tier 0 checksum baseline is recorded at intake.

## Reconstructability

Obtainable and reconstructable as a life-cycle model: the Excel and openLCA model files are the model itself (the request's "model artifacts and supporting materials"), and the supporting inventory data is included. (Note: as an LCA data-model rather than a scripted codebase, "human-readable source code" maps to the openLCA model logic and Excel formulas.)

## License

U.S. Government work (NETL). Public domain under 17 U.S.C. § 105.
