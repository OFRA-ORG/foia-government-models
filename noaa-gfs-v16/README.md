# NOAA NCEP — Global Forecast System (GFS) v16

**Agency:** National Oceanic and Atmospheric Administration — National Weather Service, National Centers for Environmental Prediction (NWS/NCEP), Environmental Modeling Center (EMC) and NCEP Central Operations (NCO)

**FOIA request:** Protect the Public's Trust to NOAA, November 3, 2025 — "the human-readable source code for GFS v16+ (current operational version of v16), excluding binaries and outputs." NOAA response DOC-NOAA-2026-000335 (interim) directed the requester to NOAA's public operational code server.

## Description

GFS is NCEP's operational global numerical weather prediction system, providing deterministic forecasts to 16 days and serving as the driver for numerous downstream regional, ocean, and wave models. The operational implementation is assembled from several source components:

- **UFS-ATM atmospheric model** (`fv3gfs.fd`) — the UFS Weather Model: the FV3 dynamical core, the CCPP physics suite, the NEMS coupling framework, WaveWatch III (WW3) coupling, and stochastic physics
- **Data assimilation** (`gsi.fd`) — the Gridpoint Statistical Interpolation (GSI) analysis and the Ensemble Kalman Filter (EnKF)
- **GSI utilities and monitoring** (`gsi_utils.fd`, `gsi_monitor.fd`)
- **Land data assimilation** (`gldas.fd`) — GLDAS
- **Pre-processing / fixed-field generation** (`ufs_utils.fd`) — UFS_UTILS
- **Post-processing** (`gfs_post.fd`) — the Unified Post Processor (UPP)
- **Verification** (`verif-global.fd`) — EMC global verification
- **Workflow and build scripts** — the `sorc/` checkout and build scripts, plus `scripts/`, `ush/`, `parm/`, `jobs/`, `modulefiles/`, `ecf/`, and `driver/`

## Source (what this links to)

The FOIA response pointed to NOAA's operational code server, which serves the current operational GFS v16 build as a browsable source tree:

**Primary source:** https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/gfs.v16.3.33/

This is a point-in-time snapshot of the operational package (served version `gfs.v16.3.33`; the snapshot's build logs are stamped 2026-08-05; provenance captured 2026-09-01). The snapshot is not a single repository — its `sorc/checkout.sh` assembles the build by cloning the following upstream repositories at fixed tags. The resolved commits below are taken from the snapshot's `sorc/checkout-*.log` files and were each re-verified against the upstream tag on 2026-09-01.

| Component (`sorc/…`) | Repository | Tag | Commit |
| --- | --- | --- | --- |
| `fv3gfs.fd` | ufs-community/ufs-weather-model | `GFS.v16.3.26` | `dbf413f912cedb9d014096a3ec62029188f76101` |
| `gsi.fd` | NOAA-EMC/GSI | `gfsda.v16.3.33` | `040b87837d013a63b5881383745865218344a689` |
| `gsi_utils.fd` | NOAA-EMC/GSI-Utils | `gsiutil.v16.3.26` | `2a15d3b514cb05a9c1343e437f134375ad260369` |
| `gsi_monitor.fd` | NOAA-EMC/GSI-Monitor | `gsimon_v16.3.26` | `e1f9f21af16ce912fdc2cd75c5b27094a550a0c5` |
| `gldas.fd` | NOAA-EMC/GLDAS | `gldas_gfsv16_release.v.2.1.0` | `fd8ba62275bb27b0beecbc7c6985c1d61537f299` |
| `ufs_utils.fd` | ufs-community/UFS_UTILS | `ops-gfsv16.3.20` | `0557212bd457ed1102621a25026b85438f7fc3eb` |
| `gfs_post.fd` | NOAA-EMC/UPP | `upp_v8.3.0` | `c5f3053c9b22ac18287f14f9e255b45994cab6a3` |
| `verif-global.fd` | NOAA-EMC/EMC_verif-global | `verif_global_v2.10.0.1` | `5c1f375027fd4ada1681b15c79718763187a3032` |

The `fv3gfs.fd` (UFS Weather Model) submodules resolve to:

| Submodule path | Repository | Commit |
| --- | --- | --- |
| `FMS` | NOAA-GFDL/FMS | `708b8d5e5e044860bfc409c22524eb9fb8b25ff3` |
| `FV3` | NOAA-EMC/fv3atm | `9a9a701333305131fa7fd20ed32b8c3cf8570399` |
| `FV3/atmos_cubed_sphere` | NOAA-EMC/GFDL_atmos_cubed_sphere | `08429c2105d1728eb2e9d35b72947413b2d24531` |
| `FV3/ccpp/framework` | NCAR/ccpp-framework | `e7721098639ee73c2a69ee0e8423e8905549e240` |
| `FV3/ccpp/physics` | NCAR/ccpp-physics | `01ed01fb0b3112e96eb619e0339d88fb0201982f` |
| `NEMS` | NOAA-EMC/NEMS | `72a4285bff3bf411ad22866133302743f654093e` |
| `NEMS/tests/produtil/NCEPLIBS-pyprodutil` | NOAA-EMC/NCEPLIBS-pyprodutil | `1c96952b092e8dc4da03b741ae1e8453fc9fe099` |
| `WW3` | NOAA-EMC/WW3 | `41e7c9ca5d24647cfbb7f685b6543354dd02ea67` |
| `stochastic_physics` | noaa-psd/stochastic_physics | `df932cfa8091374e8c80a0d5075404a1d0d7335b` |

The `gsi.fd` `fix` submodule (NOAA-EMC/GSI-fix) is pinned at `04e1baecd53dce1b2265e66b736d908ec51ee564`. It holds static fixed-field data, not source code, and is outside the request's scope ("excluding binaries and outputs"); it is documented here for completeness but not mirrored.

## Verification

Verified locally with the harness `link_mirror` check: each component is mirrored at the pinned commit above, the working tree is clean, and the pinned commit is still served upstream. Consistent with the other link-only models in this repository, the full source is **not** committed here — this folder records the verified, pinned pointer to the public source; the source itself lives on the NCO server and in the local mirror.

## FOIA status

NOAA's response was styled interim, but the request was scoped to source code only, and the operational source it points to is complete and publicly served — every component the request enumerated (UFS-ATM configuration, physics suite, data assimilation, coupling, and workflow/build scripts) is present in the snapshot at the pins above.

## License

U.S. Government work. Public domain under 17 U.S.C. § 105.
