# NOAA — Flood Inundation Mapping (OWP)

**Agency:** NOAA National Weather Service, Office of Water Prediction (National Water Center)

**FOIA:** PPT to NOAA; DOC-NOAA-2025-001730 (publicly available)

## Description

The operational flood inundation mapping toolchain: inundation-mapping (HAND terrain method), ras2fim (HEC-RAS-derived synthetic rating curves), and ripple1d (HEC-RAS repurposing for FIM/SRC production), integrated with the National Water Model.

## Source (pinned mirror)

Mirrored from the public repository/repositories below at the commit pinned on 2026-09-15 (the reviewed HEAD). Consistent with the other link-only models in this repository, the source itself is not committed; this records the verified, pinned pointer.

| Repository | URL | Pinned commit |
| --- | --- | --- |
| `inundation-mapping` | https://github.com/NOAA-OWP/inundation-mapping | `055e50ad8a464f9b0a43ae7e06fa9cdf32d7d4eb` |
| `ras2fim` | https://github.com/NOAA-OWP/ras2fim | `bc1f00fdccbd2588e42284f90abcb0afbfa2daef` |
| `ripple1d` | https://github.com/NOAA-OWP/ripple1d | `5b08f478617a44a9b5e09bb843dd23e1d62233be` |

## Reconstructability note

Full Python source with Docker, declared dependencies, docs, and tests. Deterministic hydraulic/terrain methods (no trained weights). Full reconstruction additionally requires HEC-RAS (free, USACE) and large public input datasets hosted on AWS S3.

## License

Government work / open-source as published by the agency. See each upstream repository's LICENSE.
