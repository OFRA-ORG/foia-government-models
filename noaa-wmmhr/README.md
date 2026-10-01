# NOAA — World Magnetic Model High Resolution (WMMHR) 2025

**Agency:** NOAA National Centers for Environmental Information (NCEI) / CIRES Geomagnetism (University of Colorado Boulder)

**FOIA:** PPT to NOAA; DOC-NOAA-2026-001882 (full grant, publicly available; 4 records)

## Description

The World Magnetic Model High Resolution (WMMHR2025): core field and secular-variation coefficients for degrees n=1-15 plus the crustal field (n=16 through n=133), i.e. 18,210 non-zero coefficients. Computes geomagnetic field components, declination, inclination, and their rates of change, with uncertainty estimates.

## Source (pinned mirror)

Mirrored from the public repository/repositories below at the commit pinned on 2026-10-01 (the reviewed HEAD). Consistent with the other link-only models in this repository, the source itself is not committed; this records the verified, pinned pointer.

| Repository | URL | Pinned commit |
| --- | --- | --- |
| `wmmhr` | https://github.com/CIRES-Geomagnetism/wmmhr | `b495af93c8ee1b355cf3db429319993d7f529da4` |

## Reconstructability note

Complete, self-contained Python implementation that INCLUDES the model coefficients (18,210 non-zero), MIT-licensed and distributed on PyPI. Fully reconstructable: an outsider can install and run the exact agency model from the public source and bundled coefficients.

## License

See each upstream repository's license (WMMHR: MIT; MIRS: CC0-1.0).
