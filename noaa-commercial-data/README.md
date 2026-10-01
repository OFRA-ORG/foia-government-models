# NOAA — Commercial Data Program (MIRS + CRTM)

**Agency:** NOAA NESDIS / Center for Satellite Applications and Research (STAR) and Environmental Modeling Center (EMC)

**FOIA:** PPT to NOAA; DOC-NOAA-2026-001729 (publicly available / partial grant)

## Description

NOAA's Commercial Data Program (CDP) evaluates and purchases commercial environmental data (e.g. GNSS radio-occultation). For the source-code portion, NOAA pointed to two processing tools used in the retrieval/assessment chain: the Microwave Integrated Retrieval System (MIRS) and the Community Radiative Transfer Model (CRTM / JCSDA_CRTM), plus numerous NESDIS reports and cost-benefit analyses for the model-artifacts portion.

## Source (pinned mirror)

Mirrored from the public repository/repositories below at the commit pinned on 2026-10-01 (the reviewed HEAD). Consistent with the other link-only models in this repository, the source itself is not committed; this records the verified, pinned pointer.

| Repository | URL | Pinned commit |
| --- | --- | --- |
| `mirs` | https://github.com/NOAA-STAR/Microwave-Integrated-Retrieval-System-MIRS | `e40709e80266606f42b455fa8d559a3ca47181ec` |
| `crtm` | https://github.com/NOAA-EMC/JCSDA_CRTM | `b4c2e86a1aa3f22b0a7801817994e4a056dec92b` |

## Reconstructability note

Code obtained but reconstructability is QUALIFIED. (1) MIRS: the source is published inside a committed tarball (mirs_v11r10_..._code.tar.gz) alongside its ATBD and user manual (CC0) - obtainable by extraction. (2) CRTM (branch release/REL-2.3.0): full Fortran source (170 files in libsrc/) and build system are present, but its coefficient data is Git-LFS-hosted and at least one LFS object (fix/AerosolCoeff/Big_Endian/AerosolCoeff.bin) returns 404 upstream, so the mirror holds LFS POINTERS, not the binaries; the coefficient data needed to actually run CRTM is not fully retrievable from the pinned release. (3) Scope: MIRS and CRTM are general retrieval / radiative-transfer tools used by CDP, not the whole Commercial Data Program (a data-procurement program) as a single model. The code is genuinely obtained and verified, but these caveats mean CDP is not a clean end-to-end reconstruction the way WMMHR is.

## License

See each upstream repository's license (WMMHR: MIT; MIRS: CC0-1.0).
