# BOEM — Stochastic Collision Risk Assessment for Movement (SCRAM)

**Agency:** U.S. Department of the Interior, Bureau of Ocean Energy Management (BOEM)
**FOIA Request Number:** DOI-2026-008000
**Date of Final Response:** July 20, 2026

> **Note:** In response to this FOIA request, BOEM provided no documentation files of its own. It stated that the materials are "already publicly available" and referred the requester to a third-party public repository and hosted web application (see **Source** below). This directory therefore *links* to the model rather than mirroring it. The source code is authored and maintained by a third party (the Biodiversity Research Institute) under no stated license. The linked repository is actively maintained and its contents change over time; the pinned commit under **Source** identifies the exact state reviewed on 2026-07-27.

## Description

SCRAM (Stochastic Collision Risk Assessment for Movement) is a stochastic collision-risk model that uses telemetry-based movement data to estimate the risk of collisions between offshore wind turbines and three U.S. Endangered Species Act–protected bird species in the U.S. Atlantic: the Roseate Tern (*Sterna dougallii*), Piping Plover (*Charadrius melodus*), and Red Knot (*Calidris canutus*). It is implemented as an R Shiny web application. BOEM uses SCRAM to inform Endangered Species Act consultations, National Environmental Policy Act analyses, and offshore wind permitting decisions.

The collision-risk estimation has four components: (1) movement modeling of monthly occupancy over the Northeast U.S. shelf, (2) linking monthly population estimates to occupancy rates to estimate density, (3) flight-height estimation from Motus and GPS telemetry, and (4) a collision-risk model producing collision estimates for a specified turbine array. SCRAM was funded by BOEM and the U.S. Fish and Wildlife Service (USFWS).

## Source

BOEM's response directed the requester to the following publicly available resources:

- **Source repository:** https://github.com/Biodiversity-Research-Institute/SCRAM2
- **Pinned commit reviewed 2026-07-27:** [`aa49c53`](https://github.com/Biodiversity-Research-Institute/SCRAM2/tree/aa49c532f3c37c17407b789bb01e7b17856d977b) — latest commit on the `main` branch, dated 2025-01-14; repository last pushed 2025-05-27
- **Hosted web application:** https://briloon.shinyapps.io/SCRAM2/

At the time of review, the repository was an R Shiny application. Its top level contained `app_SCRAM2.R` (main application), `SCRAM2_shiny.Rproj`, `README.md`, and the folders `GPS Movement Models`, `Motus Movement Models`, `scripts`, `data`, `www`, and `rsconnect`. The upstream README described the then-current release as **v2.1.8** and stated that updates were expected to continue through at least 2026. Because the public repository is a moving target, the specific version BOEM relied upon for any given regulatory decision may differ from what is currently posted.

## License / provenance

The SCRAM software linked above is a federally funded **third-party** work authored by the Biodiversity Research Institute. It is **not** a U.S. Government work, carries no stated license in the upstream repository, and is therefore linked here rather than redistributed. Consult the upstream repository for the source code and any applicable terms.
