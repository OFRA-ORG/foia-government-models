# NOAA GFDL — OM4 Ocean Model Configuration

**Agency:** National Oceanic and Atmospheric Administration, Geophysical Fluid Dynamics Laboratory (NOAA GFDL)

## Description

OM4 is the ocean model configuration used by NOAA GFDL as the ocean component of its coupled climate models, including CM4 and ESM4. It runs on MOM6 (Modular Ocean Model version 6) at 0.25-degree horizontal resolution (OM4_025) and couples with the SIS2 sea-ice model.

The configuration includes:

- **Grid and bathymetry** — horizontal grid files and ocean bottom topography
- **Parameter files** — MOM6 and SIS2 input namelists controlling model physics and numerics
- **Sea-ice settings** — SIS2 configuration files
- **Surface forcing** — atmospheric boundary condition inputs
- **Submodule references** — pinned versions of the MOM6 and SIS2 source code

## Source

The OM4 configuration is publicly available in the NOAA-GFDL/MOM6-examples repository on GitHub:

**Repository:** https://github.com/NOAA-GFDL/MOM6-examples  
**Branch:** `dev/gfdl`  
**Path:** `ice_ocean_SIS2/OM4_025/`

## License

U.S. Government work. Public domain under 17 U.S.C. § 105.
