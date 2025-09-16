
# Evolution of the East German Wage Structure

This repository contains code, data preparation scripts, and supporting material for the paper:

**Eduard Brüll (ZEW Mannheim) & Christina Gathmann (LISER, University of Luxembourg and CEPR)**  
*"Evolution of the East German Wage Structure"*:contentReference[oaicite:0]{index=0}



## Project Structure

```
EastGermanWageStructure/
├── EastGermanWageStructure.Rproj   # R project file
├── FDZ1489/                         # Stata .do files for SIAB/BHP remote data preparation
├── functions/                       # R helper functions (factors, general utilities, plotting)
├── logs/                            # Log files from remote Stata runs
├── plots/                           # Generated plots for the paper from remote run logs
├── smalldata/                       # Supplementary input data (Destatis, union coverage, KLEMS, etc.)
├── paper\_plots.R                   # R script for producing paper figures/tables from remote run logs
├── prepare\_KLEMS.R                 # Script to process KLEMS data
├── retrieve\_destatis\_data.R       # Script to download and cache Destatis data
```

---

## Data

- **Administrative data**: SIAB worker histories and BHP plant-level information (remote access via FDZ IAB).  
- **Supplementary data**: Destatis (national accounts, employment, CPI), KLEMS, and union coverage (IAB Establishment Panel).  
- **Note**: Access to restricted FDZ data requires an approved project. Only small derived datasets and public sources are stored in `smalldata/`.

## Workflow

1. **Data Preparation and textual analysis output in logs(Stata)**  
   - Run the scripts in `FDZ1489/` via josua (starting from `master.do`)
   - Produces raw output of the Ppaer analyses saved in logs

2. **Analysis (R)**  
   - Core functions in `functions/` provide factor definitions, general utilities, and plotting helpers.
   - `paper_plots.R` reproduces the figures for the manuscript.
   - `prepare_KLEMS.R` and `retrieve_destatis_data.R` handle additional external datasets.

## Paper Abstract

We analyze the evolution of the wage structure in East Germany over the past two decades and compare it to West Germany. Wage inequality rose in both regions between 1995 and 2009, with dispersion in East Germany exceeding that of the West, especially at the top. Since 2009, inequality has leveled off and even declined in the East, largely due to sectoral minimum wages. Union decline, in contrast, played a limited role in East Germany. Demand-side forces account for much of the rise in top-end inequality:contentReference[oaicite:1]{index=1}.


## Citation

If you use this code, please cite:

```

Brüll, Eduard & Gathmann, Christina.
"Evolution of the East German Wage Structure."
ZEW Mannheim & LISER, University of Luxembourg and CEPR. 2025.

```

## Contact

- **Eduard Brüll** – ZEW Mannheim  
  📧 [eduard.bruell@zew.de](mailto:eduard.bruell@zew.de)

