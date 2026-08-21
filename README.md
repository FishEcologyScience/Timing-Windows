# 2026_Midwood_Timing_Windows

![Journal DOI](https://img.shields.io/badge/DOI-pending%20(in%20submission%20to%20CJFAS)-lightgrey)
![Zenodo DOI](https://img.shields.io/badge/DOI-pending%20Zenodo%20archive-lightgrey)


## Overview

Code and data processing pipeline supporting the manuscript:

> Midwood JD, Bzonek PA, Piczak M, Theÿsmeÿer T, Braun DC, Tunney TD, Naman SM, Lake C, Brownscombe JW. In submission. Efficacy of exclusion periods for mitigating harm to freshwater fishes moving into a Great Lakes coastal wetland. Canadian Journal of Fisheries and Aquatic Sciences.

Exclusion periods (also called timing windows or restricted activity periods) restrict
harmful in-water and near-water activities during critical fish life-history windows.
This study used a ~25-year dataset (1996-2022) collected at an actively managed fish
passage barrier in western Lake Ontario (the Cootes Paradise Fishway) to assess the
efficacy of coolwater (15 March - 31 May) and warmwater (01 May - 15 July) exclusion
periods for 16 fish species. The two periods together protect all species well (no
species with more than 30% of its spawning run left unprotected), but each period is
less effective on its own (6 of 16 species with more than 30% unprotected). Early
spring-spawning cold- and coolwater species (e.g., Rainbow Trout, Northern Pike) are
most at risk if in-water works persist into the first few weeks of the coolwater
period. Interannual variation in peak arrival timing was considerable across the 16
species, and the manuscript recommends developing within-year forecast models (e.g.,
from seasonal water temperature) to support risk-based decisions about extending works
into an exclusion period.

## Data Sources

- ~68,000 fish-passage records (1996-2022) collected at the Cootes Paradise Fishway barrier, Royal Botanical
  Gardens, western Lake Ontario.
- Restricted Activity Period (RAP) / exclusion-period definitions applied per species, following DFO's
  standard timing windows for southern Ontario:

  | Season | Species / group | Window | Day Of Year |
  |---|---|---|---|
  | Spring | Walleye, Pike | 15 Mar - 31 May | 74 - 151 |
  | Spring | Large/Smallmouth Bass | 01 May - 15 Jul | 121 - 196 |
  | Spring | Other/unknown spawners | 15 Mar - 15 Jul | 74 - 196 |
  | Fall | Lake Trout | 01 Oct - 31 May | 274-151 |
  | Fall | Lake Whitefish / Lake Herring | 15 Oct - 31 May | 288-151 |
  | Fall | Other/unknown spawners | 01 Oct - 31 May | 274-151 |


## Repository Structure

| Folder | Contents |
|---|---|
| `01_Scripts/` | R scripts for data preparation, plotting, and modelling (see pipeline below) |
| `02_Data/` | Raw base dataset plus derived weekly/yearly summary datasets |
| `03_Output/` | Figures and summary tables; `03_Output/OldFigures/` holds superseded versions kept for reference |

## Analysis Pipeline

`Script_0-0_UserInterface.R` sources the core pipeline in order. 

**Data preparation**
- `Script_0-0_UserInterface.R` - console script; sources the pipeline scripts below in order
- `Script_0-1_Fishway_TW_DataPrep.R` - cleans the raw barrier dataset and derives the weekly/yearly summary datasets and RAP-overlap tables used throughout

**Community and species plots**
- `Script_1-1_Fishway_TW_CommunityPlots.R` - community-level weekly catch-proportion heatmap by thermal guild
- `Script_2-1_Fishway_TW_SpeciesPlots.R` - species-specific weekly catch-proportion heatmaps by year
- `Script_2-2_Fishway_TW_SpeciesCumulativePlots.R` - cumulative catch-proportion plots by species and year *(run separately - not sourced by Script_0-0)*

**Run width and abundance**
- `Script_3-1_Fishway_TW_WidthAbundance.R` - helper functions computing run "width" (first/last detection window) by species and year
- `Script_3-2_Fishway_TW_WidthAbundance_GLMM.R` - mixed model relating annual catch to run duration

**Risk within exclusion periods**
- `Script_4-1_Fishway_TW_RiskWithinRAP.R` - logistic models and plots for risk of exposure within the exclusion periods
- `Script_4-2_Fishway_TW_RiskWithinRAP_Summaries.R` - summary risk figures and tables combining thermal-guild and RAP-proportion data *(run separately - not sourced by Script_0-0)*


## Citation

If you use this code or data, please cite the associated manuscript:

> Midwood JD, Bzonek PA, Piczak M, Theÿsmeÿer T, Braun DC, Tunney TD, Naman SM, Lake C, Brownscombe JW. In submission. Efficacy of exclusion periods for mitigating harm to freshwater fishes moving into a Great Lakes coastal wetland. Canadian Journal of Fisheries and Aquatic Sciences.

## License

This project is licensed under the [MIT License](LICENSE). Copyright (c) 2026 His
Majesty the King in Right of Canada, as represented by the Minister of Fisheries and
Oceans.

## Author

Jonathan D. Midwood (corresponding contact)
jon.midwood@dfo-mpo.gc.ca
