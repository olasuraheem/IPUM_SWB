# READ ME 
# Temporal Changes in Youth Subjective Well-Being in Nigeria, 2016–2021

Analysis of affective and evaluative well-being among Nigerian youth aged 15–24 using IPUMS-harmonised MICS data (2016–2017 and 2021), linked with state-year structural indicators from the Global Data Lab (GVI, Cleanliness Index, Gini inequality). Examines temporal decline and whether state-level vulnerability moderates the trend (Year × GVI interaction).

### Table of Contents

- [Overview](#overview)
- [Key Findings](#key-findings)
- [Data Sources](#data-sources)
- [Requirements](#requirements)
- [How to Reproduce](#how-to-reproduce)
- [Folder Structure](#folder-structure)
- [Results Summary](#results-summary)
- [License](#license)
- [Contact & Citation](#contact--citation)

## Overview

This repository contains the code and documentation for the study examining changes in youth subjective well-being (SWB) in Nigeria between 2016 and 2021, with a focus on the role of state-level structural factors.

## Key Findings

- Affective happiness declined significantly from 2016 (mean 4.5, 61% very happy) to 2021 (mean 4.1, 41% very happy; p < 0.001)
- The negative association between state-level vulnerability (GVI) and happiness strengthened in 2021 (Year × GVI interaction)
- Female gender, higher education, and rural residence were protective
- Formerly/never married status, non-Hausa ethnicity, and worsened perceived well-being trajectories were strong risk factors
- Low between-state variation (ICC ≈ 3.4%) supports survey-weighted models as primary

## Data Sources

- **Individual-level**: IPUMS-harmonised Nigeria MICS 2016 (MICS5) and 2021 (MICS6) → youth aged 15–24 (unweighted n = 41,175)
- **Contextual**: Global Data Lab (GDL) → state-year values for:
  - Global Vulnerability Index (GVI)
  - Institutional quality (Cleanliness Index / fullsci)
  - Gini coefficient of income inequality
  - **GDL data of years 2016 and 2021 were downloaded**
- Final cleaned dataset: `4youth_happiness.rds`

## Requirements

tidyverse,
               srvyr,
               survey,
               janitor,
               broom,
               gtsummary,
               car,
               sandwich,
               questionr,
               jtools,
               lme4,
               lmeSurvey,
               sjPlot,
               ggplot2

- R version ≥ 4.4.1
- Required packages:
  ```r
  install.packages(c( "tidyverse", "srvyr", "janitor", "broom", "jtools", "questionr",
    "survey", "gtsummary", "lme4", "haven", "forcats", "dplyr", "ggplot2", "readr"
  ))
How to Reproduce
Follow these steps to fully reproduce the analysis from raw data to tables and robustness check.
Step 1: Clone or download the repository
Bashgit clone https://github.com/olasuraheem/youth-swb-nigeria.git
cd youth-swb-nigeria
(or download ZIP and extract)
Step 2: Install required packages (once) 
Rinstall.packages(c(tidyverse", "srvyr", "janitor", "broom", "jtools", "questionr",
    "survey", "gtsummary", "lme4", "haven", "forcats", "dplyr", "ggplot2", "readr"))
Step 3: Run the analysis scripts in this order

Data cleaning and preparationRsource("01_data_cleaning.R")
# Output: 4youth_happiness.rds (cleaned dataset)
Descriptive statistics (Table 1) Rsource("02_descriptive_and linkage.R")
# Output: Table 1 (descriptive table by year)
Main models (Tables 2, 3, 4) Rsource("03_modelling.R")
# Outputs:
# - Table 2: Sociodemographic models (separate 2016 vs 2021)
# - Table 3: Main model with Year × GVI interaction
# - Table 4: Three contextual models (Cleanliness, GVI, Gini)
Robustness check (linear mixed model)Rsource("lmm_robustness.R")
# Output: ICC ≈ 3.34%, confirming low between-state variation

All scripts assume the cleaned dataset (4youth_happiness.rds) is in the working directory.

Quick start (if you just want to load and explore)
Rlibrary(readr)
library(survey)

# Load cleaned data
swb_final_clean <- read_rds("4youth_happiness.rds")

# Create survey design object
multi_design_final <- svydesign(
  ids = ~cluster,
  strata = ~state,
  weights = ~scaled_weight,
  data = swb_final_clean,
  nest = TRUE
)

# quick summary of happiness by year
svyby(~sat_happy_fn, ~year, multi_design_final, svymean)

Folder Structure
textyouth-swb-nigeria/
├── README.md
├── data_cleaning.R
├── descriptive_analysis.R
├── main_models.R
├── lmm_robustness.R
├── 4youth_happiness.rds          # cleaned dataset
├── results_tables.docx           # exported tables (optional)
└── figures/                      # plots (if created)
Results Summary

Table 1: Descriptive characteristics by year (happiness decline, demographics, TV watching, perceived trends)
Table 2: Sociodemographic associations with life satisfaction (separate 2016 vs 2021)
Table 3: Main model — year effect + Year × GVI interaction
Table 4: Three separate contextual models (Cleanliness Index, GVI, Gini)

License
MIT License 
Contact & Citation
Kamaldeen Sunkanmi Abdulraheem
Lagos, Nigeria
olasuraheem@gmail.com 
Preferred citation 
Sunkanmi, A. K. (2026). Temporal Changes in Youth Subjective Well-Being in Nigeria, 2016–2021. GitHub repository: https://github.com/yourusername/youth-swb-nigeria