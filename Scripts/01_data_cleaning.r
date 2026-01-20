# =============================================================================
#  Temporal Changes in Youth Subjective Well-Being in Nigeria, 2016–2021
#  Data Preparation Script – Part 1: Environment & Data Import
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 1. Environment Setup
# ─────────────────────────────────────────────────────────────────────────────

# Clear workspace (optional – comment out if you want to keep previous objects)
rm(list = ls())

# Set working directory – use an absolute path or project-relative path
# Recommended: use here::here() if you install the 'here' package
# here::i_am("code/01_data_import.R")  # ← uncomment if using 'here'

setwd("~/Documents/Working directory/IPUM")  # ← change to your actual path

# Check that the directory exists
if (!dir.exists(getwd())) {
  stop("Working directory does not exist. Please check the path: ", getwd())
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. Package Loading
# ─────────────────────────────────────────────────────────────────────────────

# pacman makes loading/installing packages safe and concise

# install pacman package if not installed

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  # Core data import & labels
  haven,          # read_dta(), labelled data
  
  # Tidyverse ecosystem
  tidyverse,      # dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats
  
  # Survey analysis
  survey,         # svydesign, svyglm, etc.
  srvyr,          # tidy survey wrappers
  
  # Table & model output
  gtsummary,      # beautiful summary tables
  broom,          # tidy model results
  jtools,         # summ() for nice regression output
  sjPlot,         # tab_model() for publication tables
  
  # Model extensions 
  lme4,           # mixed models
 
  # Helper tools
  janitor,        # clean_names()
  questionr       # freq() for quick tables (optional)
)

# ─────────────────────────────────────────────────────────────────────────────
# 3. Import Data
# ─────────────────────────────────────────────────────────────────────────────

# Path to the merged Stata file (adjust if name/location differs)
dta_file <- "Happy_Merged.dta"


# Import Stata file – haven preserves variable and value labels
message("Importing merged MICS data from: ", dta_file)
happy_data <- haven::read_dta(dta_file)

# Quick sanity checks after import
dim(happy_data)                    # number of rows & columns
names(happy_data)[1:10]            # first 10 variable names
str(happy_data[c("year", "gender", "agewm", "agemn", "lshappy")])  # look at key vars

# Optional: view frequency of year to confirm both waves are present
questionr::freq(happy_data$year)

# Optional: save a quick copy in case of mistakes later

# saveRDS(happy_data, file = "happy_data_imported.rds") uncomment if necessary 
# message("Initial import complete. Backup saved as happy_data_imported.rds")

# ─────────────────────────────────────────────────────────────────────────────
# 4. Initial Cleaning & Selection
# ─────────────────────────────────────────────────────────────────────────────

clean_happy <- happy_data %>%
  janitor::clean_names() %>%  # snake_case column names
  select(
    cluster, strata = geo1_ng, final_weight, year,
    residence = urban, state = geo1_ng, marital_status = marst,
    ethnicity = ethnic_ng, gender, literacy = lit, media_exposure = tvfreq,
    wscore, windex5, edlevelmn, edlevelwm, agemn, agewm,
    lshappy, lsoverall, lsladder,
    lshealth, lshome, lsincome, lsjob, lslooks, lsschool,
    lslastyr, lsnextyr
  )
# alternatively clean all varibales in dataset using the clean_names function
clean_happy <- clean_names(happy_data)

# ─────────────────────────────────────────────────────────────────────────────
# 4. Harmonisation & Youth Subset
# ─────────────────────────────────────────────────────────────────────────────

happy_data2 <- clean_happy %>%
  # Coalesce gender-specific variables
  mutate(
    education = haven::as_factor(coalesce(edlevelmn, edlevelwm), levels = "labels"),
    age_continuous = coalesce(as.numeric(agemn), as.numeric(agewm)),
    
    # Affective happiness (consistent across years)
    sat_happy_num = as.numeric(lshappy) %>% na_if(8) %>% na_if(9),
    sat_happy_rev = 6 - sat_happy_num,  # reverse: 5 = very happy → 1 = very unhappy
    
    # Label happiness levels
    sat_happy_rev = haven::labelled(sat_happy_rev,
                                    labels = c("Very unhappy" = 1,
                                               "Somewhat unhappy" = 2,
                                               "Neither" = 3,
                                               "Somewhat happy" = 4,
                                               "Very happy" = 5)),
    
    # Media exposure (TV frequency)
    media_exposure = haven::as_factor(media_exposure),
    media_exposure = forcats::fct_recode(media_exposure,
                                         "Almost every day"      = "1",
                                         "At least once a week"  = "2",
                                         "Less than once a week" = "3",
                                         "Not at all"            = "4",
                                         "Missing"               = "5",
                                         "NIU (not in universe)" = "6") %>%
      forcats::fct_explicit_na(na_level = "Missing / NIU"),
    
    # Wealth quintile labels
    windex5 = haven::as_factor(windex5),
    windex5 = forcats::fct_recode(windex5,
                                  "1 (Poorest)"   = "1",
                                  "2"             = "2",
                                  "3"             = "3",
                                  "4"             = "4",
                                  "5 (Richest)"   = "5",
                                  "Missing / NIU" = "0",
                                  "Missing / NIU" = "NIU (not in universe)",
                                  "Missing / NIU" = "Missing") %>%
      forcats::fct_explicit_na(na_level = "Missing / NIU")
  ) %>%
  
  # Rename domain variables
  rename(
    sat_health     = lshealth,
    sat_home       = lshome,
    sat_income     = lsincome,
    sat_job        = lsjob,
    sat_looks      = lslooks,
    sat_school     = lsschool,
    sat_life_ladder = lsladder,
    sat_life_likert = lsoverall,
    sat_happy_fn   = sat_happy_rev,
    wealth         = windex5
  ) %>%
  
  # Final domain cleaning (numeric + NA codes)
  mutate(across(c(sat_health:sat_school, sat_life_ladder, sat_life_likert, sat_happy_fn),
                ~ as.numeric(.) %>% na_if(8) %>% na_if(9) %>% na_if(99))) %>%
  
  # Perceived trends (factor)
  mutate(
    trend_past   = haven::as_factor(lslastyr),
    trend_future = haven::as_factor(lsnextyr)
  ) %>%
  
  # Final selection
  select(
    cluster, state, strata, final_weight, year,
    residence, gender, age_continuous, wealth,
    education, marital_status, ethnicity, literacy, media_exposure,
    sat_happy_fn, sat_life_ladder, sat_life_likert,
    sat_health, sat_home, sat_income, sat_job, sat_looks, sat_school,
    trend_past, trend_future
  )

# ─────────────────────────────────────────────────────────────────────────────
# 4. Youth Subset & Final Checks
# ─────────────────────────────────────────────────────────────────────────────

swb_analysis_data <- happy_data2 %>%
  filter(age_continuous >= 15 & age_continuous <= 24) %>%
  mutate(across(where(is.factor), droplevels))

# Critical verification: confirm both years are present
message("Years present in youth subset:")
print(table(swb_analysis_data$year, useNA = "always"))

# Save final cleaned youth dataset
saveRDS(swb_analysis_data, file = "4youth_happiness.rds")
message("Cleaned youth dataset saved: 4youth_happiness.rds")