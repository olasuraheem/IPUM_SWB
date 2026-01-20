# =============================================================================
# 02_descriptive_and_linkage.R
# Purpose: 
#   1. Load final cleaned youth dataset
#   2. Re-create survey design object
#   3. Generate polished Table 1 (descriptive summary by year)
#   4. Save Table 1 as Word document
#   5. Link with Global Data Lab (GDL) state-year indicators
#   6. Check join quality (missingness)
#   7. Compute weighted correlations between key variables
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 1. Environment & Packages
# ─────────────────────────────────────────────────────────────────────────────

# if your working directory and space is still active with, you jump to data imprtation

rm(list = ls())  # optional: clear workspace,

# Set working directory (change to your actual path)
setwd("~/Documents/Working directory/IPUM")

# Check directory exists
if (!dir.exists(getwd())) stop("Working directory not found: ", getwd())

# Load/install required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,      # dplyr, readr, forcats, etc.
  survey,         # svydesign, svyvar, etc.
  srvyr,          # tidy survey wrappers (optional but useful)
  gtsummary,      # beautiful tables
  flextable,      # export to Word
  janitor,        # clean_names() if needed later
  jtools          # svycor() for weighted correlations
)

# ─────────────────────────────────────────────────────────────────────────────
# 2. Load Cleaned Youth Dataset
# ─────────────────────────────────────────────────────────────────────────────

rds_file <- "4youth_happiness.rds"

if (!file.exists(rds_file)) {
  stop("RDS file not found: ", rds_file, 
       "\nCurrent wd: ", getwd(),
       "\nPlease check file name and location.")
}

message("Loading cleaned youth dataset: ", rds_file)
swb_final_clean <- read_rds(rds_file)

# Quick verification
dim(swb_final_clean)
table(swb_final_clean$year, useNA = "always")  # confirm both years

# ─────────────────────────────────────────────────────────────────────────────
# 3. Re-initialize Survey Design Object
# ─────────────────────────────────────────────────────────────────────────────

final_design <- svydesign(
  ids    = ~cluster,
  strata = ~state,
  weights = ~scaled_weight,
  data   = swb_final_clean,
  nest   = TRUE
)

message("Survey design object created successfully.")

# ─────────────────────────────────────────────────────────────────────────────
# 4. Generate Polished Descriptive Table 1
# ─────────────────────────────────────────────────────────────────────────────

table_final <- final_design %>%
  tbl_svysummary(
    by = year,
    include = c(
      sat_happy_fn, sat_life_ladder, sat_life_likert,
      gender, age_group, age_continuous, education_clean,
      marital_clean, ethnicity_clean, wealth, media_exposure,
      residence_clean, trend_past, trend_future
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    missing = "no",
    label = list(
      sat_happy_fn    = "Affective Happiness (Reversed 1–5)",
      sat_life_ladder = "Cantril Ladder (0–10, 2021 only)",
      sat_life_likert = "Verbal Life Satisfaction (1–5, 2016 only)",
      gender          = "Gender",
      age_continuous  = "Age (continuous, years)",
      education_clean = "Education Level",
      marital_clean   = "Marital Status",
      ethnicity_clean = "Ethnicity (Household Head)",
      wealth          = "Household Wealth Quintile",
      media_exposure  = "Frequency of Watching TV",
      residence_clean = "Place of Residence",
      trend_past      = "Well-being Compared to Last Year",
      trend_future    = "Expected Well-being Next Year"
    )
  ) %>%
  add_overall(last = TRUE) %>%
  add_p(test = list(all_continuous() ~ "svy.t.test",
                    all_categorical() ~ "svy.chisq.test")) %>%
  bold_labels() %>%
  bold_p(t = 0.05) %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Survey Year**") %>%
  modify_caption("Table 1: Sociodemographic Covariates and Subjective Well-Being by Survey Year (Weighted Estimates)")

# Display table in viewer
table_final

# ─────────────────────────────────────────────────────────────────────────────
# 5. Save Table 1 as Word document
# ─────────────────────────────────────────────────────────────────────────────

table_final %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "1.0 Socio-demographic.docx")

message("Table 1 saved as: 1.0 Socio-demographic.docx")

# ─────────────────────────────────────────────────────────────────────────────
# 6. Data Linkage – Merge with Global Data Lab (GDL) Indicators
# ─────────────────────────────────────────────────────────────────────────────

gdl_file <- "gdl.csv"

if (!file.exists(gdl_file)) {
  stop("GDL CSV file not found: ", gdl_file)
}

message("Loading GDL data: ", gdl_file)
gdl_raw <- read_csv(gdl_file)

gdl_clean <- gdl_raw %>%
  filter(Region != "Total") %>%  # remove national totals
  mutate(
    year = as.numeric(Year),
    # Standardize state names to match IPUMS/MICS format
    state = case_when(
      Region == "Abuja FCT" ~ "Federal Capital Territory Abuja",
      Region == "Nassarawa" ~ "Nasarawa",
      Region == "Zamfora"   ~ "Zamfara",
      TRUE ~ Region
    )
  ) %>%
  select(state, year, GVI, fullsci, gini)

# Check number of states per year
gdl_clean %>%
  group_by(year) %>%
  summarise(state_count = n_distinct(state))

# Prepare keys in youth data (ensure character type and matching case)
swb_final_clean <- swb_final_clean %>%
  mutate(
    state = as.character(haven::as_factor(state)),
    year  = as.numeric(year)
  )

# Many-to-one join: each youth row gets the state-year GDL values
swb_multilevel <- swb_final_clean %>%
  left_join(gdl_clean, by = c("state", "year"))

# Check missingness after join (should be very low)
message("Missingness after GDL merge:")
colSums(is.na(swb_multilevel[, c("GVI", "fullsci", "gini")]))

# Optional: save linked dataset for multilevel models later
saveRDS(swb_multilevel, file = "swb_multilevel_linked.rds")
message("Linked dataset saved: swb_multilevel_linked.rds")

# ─────────────────────────────────────────────────────────────────────────────
# 7. Weighted Correlation Analysis
# ─────────────────────────────────────────────────────────────────────────────

# First approach: jtools::svycor (weighted Pearson correlations)
svycor(~ sat_happy_fn + fullsci + GVI + gini,
       design = final_design,
       sig.stats = TRUE)

# Second approach: manual weighted variance-covariance → correlation
var_cov_matrix <- svyvar(~ sat_happy_fn + fullsci + GVI + gini,
                         design = final_design,
                         na.rm = TRUE)

cor_matrix <- cov2cor(as.matrix(var_cov_matrix))
print("Weighted Correlation Matrix:")
print(cor_matrix)

# Optional: round and view nicely - rounded to three decimal places
round(cor_matrix, 3)

# Nicer display
cor_matrix_rounded <- round(cor_matrix, 3)
print("Weighted Correlation Matrix (rounded to 3 decimals):")
print(cor_matrix_rounded)
