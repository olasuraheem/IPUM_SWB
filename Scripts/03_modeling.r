# =============================================================================
# 03_modeling.R – Main Regression Models & Tables
# Purpose: 
#   - Prepare final analysis dataset (drop NAs, set references)
#   - Re-create survey design
#   - Run sociodemographic model
#   - Run separate-year life satisfaction models
#   - Run main temporal + GVI interaction model
#   - Run three contextual models (Cleanliness, GVI, Gini)
#   - Export all tables to Word
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load Packages & Cleaned Data
# ─────────────────────────────────────────────────────────────────────────────

if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse, survey, gtsummary, flextable, janitor, jtools, lme4
)

# Load your final cleaned youth dataset (from previous script)
swb_analysis_final <- read_rds("4youth_happiness.rds")

# Quick check
dim(swb_analysis_final)
table(swb_analysis_final$year, useNA = "always")

# ─────────────────────────────────────────────────────────────────────────────
# 2. Final Variable Preparation & NA Handling
# ─────────────────────────────────────────────────────────────────────────────

swb_analysis_final <- swb_analysis_final %>%
  mutate(
    # Ensure factor levels are dropped if empty
    across(where(is.factor), forcats::fct_drop),
    
    # Set reference levels explicitly (controls model output)
    year           = relevel(as.factor(year), ref = "2016"),
    gender         = relevel(as.factor(gender), ref = "Male"),
    residence_clean = relevel(as.factor(residence_clean), ref = "Urban"),
    marital_clean  = relevel(as.factor(marital_clean), ref = "Married/In Union"),
    education_clean = relevel(as.factor(education_clean), ref = "Primary or less")
  ) %>%
  # Drop rows with NA in key model variables
  filter(
    !is.na(sat_happy_fn),
    !is.na(year),
    !is.na(GVI),
    !is.na(gender),
    !is.na(age_continuous),
    !is.na(education_clean),
    !is.na(marital_clean),
    !is.na(ethnicity_clean),
    !is.na(wealth),
    !is.na(media_exposure),
    !is.na(residence_clean),
    !is.na(trend_past),
    !is.na(trend_future)
  )

# Check how many rows remain
nrow(swb_analysis_final)

# ─────────────────────────────────────────────────────────────────────────────
# 3. Re-create Survey Design (critical after filtering)
# ─────────────────────────────────────────────────────────────────────────────

multi_design_final <- svydesign(
  ids    = ~cluster,
  strata = ~state,
  weights = ~scaled_weight,
  data   = swb_analysis_final,
  nest   = TRUE
)

message("Survey design re-created. Sample size: ", nrow(swb_analysis_final))

# ─────────────────────────────────────────────────────────────────────────────
# 4. Socio-demographic Model (pooled, no year interaction)
# ─────────────────────────────────────────────────────────────────────────────

m_socio <- svyglm(
  sat_happy_fn ~ gender + age_continuous + education_clean +
    marital_clean + ethnicity_clean + wealth + media_exposure +
    residence_clean + trend_past + trend_future,
  design = multi_design_final
)

# Table
tbl_socio <- tbl_regression(
  m_socio,
  label = list(
    trend_past      ~ "Well-being compared to last year",
    trend_future    ~ "Expected well-being next year",
    ethnicity_clean ~ "Ethnic Group",
    marital_clean   ~ "Marital Status"
  )
) %>%
  bold_p(t = 0.05) %>%
  bold_labels()

tbl_socio

# Save
tbl_socio %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "1.0_socio_demographic_model.docx")

# ─────────────────────────────────────────────────────────────────────────────
# 5. Separate-Year Models for Life Satisfaction
# ─────────────────────────────────────────────────────────────────────────────

# 2016 Verbal Satisfaction
m_2016 <- svyglm(
  sat_life_likert ~ gender + age_continuous + education_clean +
    marital_clean + ethnicity_clean + wealth + media_exposure +
    residence_clean + trend_past + trend_future,
  design = subset(multi_design_final, year == "2016")
)

tbl_2016 <- tbl_regression(m_2016) %>% bold_p(t = 0.05)

# 2021 Cantril Ladder
m_2021 <- svyglm(
  sat_life_ladder ~ gender + age_continuous + education_clean +
    marital_clean + ethnicity_clean + wealth + media_exposure +
    residence_clean + trend_past + trend_future,
  design = subset(multi_design_final, year == "2021")
)

tbl_2021 <- tbl_regression(m_2021) %>% bold_p(t = 0.05)

# Merge into one table
merged_tbl_sat <- tbl_merge(
  tbls = list(tbl_2016, tbl_2021),
  tab_spanner = c("**2016 (Verbal Satisfaction)**", "**2021 (Cantril Ladder)**")
) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("Table 2: Separate Models of Life Satisfaction by Survey Year")

merged_tbl_sat

merged_tbl_sat %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "2_satisfaction_models.docx")

# ─────────────────────────────────────────────────────────────────────────────
# 6. Main Model: Year × GVI Interaction
# ─────────────────────────────────────────────────────────────────────────────

m_main <- svyglm(
  sat_happy_fn ~ year * GVI +
    gender + age_continuous + education_clean +
    marital_clean + ethnicity_clean + wealth +
    media_exposure + residence_clean + trend_past + trend_future,
  design = multi_design_final
)

tbl_main <- tbl_regression(
  m_main,
  label = list(
    year = "Survey Year (2021 vs 2016)",
    GVI  = "Global Vulnerability Index"
  )
) %>%
  add_glance_table() %>%  # model fit stats
  bold_p(t = 0.05) %>%
  bold_labels()

tbl_main

tbl_main %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "3_main_happiness_model.docx")

# ─────────────────────────────────────────────────────────────────────────────
# 7. Three Contextual Models (Institutional, Fragility, Inequality)
# ─────────────────────────────────────────────────────────────────────────────

# Model 1: Institutional (Cleanliness Index)
m_inst <- svyglm(
  sat_happy_fn ~ year + gender + education_clean + residence_clean +
    marital_clean + fullsci,
  design = multi_design_final
)

# Model 2: Fragility (GVI)
m_frag <- svyglm(
  sat_happy_fn ~ year + gender + education_clean + residence_clean +
    marital_clean + GVI,
  design = multi_design_final
)

# Model 3: Inequality (Gini)
m_ineq <- svyglm(
  sat_happy_fn ~ year + gender + education_clean + residence_clean +
    marital_clean + gini,
  design = multi_design_final
)

# Merge tables
merged_context <- tbl_merge(
  tbls = list(
    tbl_regression(m_inst, label = list(fullsci ~ "Cleanliness Index")),
    tbl_regression(m_frag,  label = list(GVI ~ "Global Vulnerability Index")),
    tbl_regression(m_ineq,  label = list(gini ~ "Gini Coefficient"))
  ),
  tab_spanner = c("**Institutional Context**", "**Fragility Context**", "**Inequality Context**")
) %>%
  bold_labels() %>%
  bold_p(t = 0.05) %>%
  modify_caption("Table 4: Association Between Contextual Indicators and Youth Happiness")

merged_context

merged_context %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "4_contextual_models.docx")

message("All models and tables completed and saved.")