# =============================================================================
# 04_robustness_lmm.R – Linear Mixed-Effects Robustness Check
# Purpose: 
#   - Fit random-intercept LMM treating states as random effects
#   - Extract ICC (intraclass correlation coefficient)
#   - Create simple variance decomposition plot
# =============================================================================

# Load required packages
library(lme4)      # lmer()
library(ggplot2)   # plotting
library(scales)    # percent labels
library(dplyr)     # data manipulation

# ─────────────────────────────────────────────────────────────────────────────
# 1. Prepare data for lmer (use the variables from the survey design)
# ─────────────────────────────────────────────────────────────────────────────

# Extract the analysis data frame from the survey design object
# This ensures we use the same filtered/cleaned data as the main models
analysis_df <- multi_design_final$variables

# Quick check
dim(analysis_df)
table(analysis_df$year, useNA = "always")

# ─────────────────────────────────────────────────────────────────────────────
# 2. Fit the random-intercept model
# ─────────────────────────────────────────────────────────────────────────────

lmm_model <- lmer(
  sat_happy_fn ~ year + gender + education_clean + residence_clean + GVI +
    (1 | state),
  data = analysis_df,
  weights = scaled_weight  # apply survey weights
)

# Model summary
summary(lmm_model)

# ─────────────────────────────────────────────────────────────────────────────
# 3. Extract variance components & compute ICC
# ─────────────────────────────────────────────────────────────────────────────

# Extract variance components
vc <- as.data.frame(VarCorr(lmm_model))

# Between-state variance (random intercept)
var_state <- vc$vcov[vc$grp == "state"]

# Residual variance
var_residual <- vc$vcov[vc$grp == "Residual"]

# ICC = between-state variance / total variance
icc <- var_state / (var_state + var_residual)

message("ICC (between-state variation): ", round(icc, 4))  # Should be ≈ 0.0334

# ─────────────────────────────────────────────────────────────────────────────
# 4. Variance decomposition plot
# ─────────────────────────────────────────────────────────────────────────────

icc_data <- data.frame(
  Source = c("Between-state", "Within-state (residual)"),
  Proportion = c(icc, 1 - icc)
)

ggplot(icc_data, aes(x = "", y = Proportion, fill = Source)) +
  geom_bar(stat = "identity", width = 0.4) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Variance Decomposition of Happiness Scores",
       x = NULL, y = "Proportion of Total Variance",
       caption = "ICC ≈ 3.4% (low between-state variation)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot
ggsave("icc_variance_plot.png", width = 6, height = 3, dpi = 300)
message("ICC plot saved: icc_variance_plot2.png")