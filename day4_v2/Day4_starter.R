# Day 4: compare groups and describe a numerical trend
# Extract all student files, open ABV_Day4.Rproj, and save as Day4_yourname.R.
# Add the website's required commands in order. Use your own words for answers.

# 1. QUESTIONS AND PREDICTIONS
# A: How does mean WSA differ among control, drought, herbicide and salinity?
# B: Among mixtures of 2, 5 and 8 factors, how is litter mass loss related to Lv?

# 2. IMPORT AND SELECT FOUR GROUPS
library(ggplot2)
soil_data <- read.csv("MultiFactor_data.csv")
group_codes <- c("CT", "D", "H", "S")
# Select by explicit remark codes. H = herbicide.
# Set factor levels and labels; inspect counts and response-specific NAs.
# Make a raw-data plot and calculate group means.

# 3. FIT AND CHECK
# Fit group_model <- aov(WSA ~ treatment, data = group_data).
# Inspect residual-versus-fitted and Q-Q plots; consider independence.
# Record one concern. Do not delete observations just to improve a test.

# 4. ANOVA AND TUKEY COMPARISONS
# State the overall null hypothesis.
# Extract all six Tukey comparisons and keep their adjustment family intact.
# Explain Drought-Control, Herbicide-Control and Salinity-Control in WSA units.

# 5. NUMERICAL TREND
# Select Lv in c(2, 5, 8), then remove only missing variables used by this model.
# Fit trend_model <- lm(Decom ~ Lv, data = regression_data).
# Make a scatterplot with a fitted line. Record slope, CI and R-squared.

# 6. INTERPRET AND CHECK
# Convert the slope from a fraction to percentage points per additional factor.
# Inspect residuals and group spreads. State the straight-line assumption.

# 7. YOUR TURN
# Predict fitted mean mass loss at Lv = 2, 5, 8 and describe the 8-minus-2 contrast.
# Explain why this does not estimate a universal effect of adding any one factor.

# 8. SAVE AND RERUN
# Save figures/tables and two short interpretations. Restart R and rerun.

# OPTIONAL: robustness, factor(Lv), and Decom ~ Lv + dissim.
