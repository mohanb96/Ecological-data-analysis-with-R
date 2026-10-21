# Day 4: compare groups and describe a numerical trend
# Extract all student files, open ABV_Day4.Rproj, and save as Day4_yourname.R.
# Add the website's required commands in order. Use your own words for answers.

# 1. TWO ANALYSIS PARTS
# A: How does mean WSA differ among control, drought, herbicide and salinity?
# B: Across 0, 1, 2, 5 and 8 factors, how is litter mass loss related to Lv?

# 2. IMPORT AND SELECT FOUR GROUPS
library(ggplot2)
soil_data <- read.csv("MultiFactor_data.csv")
group_codes <- c("CT", "D", "H", "S")
# Select by explicit remark codes. H = herbicide.
# Set factor levels and labels; inspect counts and response-specific NAs.
# Make a raw-data plot and calculate group means.

# 3. ANOVA AND TUKEY COMPARISONS
# Continue directly with group_data from the treatment inspection above.
# Fit group_model <- aov(WSA ~ treatment, data = group_data), then summary(group_model).
# Use this same group_model in TukeyHSD().
# State the overall null hypothesis.
# Extract all six Tukey comparisons and keep their adjustment family intact.
# Explain Drought-Control, Herbicide-Control and Salinity-Control in WSA units.

# 4. NUMERICAL TREND
# Select Lv in c(0, 1, 2, 5, 8) with remark in c("CT", "1", "2", "5", "8").
# These labels retain one copy of the single-factor records.
# Remove only rows missing Decom or Lv; expect 252 complete observations.
# Fit trend_model <- lm(Decom ~ Lv, data = regression_data).
# Make a scatterplot with a fitted line. Record slope, CI and R-squared.

# Read the slope and R-squared interpretation in the Step 4 Check.

# 5. YOUR TURN: FACTOR DISSIMILARITY
# Use Decom as the response again, with dissim as the numerical predictor.
# Start with regression_data from Step 4 and check complete Decom/dissim rows.
# Fit dissim_model <- lm(Decom ~ dissim, data = dissim_data).
# Make dissim_plot with dissim on x and Decom on y.
# Record dissim_slope, dissim_interval and dissim_r_squared.
# Interpret a 0.1 index increase and compare R-squared with the Lv model.
# Both regressions use the same 252 observations in the supplied data.

# IF YOU FINISH EARLY: TWO THINKING QUESTIONS
# 1. Combine the predictors with Decom ~ Lv + dissim, using the same observations.
# Interpret the dissim coefficient at a fixed number of factors and compare model fit.
# See the tutorial for the supplied code and Check.
# 2. How was the dissimilarity index calculated for 2-, 5- and 8-factor treatments?
# Connect to Day 3: compare factors as you compared fungal isolates in trait space.
# Hint: each factor has a trait profile of responses across seven soil functions.
# Think back to trait scaling and distances, then consider a combination of factors.
