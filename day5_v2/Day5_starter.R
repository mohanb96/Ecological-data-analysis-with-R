# Day 5: Read a tree, rank the factors (120 minutes)
# Extract the student ZIP, open ABV_Day5.Rproj, and save your own copy.
# Run selected lines with Ctrl + Enter / Cmd + Enter.
# Follow index.html in your browser. Copy the guided code when requested.

# 0. Load packages (install once in the Console before class if needed).
# install.packages(c("rpart", "rpart.plot", "randomForest"))
library(rpart)
library(rpart.plot)
library(randomForest)
dir.create("outputs", showWarnings = FALSE)

# My prediction: Which factor will matter, and why?
#

# 1. Import once and retain one copy of each experimental unit.
soil_raw <- read.csv("MultiFactor_data.csv")
factor_names <- c("P", "C", "L", "N", "A", "I", "SU", "FU", "H", "M", "S", "D")
soil_once <- soil_raw[soil_raw$remark %in% c("CT", "1", "2", "5", "8"), ]

# Copy the checks, complete-case selection and illustrative split from Step 1.
# Explain: What does 0 mean? What does 1 mean? Why exclude Lv and dissim?
#
# Why use a separate test set? What could happen if we train and evaluate
# on the same observations?
#

# 2. CART: Copy the supplied fit and plotting commands.
# Why is method = "anova" used for this response?
# Check this first tree using test_data; the split is used only here.
#

# 3. Return to ALL 252 observations in model_data for five-fold CART CV.
# Copy the cv_folds allocation, fit, pruning, plot and held-out prediction code.
# What is a fold? How does CV differ from OOB error and test error?
# Selected CP:
# Number of leaves before / after pruning:
# One complete path and its predicted mass loss (%):
# Which splits remain? What happens to lithium?
# The final CART tree is tree_cv_pruned. tree_model is the first illustration.
#

# 4. Fit the guided random forest using ALL model_data; fill both settings.
# Number of trees (maximum 25):
# Permutations for OOB importance (50):
# What does mtry = 4 control?
# OOB RMSE (percentage points):
#

# 5. Copy the importance extraction, then write the descending sort.
# Compare your answer with the supplied check, then make the ranking plot.
# My top three factors:
# Does positive importance mean decomposition increased? Explain:
#

# Save. Restart R and run the whole script to check reproducibility.
