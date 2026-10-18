# Day 3: fungal traits, PCA and similarity
# Unzip all student files and open ABV_Day3.Rproj first.
# Save this script as Day3_yourname.R. Add commands from the website.
# Run the script from the beginning in a fresh R session before submitting.

# 1. QUESTION
# Which fungal isolates have similar combinations of traits?
# Write a prediction about two traits. Give an ecological reason.

# 2. IMPORT AND CHECK
library(ggplot2)
fungi <- read.csv("Fungal_traits_day3.csv")
trait_names <- c("extension", "melanin", "enz_cel", "spore_abund")
# Inspect dimensions and the dictionary. Select analysis traits only.
# Count missing values, select complete rows, and record retained/excluded IDs.

# 3. SCALE
# Create trait_matrix and trait_scaled. Keep isolate IDs as row names.
# Explain what a scaled value of +1 means.

# 4. PCA
# Fit pca_result, examine explained variance, and create scores.
# Make the PCA plot using the provided recipe. Keep the aspect ratio equal.

# 5. INTERPRET
# Extract the PC1/PC2 loadings. Compare isolate A with isolate DF56.
# Describe the trait contrast, then check it against their recorded values.

# 6. CLUSTER
# Use distances from all FOUR scaled traits and complete linkage.
# Compare cuts with k = 2 and k = 3. Cluster numbers are just labels.

# 7. YOUR TURN
# Choose a cluster, count its members, and compare mean scaled traits.
# Support one ecological description with two trait values.

# 8. SAVE
# Save plots, PCA scores, loadings, explained variance and cluster profiles.
# Write 4-5 sentences answering the ecological question with a limitation.
