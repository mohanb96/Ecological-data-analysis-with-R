# Day 3: two-hour fungal traits practical
# Extract the student ZIP and open ABV_Day3.Rproj.
# Save as Day3_yourname.R. Run the supplied sections in order.
# Add brief interpretation comments at NOTE lines; no separate report is needed.

# 1. An example workflow with 5 fungi traits ----
library(ggplot2)
file.exists("Fungal_traits_day3.csv")
fungi <- read.csv("Fungal_traits_day3.csv")
output_dir <- file.path("outputs", "guided")

dim(fungi)
head(fungi)
trait_names <- c("extension", "melanin", "enz_cel", "spore_abund", "hyphal_diam")
trait_data <- fungi[, trait_names]
str(trait_data)
summary(trait_data)


# 1. Check missing values ----
# NOTE: record the retained sample size and excluded IDs.
colSums(is.na(trait_data))

keep <- complete.cases(trait_data)
excluded_ids <- fungi$strainID[!keep]
excluded_ids
sum(keep)

fungi_complete <- fungi[keep, ]
trait_matrix <- fungi_complete[, trait_names]
rownames(trait_matrix) <- fungi_complete$strainID
dim(trait_matrix)


# 2. Standardise ----
# NOTE: explain in one line why we standardise.
trait_scaled <- scale(trait_matrix)

round(colMeans(trait_scaled), 6)
apply(trait_scaled, 2, sd)


# 3. PCA ----
# NOTE: record the percentage shown by PC1 and PC2 together.
pca_result <- prcomp(trait_scaled, center = FALSE, scale. = FALSE)
summary(pca_result)

variance_percent <- 100 * pca_result$sdev^2 / sum(pca_result$sdev^2)
variance_table <- data.frame(
  PC = paste0("PC", seq_along(variance_percent)),
  percent = variance_percent
)
variance_table
sum(variance_percent[1:2])


# 3. Isolate scores ----
scores <- data.frame(
  strainID = fungi_complete$strainID,
  Species = fungi_complete$Species,
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)
head(scores)


# 3. Draw the score map ----
label_data <- scores[scores$strainID %in% c("A", "DF56", "C31", "DF36"), ]

pca_plot <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(size = 3, colour = "#245e49", alpha = 0.85) +
  geom_text(data = label_data, aes(label = strainID),
            nudge_y = ifelse(label_data$strainID == "DF56", 0.35, 0),
            vjust = -0.8, size = 3.5) +
  coord_equal() +
  labs(
    x = paste0("PC1 (", round(variance_percent[1], 1), "% of variance)"),
    y = paste0("PC2 (", round(variance_percent[2], 1), "% of variance)"),
    title = paste("Similarities among", nrow(scores), "fungal isolates"),
    subtitle = "PCA of five standardised traits; selected isolate IDs labelled"
  ) +
  theme_minimal(base_size = 12)
pca_plot




# 4. Trait loadings ----
loadings <- pca_result$rotation[, 1:2]
round(loadings, 2)


# 4. Add five trait arrows ----
arrow_multiplier <- 2.5
trait_arrows <- data.frame(
  trait = rownames(loadings),
  PC1 = loadings[, 1] * arrow_multiplier,
  PC2 = loadings[, 2] * arrow_multiplier
)

pca_plot <- pca_plot +
  geom_segment(
    data = trait_arrows,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    inherit.aes = FALSE,
    arrow = grid::arrow(length = grid::unit(0.18, "cm")),
    colour = "#884b24", linewidth = 0.7
  ) +
  geom_label(
    data = trait_arrows,
    aes(x = PC1 * 1.2, y = PC2 * 1.2, label = trait),
    inherit.aes = FALSE,
    colour = "#884b24", fill = "white", size = 3
  ) +
  labs(subtitle = "Five standardised traits; arrows show positive trait directions",
       caption = "Loading arrows multiplied by 2.5 for display; points are isolate scores")
pca_plot




# 5. One exploratory cut at k = 3 ----
trait_distances <- dist(trait_scaled, method = "euclidean")
cluster_tree <- hclust(trait_distances, method = "complete")
groups_k3 <- cutree(cluster_tree, k = 3)
table(groups_k3)

plot(cluster_tree, labels = fungi_complete$strainID,
     main = "Fungal similarity in five standardised traits",
     xlab = "Isolate ID", sub = "Euclidean distance; complete linkage; k = 3",
     ylab = "Complete-linkage distance", cex = 0.75)
rect.hclust(cluster_tree, k = 3, border = "#6a8f62")


# 5. Descriptive mean scaled traits ----
cluster_profiles <- aggregate(
  as.data.frame(trait_scaled),
  by = list(cluster = factor(groups_k3)),
  FUN = mean
)
cluster_profiles






# 7. Save outputs (also run before copying this script for Step 6) ----
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

scores_export <- data.frame(
  strainID = fungi_complete$strainID,
  Species = fungi_complete$Species,
  pca_result$x,
  cluster_k3 = groups_k3
)
loadings_export <- data.frame(trait = rownames(loadings), loadings)

write.csv(data.frame(trait = trait_names), file.path(output_dir, "Day3_selected_traits.csv"), row.names = FALSE)

write.csv(scores_export, file.path(output_dir, "Day3_PCA_scores_and_clusters.csv"), row.names = FALSE)
write.csv(loadings_export, file.path(output_dir, "Day3_PC1_PC2_loadings.csv"), row.names = FALSE)
write.csv(variance_table, file.path(output_dir, "Day3_variance_explained.csv"), row.names = FALSE)
write.csv(cluster_profiles, file.path(output_dir, "Day3_cluster_profiles_scaled.csv"), row.names = FALSE)
write.csv(fungi_complete, file.path(output_dir, "Day3_retained_trait_records.csv"), row.names = FALSE)
write.csv(data.frame(strainID = excluded_ids), file.path(output_dir, "Day3_excluded_isolates.csv"), row.names = FALSE)

ggsave(file.path(output_dir, "Day3_PCA.png"), plot = pca_plot, width = 7.5, height = 5.5, dpi = 300, bg = "white")

png(file.path(output_dir, "Day3_dendrogram.png"), width = 2400, height = 1500, res = 300)
plot(cluster_tree, labels = fungi_complete$strainID,
     main = "Fungal similarity in five standardised traits",
     xlab = "Isolate ID", sub = "Euclidean distance; complete linkage; k = 3",
     ylab = "Complete-linkage distance", cex = 0.75)
rect.hclust(cluster_tree, k = 3, border = "#6a8f62")
dev.off()


# 6. Repeat with five OTHER traits ----
# After saving the guided outputs, save a copy as Day3_yourname_other_traits.R.
# In the copy, change the import to Fungal_traits_full.csv, output_dir to
# file.path("outputs", "your_five_traits"), and trait_names to five other names.
# Use the Step 6 selection checks and choose from the documented trait pool.
# Explain your selection in one sentence, then rerun the same flow.
# Add three brief notes: sample size and PC1-PC2 percent; two-isolate comparison
# using two arrows and recorded values; one difference or limitation.
# Recalculate exclusions and variance rather than copying guided results.
# Restart R and rerun each saved script to finish.
