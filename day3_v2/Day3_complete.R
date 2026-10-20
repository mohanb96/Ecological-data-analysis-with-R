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

colSums(is.na(trait_data))

keep <- complete.cases(trait_data)
excluded_ids <- fungi$strainID[!keep]
excluded_ids
sum(keep)

fungi_complete <- fungi[keep, ]
trait_matrix <- fungi_complete[, trait_names]
rownames(trait_matrix) <- fungi_complete$strainID
dim(trait_matrix)

trait_scaled <- scale(trait_matrix)

round(colMeans(trait_scaled), 6)
apply(trait_scaled, 2, sd)

pca_result <- prcomp(trait_scaled, center = FALSE, scale. = FALSE)
summary(pca_result)

variance_percent <- 100 * pca_result$sdev^2 / sum(pca_result$sdev^2)
variance_table <- data.frame(
  PC = paste0("PC", seq_along(variance_percent)),
  percent = variance_percent
)
variance_table
sum(variance_percent[1:2])

scores <- data.frame(
  strainID = fungi_complete$strainID,
  Species = fungi_complete$Species,
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)
head(scores)

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



loadings <- pca_result$rotation[, 1:2]
round(loadings, 2)

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



trait_distances <- dist(trait_scaled, method = "euclidean")
cluster_tree <- hclust(trait_distances, method = "complete")
groups_k3 <- cutree(cluster_tree, k = 3)
table(groups_k3)

plot(cluster_tree, labels = fungi_complete$strainID,
     main = "Fungal similarity in five standardised traits",
     xlab = "Isolate ID", sub = "Euclidean distance; complete linkage; k = 3",
     ylab = "Complete-linkage distance", cex = 0.75)
rect.hclust(cluster_tree, k = 3, border = "#6a8f62")

cluster_profiles <- aggregate(
  as.data.frame(trait_scaled),
  by = list(cluster = factor(groups_k3)),
  FUN = mean
)
cluster_profiles





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
