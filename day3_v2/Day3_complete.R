library(ggplot2)
file.exists("Fungal_traits_day3.csv")
fungi <- read.csv("Fungal_traits_day3.csv")

dim(fungi)
head(fungi)
trait_names <- c("extension", "melanin", "enz_cel", "spore_abund")
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
  PC = c("PC1", "PC2", "PC3", "PC4"),
  percent = variance_percent
)
variance_table
sum(variance_percent[1:2])

scores <- data.frame(
  strainID = fungi_complete$strainID,
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)
head(scores)

label_data <- scores[scores$strainID %in% c("A", "DF56", "C31", "DF36"), ]

pca_plot <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, colour = "grey85") +
  geom_vline(xintercept = 0, colour = "grey85") +
  geom_point(size = 3, colour = "#245e49", alpha = 0.85) +
  geom_text(data = label_data, aes(label = strainID), vjust = -0.8, size = 3.5) +
  coord_equal() +
  labs(
    x = paste0("PC1 (", round(variance_percent[1], 1), "% of variance)"),
    y = paste0("PC2 (", round(variance_percent[2], 1), "% of variance)"),
    title = "Similarities among 28 fungal isolates",
    subtitle = "PCA of four standardised traits; four example IDs labelled"
  ) +
  theme_minimal(base_size = 12)
pca_plot



loadings <- pca_result$rotation[, 1:2]
round(loadings, 2)

fungi_complete[fungi_complete$strainID %in% c("A", "DF56"),
               c("strainID", trait_names)]



trait_distances <- dist(trait_scaled, method = "euclidean")
cluster_tree <- hclust(trait_distances, method = "complete")

plot(cluster_tree, labels = fungi_complete$strainID,
     main = "Fungal similarity in four standardised traits",
     xlab = "Isolate ID", sub = "Euclidean distance; complete linkage",
     ylab = "Complete-linkage distance", cex = 0.75)

groups_k2 <- cutree(cluster_tree, k = 2)
groups_k3 <- cutree(cluster_tree, k = 3)

membership <- data.frame(
  strainID = fungi_complete$strainID,
  k2 = groups_k2,
  k3 = groups_k3
)
membership
table(groups_k3)
table(k2 = groups_k2, k3 = groups_k3)



scores$cluster <- factor(groups_k3)

cluster_plot <- ggplot(scores, aes(x = PC1, y = PC2, colour = cluster, shape = cluster)) +
  geom_point(size = 3, alpha = 0.9) +
  coord_equal() +
  scale_colour_manual(values = c("#245e49", "#b67931", "#6266a4")) +
  labs(
    x = paste0("PC1 (", round(variance_percent[1], 1), "% of variance)"),
    y = paste0("PC2 (", round(variance_percent[2], 1), "% of variance)"),
    title = "An exploratory grouping on the PCA map",
    subtitle = "Memberships calculated from all four scaled traits",
    colour = "Cluster (k = 3)", shape = "Cluster (k = 3)"
  ) +
  theme_minimal(base_size = 12)
cluster_plot



cluster_profiles <- aggregate(
  as.data.frame(trait_scaled),
  by = list(cluster = factor(groups_k3)),
  FUN = mean
)
cluster_profiles





dir.create("outputs", showWarnings = FALSE)

scores_export <- data.frame(
  strainID = fungi_complete$strainID,
  pca_result$x,
  cluster_k2 = groups_k2,
  cluster_k3 = groups_k3
)
loadings_export <- data.frame(trait = rownames(loadings), loadings)

write.csv(scores_export, "outputs/Day3_PCA_scores_and_clusters.csv", row.names = FALSE)
write.csv(loadings_export, "outputs/Day3_PC1_PC2_loadings.csv", row.names = FALSE)
write.csv(variance_table, "outputs/Day3_variance_explained.csv", row.names = FALSE)
write.csv(cluster_profiles, "outputs/Day3_cluster_profiles_scaled.csv", row.names = FALSE)
write.csv(fungi_complete, "outputs/Day3_retained_trait_records.csv", row.names = FALSE)
write.csv(data.frame(strainID = excluded_ids), "outputs/Day3_excluded_isolates.csv", row.names = FALSE)

ggsave("outputs/Day3_PCA.png", plot = pca_plot, width = 7.5, height = 5.5, dpi = 300, bg = "white")
ggsave("outputs/Day3_PCA_clusters.png", plot = cluster_plot, width = 7.5, height = 5.5, dpi = 300, bg = "white")

png("outputs/Day3_dendrogram.png", width = 2400, height = 1500, res = 300)
plot(cluster_tree, labels = fungi_complete$strainID,
     main = "Fungal similarity in four standardised traits",
     xlab = "Isolate ID", sub = "Euclidean distance; complete linkage; k = 3",
     ylab = "Complete-linkage distance", cex = 0.75)
rect.hclust(cluster_tree, k = 3, border = "#6a8f62")
dev.off()
