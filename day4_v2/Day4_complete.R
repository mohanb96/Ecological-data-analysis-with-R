library(ggplot2)
file.exists("MultiFactor_data.csv")
soil_data <- read.csv("MultiFactor_data.csv")
dim(soil_data)

group_codes <- c("CT", "D", "H", "S")
group_raw <- soil_data[soil_data$remark %in% group_codes, ]
group_raw$treatment <- factor(
  group_raw$remark,
  levels = group_codes,
  labels = c("Control", "Drought", "Herbicide", "Salinity")
)
table(group_raw$treatment)
sum(is.na(group_raw$WSA))

group_excluded_ids <- group_raw$X[is.na(group_raw$WSA)]
group_data <- group_raw[!is.na(group_raw$WSA), ]
rownames(group_data) <- paste0("record_", group_data$X)

group_counts <- table(group_data$treatment)
group_means <- aggregate(WSA ~ treatment, data = group_data, FUN = mean)
group_sds <- aggregate(WSA ~ treatment, data = group_data, FUN = sd)
group_summary <- data.frame(
  treatment = levels(group_data$treatment),
  n = as.integer(group_counts),
  mean_WSA = group_means$WSA,
  SD_WSA = group_sds$WSA
)
group_summary

group_plot <- ggplot(group_data, aes(x = treatment, y = WSA)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "#edf3e7") +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 4),
             alpha = 0.8, colour = "#245e49", size = 2.2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, colour = "#a56424") +
  labs(x = "Treatment applied alone", y = "Water-stable aggregate proportion",
       title = "Aggregate stability in four soil treatments",
       subtitle = "Each circle = one soil unit; diamond = group mean") +
  theme_minimal(base_size = 12)
group_plot



group_model <- aov(WSA ~ treatment, data = group_data)
summary(group_model)

tukey_result <- TukeyHSD(group_model, conf.level = 0.95)
tukey_result

pairwise_table <- data.frame(
  comparison = rownames(tukey_result$treatment),
  tukey_result$treatment,
  row.names = NULL,
  check.names = FALSE
)
pairwise_table

control_comparisons <- pairwise_table[
  pairwise_table$comparison %in% c("Drought-Control", "Herbicide-Control", "Salinity-Control"),
]
control_comparisons

regression_labels <- c("CT", "1", "2", "5", "8")
regression_raw <- soil_data[
  soil_data$Lv %in% c(0, 1, 2, 5, 8) &
    soil_data$remark %in% regression_labels, ]
table(regression_raw$Lv)

reg_keep <- complete.cases(regression_raw[, c("Decom", "Lv")])
reg_excluded_ids <- regression_raw$X[!reg_keep]
regression_data <- regression_raw[reg_keep, ]
rownames(regression_data) <- paste0("record_", regression_data$X)

nrow(regression_data)
table(regression_data$Lv)
reg_excluded_ids
str(regression_data[, c("Decom", "Lv")])

trend_model <- lm(Decom ~ Lv, data = regression_data)
summary(trend_model)
coef(trend_model)
confint(trend_model)

slope <- coef(trend_model)["Lv"]
slope_interval <- confint(trend_model)["Lv", ]
r_squared <- summary(trend_model)$r.squared

trend_plot <- ggplot(regression_data, aes(x = Lv, y = Decom)) +
  geom_point(position = position_jitter(width = 0.10, height = 0, seed = 4),
             colour = "#245e49", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "#a56424", fill = "#e6d4b9") +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 8)) +
  labs(x = "Number of factors", y = "Proportional litter mass loss",
       title = "Linear regression of litter mass loss",
       subtitle = paste(nrow(regression_data), "observed units; points jittered horizontally only")) +
  theme_minimal(base_size = 12)
trend_plot



dissim_keep <- complete.cases(regression_data[, c("Decom", "dissim")])
dissim_excluded_ids <- regression_data$X[!dissim_keep]
dissim_data <- regression_data[dissim_keep, ]
nrow(dissim_data)

dissim_model <- lm(Decom ~ dissim, data = dissim_data)
summary(dissim_model)
confint(dissim_model)

dissim_slope <- coef(dissim_model)["dissim"]
dissim_interval <- confint(dissim_model)["dissim", ]
dissim_r_squared <- summary(dissim_model)$r.squared

dissim_plot <- ggplot(dissim_data, aes(x = dissim, y = Decom)) +
  geom_point(colour = "#245e49", alpha = 0.55, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "#a56424", fill = "#e6d4b9") +
  labs(x = "Factor dissimilarity index", y = "Proportional litter mass loss",
       title = "Linear regression with factor dissimilarity",
       subtitle = paste(nrow(dissim_data), "observations; same litter mass loss response")) +
  theme_minimal(base_size = 12)
dissim_plot
