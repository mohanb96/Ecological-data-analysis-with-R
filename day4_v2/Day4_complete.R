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
head(fitted(group_model))
head(residuals(group_model))

par(mfrow = c(1, 2))
plot(group_model, which = 1)
plot(group_model, which = 2)
par(mfrow = c(1, 1))



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



mixture_raw <- soil_data[soil_data$Lv %in% c(2, 5, 8), ]
table(mixture_raw$Lv)

reg_keep <- complete.cases(mixture_raw[, c("Decom", "Lv")])
reg_excluded_ids <- mixture_raw$X[!reg_keep]
regression_data <- mixture_raw[reg_keep, ]
rownames(regression_data) <- paste0("record_", regression_data$X)

nrow(regression_data)
table(regression_data$Lv)
reg_excluded_ids
str(regression_data[, c("Decom", "Lv")])

trend_model <- lm(Decom ~ Lv, data = regression_data)
summary(trend_model)
coef(trend_model)
confint(trend_model)

trend_plot <- ggplot(regression_data, aes(x = Lv, y = Decom)) +
  geom_point(position = position_jitter(width = 0.10, height = 0, seed = 4),
             colour = "#245e49", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "#a56424", fill = "#e6d4b9") +
  scale_x_continuous(breaks = c(2, 5, 8)) +
  labs(x = "Number of applied factors", y = "Proportional litter mass loss",
       title = "A straight-line summary across factor mixtures",
       subtitle = "141 observed units; points jittered horizontally only") +
  theme_minimal(base_size = 12)
trend_plot



slope <- coef(trend_model)["Lv"]
slope_interval <- confint(trend_model)["Lv", ]
r_squared <- summary(trend_model)$r.squared

slope
100 * slope
slope_interval
r_squared

par(mfrow = c(1, 2))
plot(trend_model, which = 1)
plot(trend_model, which = 2)
par(mfrow = c(1, 1))

mixture_means <- aggregate(Decom ~ Lv, data = regression_data, FUN = mean)
mixture_sds <- aggregate(Decom ~ Lv, data = regression_data, FUN = sd)
mixture_means
mixture_sds





prediction_grid <- data.frame(Lv = c(2, 5, 8))
mean_predictions <- predict(trend_model, newdata = prediction_grid,
                            interval = "confidence", level = 0.95)
prediction_table <- data.frame(prediction_grid, mean_predictions)
prediction_table

change_8_minus_2 <- prediction_table$fit[3] - prediction_table$fit[1]
100 * change_8_minus_2

dir.create("outputs", showWarnings = FALSE)

anova_export <- data.frame(term = rownames(summary(group_model)[[1]]),
                           summary(group_model)[[1]], row.names = NULL)
trend_export <- data.frame(
  term = names(coef(trend_model)), estimate = as.numeric(coef(trend_model)),
  confint(trend_model), row.names = NULL
)
names(trend_export)[3:4] <- c("ordinary_CI_lower", "ordinary_CI_upper")
trend_export$interval_method <- "Ordinary lm; conditional on model assumptions"
trend_fit_export <- data.frame(n = nobs(trend_model), r_squared = r_squared,
                               response = "Proportional litter mass loss")
exclusions <- data.frame(
  analysis = c(rep("WSA groups", length(group_excluded_ids)),
                rep("Litter mass loss trend", length(reg_excluded_ids))),
  record_X = c(group_excluded_ids, reg_excluded_ids)
)

write.csv(group_summary, "outputs/Day4_WSA_group_summary.csv", row.names = FALSE)
write.csv(anova_export, "outputs/Day4_WSA_ANOVA.csv", row.names = FALSE)
write.csv(pairwise_table, "outputs/Day4_WSA_Tukey_all_six.csv", row.names = FALSE)
write.csv(trend_export, "outputs/Day4_trend_coefficients.csv", row.names = FALSE)
write.csv(trend_fit_export, "outputs/Day4_trend_fit.csv", row.names = FALSE)
write.csv(prediction_table, "outputs/Day4_fitted_means.csv", row.names = FALSE)
write.csv(exclusions, "outputs/Day4_excluded_records.csv", row.names = FALSE)
write.csv(group_data, "outputs/Day4_WSA_analysis_data.csv", row.names = FALSE)
write.csv(regression_data, "outputs/Day4_regression_analysis_data.csv", row.names = FALSE)

ggsave("outputs/Day4_WSA_groups.png", plot = group_plot,
       width = 7.5, height = 5, dpi = 300, bg = "white")
ggsave("outputs/Day4_litter_mass_loss_trend.png", plot = trend_plot,
       width = 7.5, height = 5.2, dpi = 300, bg = "white")

png("outputs/Day4_WSA_diagnostics.png", width = 2700, height = 1350, res = 300)
par(mfrow = c(1, 2))
plot(group_model, which = 1)
plot(group_model, which = 2)
dev.off()

png("outputs/Day4_trend_diagnostics.png", width = 2700, height = 1350, res = 300)
par(mfrow = c(1, 2))
plot(trend_model, which = 1)
plot(trend_model, which = 2)
dev.off()
par(mfrow = c(1, 1))
