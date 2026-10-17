library(ggplot2)
file.exists("MultiFactor_data.csv")
soil_data <- read.csv("MultiFactor_data.csv")

control_data <- soil_data[soil_data$remark == "CT", ]
drought_data <- soil_data[soil_data$remark == "D", ]

day2_data <- rbind(control_data, drought_data)
day2_data <- day2_data[, c("remark", "WSA", "PH", "Decom")]
day2_data$remark <- factor(day2_data$remark, levels = c("CT", "D"))

table(day2_data$remark)
sum(is.na(day2_data$WSA))

control_WSA <- control_data$WSA
drought_WSA <- drought_data$WSA

control_WSA <- control_WSA[!is.na(control_WSA)]
drought_WSA <- drought_WSA[!is.na(drought_WSA)]
wsa_data <- day2_data[!is.na(day2_data$WSA), ]

wsa_plot <- ggplot(wsa_data, aes(x = remark, y = WSA)) +
  geom_jitter(width = 0.08, height = 0,
              size = 2.8, alpha = 0.8, colour = "#24624b") +
  scale_x_discrete(labels = c(CT = "Control", D = "Drought")) +
  labs(x = "Treatment",
       y = "Water-stable aggregates (proportion)",
       title = "Soil aggregate stability: control and drought") +
  theme_classic(base_size = 12)
wsa_plot



mean_control <- mean(control_WSA)
mean_drought <- mean(drought_WSA)
mean_difference <- mean_drought - mean_control

mean_control
mean_drought
mean_difference
100 * mean_difference

length(control_WSA)
length(drought_WSA)
summary(control_WSA)
summary(drought_WSA)



test_result <- t.test(drought_WSA, control_WSA,
                      alternative = "two.sided",
                      var.equal = FALSE,
                      conf.level = 0.95)
test_result

test_result$estimate
test_result$conf.int

test_result$p.value



dir.create("outputs", showWarnings = FALSE)

wsa_results <- data.frame(
  response = "WSA",
  contrast = "Drought minus control",
  n_control = length(control_WSA),
  n_drought = length(drought_WSA),
  mean_control = mean_control,
  mean_drought = mean_drought,
  mean_difference = mean_difference,
  ci_lower = test_result$conf.int[1],
  ci_upper = test_result$conf.int[2],
  p_value = test_result$p.value
)

write.csv(wsa_results, "outputs/Day2_WSA_results.csv", row.names = FALSE)
write.csv(wsa_data, "outputs/Day2_WSA_data.csv", row.names = FALSE)
ggsave("outputs/Day2_WSA_figure.png", plot = wsa_plot,
       width = 7, height = 4.5, dpi = 300)
