# Day 1: complete reference workflow
# Run inside the extracted ABV_Day1 project folder.
# Attempt the tutorial tasks before using this reference.

getwd()
list.files()
file.exists("MultiFactor_data.csv")
library(ggplot2)

# Practice values are invented; they are not observations from the dataset.
sample_count <- 4
practice_wsa <- c(0.40, 0.50, 0.60, 0.70)
mean(practice_wsa)
length(practice_wsa)
practice_wsa > 0.50
practice_wsa[practice_wsa > 0.50]

# Import and inspect the unchanged source dataset.
soil_data <- read.csv("MultiFactor_data.csv")
head(soil_data)
dim(soil_data)
names(soil_data)
str(soil_data)
summary(soil_data$WSA)
unique(soil_data$remark)

# Select drought alone, not all combinations that contain drought.
control_data <- soil_data[soil_data$remark == "CT", ]
drought_data <- soil_data[soil_data$remark == "D", ]
nrow(control_data)
nrow(drought_data)
day1_data <- rbind(control_data, drought_data)
day1_data <- day1_data[, c("remark", "WSA", "Decom", "PH")]
table(day1_data$remark)
nrow(day1_data)
names(day1_data)

# Treat the treatment label as a categorical variable.
day1_data$remark <- factor(day1_data$remark, levels = c("CT", "D"))
str(day1_data)
sum(is.na(day1_data$WSA))
sum(is.na(soil_data$WSA))

# Invented missing-value example; the real data remain unchanged.
practice_missing <- c(0.50, NA, 0.70)
mean(practice_missing)
mean(practice_missing, na.rm = TRUE)
sum(!is.na(practice_missing))

# Require an observed response for the WSA description.
day1_wsa <- day1_data[!is.na(day1_data$WSA), ]
nrow(day1_data) - nrow(day1_wsa)
table(day1_wsa$remark)
sum(complete.cases(soil_data))
sum(!is.na(soil_data$WSA))

mean_control <- mean(day1_wsa$WSA[day1_wsa$remark == "CT"])
mean_drought <- mean(day1_wsa$WSA[day1_wsa$remark == "D"])
mean_control
mean_drought
mean_drought - mean_control

set.seed(1)
wsa_plot <- ggplot(day1_wsa, aes(x = remark, y = WSA)) +
  geom_jitter(width = 0.08, height = 0,
              size = 2.8, alpha = 0.8, colour = "#24624b") +
  scale_x_discrete(labels = c(CT = "Control", D = "Drought")) +
  labs(x = "Treatment",
       y = "Water-stable aggregates (proportion)",
       title = "Soil aggregate stability: control and drought") +
  theme_classic(base_size = 12)
wsa_plot

dir.create("outputs", showWarnings = FALSE)
write.csv(day1_wsa, "outputs/Day1_CT_D_WSA.csv", row.names = FALSE)
ggsave("outputs/Day1_CT_D_WSA.png", plot = wsa_plot,
       width = 7, height = 4.5, dpi = 300)
list.files("outputs")

# Describe the figure in your own words and connect it to your prediction.
# The group difference here is descriptive. Evaluate uncertainty on Day 2.
