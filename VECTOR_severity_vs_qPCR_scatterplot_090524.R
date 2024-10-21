library(dplyr)
library(readxl)
library(ggplot2)

a <- read_excel("VECTOR_severity_to_average_forR.xlsx")

avg_sev_by_tank <- a %>%
  group_by(tank) %>%
  summarise(average_sev = mean(sev, na.rm = TRUE))

View(avg_sev_by_tank)
av<-avg_sev_by_tank

write.csv(avg_sev_by_tank, "average_severity_by_tank.csv", row.names = FALSE)

q <- read_excel("average_qPCR_tank_090524.xlsx")

merged_data <- av %>%
  inner_join(q, by = "tank")

ggplot(merged_data, aes(x = logSQ200, y = average_sev)) +
  geom_point() + 
  labs(x = "logSQ200", y = "Average Severity", title = "Scatter Plot of Average Severity vs. SQ200") +
  theme_minimal()

# Fit the linear model
model <- lm(average_sev ~ logSQ200, data = merged_data)

# Get the R² value
r_squared <- summary(model)$r.squared

# Print the R² value
print(r_squared)
