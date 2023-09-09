library(dplyr)

fall_experiment <- tibble(
  time = c(1.63, 2.07, 2.11, 2.26, 2.31),
  height = c(13.5, 18, 22.5, 27, 31.5)
)

fall_experiment2 <- fall_experiment %>%
  mutate(
    dev_time_sq = time^2 - mean(time^2),
    dev_height = height - mean(height)
  )

fall_experiment2

fit3_galileo <- lm(dev_height ~ dev_time_sq, data = fall_experiment2)

fit3_galileo
