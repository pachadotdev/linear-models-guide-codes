library(dplyr)
library(janitor)
library(broom)
library(ggplot2)

fall_experiment <- tibble(
  time = c(1.63, 2.07, 2.11, 2.26, 2.31),
  height = c(13.5, 18, 22.5, 27, 31.5)
)

fit1_galileo <- lm(height ~ I(time^2), data = fall_experiment)

fit1_galileo_aug <- fit1_galileo %>%
  augment() %>%
  clean_names()

fit1_galileo_aug

fit1_galileo

fit2_galileo <- lm(height ~ I(time^2) - 1, data = fall_experiment)

fit2_galileo

fit2_galileo_aug <- clean_names(augment(fit2_galileo))
fit2_galileo_aug$time <- sqrt(fit2_galileo_aug$i_time_2)

fit2_galileo_pred <- clean_names(augment(fit2_galileo,
  newdata = data.frame(time = seq(0, 5, 0.5))
))

ggplot() +
  geom_point(data = fit2_galileo_aug, aes(x = time, y = height)) +
  geom_line(
    data = fit2_galileo_pred, aes(x = time, y = fitted),
    color = "blue"
  ) +
  labs(
    title = "h ~ t^2",
    subtitle = "Based on my own experiment",
    x = "Time (seconds)",
    y = "Height (meters)"
  ) +
  theme_minimal()

ggplot() +
  geom_point(data = fit2_galileo_aug, aes(x = time, y = height)) +
  geom_line(
    data = fit2_galileo_pred, aes(x = time, y = fitted),
    color = "blue"
  ) +
  labs(
    title = "h ~ t^2",
    subtitle = "Based on my own experiment",
    x = "Time (seconds)",
    y = expression(Height ~ (sqrt(metres)))
  ) +
  theme_minimal() +
  scale_y_sqrt()
