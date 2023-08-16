fall_experiment <- data.frame(
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
  newdata = data.frame(time = seq(0, 5, 0.5))))

ggplot() +
  geom_point(data = fit2_galileo_aug, aes(x = time, y = height),
             color = "#98b6b8") +
  geom_line(data = fit2_galileo_pred, aes(x = time, y = fitted),
            color = "#f2c162") +
  labs(
    title = "h ~ t^2",
    subtitle = "Based on my own experiment",
    x = "Time (seconds)",
    y = "Height (meters)"
  ) +
  theme_minimal()

galton <- galton %>%
  mutate(
    parent_cm = parent * 2.54,
    child_cm = child * 2.54
  )

galton

# galton$parent_cm <- galton$parent * 2.54
# galton$child_cm <- galton$child * 2.54

fit6_galton <- lm(child_cm ~ parent_cm, data = galton)

fit6_galton

galton <- galton %>%
  mutate(
    log_parent = log(parent),
    log_child = log(child)
  )

galton

fit7_galton <- lm(log_child ~ log_parent, data = galton)

galton <- galton %>%
  mutate(
    dev_parent = parent - mean(parent),
    dev_child = child - mean(child)
  )

galton

fit8_galton <- lm(dev_child ~ dev_parent, data = galton)

fit8_galton

fall_experiment2 <- fall_experiment %>%
  mutate(
    dev_time_sq = time^2 - mean(time^2),
    dev_height = height - mean(height)
  )

fall_experiment2

fit3_galileo <- lm(dev_height ~ dev_time_sq, data = fall_experiment2)

fit3_galileo
