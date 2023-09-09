library(HistData)
library(dplyr)

galton <- Galton %>%
  as_tibble()

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
