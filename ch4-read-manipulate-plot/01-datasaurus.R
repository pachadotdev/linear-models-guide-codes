# install.packages("datasauRus")
# install.packages("dplyr")

library(datasauRus)
library(dplyr)
library(ggplot2)

datasaurus_dozen

datasaurus_dozen %>%
  summarize(
    mean_x = mean(x),
    mean_y = mean(y)
  )

datasaurus_dozen %>%
  summarize(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y)
  )

datasaurus_dozen %>%
  group_by(dataset) %>%
  summarize(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y)
  )

ggplot(datasaurus_dozen) +
  geom_point(aes(x = x, y = y))

ggplot(datasaurus_dozen) +
  geom_point(aes(x = x, y = y)) +
  facet_wrap(~dataset)

datasaurus_plot <- ggplot(datasaurus_dozen) +
  geom_point(aes(x = x, y = y))

datasaurus_plot +
  facet_wrap(~dataset)
