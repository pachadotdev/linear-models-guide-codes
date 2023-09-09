library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

url <- "https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.sav"
qog_sav <- "qog_bas_ts_jan23.sav"

if (!file.exists(qog_sav)) {
  download.file(url, qog_sav)
}

qog <- read_sav(qog_sav)

qog

gini_canada_2010_long <- qog %>%
  filter(cname == "Canada") %>%
  select(year, lis_gini) %>%
  mutate(
    lis_gini_5 = (lis_gini - lag(lis_gini, 5)) /
      lag(lis_gini, 5)
  ) %>%
  pivot_longer(
    cols = lis_gini_5,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(year >= 2010)

ggplot(gini_canada_2010_long) +
  geom_line(aes(x = year, y = value, color = variable))

gini_plot <- ggplot(gini_canada_2010_long) +
  geom_line(aes(x = year, y = value, color = variable))

gini_plot <- gini_plot +
  labs(
    title = "Change in the Gini index with respect to 5 years ago",
    subtitle = "Based on the Quality of Government dataset",
    x = "Change",
    y = "Year"
  ) +
  theme_minimal()

gini_plot

gini_plot +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = as.integer)
