library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

url <- "https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23_stata14.dta"
qog_dta <- "qog_bas_ts_jan23.dta"

if (!file.exists(qog_dta)) {
  download.file(url, qog_dta)
}

qog <- read_dta(qog_dta)

qog

corruption_canada_2010_long <- qog %>%
  filter(cname == "Canada") %>%
  select(year, ti_cpi) %>%
  mutate(
    ti_cpi_1 = (ti_cpi - lag(ti_cpi)) /
      lag(ti_cpi),
    ti_cpi_2 = (ti_cpi - lag(ti_cpi, 2)) /
      lag(ti_cpi, 2),
    ti_cpi_3 = (ti_cpi - lag(ti_cpi, 3)) /
      lag(ti_cpi, 3)
  ) %>%
  pivot_longer(
    cols = ti_cpi_1:ti_cpi_3,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(year >= 2010)

ggplot(corruption_canada_2010_long) +
  geom_line(aes(x = year, y = value, color = variable)) +
  labs(
    title = "Change in the corruption index with respect to 5 years ago",
    subtitle = "Based on the Quality of Government dataset",
    x = "Change",
    y = "Year"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = as.integer) +
  facet_wrap(~variable, nrow = 3)
