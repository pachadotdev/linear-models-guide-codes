library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

url <- "https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv"
qog_csv <- "qog_bas_ts_jan23.csv"

if (!file.exists(qog_csv)) {
  download.file(url, qog_csv)
}

qog <- read_csv(qog_csv)

qog

vdem_canada <- qog %>%
  filter(cname == "Canada") %>%
  select(year, vdem_partipdem)

vdem_canada

vdem_canada <- vdem_canada %>%
  mutate(
    vdem_partipdem_1 = vdem_partipdem - lag(vdem_partipdem),
    vdem_partipdem_2 = vdem_partipdem - lag(vdem_partipdem, 2),
    vdem_partipdem_5 = vdem_partipdem - lag(vdem_partipdem, 5)
  )

vdem_canada

vdem_canada_2010 <- vdem_canada %>%
  filter(year >= 2010)

ggplot(vdem_canada_2010) +
  geom_line(aes(x = year, y = vdem_partipdem_1))

vdem_canada_2010_long <- vdem_canada_2010 %>%
  pivot_longer(
    cols = c(vdem_partipdem_1, vdem_partipdem_2, vdem_partipdem_5),
    names_to = "variable",
    values_to = "value"
  )

vdem_canada_2010_long

ggplot(vdem_canada_2010_long) +
  geom_line(aes(x = year, y = value, color = variable))
