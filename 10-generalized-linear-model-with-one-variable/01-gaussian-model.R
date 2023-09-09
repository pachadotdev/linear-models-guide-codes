### Gaussian model ----

library(haven)
library(dplyr)
library(tidyr)

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year >= 2005 & year <= 2010) %>%
  select(life_exp = wdi_lifexp, gdp_pc = gle_cgdpc) %>%
  drop_na()

qog

x <- qog$gdp_pc / 1000
y <- qog$life_exp

coef(lm(y ~ x))

# same as
coef(glm(y ~ x, family = gaussian))
