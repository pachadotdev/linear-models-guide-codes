library(haven)
library(dplyr)
library(tidyr)
library(AER)

### Quasi-Poisson model ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year >= 2005 & year <= 2010) %>%
  select(terror = gd_ptsa, gdp_pc = gle_cgdpc) %>%
  drop_na()

fit_qog_terror <- glm(terror ~ gdp_pc, data = qog, family = poisson)

# test underdispersion
dispersiontest(fit_qog_terror, alternative = "less")

# test overdispersion
dispersiontest(fit_qog_terror, alternative = "greater")

var(qog$terror) / mean(qog$terror)

x <- qog$gdp_pc / 1000
y <- qog$terror

fit_qog_terror2 <- glm(y ~ x, family = quasipoisson)

summary(fit_qog_terror)

summary(fit_qog_terror2)
