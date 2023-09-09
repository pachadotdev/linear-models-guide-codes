library(haven)
library(dplyr)
library(tidyr)
library(broom)
library(stringr)

## Year fixed effects ----

### Estimating year fixed effects' coefficients ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  select(
    country = cname, year, corruption = vdem_corr,
    pop_vote = chga_demo, gdp_pc = gle_cgdpc
  ) %>%
  drop_na() %>%
  filter(year >= 1990 & year <= 2005) %>%
  mutate(
    country = as_factor(country),
    year = as_factor(year),
    pop_vote = as_factor(pop_vote),
    gdp_pc_1000 = gdp_pc / 1000
  )

qog

qog %>%
  select(country, year) %>%
  distinct() %>%
  summarise(
    n_countries = n_distinct(country),
    n_years = n_distinct(year)
  )

### Estimating year fixed effects' coefficients ----

fit1_qog_corr <- lm(corruption ~ pop_vote + gdp_pc_1000, data = qog)
fit2_qog_corr <- lm(corruption ~ year + pop_vote + gdp_pc_1000, data = qog)

fit1_qog_corr_tidy <- fit1_qog_corr %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  pivot_longer(
    cols = c(estimate:conf.high),
    names_to = "stat",
    values_to = "fit1"
  )

fit2_qog_corr_tidy <- fit2_qog_corr %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  pivot_longer(
    cols = c(estimate:conf.high),
    names_to = "stat",
    values_to = "fit2"
  )

fit1_qog_corr_tidy

fit2_qog_corr_tidy

fit_qog_corr_tidy <- full_join(fit1_qog_corr_tidy, fit2_qog_corr_tidy)

fit_qog_corr_tidy

fit_qog_corr_tidy <- fit_qog_corr_tidy %>%
  pivot_longer(
    cols = c(fit1, fit2),
    names_to = "model",
    values_to = "value"
  ) %>%
  mutate(
    model = if_else(model == "fit1", "Single intercept", "Fixed effects"),
    term = case_when(
      term == "(Intercept)" ~ "Year 1990",
      term == "gdp_pc" ~ "GDP pc (1000 USD)",
      grepl("pop_vote", term) ~ str_replace(term, "pop_vote", "Popular vote "),
      grepl("year", term) ~ str_replace(term, "year", "Year ")
    )
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "value"
  ) %>%
  drop_na()

fit_qog_corr_tidy
