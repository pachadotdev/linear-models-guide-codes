library(haven)
library(dplyr)
library(tidyr)
library(broom)
library(stringr)
library(ggplot2)

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

## Country fixed effects ----

### Estimating country-time fixed effects' coefficients ----

fit3_qog_corr <- lm(corruption ~ country + pop_vote + gdp_pc_1000, data = qog)

fit3_qog_corr_tidy <- fit3_qog_corr %>%
  tidy(conf.int = TRUE) %>%
  filter(p.value < 0.05) %>%
  select(term, estimate, conf.low, conf.high) %>%
  pivot_longer(
    cols = c(estimate:conf.high),
    names_to = "stat",
    values_to = "fit1"
  )

fit3_qog_corr_tidy

fit3_qog_corr_tidy <- fit3_qog_corr_tidy %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Afghanistan",
      term == "pop_vote" ~ "Popular vote",
      term == "gdp_pc_1000" ~ "GDP pc (1000 USD)",
      grepl("country", term) ~ str_replace(term, "country", "")
    )
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "fit1"
  ) %>%
  drop_na()

fit3_qog_corr_tidy

fit3_qog_corr_pos <- fit3_qog_corr_tidy %>%
  filter(conf.low > 0) %>%
  mutate(term = str_replace(term, " \\(.*", ""))

ggplot(fit3_qog_corr_pos) +
  geom_pointrange(
    aes(x = estimate, xmin = conf.low, xmax = conf.high, y = term),
    position = position_dodge(width = 0.5)
  ) +
  labs(
    x = "Estimated Coefficient",
    y = "Country",
    title = "Country-specific positive effects on corruption"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
