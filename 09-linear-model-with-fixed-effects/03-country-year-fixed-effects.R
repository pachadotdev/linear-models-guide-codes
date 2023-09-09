source("01-year-fixed-effects.R")

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

## Country-year fixed effects ----

### Estimating country-time fixed effects' coefficients

fit4_qog_corr <- lm(corruption ~ country + year + pop_vote + gdp_pc_1000,
  data = qog
)

fit4_qog_corr_tidy <- fit4_qog_corr %>%
  tidy(conf.int = TRUE) %>%
  filter(!grepl("country|year|Intercept", term)) %>%
  select(term, estimate, conf.low, conf.high) %>%
  pivot_longer(
    cols = c(estimate:conf.high),
    names_to = "stat",
    values_to = "fit1"
  )

fit4_qog_corr_tidy <- fit4_qog_corr_tidy %>%
  mutate(
    term = case_when(
      term == "gdp_pc_1000" ~ "GDP pc (1000 USD)",
      grepl("pop_vote", term) ~ "Democracy"
    )
  ) %>%
  pivot_wider(
    names_from = "stat",
    values_from = "fit1"
  ) %>%
  drop_na()

fit4_qog_corr_tidy

fit4_qog_corr_tidy <- fit4_qog_corr_tidy %>%
  mutate(model = "Country-year FEs") %>%
  bind_rows(
    fit1_qog_corr_tidy %>%
      filter(term != "(Intercept)") %>%
      mutate(
        term = case_when(
          term == "gdp_pc_1000" ~ "GDP pc (1000 USD)",
          grepl("pop_vote", term) ~ "Democracy"
        ),
        model = "Simple intercept"
      ) %>%
      pivot_wider(
        names_from = "stat",
        values_from = "fit1"
      )
  )

ggplot(fit4_qog_corr_tidy) +
  geom_pointrange(
    aes(
      x = estimate, xmin = conf.low, xmax = conf.high, y = term,
      color = model
    )
  ) +
  labs(
    x = "Estimated Coefficient",
    y = "Variable",
    color = "Model",
    title = "Slopes change when including FEs"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
