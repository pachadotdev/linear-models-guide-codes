library(haven)
library(dplyr)
library(tidyr)
library(broom)
library(stringr)
library(forcats)
library(ggplot2)
library(purrr)

## Model specificaction with binary interactions ----

### Corruption and interaction variables in the Quality of Government dataset ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  select(
    country = cname, year, corruption = vdem_corr, pop_vote = chga_demo,
    gdp_pc = gle_cgdpc
  ) %>%
  drop_na() %>%
  filter(year == 2008) %>%
  mutate(
    pop_vote = as_factor(pop_vote),
    gdp_pc_1000 = gdp_pc / 1000
  )

ggplot(qog) +
  geom_point(
    aes(x = log(gdp_pc_1000), y = log(corruption), color = pop_vote),
    alpha = 0.3
  ) +
  geom_smooth(
    aes(x = log(gdp_pc_1000), y = log(corruption), color = pop_vote),
    method = "lm", se = FALSE
  ) +
  labs(
    x = "Log(GDP per capita in thousands of dollars)",
    y = "Log(Corruption Index)",
    color = "Popular Vote",
    title = "Corruption index as a linear function of GDP per capita"
  ) +
  theme_minimal()

fit5_qog_corruption <- lm(corruption ~ pop_vote + gdp_pc_1000,
  data = qog
)

tidy(fit5_qog_corruption)
glance(fit5_qog_corruption)

### Estimating a linear model with binary interactions ----

fit6_qog_corruption <- lm(corruption ~ pop_vote * gdp_pc_1000,
  data = qog
)

tidy(fit6_qog_corruption)
glance(fit6_qog_corruption)

qog <- qog %>%
  mutate(
    pop_vote_int = as.integer(pop_vote) - 1L,
    vote_x_gdp = pop_vote_int * gdp_pc_1000
  )

fit7_qog_corruption <- lm(corruption ~ pop_vote + gdp_pc_1000 + vote_x_gdp,
  data = qog
)

tidy(fit7_qog_corruption)
glance(fit7_qog_corruption)

### Confidence intervals with binary interactions ----

tidy(fit6_qog_corruption, conf.int = TRUE)

model_conf_int <- function(conf_level = 0.95) {
  d <- tidy(fit6_qog_corruption, conf.int = TRUE, conf.level = conf_level) %>%
    select(term, estimate, conf_low = conf.low, conf_high = conf.high) %>%
    mutate(conf_level = conf_level)

  return(d)
}

confint_qog_corruption <- map_df(
  c(0.90, 0.95, 0.99),
  model_conf_int
)

confint_qog_corruption

model_conf_int <- function(model, conf_level = 0.95) {
  d <- tidy(model, conf.int = TRUE, conf.level = conf_level) %>%
    select(term, estimate, conf_low = conf.low, conf_high = conf.high) %>%
    mutate(conf_level = conf_level)

  return(d)
}

confint_qog_corruption <- map_df(
  c(0.90, 0.95, 0.99),
  model_conf_int,
  model = fit6_qog_corruption
)

confint_qog_corruption <- confint_qog_corruption %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "1 - Intercept",
      term == "pop_vote1. Democracy" ~ "2 - Democracy",
      term == "gdp_pc_1000" ~ "3 - GDP per capita",
      term == "pop_vote1. Democracy:gdp_pc_1000" ~ "4 - Dem x GDP pc"
    ),
    conf_level = as_factor(paste(conf_level * 100, "%", sep = ""))
  )

ggplot(confint_qog_corruption) +
  geom_vline(
    xintercept = 0, linetype = "dashed"
  ) +
  geom_pointrange(
    aes(
      x = estimate, xmin = conf_low, xmax = conf_high, y = term,
      color = conf_level
    ),
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = "Estimated Coefficient",
    y = "Term",
    color = "Confidence Level",
    title = "Binary interaction effect on corruption"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

## Model specification with categorical interactions ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  select(
    country = cname, year, corruption = vdem_corr, regime = wr_regtype,
    gdp_pc = gle_cgdpc
  ) %>%
  drop_na() %>%
  filter(year == 2008) %>%
  mutate(
    gdp_pc_1000 = gdp_pc / 1000,
    regime = fct_relevel(fct_collapse(
      as_factor(regime),
      Democracy = "Party",
      Military = c("Indirect Military", "Military"),
      "One-Party" = c(
        "Party-Military", "Party-Military-Personal",
        "Party-Personal"
      )
    ), "Democracy", after = 0)
  )

### Estimating a linear model with categorical interactions ----

fit8_qog_corruption <- lm(corruption ~ regime * gdp_pc_1000,
  data = qog
)

tidy(fit8_qog_corruption)
glance(fit8_qog_corruption)

### Confidence intervals with categorical interactions ----

confint2_qog_corruption <- map_df(
  c(0.90, 0.95, 0.99),
  model_conf_int,
  model = fit8_qog_corruption
)

confint2_qog_corruption

qog %>%
  filter(regime == "Military-Personal") %>%
  select(regime, gdp_pc_1000)

confint2_qog_corruption <- confint2_qog_corruption %>%
  mutate(
    term = term %>%
      str_replace("^regime", "") %>%
      str_replace(":", " x ") %>%
      str_replace("gdp_pc_1000", "GDP pc"),
    conf_level = as_factor(paste(conf_level * 100, "%", sep = ""))
  ) %>%
  drop_na()

ggplot(confint2_qog_corruption) +
  geom_vline(
    xintercept = 0, linetype = "dashed"
  ) +
  geom_pointrange(
    aes(
      x = estimate, xmin = conf_low, xmax = conf_high, y = term,
      color = conf_level
    ),
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = "Estimated Coefficient",
    y = "Term",
    color = "Confidence Level",
    title = "Categorical interaction effect on corruption"
  ) +
  theme_minimal()
