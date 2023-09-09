library(haven)
library(dplyr)
library(tidyr)
library(broom)
library(forcats)
library(ggplot2)
library(janitor)
library(stringr)

## Model specification with binary variables ----

### Corruption and popular vote in the Quality of Government dataset ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  select(country = cname, year, corruption = vdem_corr, vote = chga_demo) %>%
  drop_na()

qog

qog %>%
  pivot_longer(
    cols = c(year, corruption, vote),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    min = min(value),
    max = max(value),
    mean = mean(value),
    sd = sd(value)
  )

### Estimating a linear model and ANOVA with one predictor and two categories ----

fit_qog_corruption <- lm(corruption ~ vote, data = qog)

summary(fit_qog_corruption)

tidy(fit_qog_corruption)
glance(fit_qog_corruption)

qog <- qog %>%
  mutate(vote2 = if_else(vote == 0, 1, 0))

fit_qog_corruption_2 <- lm(corruption ~ vote2, data = qog)

tidy(fit_qog_corruption_2)
glance(fit_qog_corruption_2)

coef(aov(corruption ~ vote, data = qog))
coef(aov(corruption ~ vote2, data = qog))

### Corruption and regime type in the Quality of Government dataset ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  select(country = cname, year, corruption = vdem_corr, regime = wr_regtype) %>%
  drop_na()

qog

qog %>%
  pivot_longer(
    cols = c(year, corruption, regime),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    min = min(value),
    max = max(value),
    mean = mean(value),
    sd = sd(value)
  )

qog %>%
  distinct(regime) %>%
  arrange(regime)

### Estimating a linear model and ANOVA with one predictor and multiple categories ----

fit2_qog_corruption <- lm(corruption ~ regime, data = qog)

tidy(fit2_qog_corruption)
glance(fit2_qog_corruption)

qog <- qog %>%
  mutate(
    regime_chr = as.character(regime),
    regime_fct = as_factor(regime)
  )

qog

qog <- qog %>%
  mutate(
    regime_fct = fct_collapse(
      regime_fct,
      Democracy = "Party",
      Military = c("Indirect Military", "Military"),
      "One-Party" = c(
        "Party-Military", "Party-Military-Personal",
        "Party-Personal"
      )
    )
  )

fit2_qog_corruption <- lm(corruption ~ regime_chr, data = qog)
tidy(fit2_qog_corruption)
glance(fit2_qog_corruption)

# or
fit2_qog_corruption <- lm(corruption ~ regime_fct, data = qog)
tidy(fit2_qog_corruption)
glance(fit2_qog_corruption)

qog_mean_corruption_mil <- qog %>%
  filter(regime_fct == "Military") %>%
  summarise(
    mean_corruption = mean(corruption)
  ) %>%
  pull()

qog %>%
  group_by(regime_fct) %>%
  summarise(
    mean_corruption = mean(corruption)
  ) %>%
  mutate(
    mean_corruption_diff = mean_corruption - qog_mean_corruption_mil
  ) %>%
  arrange(regime_fct)

qog <- qog %>%
  mutate(
    regime_fct_2 = fct_relevel(regime_fct, "Democracy", after = 0)
  )

fit3_qog_corruption <- lm(corruption ~ regime_fct_2, data = qog)

tidy(fit3_qog_corruption)
glance(fit3_qog_corruption)

qog_mean_corruption_dem <- qog %>%
  filter(regime_fct == "Democracy") %>%
  summarise(mean_corruption = mean(corruption)) %>%
  pull()

qog_mean_corruption <- qog %>%
  group_by(regime_fct_2) %>%
  summarise(mean_corruption = mean(corruption)) %>%
  mutate(
    mean_corruption_dem = qog_mean_corruption_dem,
    mean_corruption_diff = mean_corruption - mean_corruption_dem,
    color = case_when(
      mean_corruption_diff < 0 ~ "Less corrupt",
      mean_corruption_diff > 0 ~ "More corrupt",
      mean_corruption_diff == 0 ~ "Reference category"
    )
  ) %>%
  left_join(
    fit3_qog_corruption %>%
      tidy() %>%
      clean_names() %>%
      mutate(
        term = str_replace(
          case_when(term == "(Intercept)" ~ "Democracy", TRUE ~ term),
          "regime_fct_2", ""
        )
      ),
    by = c("regime_fct_2" = "term")
  ) %>%
  mutate(
    regime_fct_2 = case_when(
      p_value >= 0.05 ~ paste(regime_fct_2, "(ns)"),
      TRUE ~ regime_fct_2
    )
  )

ggplot(qog_mean_corruption) +
  geom_vline(
    xintercept = qog_mean_corruption_dem, linetype = "dashed"
  ) +
  geom_linerange(
    aes(
      xmin = mean_corruption_dem, xmax = mean_corruption,
      y = regime_fct_2
    ),
    color = "gray50"
  ) +
  geom_point(
    aes(x = mean_corruption, y = regime_fct_2, color = color),
    size = 3
  ) +
  labs(
    x = "Estimated Coefficient",
    y = "Regime Type",
    color = "Corruption level",
    title = "Mean corruption by regime type"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Less corrupt" = "blue",
      "More corrupt" = "red",
      "Reference category" = "black"
    )
  ) +
  expand_limits(x = 0.4)

coef(aov(corruption ~ regime_fct_2, data = qog))

### Estimating a linear model with continuous and categorical predictors ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  select(
    country = cname, year, corruption = vdem_corr, regime = wr_regtype,
    state_fragility = cspf_sfi, gdp_pc = gle_cgdpc
  ) %>%
  drop_na() %>%
  mutate(
    regime_fct = fct_collapse(
      as_factor(regime),
      Democracy = "Party",
      Military = c("Indirect Military", "Military"),
      "One-Party" = c(
        "Party-Military", "Party-Military-Personal",
        "Party-Personal"
      )
    ),
    regime_fct = fct_relevel(regime_fct, "Democracy", after = 0),
    gdp_pc_1000 = gdp_pc / 1000
  )

qog %>%
  pivot_longer(
    cols = c(year, corruption, state_fragility, gdp_pc_1000),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    min = min(value),
    max = max(value),
    mean = mean(value),
    sd = sd(value)
  )

fit4_qog_corruption <- lm(corruption ~ regime_fct + state_fragility +
  gdp_pc_1000, data = qog)

tidy(fit4_qog_corruption)
glance(fit4_qog_corruption)
