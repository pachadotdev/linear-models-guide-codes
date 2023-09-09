library(haven)
library(dplyr)
library(tidyr)
library(broom)
library(janitor)
library(ggplot2)
library(lmtest)

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year >= 2005 & year <= 2019) %>%
  select(year,
    country = cname, life_exp = wdi_lifexp, gdp_pc = gle_cgdpc,
    sub_hap = whr_hap
  ) %>%
  drop_na()

qog

qog %>%
  pivot_longer(
    cols = c(year, life_exp, gdp_pc, sub_hap),
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

lm(life_exp ~ gdp_pc + sub_hap, data = qog)

qog <- qog %>%
  mutate(gdp_pc_1000 = gdp_pc / 1000)

lm(life_exp ~ gdp_pc_1000 + sub_hap, data = qog)

qog <- qog %>%
  mutate(
    gdp_pc_1000_dev = gdp_pc_1000 - mean(gdp_pc_1000),
    sub_hap_dev = sub_hap - mean(sub_hap)
  )

lm(life_exp ~ gdp_pc_1000_dev + sub_hap_dev, data = qog)

mean_gdp_pc <- mean(qog$gdp_pc)
mean_sub_hap <- mean(qog$sub_hap)

thresholds <- c(200, 0.3)

qog %>%
  filter(
    gdp_pc >= mean_gdp_pc - thresholds[1] &
      gdp_pc <= mean_gdp_pc + thresholds[1],
    sub_hap >= mean_sub_hap - thresholds[2] &
      sub_hap <= mean_sub_hap + thresholds[2]
  )

fit_qog_lifexp <- lm(life_exp ~ gdp_pc_1000_dev + sub_hap_dev, data = qog)

fit_qog_lifexp_aug <- fit_qog_lifexp %>%
  augment() %>%
  clean_names()

fit_qog_lifexp_aug

qog %>%
  left_join(fit_qog_lifexp_aug)

fit_qog_lifexp_aug <- fit_qog_lifexp %>%
  augment(qog) %>%
  clean_names() %>%
  select(-c(gdp_pc, sub_hap, gdp_pc_1000))

fit_qog_lifexp_aug

ggplot(fit_qog_lifexp_aug) +
  geom_point(aes(x = life_exp, y = fitted, color = year)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Observed life expectancy",
    y = "Predicted life expectancy"
  ) +
  theme_minimal()

fit_qog_lifexp_aux <- fit_qog_lifexp_aug %>%
  filter(country == "Canada") %>%
  select(year, life_exp, life_exp_pred = fitted)

fit_qog_lifexp_aux_2 <- fit_qog_lifexp_aug %>%
  filter(country == "Canada") %>%
  select(year, life_exp, life_exp_pred = fitted) %>%
  pivot_longer(
    cols = c(life_exp, life_exp_pred),
    names_to = "variable",
    values_to = "value"
  )

ggplot(fit_qog_lifexp_aux) +
  geom_linerange(
    data = fit_qog_lifexp_aux,
    aes(x = year, ymin = life_exp, ymax = life_exp_pred),
  ) +
  geom_point(
    data = fit_qog_lifexp_aux_2,
    aes(x = year, y = value, color = variable),
    size = 3
  ) +
  labs(
    x = "Observation",
    y = "Life expectancy",
    color = "Variable",
    title = "Canada's prediction error as vertical distances"
  ) +
  expand_limits(y = fit_qog_lifexp$coef[1]) +
  theme_minimal() +
  theme(legend.position = "top")

rm(fit_qog_lifexp_aux, fit_qog_lifexp_aux_2)

ggplot(fit_qog_lifexp_aug) +
  geom_point(aes(x = life_exp, y = fitted, color = year)) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Observed life expectancy",
    y = "Predicted life expectancy"
  ) +
  theme_minimal() +
  facet_wrap(~year, ncol = 3)

fit_qog_lifexp_aug %>%
  summarise(
    rmse = sqrt(mean(resid^2)),
    mae = mean(abs(resid))
  )

rmse <- function(x, y) {
  sqrt(mean((x - y)^2))
}

mae <- function(x, y) {
  mean(abs(x - y))
}

rmse(pull(fit_qog_lifexp_aug, life_exp), pull(fit_qog_lifexp_aug, fitted))
mae(pull(fit_qog_lifexp_aug, life_exp), pull(fit_qog_lifexp_aug, fitted))

# or
rmse(fit_qog_lifexp_aug$life_exp, fit_qog_lifexp_aug$fitted)
mae(fit_qog_lifexp_aug$life_exp, fit_qog_lifexp_aug$fitted)

fit_qog_lifexp_aug %>%
  summarise(
    life_exp_sd = sd(life_exp),
    rmse = sqrt(mean(resid^2)),
    mae = mean(abs(resid))
  ) %>%
  mutate(
    rmse_prop = rmse / life_exp_sd,
    mae_prop = mae / life_exp_sd
  )

fit_qog_lifexp_aux <- tibble(
  resid_mean = fit_qog_lifexp_aug %>%
    pull(resid) %>%
    mean(),
  life_exp_rmse = fit_qog_lifexp_aug %>%
    summarise(
      rmse = rmse(life_exp, fitted)
    ) %>%
    pull(rmse),
  life_exp_mae = fit_qog_lifexp_aug %>%
    summarise(
      mae = mae(life_exp, fitted)
    ) %>%
    pull(mae)
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

ggplot() +
  geom_density(
    data = fit_qog_lifexp_aug,
    aes(x = resid)
  ) +
  geom_vline(
    data = fit_qog_lifexp_aux,
    aes(xintercept = value, color = variable),
    linetype = "dashed"
  ) +
  labs(
    x = "Error",
    y = "Density",
    color = "Variable",
    title = "Model error's density"
  ) +
  theme_minimal()

summary(fit_qog_lifexp)

se <- sqrt(diag(vcov(fit_qog_lifexp)))
se

ts <- coef(fit_qog_lifexp) / se
ts

res_df <- fit_qog_lifexp$df.residual

2 * pt(abs(ts), df = res_df, lower.tail = FALSE)

sum_res_sq <- sum(fit_qog_lifexp$residuals^2)
rse <- c("RSE" = sqrt(sum_res_sq / res_df), "df" = res_df)

# Rounding for display only
round(rse, 3)

sum_y_my_sq <- sum((fit_qog_lifexp_aug$life_exp - mean(fit_qog_lifexp_aug$life_exp))^2)

1 - sum_res_sq / sum_y_my_sq

# do not use the dataset directly
# except if you are sure that there are no missing values

nobs <- length(fit_qog_lifexp_aug$life_exp)
# or
nobs <- fit_qog_lifexp$df + length(fit_qog_lifexp$coefficients)

1 - (sum_res_sq / res_df) / (sum_y_my_sq / (nobs - 1))

# degrees of freedom
df1 <- length(coef(fit_qog_lifexp)) - 1
df2 <- length(resid(fit_qog_lifexp)) - length(coef(fit_qog_lifexp))

# F-statistic
fs <- (sum((fit_qog_lifexp_aug$fitted - mean(fit_qog_lifexp_aug$life_exp))^2) / df1) /
  (sum(fit_qog_lifexp_aug$resid^2) / df2)

# Associated p-value
pv <- pf(fs, df1 = df1, df2 = df2, lower.tail = FALSE)

# Rounding for display only
round(c("F-stat" = fs, "df1" = df1, "df2" = df2, "p-val" = pv), 3)

shapiro.test(fit_qog_lifexp_aug$resid)

ks.test(
  fit_qog_lifexp_aug$resid, "pnorm", mean(fit_qog_lifexp_aug$resid),
  sd(fit_qog_lifexp_aug$resid)
)

bptest(fit_qog_lifexp)

gqtest(fit_qog_lifexp)
