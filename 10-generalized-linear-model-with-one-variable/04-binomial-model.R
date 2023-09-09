library(haven)
library(dplyr)
library(tidyr)
library(broom)

### Binomial model (or logit model) ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year >= 2005 & year <= 2010) %>%
  select(democracy = bmr_dem, gdp_pc = gle_cgdpc) %>%
  drop_na()

qog

x <- qog$gdp_pc / 1000
y <- qog$democracy

coef(glm(y ~ x, family = binomial))

gdp1 <- 10
gdp2 <- 11

betas <- coef(glm(y ~ x, family = binomial))

# logit prediction for income1
exp(betas[1] + betas[2] * gdp2) / (1 + exp(betas[1] + betas[2] * gdp2)) -
  exp(betas[1] + betas[2] * gdp1) / (1 + exp(betas[1] + betas[2] * gdp1))

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year >= 2005 & year <= 2010) %>%
  select(democracy = bmr_dem, gdp_pc = gle_cgdpc) %>%
  drop_na()

fit_qog_democracy <- glm(democracy ~ gdp_pc, data = qog, family = binomial)

qog2 <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year == 2010) %>%
  select(country = cname, democracy = bmr_dem, gdp_pc = gle_cgdpc) %>%
  drop_na()

qog2 %>%
  mutate(
    dem_prob = predict(fit_qog_democracy,
      newdata = qog2,
      type = "response"
    ),
    dem_prob_050 = ifelse(dem_prob < 0.5, 0, 1),
    dem_prob_075 = ifelse(dem_prob < 0.75, 0, 1)
  ) %>%
  select(-gdp_pc)

augment(fit_qog_democracy)

summary(fit_qog_democracy)

# define the new variables

mu <- (y + 0.5) / 2
eta <- log(mu / (1 - mu))
z <- eta + (y - mu) / mu

# iterate with initial values for the difference and the sum of sq residuals

dif <- 1
rss1 <- 1

while (abs(dif) > 1e-10) {
  fit <- lm(z ~ x, weights = mu)
  eta <- z - fit$residuals
  mu <- exp(eta) / (1 + exp(eta))
  z <- eta + (y - mu) / mu
  res <- y - mu
  rss2 <- sum(res^2)
  dif <- rss2 - rss1
  rss1 <- rss2
}

coef(lm(z ~ x, weights = mu))
