library(haven)
library(dplyr)
library(tidyr)

### Poisson model ----

qog <- read_sav("qog_bas_ts_jan23.sav") %>%
  filter(year >= 2005 & year <= 2010) %>%
  select(terror = gd_ptsa, gdp_pc = gle_cgdpc) %>%
  drop_na()

qog

x <- qog$gdp_pc / 1000
y <- qog$terror

coef(glm(y ~ x, family = poisson))

gdp1 <- 10
gdp2 <- 11

betas <- coef(glm(y ~ x, family = poisson))

exp(betas[1] + betas[2] * gdp2) - exp(betas[1] + betas[2] * gdp1)

# define the new variables

max_y <- max(y)
y2 <- y / max_y
mu <- (y2 + 0.5) / 2
eta <- log(mu)
z <- eta + (y2 - mu) / mu

# iterate with initial values for the difference and the sum of sq residuals

dif <- 1
rss1 <- 1

while (abs(dif) > 1e-10) {
  fit <- lm(z ~ x, weights = mu)
  eta <- z - fit$residuals
  mu <- exp(eta)
  z <- eta + (y2 - mu) / mu
  res <- y2 - mu
  rss2 <- sum(res^2)
  dif <- rss2 - rss1
  rss1 <- rss2
}

z <- z + log(max_y)

coef(lm(z ~ x, weights = mu))
