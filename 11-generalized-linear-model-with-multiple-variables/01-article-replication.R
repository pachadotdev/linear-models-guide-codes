library(haven)
library(broom)
library(knitr)
library(censReg)
library(stargazer)

## Obtaining the original codes and data ----

url <- "https://personal.lse.ac.uk/tenreyro/regressors.zip"
zip <- gsub(".*/", "", url)
if (!file.exists(zip)) try(download.file(url, zip))

dout <- "log-of-gravity"
if (!dir.exists(dout)) unzip(zip, exdir = dout)

## Loading the original data ----

log_of_gravity <- read_dta(paste0(dout, "/Log of Gravity.dta"))

## Ordinary Least Squares ----

gravity_formula <- trade ~ lypex + lypim + lyex + lyim + ldist + border + 
  comlang + colony + landl_ex + landl_im + lremot_ex + lremot_im + 
  comfrt_wto + open_wto

fit_ols_1 <- glm(
  update.formula(gravity_formula, log(.) ~ .),
  data = log_of_gravity,
  subset = trade > 0
)

fit_ols_2 <- glm(
  update.formula(gravity_formula, log(1 + .) ~ .),
  data = log_of_gravity
)

kable(tidy(fit_ols_1))
kable(tidy(fit_ols_2))

## Poisson Pseudo Maximum Likelihood ----

fit_ppml_1 <- glm(
  gravity_formula,
  data = log_of_gravity,
  subset = trade > 0,
  family = quasipoisson()
)

fit_ppml_2 <- glm(
  gravity_formula,
  data = log_of_gravity,
  family = quasipoisson()
)

kable(tidy(fit_ppml_1))
kable(tidy(fit_ppml_2))

## Tobit ----

a <- 200
lypex_ref <- 1.058
tol <- 0.001
lypex_estimate <- 2 * lypex_ref
iter <- 0

while (abs(lypex_estimate - lypex_ref) > tol) {
  log_of_gravity$log_trade_cens <- log(a + log_of_gravity$trade)
  log_trade_cens_min <- min(log_of_gravity$log_trade_cens, na.rm = TRUE)
  
  fit_tobit <- censReg(
    formula = update.formula(gravity_formula, log_trade_cens ~ .),
    left = log_trade_cens_min,
    right = Inf,
    data = log_of_gravity,
    start = rep(0, 2 + length(attr(terms(gravity_formula), "term.labels"))),
    method = "BHHH"
  )
  
  lypex_estimate <- coef(fit_tobit)[2]
  if (abs(lypex_estimate - lypex_ref) > 2 * tol) {
    a <- a - 5
  } else {
    a <- a - 1
  }
  iter <- iter + 1
}

kable(tidy(fit_tobit))

## Reporting multiple models ----

stargazer(
  fit_ols_1, fit_ols_2, fit_ppml_1, fit_ppml_2, fit_tobit,
  header = FALSE,
  font.size = "footnotesize",
  model.names = F, 
  omit.table.layout = "d",
  omit.stat = c("f","ser","ll","aic","bic","rsq","adj.rsq"),
  title = "Replication for OLS (1-2), PPML (3-4), and Tobit (5).",
  dep.var.caption = ""
)
