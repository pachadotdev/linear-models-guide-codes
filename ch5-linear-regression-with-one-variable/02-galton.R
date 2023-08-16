library(HistData)
library(dplyr)
library(ggplot2)
library(broom)
library(janitor)

galton <- Galton %>%
  as_tibble()

galton

child <- galton$child
parent <- galton$parent

# or using dplyr

# child <- galton %>%
#     pull(child)

# parent <- galton %>%
#     pull(parent)

beta1 <- cor(parent, child) *  sd(child) / sd(parent)
beta0 <- mean(parent) - beta1 * mean(child)

c(beta0, beta1)

lm(child ~ parent)

head(Galton)
tail(Galton)

X <- cbind(1, parent)

dim(X)
head(X)
tail(X)

dim(t(X))

t(X) %*% X

XtXinv <- solve(t(X) %*% X)

XtXinv

y <- child

Xty <- t(X) %*% y

Xty

XtXinv %*% Xty

fit1_galton <- solve(t(X) %*% X) %*% t(X) %*% y

round(fit1_galton, 3)

x <- parent

fit2_galton <- lm(y ~ x)

round(fit2_galton$coefficients, 3)

all.equal(as.numeric(fit1_galton), as.numeric(fit2_galton$coefficients))

class(fit1_galton)
class(fit2_galton)
class(fit2_galton$coefficients)

rm(X, x, y)

fit3_galton <- lm(child ~ log(parent))

fit3_galton$coefficients[2] / 100

fit4_galton <- lm(log(child) ~ log(parent))

fit4_galton$coefficients[2]

fit5_galton <- lm(log(child) ~ parent)

exp(fit5_galton$coefficients[2])

exp(fit5_galton$coefficients[2])

fit2_galton_aug <- fit2_galton %>%
  augment()

fit2_galton_aug

ggplot(fit2_galton_aug) +
  geom_point(aes(x = parent, y = child))

ggplot(fit2_galton_aug) +
  geom_point(aes(x = parent, y = child), color = "blue") +
  geom_line(aes(x = parent, y = .fitted), color = "red")

fit2_galton_plot <- ggplot(fit2_galton_aug) +
  geom_point(aes(x = parent, y = child), color = "blue")

fit2_galton_plot +
  geom_line(aes(x = parent, y = .fitted), color = "red")

fit5_galton_aug <- fit5_galton %>%
  augment()

fit5_galton_aug

ggplot(fit5_galton_aug) +
  geom_point(aes(x = parent, y = `log(child)`), color = "orange") +
  geom_line(aes(x = parent, y = .fitted), color = "blue")

fit5_galton_aug <- fit5_galton_aug %>%
  clean_names()

fit5_galton_aug

fit5_galton_plot <- ggplot(fit5_galton_aug) +
  geom_point(aes(x = parent, y = log_child), color = "#165976") +
  geom_line(aes(x = parent, y = fitted), color = "#d04e66") +
  labs(
    title = "log(y) ~ x",
    subtitle = "Based on Galton's data",
    x = "Parent's height (inches)",
    y = "Child's height (log inches)"
  ) +
  theme_minimal()

fit5_galton_plot
