library(ggplot2)
library(patchwork)
library(datasauRus)

temperature_experiment <- data.frame(
  celcius = c(24,25,26,27,24),
  fahrenheit = c(75,77,79,81,75)
)

ggplot(temperature_experiment) +
  geom_point(aes(x = celcius, y = fahrenheit)) +
  geom_line(aes(x = celcius, y = 32 + 9 * celcius / 5), color = "red") +
  theme_minimal() +
  labs(x = "Celcius", y = "Fahrenheit", title = "Temperature conversion")

set.seed(1234)
x <- seq(0, 10, 0.5)
y <- 2 + 3 * x + rnorm(length(x), 0, 1)
d <- data.frame(x = x, y = y)

ggplot(d) +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "x", y = "y", title = "Trend for simulated variables")

rm(x,y)

d <- datasaurus_dozen[datasaurus_dozen$dataset == "dino", ]

ggplot(d) +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "lm", se = FALSE, color = "green") +
  theme_minimal() +
  labs(x = "x", y = "y", title = "Trend for the dataSaurus' dino")

set.seed(1234)
x <- c(rnorm(10, 0, 1), 10)
y <- c(rnorm(10, 0, 1), 8)
d <- data.frame(x = x, y = y)

g1 <- ggplot(d) +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "lm", se = FALSE, color = "orange") +
  theme_minimal() +
  labs(x = "x", y = "y", title = "Trend with influential point")

g2 <- ggplot(d[-which.max(d$x),]) +
  geom_point(aes(x = x, y = y)) +
  geom_smooth(aes(x = x, y = y), method = "lm", se = FALSE, color = "purple") +
  theme_minimal() +
  labs(x = "x", y = "y", title = "Trend without influential point")

g1 | g2

rm(x,y)


