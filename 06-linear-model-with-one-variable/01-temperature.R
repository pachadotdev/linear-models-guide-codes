celcius <- c(24, 25, 25, 27, 24)
fahrenheit <- c(75, 77, 77, 81, 76)

beta1 <- cor(fahrenheit, celcius) * sd(fahrenheit) / sd(celcius)
beta0 <- mean(fahrenheit) - beta1 * mean(celcius)

c(beta0, beta1)

c(beta0, beta1) - c(32, 9 / 5)
