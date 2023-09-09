# artificial normally distributed variables and one poisson distributed
set.seed(42)
x1 <- rnorm(100)
x2 <- 2 * x1
x3 <- 3 * x1 + 1
y <- rpois(100, 1)

# x2 = 2x1
lm(y ~ x1 + x2)

# x3 = 3x1 + 1
lm(y ~ x1 + x3)

# x2 = 2x1
# x3 = 3x1 + 1
# => 2x3 - 3x2 = 6x1 + 2 - 6x1
# => x3 = (3x2 + 2) / 2
lm(y ~ x2 + x3)

# artificial normally, poisson, binomial and t distributed variables
set.seed(42)
x1 <- rnorm(100)
x2 <- rpois(100, 2)
x3 <- rbinom(100, 1, 0.5)
y <- rt(100, 1)

lm(y ~ x1 + x2)

lm(y ~ x1 + x3)

lm(y ~ x2 + x3)
