library(HistData)
library(dplyr)

# create a variable with the frequency of each observation
galton <- Galton %>%
  as_tibble() %>%
  mutate(freq = n()) %>%
  ungroup()

galton

# estimate the model with weights
lm(child ~ parent, data = galton, weights = freq)
