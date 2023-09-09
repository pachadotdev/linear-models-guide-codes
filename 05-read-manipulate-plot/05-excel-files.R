library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(ggplot2)

url <- "https://freedomhouse.org/sites/default/files/2023-02/Country_and_Territory_Ratings_and_Statuses_FIW_1973-2023%20.xlsx"

fiw_xlsx <- url %>%
  str_replace("%20", "") %>%
  str_replace(".*/", "")

if (!file.exists(fiw_xlsx)) {
  download.file(url, fiw_xlsx)
}

fiw <- read_excel(fiw_xlsx, sheet = 2, na = "-")

fiw

fiw <- fiw %>%
  clean_names() %>%
  rename(country = survey_edition)

fiw

fiw <- fiw %>%
  filter(
    country != "Year(s) Under Review",
    !is.na(country)
  ) %>%
  pivot_longer(cols = -country, names_to = "year", values_to = "value")

fiw

fiw <- fiw %>%
  mutate(
    year = as.integer(str_replace_all(year, "[^0-9]", "")) - 1L,
    year = case_when(
      year < 1972 ~ NA_integer_,
      TRUE ~ year
    )
  )

fiw

fiw <- fiw %>%
  fill(year)

fiw

fiw <- fiw %>%
  group_by(country, year) %>%
  mutate(
    n = row_number(),
    category = case_when(
      n == 1 ~ "political_rights",
      n == 2 ~ "civil_liberties",
      n == 3 ~ "status"
    )
  ) %>%
  ungroup()

fiw

fiw <- fiw %>%
  select(-n) %>%
  pivot_wider(names_from = category, values_from = value)

fiw

fiw %>%
  filter(
    str_length(political_rights) > 1,
    str_length(civil_liberties) > 1
  )

remove_parenthesis <- function(x) {
  x %>%
    str_extract("\\((.*?)\\)") %>%
    str_replace_all("\\(|\\)", "")
}

remove_parenthesis("2(5)")

fiw <- fiw %>%
  mutate(
    political_rights = as.integer(case_when(
      country == "South Africa" &
        year == 1972 ~ remove_parenthesis(political_rights),
      TRUE ~ political_rights
    )),
    civil_liberties = as.integer(case_when(
      country == "South Africa" &
        year == 1972 ~ remove_parenthesis(civil_liberties),
      TRUE ~ civil_liberties
    )),
    status = case_when(
      country == "South Africa" &
        year == 1972 ~ remove_parenthesis(status),
      TRUE ~ status
    )
  )

fiw %>%
  filter(
    country == "South Africa",
    year == 1972
  )

fiw %>%
  filter(
    is.na(political_rights) |
      is.na(civil_liberties)
  )

fiw <- fiw %>%
  mutate(
    year = case_when(
      nchar(year) > 4 ~ as.integer(substr(year, 1, 4)) - 1L,
      TRUE ~ year
    )
  )

fiw <- fiw %>%
  drop_na(political_rights, civil_liberties, status)

fiw %>%
  distinct(year) %>%
  arrange() %>%
  pull() %>%
  range()

fiw

ggplot(
  fiw %>%
    filter(
      country %in% c("Canada", "United States", "Mexico"),
      year >= 2010
    )
) +
  geom_line(aes(x = year, y = civil_liberties, color = country)) +
  labs(
    title = "Civil Liberties in the CAN-USA-MEX",
    subtitle = "Based on the Freedom in the World dataset",
    x = "Year",
    y = "Score"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = as.integer) +
  facet_wrap(~country, nrow = 3)
