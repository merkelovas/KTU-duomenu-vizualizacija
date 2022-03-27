library(tidyverse)
library(stringr)
# https://www.kaggle.com/prasertk/defense-contractors-market-cap-revenue-earning?select=defense+contractor.csv

data <- read_csv("defense contractor.csv")

data %>%
  group_by(Country) %>%
  mutate(Cap = str_remove_all(`Market cap`, "B|\\$")) %>%
  mutate(Cap = as.numeric(trimws(Cap))) %>%
  filter(Year == "2022") %>%
  summarise(n = sum(Cap)) %>%
  mutate(Country = fct_reorder(Country, n)) %>%
  ggplot(aes(x = Country, y = n)) +
    geom_col()

data %>%
  filter(Name == "Boeing") %>%
  mutate(test = str_remove_all(`Market cap`, "B|\\$")) %>%
  mutate(test = as.numeric(trimws(test))) %>%
  ggplot(aes(x = Year, y = test)) +
  geom_line() +
  ylab("Market cap") +
  theme_dark()
