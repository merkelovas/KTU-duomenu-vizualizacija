library(tidyverse)
data <- read_csv("../laboratorinis/data/lab_sodra.csv")

summary(data)

data$municipality <- as.factor(data$municipality)


data %>%
  group_by(ecoActName) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

hist(data$avgWage, breaks = 200)

## 1 uzduotis
data %>%
  filter(tax < 1000000) %>%
  ggplot(aes(x = tax)) +
  geom_histogram(bins = 200)

top5 <- data %>%
  group_by(name, code) %>%
  summarise(wage = max(avgWage)) %>%
  arrange(desc(wage)) %>%
  head(5)

data %>%
  filter(code == 80333) %>%
  ggplot(aes(x = month, y = avgWage)) +
    geom_line() + 
    geom_point() +
    theme_bw()

ggsave("maxima.png")
