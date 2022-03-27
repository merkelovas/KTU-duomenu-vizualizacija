library(tidyverse)
library(dplyr)

sodra = read.csv("data/lab_sodra.csv", encoding = "UTF-8")
sodra = sodra[sodra$ecoActCode == 471100,]

#1
qplot(sodra$avgWage, geom="histogram")

#2
sodraTopFiveNames = 
  sodra[order(-sodra$avgWage), ] %>%
  distinct(sodra$name) %>% head(5)

vec = as.vector(sodraTopFiveNames$`sodra$name`)

sodra[sodra$name %in% vec,] %>%
  select(name, avgWage, month) %>%
  group_by(name) %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  geom_point(aes(colour = name)) +
  scale_x_continuous("Month",breaks=202101:202112,limits=c(202101,202112)) + 
  geom_line(aes(colour = name)) +
  theme_light() +
  labs(title = "Avarage wage by company", x = "Month", y = "Average wage")

#3
sodra[sodra$name %in% vec,] %>%
  arrange(desc(numInsured)) %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name, numInsured) %>%
  ggplot(aes(x= reorder(name, -numInsured), y=numInsured, group=name)) +
  geom_col(aes(fill = name)) +
  theme_light() + 
  labs(title = "Max insured employees by company", 
       x = "Company", y = "Insured")
  