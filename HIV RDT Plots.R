#loading packages
library(srvyr) 
library(tidyverse)
library(haven)
library(table1)
library(dplyr)
library(labelled)
library(survey)
library(tidyr)
library(scales)

#loading datasets
NP.2015 <- readRDS("NP.2015.rds")
NP.2021 <- readRDS("NP.2021.rds")
NP.2015.survey <- readRDS("NP.2015.survey.rds")
NP.2021.survey <- readRDS("NP.2021.survey.rds")
NP.ALL <- readRDS("NP.ALL.rds")
NP.ALL.survey <- readRDS("NP.ALL.survey.rds")

###########################################
## Exploring Changes in HIV RDTs Over Time   
###########################################

#Tabulating unweighted proportions of facilities that offer hiv rapid tests, broken
#down by year and facility type
hiv.rdt.unweighted <- NP.ALL %>%
  group_by(v007, v000) %>%
  summarise(vt807 = mean(vt807, na.rm = TRUE))

print(hiv.rdt.unweighted)

#visualizing unweighted proportions offacilities offering hiv rapid tests
hiv.rdt.unweighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = vt807, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering HIV Rapid Diagnostic Tests (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1))

#Tabulating weighted proportions of facilities that offer hiv rapid tests, broken
#down by year and facility type
hiv.rdt.weighted <- svyby(~vt807, ~v000 + v007, NP.ALL.survey, svymean, na.rm = TRUE)

print(hiv.rdt.weighted)

#visualizing weighted proportions of facilities offering hiv rapid tests
hiv.rdt.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = vt807, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = vt807 - se, 
                    ymax = vt807 + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering HIV Rapid Diagnostic Tests (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1))