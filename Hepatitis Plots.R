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
#Exploring Changes in HCV Testing Over Time
###########################################

#Tabulating unweighted proportions of facilities that offer hcv testing, broken
#down by year and facility type
hcv.unweighted <- NP.ALL %>%
  group_by(v007, v000) %>%
  summarise(sf874d = mean(sf874d, na.rm = TRUE))

print(hcv.unweighted)

#visualizing unweighted proportions offacilities offering hcv testing
hcv.unweighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = sf874d, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis C (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1))

#Tabulating weighted proportions of facilities that offer hcv testing, broken
#down by year and facility type
hcv.weighted <- svyby(~sf874d, ~v000 + v007, NP.ALL.survey, svymean, na.rm = TRUE)

print(hcv.weighted)

#visualizing weighted proportions of facilities offering hcv testing
hcv.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = sf874d, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = sf874d - se, 
                    ymax = sf874d + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis C (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1))

###########################################
#Exploring Changes in HBV Testing Over Time
###########################################

#Tabulating unweighted proportions of facilities that offer hbv testing, broken
#down by year and facility type
hbv.unweighted <- NP.ALL %>%
  group_by(v007, v000) %>%
  summarise(sf874a = mean(sf874a, na.rm = TRUE))

print(hbv.unweighted)

#visualizing unweighted proportions offacilities offering hbv testing
hbv.unweighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = sf874a, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis B (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1))

#Tabulating weighted proportions of facilities that offer hbv testing, broken
#down by year and facility type
hbv.weighted <- svyby(~sf874a, ~v000 + v007, NP.ALL.survey, svymean, na.rm = TRUE)

print(hbv.weighted)

#visualizing weighted proportions of facilities offering hbv testing
hbv.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = sf874a, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = sf874a - se, 
                    ymax = sf874a + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis B (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1))
