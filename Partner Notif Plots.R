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

###############################################################
#Exploring Changes in Partner Notification Strategies Over Time
###############################################################

#Tabulating unweighted proportions of facilities that perform partner 
#notification strategies for STIs, broken down by facility type and type 
#of partner notification strategy

#government hospitals
NP.ALL %>%
  filter(v007 == 1, v016 == 1) %>%
  group_by(partner_notification,v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#private hospitals
NP.ALL %>%
  filter(v007 == 2, v016 == 1) %>%
  group_by(as_factor(v642a),v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#peripheral facilities
NP.ALL %>%
  filter(v007 == 3, v016 == 1) %>%
  group_by(as_factor(v642a),v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#stand-alone HTCs
NP.ALL %>%
  filter(v007 == 4, v016 == 1) %>%
  group_by(as_factor(v642a),v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#Visualizing unweighted proportions of health facilities that perform partner
#notification strategies for STIs
NP.ALL %>%
  filter(v016 == 1) %>%
  group_by(v007,partner_notification,v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot(aes(x = as_factor(v000), y = percent, fill = as_factor(partner_notification))) +
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(. ~ as_factor(v007), labeller = labeller(v007 = function(x) str_wrap(x, width = 0.1))) +
  labs(fill = "Partner Notification Strategy", 
       y = str_wrap("Proportion of Facilities Performing Partner Notifications for STIs (Unweighted %)", 
                    width = 50), x = "Survey Year") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = percent_format())

#Tabulating weighted proportions of facilities that perform partner 
#notification strategies for STIs, broken down by facility type and type 
#of partner notification strategy

#government hospitals
NP.ALL.survey %>%
  filter(v007 == 1, v016 == 1) %>%
  group_by(v000, partner_notification) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100) 

#private hospitals
NP.ALL.survey %>%
  filter(v007 == 2, v016 == 1) %>%
  group_by(as_factor(partner_notification),v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#peripheral facilities
NP.ALL.survey %>%
  filter(v007 == 3, v016 == 1) %>%
  group_by(as_factor(partner_notification),v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#stand-alone HTCs
NP.ALL.survey %>%
  filter(v007 == 4, v016 == 1) %>%
  group_by(as_factor(partner_notification),v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100)

#Visualizing weighted proportions of health facilities that perform partner
#notification strategies for STIs
NP.ALL.survey %>%
  filter(v016 == 1) %>%
  group_by(v007,partner_notification,v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot(aes(x = as_factor(v000), y = percent, fill = as_factor(partner_notification))) +
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(. ~ as_factor(v007), labeller = labeller(v007 = function(x) str_wrap(x, width = 0.1))) +
  labs(fill = "Partner Notification Strategy", 
       y = str_wrap("Proportion of Facilities Performing Partner Notifications for STIs (Weighted %)", 
                    width = 50), x = "Survey Year") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = percent_format()) 