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
library(patchwork)

#loading datasets
NP.2015 <- readRDS("NP.2015.rds")
NP.2021 <- readRDS("NP.2021.rds")
NP.2015.survey <- readRDS("NP.2015.survey.rds")
NP.2021.survey <- readRDS("NP.2021.survey.rds")
NP.ALL <- readRDS("NP.ALL.rds")
NP.ALL.survey <- readRDS("NP.ALL.survey.rds")

###############################################################
#Plotting Partner Notification Strategies in Ethiopia, 2022
###############################################################

#opening dataset
ET.2022 <-read_dta("~/Downloads/NEPAL DATASETS/ETFC81DTSR/ETFC81FLSR.DTA")

#removing facilities that did not complete the survey 
ET.2022 <- ET.2022 %>%
  filter(v010a == 1)

#recoding Ethiopia facility types

#taking survey design into account
ET.2022.survey <- ET.2022 |>
  mutate(v005 = v005/1000000) |> 
  as_survey_design(weights = v005,
                   ids = inv_id)

#creating simplified partner notification variable
ET.2022.survey <- ET.2022.survey %>%
  mutate(partner_notification = case_when(
    v642a == 8 ~ 0,
    v642a == 0 ~ 1,
    v642a == 3 ~ 2,
    v642a == 1 | v642a == 2 | v642a == 4 ~ 3
  )) %>%
  mutate(
    partner_notification = labelled::labelled(
      partner_notification,
      c("no STI service" = 0, "no partner notification" = 1, "yes, only passive" = 2,
        "yes, some active" = 3)
    ))

#Visualizing weighted proportions of health facilities that perform partner
#notification strategies for STIs in 2022
ethiopia.partner.notif<- ET.2022.survey %>%
  filter(v016 == 1) %>%
  group_by(v007,partner_notification) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot(aes(x = as_factor(v007), y = percent, fill = as_factor(partner_notification))) +
  geom_bar(stat = "identity", position = "fill")  +
  labs(fill = "Partner Notification Strategy", 
       y = str_wrap("Proportion of Facilities Performing Partner Notifications for STIs (Weighted %)", 
                    width = 50), x = "Facility Type", title = "Ethiopia, 2022 Survey") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = percent_format()) 


###############################################################
#Plotting Partner Notification Strategies in Nepal, 2021
###############################################################

#creating simplified partner notification variable
NP.ALL.survey <- NP.ALL.survey %>%
  mutate(partner_notification = case_when(
    v642a == 8 ~ 0,
    v642a == 0 ~ 1,
    v642a == 3 ~ 2,
    v642a == 1 | v642a == 2 | v642a == 4 ~ 3
  )) %>%
  mutate(
    partner_notification = labelled::labelled(
      partner_notification,
      c("no STI service" = 0, "no partner notification" = 1, "yes, only passive" = 2,
        "yes, some active" = 3)
  ))

#Visualizing weighted proportions of health facilities that perform partner
#notification strategies for STIs in 2021
nepal.partner.notif <- NP.ALL.survey %>%
  filter(v016 == 1, v000 == "NP8") %>%
  group_by(v007,partner_notification,v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot(aes(x = as_factor(v007), y = percent, fill = as_factor(partner_notification))) +
  geom_bar(stat = "identity", position = "fill")  +
  labs(fill = "Partner Notification Strategy", 
       y = str_wrap("Proportion of Facilities Performing Partner Notifications for STIs (Weighted %)", 
                    width = 50), x = "Facility Type", title = "Nepal, 2021 Survey") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = percent_format()) + guides(fill = "none")

################
#Combining Plots
################

print(nepal.partner.notif + ethiopia.partner.notif)
