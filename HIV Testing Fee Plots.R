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
library(sjlabelled)
library(ggmosaic)

#loading datasets
NP.2015 <- readRDS("NP.2015.rds")
NP.2021 <- readRDS("NP.2021.rds")
NP.2015.survey <- readRDS("NP.2015.survey.rds")
NP.2021.survey <- readRDS("NP.2021.survey.rds")
NP.ALL <- readRDS("NP.ALL.rds")
NP.ALL.survey <- readRDS("NP.ALL.survey.rds")

###############################################
#Plotting Changes in HIV Testing Fees Over Time
###############################################

#Tabulating weighted proportions of facilities that charge for hiv testing, broken
#down by year and facility type

#filtering out don't know/NA values & calculating proportions by facility type & year
recoded.hiv.test.fee <- NP.ALL.survey %>%
  mutate(hiv_diagnostic_test_fee = case_when(
    v144o == 0 ~ 0,
    v144o == 1 ~ 1,
    v144o == 7 | v144o == 8 | is.na(v144o) ~ 7
  )) %>%
  mutate(
    hiv_diagnostic_test_fee = labelled::labelled(
      hiv_diagnostic_test_fee,
      c("no fee for hiv testing" = 0, "charges fee for hiv testing" = 1, "does not offer hiv testing" = 7)
    )
  )

#visualizing weighted proportions of facilities charging fees for hiv testing
recoded.hiv.test.fee %>%
  group_by(v007,hiv_diagnostic_test_fee,v000) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot(aes(x = as_factor(v007), y = percent, fill = as_factor(hiv_diagnostic_test_fee))) +
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(. ~ as_factor(v000), labeller = labeller(v000 = function(x) str_wrap(x, width = 0.1))) +
  labs(fill = "Does Facility Charge for HIV Testing?", 
       y = str_wrap("Proportion of Facilities Charging Fees for HIV Testing (Weighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels = percent_format()) 

##################
#Old Code - Ignore
##################

###code used to create tables used in old plots
# hiv.test.fee <- svyby(~hiv_diagnostic_test_fee, ~v000 + v007, recoded.hiv.test.fee, svymean, na.rm = TRUE)
# 
# #calculating overall proportions by year
# overall.hiv.test.fee <- svyby(~hiv_diagnostic_test_fee, ~v000, recoded.hiv.test.fee, svymean, na.rm = TRUE) 
# overall.hiv.test.fee <- overall.hiv.test.fee %>% 
#   mutate(v007 = 0)
# 
# #adding overall proportions to facility-specific proportions
# weighted.hiv.test.fee <- rbind(hiv.test.fee, overall.hiv.test.fee)
# 
# #adding labels
# weighted.hiv.test.fee$v007 <- labelled::labelled(
#   weighted.hiv.test.fee$v007,
#   c("overall" = 0, "government hopsitals" = 1, "private hospitals" = 2, 
#     "peripheral facilities" = 3, "stand-alone HTCs" = 4)
# )

###bar plot
# weighted.hiv.test.fee %>% 
#   ggplot() +
#   geom_bar(aes(x = as_factor(v007), y = v144o, fill = v000), 
#            stat = "identity", position = position_dodge(width = 0.9)) +
#   geom_errorbar(aes(x = as_factor(v007), ymin = v144o - se, 
#                     ymax = v144o + se, group = v000),
#                 position = position_dodge(width = 0.9), width = 0.25) +
#   labs(fill = "Survey Year", 
#        y = str_wrap("Proportion of Facilities Charging Fees for HIV Diagnostic Testing (Weighted %)", 
#                     width = 50), 
#        x = "Facility Type") +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
#   scale_y_continuous(labels = label_percent(accuracy = 1),
#                      breaks = seq(0, 1, by = 0.1)) 


