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

###############
## 2015 Dataset  
############### 

#opening 2015 dataset
NP.2015 <-read_dta("~/Downloads/NEPAL DATASETS/Facility Recode/NPFC71DTSR/NPFC71FLSR.DTA")

#removing facilities that did not complete the survey 
NP.2015 <- NP.2015 %>%
  filter(v010a == 1)

#recoding sub regions into three categories 
NP.2015 <- NP.2015 %>%
  mutate(v001 = case_when(
    v001 <= 3 ~ 1,
    v001 >= 4 & v001 <= 8 ~ 2,
    v001 > 8 ~ 3))

#labelling variable & values
NP.2015$v001 <- labelled::labelled(
  NP.2015$v001,
  c("mountain" = 1, "hill" = 2, "terai" = 3)
)

var_label(NP.2015$v001) <- "ecological region"

#recoding managing authority into two categories
NP.2015 <- NP.2015 %>%
  mutate(v008 = ifelse(v008 == 1,1,0))

#labelling variable & values
NP.2015$v008 <- labelled::labelled(
  NP.2015$v008,
  c("private" = 0, "public" = 1)
)

var_label(NP.2015$v008) <- "managing authority"

#recoding facility types to match report table 
NP.2015 <- NP.2015 %>%
  mutate(v007 = case_when(
    v007 == 1 | v007 == 2 | v007 == 3 | v007 == 4 | v007 == 12 | v007 == 13 ~ 1,
    v007 == 5 | v007 == 14 ~ 2,
    v007 == 6 ~ 3,
    v007 == 7 ~ 4,
    v007 == 8 | v007 == 9 ~ 5,
    v007 == 10 ~ 6,
    v007 == 11 ~ 7))

#labelling variable & values
NP.2015$v007 <- labelled::labelled(
  NP.2015$v007,
  c("zonal and above hospitals" = 1, "district-level hospitals" = 2, 
    "private hospitals" = 3, "PHCCs" = 4, "HPs" = 5, "UHCs" = 6,
    "stand-alone HTCs" = 7)
)

var_label(NP.2015$v007) <- "facility type"

#creating a categorical variable for earthquake affected areas
NP.2015 <- NP.2015 %>%
  mutate(earthquake = ifelse(
    v002 %in% c(12, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 36),1,0))

#labelling variable & values
NP.2015$earthquake <- labelled::labelled(
  NP.2015$earthquake,
  c("unaffected" = 0, "affected" = 1)
)

var_label(NP.2015$earthquake) <- "earthquake-affected districts"

#displaying table
table1(~as_factor(v007) + as_factor(v001) + as_factor(v008) + 
         as_factor(earthquake), data=NP.2015)

#taking survey design into account
NP.2015.survey <- NP.2015 |>
  mutate(v005 = v005/1000000) |> 
  as_survey_design(weights = v005,
                   ids = inv_id)

#tabulating weighted counts & percentages

#by facility type
as.data.frame(round(svytable(~ as_factor(v007), design = NP.2015.survey)))
svymean(~as_factor(v007), design = NP.2015.survey)

#by ecological region
as.data.frame(round(svytable(~ as_factor(v001), design = NP.2015.survey)))
svymean(~as_factor(v001), design = NP.2015.survey)

#by managing authority
as.data.frame(round(svytable(~ as_factor(v008), design = NP.2015.survey)))
svymean(~as_factor(v008), design = NP.2015.survey)

#by earthquake affected districts
as.data.frame(round(svytable(~ as_factor(earthquake), design = NP.2015.survey)))
svymean(~as_factor(earthquake), design = NP.2015.survey)

#creating HIV testing variable
NP.2015.survey <- NP.2015.survey %>%
  update(hiv_testing = ifelse(vt811 == 1,1,0))

#weighted percentage and number of facilities with hiv testing system by facility type
svyby(~hiv_testing, ~as_factor(v007), NP.2015.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v007), NP.2015.survey, svymean)

#weighted percentage and number of facilities with hiv testing system by ecological region
svyby(~hiv_testing, ~as_factor(v001), NP.2015.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v001), NP.2015.survey, svymean)

#weighted percentage and number of facilities with hiv testing system by managing authority
svyby(~hiv_testing, ~as_factor(v008), NP.2015.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v008), NP.2015.survey, svymean)

#weighted percentage and number of facilities with hiv testing system by earthquake affected districts
svyby(~hiv_testing, ~as_factor(earthquake), NP.2015.survey, svytotal)
svyby(~hiv_testing, ~as_factor(earthquake), NP.2015.survey, svymean)

###############
## 2021 Dataset  
############### 

#opening 2021 dataset
NP.2021 <-read_dta("~/Downloads/NEPAL DATASETS/Facility Recode/NPFC8ADTSR (1)/NPFC8AFLSR.DTA")

#removing facilities that did not complete the survey 
NP.2021 <- NP.2021 %>%
  filter(v010a == 1)

#recoding facility types to match report table 
NP.2021 <- NP.2021 %>%
  mutate(v007 = case_when(
    v007 == 1 | v007 == 2 | v007 == 11 ~ 1,
    v007 == 3 | v007 == 4 ~ 2,
    v007 == 5 ~ 3,
    v007 == 6 ~ 4,
    v007 == 7 ~ 5,
    v007 == 9 ~ 6,
    v007 == 8 ~ 7,
    v007 == 10 ~ 8))

#labelling variable & values
NP.2021$v007 <- labelled::labelled(
  NP.2021$v007,
  c("federal/provincial-level hospitals" = 1, "local-level hospitals" = 2, 
    "private hospitals" = 3, "PHCCs" = 4, "HPs" = 5, "UHCs" = 6,
    "CHUs" = 7, "stand-alone HTCs" = 8)
)

var_label(NP.2021$v007) <- "facility type"

#recoding managing authority into two categories
NP.2021 <- NP.2021 %>%
  mutate(v008 = ifelse(v008 == 1,1,0))

#labelling variable & values
NP.2021$v008 <- labelled::labelled(
  NP.2021$v008,
  c("private" = 0, "public" = 1)
)

var_label(NP.2021$v008) <- "managing authority"

#tabulating unweighted counts
table1(~as_factor(v007) + as_factor(v008) + as_factor(v003) + 
         as_factor(v001), data=NP.2021)

#taking survey design into account
NP.2021.survey <- NP.2021 |>
  mutate(v005 = v005/1000000) |> 
  as_survey_design(weights = v005,
                   ids = inv_id)

#by facility type
as.data.frame(round(svytable(~ as_factor(v007), design = NP.2021.survey)))
svymean(~as_factor(v007), design = NP.2021.survey)

#by managing authority
as.data.frame(round(svytable(~ as_factor(v008), design = NP.2021.survey)))
svymean(~as_factor(v008), design = NP.2021.survey)

#by location
as.data.frame(round(svytable(~ as_factor(v003), design = NP.2021.survey)))
svymean(~as_factor(v003), design = NP.2021.survey)

#by province
as.data.frame(round(svytable(~ as_factor(v001), design = NP.2021.survey)))
svymean(~as_factor(v001), design = NP.2021.survey)

#creating HIV testing variable
NP.2021.survey <- NP.2021.survey %>%
  update(hiv_testing = ifelse(vt811 == 1,1,0))

#weighted percentage and number of facilities with hiv testing system by facility type
svyby(~hiv_testing, ~as_factor(v007), NP.2021.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v007), NP.2021.survey, svymean)

#weighted percentage and number of facilities with hiv testing system by managing authority
svyby(~hiv_testing, ~as_factor(v008), NP.2021.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v008), NP.2021.survey, svymean)

#weighted percentage and number of facilities with hiv testing system by location
svyby(~hiv_testing, ~as_factor(v003), NP.2021.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v003), NP.2021.survey, svymean)

#weighted percentage and number of facilities with hiv testing system by province
svyby(~hiv_testing, ~as_factor(v001), NP.2021.survey, svytotal)
svyby(~hiv_testing, ~as_factor(v001), NP.2021.survey, svymean)

#####################
## Combining datasets   
#####################

#renaming province variable in 2021 dataset so it matches 2015 dataset
NP.2021 <- NP.2021 %>%
  rename(province = v001)


#recoding districts in 2021 dataset to match 2015 dataset
NP.2021 <- NP.2021 %>%
  mutate(v002 = case_when(
    v002 == 101 ~ 1, 
    v002 == 109 ~ 2,
    v002 == 110 ~ 3,
    v002 == 111 ~ 4,
    v002 == 112 ~ 5,
    v002 == 113 ~ 6,
    v002 == 107 ~ 7,
    v002 == 108 ~ 8,
    v002 == 102 ~ 9,
    v002 == 106 ~ 10,
    v002 == 103 ~ 11,
    v002 == 104 ~ 12,
    v002 == 105 ~ 13,
    v002 == 114 ~ 14,
    v002 == 201 ~ 15,
    v002 == 202 ~ 16,
    v002 == 203 ~ 17,
    v002 == 204 ~ 18,
    v002 == 205 ~ 19,
    v002 == 311 ~ 20,
    v002 == 310 ~ 21,
    v002 == 301 ~ 22,
    v002 == 302 ~ 23,
    v002 == 309 ~ 24, #kavrepalanchok -> kavre
    v002 == 308 ~ 25,
    v002 == 307 ~ 26,
    v002 == 306 ~ 27,
    v002 == 305 ~ 28,
    v002 == 303 ~ 29,
    v002 == 304 ~ 30,
    v002 == 312 ~ 31,
    v002 == 206 ~ 32,
    v002 == 207 ~ 33,
    v002 == 208 ~ 34,
    v002 == 313 ~ 35, #chitawan -> chitwan
    v002 == 401 ~ 36,
    v002 == 406 ~ 37,
    v002 == 407 ~ 38,
    v002 == 409 ~ 39,
    v002 == 405 ~ 40,
    v002 == 402 ~ 41,
    v002 == 403 ~ 42,
    v002 == 404 ~ 43,
    v002 == 410 ~ 44,
    v002 == 411 ~ 45,
    v002 == 504 ~ 46,
    v002 == 506 ~ 47,
    v002 == 408 | v002 == 507 ~ 48, #no east & west nawalparasi in 2015 data
    v002 == 508 ~ 49,
    v002 == 509 ~ 50,
    v002 == 505 ~ 51,
    v002 == 503 ~ 52,
    v002 == 502 ~ 53,
    v002 == 501 | v002 == 608 ~ 54, #no east & west rukum in 2015 data
    v002 == 609 ~ 55,
    v002 == 510 ~ 56,
    v002 == 511 ~ 57,
    v002 == 512 ~ 58, #bardiya -> bardia
    v002 == 610 ~ 59,
    v002 == 606 ~ 60,
    v002 == 607 ~ 61,
    v002 == 601 ~ 62,
    v002 == 604 ~ 63,
    v002 == 605 ~ 64,
    v002 == 602 ~ 65,
    v002 == 603 ~ 66,
    v002 == 701 ~ 67,
    v002 == 702 ~ 68,
    v002 == 707 ~ 69,
    v002 == 706 ~ 70,
    v002 == 708 ~ 71,
    v002 == 709 ~ 72,
    v002 == 705 ~ 73,
    v002 == 704 ~ 74,
    v002 == 703 ~ 75))

#copying the district labels from the 2015 dataset
#Note the following changes:
  #kavrepalanchok (2021 data) is now kavre (from 2015 data)
  #chitawan (2021 data) is now chitwan (from 2015 data)
  #bardiya (2021 data) is now bardia (from 2015 data)

val_labels(NP.2021$v002) <- val_labels(NP.2015$v002)
var_label(NP.2021$v002) <- "district (country-specific)" 

#recoding 2015 facility types  
NP.2015 <- NP.2015 %>%
  mutate(v007 = case_when(
    v007 == 1 | v007 == 2 ~ 1,
    v007 == 3 ~ 2,
    v007 == 4 | v007 == 5 | v007 == 6 ~ 3, #grouping PHCCs,HPs & UHCs as peripheral
    v007 == 7 ~ 4))

#changing 2015 facility type labels 
NP.2015$v007 <- labelled::labelled(
  NP.2015$v007,
  c("government hospitals" = 1, "private hospitals" = 2, 
    "peripheral facilities" = 3, "stand-alone HTCs" = 4))

var_label(NP.2015$v007) <- "facility type"

#recoding 2021 facility types  
NP.2021 <- NP.2021 %>%
  mutate(v007 = case_when(
    v007 == 1 | v007 == 2 ~ 1,
    v007 == 3 ~ 2,
    v007 == 4 | v007 == 5 | v007 == 6 | v007 == 7 ~ 3, #grouping PHCCs,HPs,UHCs & CHUs as peripheral
    v007 == 8 ~ 4))

#changing 2021 facility type labels
NP.2021$v007 <- labelled::labelled(
  NP.2021$v007,
  c("government hospitals" = 1, "private hospitals" = 2, 
    "peripheral facilities" = 3, "stand-alone HTCs" = 4))

var_label(NP.2021$v007) <- "facility type"

#merging dataframes
NP.ALL <- bind_rows(NP.2015, NP.2021)

#creating simplified partner notification variable
NP.ALL <- NP.ALL %>%
  mutate(partner_notification = case_when(
    v642a == 0 ~ 0,
    v642a == 3 ~ 1,
    v642a == 1 | v642a == 2 | v642a == 4 ~ 2
  ))

NP.ALL$partner_notification <- labelled::labelled(
  NP.ALL$partner_notification,
  c("no" = 0, "yes, only passive" = 1, "yes, active or passive" = 2))

#taking survey design into account
NP.ALL.survey <- NP.ALL |>
  mutate(v005 = v005/1000000) |> 
  as_survey_design(weights = v005,
                   ids = inv_id)

###########################################
## Exploring Changes in HIV RDTs Over Time   
###########################################

#Tabulating unweighted proportions of facilities that offer hiv rapid tests, broken
#down by year and facility type
hiv.rdt.unweighted <- NP.ALL %>%
  group_by(v007, v000) %>%
  summarise(percent_vt807 = mean(vt807 * 100, na.rm = TRUE))

print(hiv.rdt.unweighted)

#visualizing unweighted proportions offacilities offering hiv rapid tests
hiv.rdt.unweighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_vt807, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering HIV Rapid Diagnostic Tests (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hiv.rdt.unweighted$percent_vt807), by = 5))

#Tabulating weighted proportions of facilities that offer hiv rapid tests, broken
#down by year and facility type
hiv.rdt.weighted <- svyby(~vt807, ~v000 + v007, NP.ALL.survey, svymean, na.rm = TRUE)

#converting proportions to percentages
hiv.rdt.weighted <- hiv.rdt.weighted %>%
  mutate(vt807 = vt807 * 100) %>%
  mutate(se = se * 100) %>%
  rename(percent_vt807 = vt807)

print(hiv.rdt.weighted)

#visualizing weighted proportions of facilities offering hiv rapid tests
hiv.rdt.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_vt807, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = percent_vt807 - se, 
                    ymax = percent_vt807 + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering HIV Rapid Diagnostic Tests (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hiv.rdt.weighted$percent_vt807) + 
                                    max(hiv.rdt.weighted$se), by = 5))

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

###########################################
#Exploring Changes in HCV Testing Over Time
###########################################

#Tabulating unweighted proportions of facilities that offer hcv testing, broken
#down by year and facility type
hcv.unweighted <- NP.ALL %>%
  group_by(v007, v000) %>%
  summarise(percent_sf874d = mean(sf874d * 100, na.rm = TRUE))

print(hcv.unweighted)

#visualizing unweighted proportions offacilities offering hcv testing
hcv.unweighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_sf874d, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis C (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hcv.unweighted$percent_sf874d), by = 5))

#Tabulating weighted proportions of facilities that offer hcv testing, broken
#down by year and facility type
hcv.weighted <- svyby(~sf874d, ~v000 + v007, NP.ALL.survey, svymean, na.rm = TRUE)

#converting proportions to percentages
hcv.weighted <- hcv.weighted %>%
  mutate(sf874d = sf874d * 100) %>%
  mutate(se = se * 100) %>%
  rename(percent_sf874d = sf874d)

print(hcv.weighted)

#visualizing weighted proportions of facilities offering hcv testing
hcv.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_sf874d, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = percent_sf874d - se, 
                    ymax = percent_sf874d + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis C (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hcv.weighted$percent_sf874d) + 
                                    max(hcv.weighted$se), by = 5))


###########################################
#Exploring Changes in HBV Testing Over Time
###########################################

#Tabulating unweighted proportions of facilities that offer hbv testing, broken
#down by year and facility type
hbv.unweighted <- NP.ALL %>%
  group_by(v007, v000) %>%
  summarise(percent_sf874a = mean(sf874a * 100, na.rm = TRUE))

print(hbv.unweighted)

#visualizing unweighted proportions offacilities offering hbv testing
hbv.unweighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_sf874a, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis B (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hbv.unweighted$percent_sf874a), by = 5))

#Tabulating weighted proportions of facilities that offer hbv testing, broken
#down by year and facility type
hbv.weighted <- svyby(~sf874a, ~v000 + v007, NP.ALL.survey, svymean, na.rm = TRUE)

#converting proportions to percentages
hbv.weighted <- hbv.weighted %>%
  mutate(sf874a = sf874a * 100) %>%
  mutate(se = se * 100) %>%
  rename(percent_sf874a = sf874a)

print(hbv.weighted)

#visualizing weighted proportions of facilities offering hbv testing
hbv.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_sf874a, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = percent_sf874a - se, 
                    ymax = percent_sf874a + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Offering Any Test for Hepatitis B (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hbv.weighted$percent_sf874a) + 
                                    max(hbv.weighted$se), by = 5))

###############################################
#Plotting Changes in HIV Testing Fees Over Time
###############################################

#plotting unweighted proportions
NP.ALL %>%
  filter(v144o == 0 | v144o == 1) %>% #filtering out dk/no response/na option & missing values
  group_by(v007, v000) %>%
  summarise(percent_v144o = mean(v144o * 100, na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_v144o, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Charging Fees for HIV Diagnostic Testing (Unweighted %)", 
                    width = 50), x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(100), by = 5)) 


#Tabulating weighted proportions of facilities that charge for hiv testing, broken
#down by year and facility type
filtered.hiv.test.fee <- subset(NP.ALL.survey, v144o == 0 | v144o == 1)
hiv.test.fee.weighted <- svyby(~v144o, ~v000 + v007, filtered.hiv.test.fee, svymean, na.rm = TRUE)

#converting proportions to percentages
hiv.test.fee.weighted <- hiv.test.fee.weighted %>%
  mutate(v144o = v144o * 100) %>%
  mutate(se = se * 100) %>%
  rename(percent_v144o = v144o)

print(hiv.test.fee.weighted)

#visualizing weighted proportions of facilities charginf fees for hiv testing
hiv.test.fee.weighted %>% 
  ggplot() +
  geom_bar(aes(x = as_factor(v007), y = percent_v144o, fill = v000), 
           stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = as_factor(v007), ymin = percent_v144o - se, 
                    ymax = percent_v144o + se, group = v000),
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(fill = "Survey Year", 
       y = str_wrap("Proportion of Facilities Charging Fees for HIV Diagnostic Testing (Weighted %)", 
                    width = 50), 
       x = "Facility Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = seq(0, max(hiv.test.fee.weighted$percent_v144o) + 
                                    max(hiv.test.fee.weighted$se), by = 5)) 