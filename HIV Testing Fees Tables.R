#loading packages
library(srvyr) 
library(tidyverse)
library(haven)
library(gtsummary)
library(dplyr)
library(labelled)
library(survey)
library(tidyr)
library(scales)
library(forcats)

#loading datasets
NP.2015 <- readRDS("NP.2015.rds")
NP.2021 <- readRDS("NP.2021.rds")
NP.2015.survey <- readRDS("NP.2015.survey.rds")
NP.2021.survey <- readRDS("NP.2021.survey.rds")
NP.ALL <- readRDS("NP.ALL.rds")
NP.ALL.survey <- readRDS("NP.ALL.survey.rds")

########################################################
## Facilities that Offer HIV Testing vs Those that Don't   
########################################################

#creating table for 2015
hiv.testing.2015 <- NP.ALL.survey %>%
  filter(v000 == "NP7", v042 !=7
         ) %>%
  mutate(
    v042 = as_factor(v042) %>% fct_drop(),
    v007 = as_factor(v007),
    v146a = as_factor(v146a),
    v146b = as_factor(v146b),
    v146e = as_factor(v146e),
    v146f = as_factor(v146f),
    v146x = as_factor(v146x),
    v173a = as_factor(v173a),
  ) %>%
  tbl_svysummary(include = c(v007,v146a,v146b,v146e,v146f, v146x, v173a),
           by = v042, percent = "row", missing = "no")  %>%
add_p() %>%
  modify_header(label ~ "**HIV Testing Data Present & Completed**")


#creating table for 2021
hiv.testing.2021 <- NP.ALL.survey %>%
  filter(v000 == "NP8",v042 != 7) %>%
  mutate(
    v042 = as_factor(v042) %>% fct_drop(),
    v007 = as_factor(v007),
    v146a = as_factor(v146a),
    v146b = as_factor(v146b),
    v146e = as_factor(v146e),
    v146f = as_factor(v146f),
    v146x = as_factor(v146x),
    v173a = as_factor(v173a),
  ) %>%
  tbl_svysummary(include = c(v007,v146a,v146b,v146e,v146f, v146x, v173a),
                                   by = v042, percent = "row", missing = "no") %>%
  add_p() %>%
  modify_header(label ~ "**HIV Testing Data Present & Completed**")

#merging tables
tbl_merge(list(hiv.testing.2015, hiv.testing.2021), 
          tab_spanner = c("2015 Survey", "2021 Survey"))

##########################################################
## Facilities Charging for HIV Testing vs Those that Don't   
##########################################################

#creating a variable to determine which facilities charge fees for hiv testing
NP.ALL.survey <- NP.ALL.survey %>%
  mutate(hiv_testing_fee = case_when(
    v144 == 0 | v144 == 1 ~ 0,
    v144 == 2 ~ 1
  )) %>%
  mutate(
    hiv_testing_fee = labelled::labelled(
      hiv_testing_fee,
      c("no fee for hiv testing" = 0, "charges fee for hiv testing" = 1)
    )
  )

#creating table for 2015
hiv.testing.fee.2015 <- NP.ALL.survey %>%
  filter(v000 == "NP7", v042 == 1, !is.na(hiv_testing_fee), 
         !is.na(v146a),
         !is.na(v146b),
         !is.na(v146e),
         !is.na(v146f),
         !is.na(v146x))  %>%
  mutate(
    hiv_testing_fee = as_factor(hiv_testing_fee), 
    v007 = as_factor(v007),
    v146a = as_factor(v146a),
    v146b = as_factor(v146b),
    v146e = as_factor(v146e),
    v146f = as_factor(v146f),
    v146x = as_factor(v146x),
    v173a = as_factor(v173a),
    vt807 = as_factor(vt807),
    vt808 = as_factor(vt808),
    vt809 = as_factor(vt809),
    vt810 = as_factor(vt810),
  )  %>%
  tbl_svysummary(include = c(v007,v146a,v146b,v146e,v146f, v146x, v173a, vt807, vt808, vt809, vt810),
                 by = hiv_testing_fee, percent = "row", missing = "no") %>%
  add_p() %>%
  modify_header(label ~ "**Does Facility Charge Fees for HIV Testing**")


#creating table for 2021
hiv.testing.fee.2021 <- NP.ALL.survey %>%
  filter(v000 == "NP8", v042 == 1,!is.na(hiv_testing_fee), 
         !is.na(v146a),
         !is.na(v146b),
         !is.na(v146e),
         !is.na(v146f),
         !is.na(v146x)) %>%
  mutate(
    hiv_testing_fee = as_factor(hiv_testing_fee) %>% fct_drop(),
    v007 = as_factor(v007),
    v146a = as_factor(v146a),
    v146b = as_factor(v146b),
    v146e = as_factor(v146e),
    v146f = as_factor(v146f),
    v146x = as_factor(v146x),
    v173a = as_factor(v173a),
    vt807 = as_factor(vt807),
    vt808 = as_factor(vt808),
    vt809 = as_factor(vt809),
    vt810 = as_factor(vt810),
    ) %>%
  tbl_svysummary(include = c(v007,v146a,v146b,v146e,v146f, v146x, v173a, vt807, vt808, vt809, vt810),
                 by = hiv_testing_fee, percent = "row", missing = "no") %>%
  add_p() %>%
  modify_header(label ~ "**Does Facility Charge Fees for HIV Testing**")

#merging tables
tbl_merge(list(hiv.testing.fee.2015, hiv.testing.fee.2021), 
          tab_spanner = c("2015 Survey", "2021 Survey"))
