#loading packages
library(srvyr) 
library(tidyverse)
library(haven)
library(table1)
library(dplyr)
library(labelled)
library(survey)

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

#storing & displaying table
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

