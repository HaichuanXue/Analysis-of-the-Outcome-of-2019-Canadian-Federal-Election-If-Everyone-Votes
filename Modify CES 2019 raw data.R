## author: 
##  - Haichuan Xue (1004070346)
## date: '2020-12-22'
## This R file is designed for cleaning the Campaign Period Survey (CPS) data 
## from the 2019 Canadian Election Study 

#### Workspace setup ####
library(haven)
library(tidyverse)

#devtools::install_github("hodgettsp/cesR")
library(cesR)

# get online survey of CES 2019
get_ces("ces2019_web")

ori_data <- labelled::to_factor(ces2019_web)



# Initially pull out the variables needed
# Keep all observations who are citizens, 18+, and responsed with a high quality
wanted_obs <- which(ori_data$cps19_inattentive == "Not inattentive" & 
                      ori_data$cps19_duplicates_flag == "Unique" &
                      ori_data$cps19_citizenship == "Canadian citizen" &
                      ori_data$cps19_age >= 18)
wanted_var <- c(5,8,9,10,22,197,282,251)
wanted_data <- ori_data[c(wanted_obs),wanted_var]

## re-assign variables
# education
wanted_data$cps19_education<-ifelse(wanted_data$cps19_education=="Some secondary/ high school"|
                                      wanted_data$cps19_education=="Completed elementary school"|
                                      wanted_data$cps19_education=="Some elementary school"|
                                      wanted_data$cps19_education=="No schooling",
                                    "Grade12&under",
                              ifelse(wanted_data$cps19_education=="Completed secondary/ high school"|
                                       wanted_data$cps19_education=="Some technical, community college, CEGEP, College Classique",
                                     "Highschool",
                                     ifelse(wanted_data$cps19_education=="Completed technical, community college, CEGEP, College Classique"|
                                              wanted_data$cps19_education=="Some university",
                                            "College/Associate Degree",
                                            ifelse(wanted_data$cps19_education=="Bachelor's degree",
                                                   "Bachelor&under(uni)",
                                                   ifelse(wanted_data$cps19_education=="Master's degree"|
                                                            wanted_data$cps19_education=="Professional degree or doctorate",
                                                                 "AboveBachelor(uni)","NA")))))


# birth country
wanted_data$cps19_bornin_canada <-ifelse(wanted_data$cps19_bornin_canada=="Yes",
                              "Yes",
                              ifelse(wanted_data$cps19_bornin_canada=="No", "No",
                                     "NA"))


# province
# remove (people are from Northwest Territories, Nunavut, or Yukon) (83 cases)                               
# length(which(wanted_data$cps19_province == "Northwest Territories"|
#               wanted_data$cps19_province == "Nunavut"|
#               wanted_data$cps19_province == "Yukon"))

omit_83cases <- which(wanted_data$cps19_province == "Northwest Territories"|
                        wanted_data$cps19_province == "Nunavut"|
                        wanted_data$cps19_province == "Yukon")
wanted_data <- wanted_data[-c(omit_83cases),]


# family income
# level 1: 1~60,000
# level 2: 60,001~110,000
# level 3: more than 110,000
wanted_data$cps19_income_cat <- ifelse(wanted_data$cps19_income_cat == "$1 to $30,000"|
                                         wanted_data$cps19_income_cat == "$30,001 to $60,000",
                                       "level 1",
                                       ifelse(wanted_data$cps19_income_cat == "$60,001 to $90,000"|
                                                wanted_data$cps19_income_cat == "$60,001 to $90,000"|
                                                wanted_data$cps19_income_cat == "$90,001 to $110,000",
                                              "level 2",
                                              ifelse(wanted_data$cps19_income_cat == "$110,001 to $150,000"|
                                                       wanted_data$cps19_income_cat == "$150,001 to $200,000"|
                                                       wanted_data$cps19_income_cat == "More than $200,000 ",
                                                     "level 3", "NA")))


# gender
# remove of "other" selection
wanted_data$cps19_gender <- ifelse(wanted_data$cps19_gender == "A man", "Male",
                                   ifelse(wanted_data$cps19_gender == "A woman","Female", "NA"))


# age
wanted_data$cps19_age <- ifelse(wanted_data$cps19_age < 30, "18-29",
                                ifelse(wanted_data$cps19_age >= 30 & wanted_data$cps19_age < 40, "30-39",
                                ifelse(wanted_data$cps19_age >= 40 & wanted_data$cps19_age < 50, "40-49",
                                ifelse(wanted_data$cps19_age >= 50 & wanted_data$cps19_age < 60, "50-59",
                                ifelse(wanted_data$cps19_age >= 60 & wanted_data$cps19_age < 70, "60-69",
                                ifelse(wanted_data$cps19_age >= 70 & wanted_data$cps19_age < 80, "70-79", "80+"))))))

# response variable
wanted_data$cps19_votechoice <- ifelse(wanted_data$cps19_votechoice == "Liberal Party","Liberal",
                                       ifelse(wanted_data$cps19_votechoice == "Conservative Party"|
                                              wanted_data$cps19_votechoice == "People's Party"|
                                              wanted_data$cps19_votechoice == "Green Party"|
                                              wanted_data$cps19_votechoice == "ndp","Others","NA"))




# rename variables
wanted_data <- wanted_data %>% 
  rename(age = cps19_age,
         income_family = cps19_income_cat,
         education = cps19_education,
         vote_Liberal = cps19_votechoice,
         sex = cps19_gender,
         province = cps19_province,
         birth_in_canada = cps19_bornin_canada)

# variables we need for analysis and omit missing values
wanted_data <- wanted_data[,c(2,7,4,6,3,8,5)]
wanted_data <- na.omit(wanted_data)
na_obs <- which(wanted_data$sex == "NA"|wanted_data$age == "NA"|
                  wanted_data$education == "NA"|wanted_data$birth_in_canada == "NA"|
                  wanted_data$province == "NA"|wanted_data$income_family == "NA"|
                  wanted_data$vote_Liberal == "NA")
wanted_data <- wanted_data[-c(na_obs),]

# create cells
wanted_data$cell <- paste(wanted_data$province,wanted_data$age, wanted_data$education,
                          wanted_data$income_family)

# Saving the survey/sample data as a csv file 

write_csv(wanted_data, "CES_2019_data.csv")







       