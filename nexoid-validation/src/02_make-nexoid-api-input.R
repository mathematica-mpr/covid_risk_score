### This script prepares Nexoid data for input to the 19andme API.
### Data source:  https://www.covid19survivalcalculator.com/en/download
### The file has additional geographic information, added by Cindy Hu; zip5 and fips.
rm(list=ls())
library(tidyverse)
library(data.table)
library(stringr)
library(rlist)
library(purrr)
library(future.apply)

# Read data.
df <- data.frame(fread('data-wrangling-nexoid/nexoid_data_spatial.csv')) %>%
  filter(country=='US') %>%
  select(ID, survey_date, zip5, fips, age, sex,
         kidney_disease, heart_disease, diabetes, hypertension,
         compromised_immune, lung_disease, smoking, other_chronic, 
         bmi, covid19_symptoms, contacts_count, house_count, 
         rate_reducing_risk_single_washing_hands, rate_reducing_risk_single_sanitizer,
         rate_reducing_mask, bmi, blood_type, height, weight, insurance, income, race, immigrant,
         smoking, alcohol, cannabis, amphetamines, cocaine, lsd, public_transport_count, working, 
         asthma, hiv_positive, nursing_home, health_worker, prescription_medication, 
         risk_infection, risk_mortality) %>%
  rename(nexoid_risk_infection = risk_infection,
         nexoid_risk_mortality = risk_mortality)

head(df)
dim(df)

# Remove observations with no zip5 or fips.
df <- df %>% filter(!(is.na(zip5) & is.na(fips)))
sum(is.na(df$zip5))

#----------------------------------------------------------------
# Mapping each covariate to appropriate values for 19andme input.
#----------------------------------------------------------------

# Age.
df$nex_age = age
df$age = as.numeric(sub('_.*', '', df$age))
df$age = ifelse(df$age>80, 80, df$age)

# Sex.
df$sex = ifelse(df$sex=='other', 'sex_other', df$sex)

# Conditions.
df <- df %>%
  rename(is_renal = kidney_disease,
         is_diabetes = diabetes,
         is_cvd = heart_disease,
         is_lung = lung_disease, 
         is_hyper = hypertension,
         is_immune = compromised_immune,
         is_other = other_chronic)

# Smoker
df <- df %>% 
  rename(is_smoker = smoking) %>%
  mutate(is_smoker =  ifelse(is_smoker=="never" | is.na(is_smoker), 0, 1))

# Obesity
df <- df %>% 
  rename(is_obesity = bmi) %>%
  mutate(is_obesity = ifelse(is_obesity <= 30, 0, 1))

#-------------------------------------------------------------------------------------------
# Number of people you come in contact with.  This includes household members in 19andme,
# does not specify in Nexoid (but seems to imply you should include household members).
# We will take the max of contacts_count and house_count.
#-------------------------------------------------------------------------------------------

# house_count = 1 is 'I live alone'.  So house_count includes the respondent; we subtract one.
housecount_minus_respondent = df$house_count - 1

df$nppl = ifelse(is.na(df$contacts_count), housecount_minus_respondent,
                 ifelse(df$contacts_count < housecount_minus_respondent, housecount_minus_respondent, df$contacts_count))
xtabs(~nppl+house_count,df, addNA=T)

# Number of people your household members come into contact with.
# There isn't an exact match for this - nexoid has your direct contacts (contacts_count), and
# a house_count for the number of people you live with.
# We will conservatively say that each person you live with comes into contact with two outside people.
df$is_roommate = ifelse(df$house_count>1, 1, 0)

# Number of people your household members come into contact with.
# There isn't an exact match for this question - they have contacts_count and
# house_count.  We'll use house count, conservatively assuming that each person you
# live with comes into contact with two outside people.
df$nppl2 = (df$house_count-1) * 2 # Subtracting 1, since house_count=1 is living alone.

# Hand sanitizer. We'll say 1 if answer "most of the time" or "always" in Nexoid.
df$hand = ifelse(df$rate_reducing_risk_single_sanitizer>0 |
                   df$rate_reducing_risk_single_washing_hands>0,1,0)

# Mask use.  We'll say 1 if 4/5 in Nexoid.  
df$ppe = ifelse(df$rate_reducing_mask >=4, 1, 0)
df$ppe[which(is.na(df$ppe))] = 0

df <- df %>% select(-rate_reducing_mask, -rate_reducing_risk_single_sanitizer, 
                    -rate_reducing_risk_single_washing_hands, -house_count)

#----------------------------------------------------------------
# Concatenate individual conditions together into a single 'conditions' column. 
#----------------------------------------------------------------

df$conditions = ""
df$conditions = ifelse(df$is_renal==1, paste0(df$conditions, "is_renal"), df$conditions)
df$conditions = ifelse(df$is_diabetes==1, paste0(df$conditions, " is_diabetes"), df$conditions)
df$conditions = ifelse(df$is_hyper==1, paste0(df$conditions, " is_hyper"), df$conditions)
df$conditions = ifelse(df$is_cvd==1, paste0(df$conditions, " is_cvd"), df$conditions)
df$conditions = ifelse(df$is_lung==1, paste0(df$conditions, " is_lung"), df$conditions)
df$conditions = ifelse(df$is_immune==1, paste0(df$conditions, " is_immune"), df$conditions)
df$conditions = ifelse(df$is_other==1, paste0(df$conditions, " is_other"), df$conditions)
df$conditions = ifelse(df$is_smoker==1, paste0(df$conditions, " is_smoker"), df$conditions)
df$conditions = ifelse(df$is_obesity==1, paste0(df$conditions, " is_obesity"), df$conditions)

# Remove leading spaces.
df$conditions = trimws(df$conditions, which='left')

# Switch no conditions to NA (from blanks).
df$conditions[which(nchar(df$conditions)<1)] = NA
table(df$conditions)

#----------------------------------------------------------------
# Handle symptoms.
#----------------------------------------------------------------

# We have to do a little extra here; Nexoid only has an indicator for whether you are experiencing Covid symptoms.
# Specifically, they have a checkbox with this language:
# COVID-19 Symptoms
# Then sub-checkboxes for dry cough, fever, other symptoms.
# But they are only storing a 1/0 for covid_symptoms.
# 19andme has these symptoms: 
# Loss of smell/taste, cough, fatigue, loss of appetite, other symptoms.  We aren't doing anything with 'other', so will leave out.
#
# Approach:
# If covid_symptoms = 0, no 19andme symptoms.
# If covid_symptoms = 1, draw the number of symptoms from a Poisson(1).  Randomly select symptoms.
#----------------------------------------------------------------

df$symptoms = ifelse(df$covid19_symptoms==0, NA, "is_other")
table(df$symptoms, useNA='always') 

# symptoms = c("'is_loss_smell_taste'", "'is_cough'", "'is_fatigue'", "'is_skip_meal'")
# 
# num_symptoms = (rpois(nrow(df), 1)+1) * df$covid19_symptoms
# num_symptoms = ifelse(num_symptoms > length(symptoms), length(symptoms), num_symptoms)
# 
# sampleSymptoms = function(x){
#   symps = paste0('[', paste(sample(symptoms, size=c(x)), collapse=', '), ']')
#   return(symps)
# }
# 
# df$symptoms = ""
# idx = which(num_symptoms>0)
# 
# for(i in idx){
#   df$symptoms[i] = sampleSymptoms(num_symptoms[i])
# }
# 
# table(num_symptoms)
                                                                

#----------------------------------------------------------------
# Save needed covariates and export to csv file.
#----------------------------------------------------------------

# Remove records with no zip or fips.
df <- df %>% 
  rename(zip=zip5) %>%
  filter(!is.na(zip), !is.na(fips))

summary(df)

# Convert zip and fips to character values.
df$zip = str_pad(df$zip, width=5, side='left', pad='0')
df$fips = str_pad(df$fips, width=5, side='left', pad='0')

# Add "person" identifiers to each row.
df$person = paste0('person_',1:nrow(df))

# Select columns to keep for "clean" nexoid output file, to match with API inputs.
# Contains extra info for analysis that the API doesn't need.  Remove duplicated cols also.
df %>% fwrite('./data-wrangling-nexoid/nexoid_data_forapi.csv')


#----------------------------------------------------------------
# Convert to list for API input.
#----------------------------------------------------------------

# Utility function.
make_list = function(id){
  mylist = as.list(df[id,])
  
  # Use list if 2+ conditions.
  if(!is.na(mylist$conditions)){
    if(str_detect(mylist$conditions, " ")){
      mylist$conditions = unlist(str_split(mylist$conditions, ' '))
    }
  }

  return(mylist)
}


# Select columns for the API input file.
df <- df %>% select(zip, age, sex, 
                    conditions, symptoms,
                    nppl, is_roommate, nppl2, 
                    hand, ppe, fips, person) 

# Cast all numeric values as characters.
df <- df %>% mutate(
  age = as.character(age),
  nppl = as.character(nppl), 
  nppl2 = as.character(nppl2),
  is_roommate = as.character(is_roommate),
  hand = as.character(hand),
  ppe = as.character(ppe))

# Test list conversion.
test = df[1:15,]
my_test_list = future_lapply(rownames(test), function(x) make_list(x))
names(my_test_list) = paste0('person_',1:nrow(test))
length(my_test_list)
my_test_list$person_15
my_test_list$person_1$conditions

# Populate list.

# NEXT TIME NEED TO DO THIS, TRY:
plan(multiprocess)
my_list <- future_lapply(rownames(df), function(x) make_list(x))
names(my_list) = paste0('person_',1:nrow(df))

# # Correct symptoms:  other --> is_other.
# sympt_fix = function(x){ gsub("is_is_other", "is_other", gsub("other", "is_other", x))}
# my_list <- modify_depth(my_list, 2, sympt_fix)

saveRDS(my_list, file='./data-wrangling-nexoid/nexoid_data_forapi.RData')
