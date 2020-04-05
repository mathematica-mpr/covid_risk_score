# Global variables can go here
prob_flu<- 35.5/327.2/26 #assume 26 weeks of flu season
fips<-""
#odds ratio for hand washing
hand_or<-0.45 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2190272/
#odds ratio for wearing PPE
ppe_or<-0.32 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2190272/
#transmissibility of regular close contact
transmissibility<-0.0045 #https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf
#transmissibility of household close contact
transmissibility_household<- 0.105 #https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf

# # susceptibility data for US, https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm
# susceptibility_total_cases = 4226
# total_hospitalized = 508
# total_icu = 121
# hosp_list =  c(2.05,  17.55, 24.75, 25.3,  36.05, 44.6,  50.8) / 100
# icu_list =   c(0,     3.1,   7.9,   7.95,  13.45, 20.75, 17.65) / 100
# infection hospitalization rate and fatality rate, https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext
age_list   = c(0,       10,      20,      30,      40,      50,      60,     70,     80)
hosp_list  = c(0,       0.0408,  1.04,    3.43,    4.25,    8.16,    11.8,   16.6,   18.4) / 100
#case to infection conversion 1.38% cfr, 0.657% infection fatality rate, use CDC ICU list
icu_list   = c(0,       0,       2,       2,       3.7,     5.05,    6.4,    9.3,    8.4) *.657/1.38 /100
death_list = c(0.00161, 0.00695, 0.0309,  0.0844,  0.161,   0.595,   1.93,   4.28,   7.8)  / 100

# odds ratios, https://www.cdc.gov/mmwr/volumes/69/wr/mm6913e2.htm
# CCDC weekly, 2020 Vol No.2
#The Epidemiological Characteristics of an Outbreak of 2019 Novel
#Coronavirus Diseases (COVID-19) — China, 2020
#The Novel Coronavirus Pneumonia Emergency Response Epidemiology Team
# first is hospitalization odds ratio, second is ICU odds ratio, third is death odds ratio
diabetes_or = c(5.00, 4.57, 4.70)
hyper_or    = c(5.00, 4.57, 4.70) # use the same as diabetes
lung_or     = c(2.53, 2.83, 3.59)
cvd_or      = c(6.60, 4.88, 7.43)
immune_or   = c(2.58, 2.86, 6.11) # death OR not available in CCDC
renal_or    = c(10.17, 5.82, 6.11) # death OR not available in CCDC
#pregnant_or = c(1.23, 0.42, 1)
#neuro_or    = c(6.18, 2.30, 6.11) # death OR not available in CCDC
#liver_or    = c(2.44, 3.05, 6.11) # death OR not available in CCDC
other_or    = c(4.21, 3.33, 6.11) # death OR not available in CCDC
smoker_or   = c(2.67, 2.64, 6.11) # death OR not available in CCDC
#all_conditions_death_or = 27.84
# OR source: https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1
male_or     = c(1.8518, 1.85, 1.69)
