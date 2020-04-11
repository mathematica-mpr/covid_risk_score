# Global variables can go here
#Flu data from CDC https://www.cdc.gov/flu/about/burden/index.html
prob_flu<- 35520883/(329.45*10^6)/26 #assume 26 weeks of flu season
hosp_flu<-490561/35520883
icu_flu<-0.075*hosp_flu #Beumer, M. C., et al. "Influenza virus and factors that are associated with ICU admission, pulmonary co-infections and ICU mortality." Journal of critical care 50 (2019): 59-65.
death_flu<-34157/35520883
  

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
renal_or    = c(10.17, 5.82, 6.11) # death OR not available in CCDC
cvd_or      = c(6.60, 4.88, 7.43)
diabetes_or = c(5.00, 4.57, 4.70)
hyper_or    = c(5.00, 4.57, 4.70) # use the same as diabetes
smoker_or   = c(2.67, 2.64, 6.11) # death OR not available in CCDC
immune_or   = c(2.58, 2.86, 6.11) # death OR not available in CCDC
lung_or     = c(2.53, 2.83, 3.59)
other_or    = c(4.21, 3.33, 6.11) # death OR not available in CCDC


#pregnant_or = c(1.23, 0.42, 1)
#neuro_or    = c(6.18, 2.30, 6.11) # death OR not available in CCDC
#liver_or    = c(2.44, 3.05, 6.11) # death OR not available in CCDC
#all_conditions_death_or = 27.84
# OR source: https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1
male_or     = c(1.8518, 1.85, 1.69)

#NYC county fips code
NY_fips_ls<-c("36005", "36047", "36061", "36081", "36085")

# urls class to store our urls
urls  = list(
  # CCDC
  ccdc_vol2_2020 = "https://www.unboundmedicine.com/medline/citation/32064853/[The_epidemiological_characteristics_of_an_outbreak_of_2019_novel_coronavirus_diseases__COVID_19__in_China]_",
  # CDC
  cdc_chatbot = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/index.html#cdc-chat-bot-open",
  cdc_get_ready = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/get-your-household-ready-for-COVID-19.html",
  cdc_hand_hygiene = "https://www.cdc.gov/handhygiene/providers/guideline.html",
  cdc_high_risk = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-at-higher-risk.html",
  cdc_if_sick = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html",
  cdc_mm6909e1 = "https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf",
  cdc_mm6912e2 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm",
  cdc_mm6913e2 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6913e2.htm",
  cdc_ppe = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/respirator-use-faq.html",
  cdc_prevention = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html",
  cdc_symptoms = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html",
  cdc_flu = "https://www.cdc.gov/flu/about/burden/index.html",
  # NYT
  nytimes_data_article = "https://www.nytimes.com/article/coronavirus-county-data-us.html",
  # papers
  caramelo_etal_2020 = "https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1",
  russel_etal_2020 = "https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html",
  verity_etal_2020 = "https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext",
  # social
  twitter_button = "https://twitter.com/intent/tweet?text=Find%20your%20COVID-19%20risk%20score!&url=https://19andme.shinyapps.io/covid_risk_score/",
  twitter_widget = "http://platform.twitter.com/widgets.js",
  facebook_button = "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2F19andme.shinyapps.io%2Fcovid_risk_score%2F",
  facebook_widget = "https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.0"
)
