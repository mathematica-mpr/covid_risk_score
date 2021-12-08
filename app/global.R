# libraries 
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyBS)
library(assertr)
library(flexdashboard)
library(httr)
library(tidyverse)
library(lubridate)

# helper functions
source("info_html.R")
source("functions.R")


# Global variables can go here

# urls class to store our urls
urls  = list(
  # Mathematica COVID Score API
  covid_score_api = "https://us-api.covid19.mathematica.org/score",
  covid_score_api_dev =  "https://awsdev.us-api.covid19.mathematica.org/score",
  # CCDC
  ccdc_vol2_2020 = "https://www.unboundmedicine.com/medline/citation/32064853/[The_epidemiological_characteristics_of_an_outbreak_of_2019_novel_coronavirus_diseases__COVID_19__in_China]",
  # CDC
  cdc_chatbot = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/index.html#cdc-chat-bot-open",
  cdc_get_ready = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/get-your-household-ready-for-COVID-19.html",
  cdc_hand_hygiene = "https://www.cdc.gov/handwashing/when-how-handwashing.html",
  cdc_high_risk = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-at-higher-risk.html",
  cdc_if_sick = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html",
  cdc_mm6909e1 = "https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf",
  cdc_mm6912e2 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm",
  cdc_mm6913e2 = "https://www.cdc.gov/mmwr/volumes/70/wr/mm6913e2.htm",
  cdc_post_covid_conditions = "https://www.cdc.gov/coronavirus/2019-ncov/long-term-effects.html",
  cdc_ppe = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/cloth-face-cover-guidance.html",
  cdc_prevention = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html",
  cdc_symptoms = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html",
  cdc_flu = "https://www.cdc.gov/flu/about/burden/index.html",
  cdc_hosp_June2020 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6925e1.htm",
  cdc_pregnancy = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-with-medical-conditions.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fneed-extra-precautions%2Fgroups-at-higher-risk.html#pregnancy",
  cdc_covidnet = "https://gis.cdc.gov/grasp/COVIDNet/COVID19_5.html",
  cdc_medicalconditions = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-with-medical-conditions.html",
  cdc_test_info = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/testing.html",
  cdc_vaccines = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/index.html",
  cdc_vaccinated_guidance = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/fully-vaccinated-guidance.html",
  cdc_case_surv = "https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/data",
  cdc_vax_science_brief = "https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/fully-vaccinated-people.html",
  cdc_delta_variant = "https://www.cdc.gov/coronavirus/2019-ncov/variants/delta-variant.html",
  # USA Facts
  usafacts_data = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/",
  # papers
  caramelo_etal_2020 = "https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1",
  russel_etal_2020 = "https://cmmid.github.io/topics/covid19/global_cfr_estimates.html",
  verity_etal_2020 = "https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext",
  open_safely = "https://www.nature.com/articles/s41586-020-2521-4:",
  jin_etal_2020 = "https://www.nature.com/articles/s41591-020-01191-8",
  wolfer_etall_2020 = "https://www.nature.com/articles/s41586-020-2196-x",
  covid_symptom_study = "https://covid19.joinzoe.com/us/about", 
  menni_etall_2020 = "https://www.nature.com/articles/s41591-020-0916-2",
  simonnet_etall_2020 = "https://onlinelibrary.wiley.com/doi/full/10.1002/oby.22831?af=R", 
  chu_etal_2020 = "https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)31142-9/fulltext",
  jefferson_etal_2008 = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2190272/",
  ihme_ifr = "http://www.healthdata.org/sites/default/files/files/Projects/COVID/briefing_US_20201112.pdf",
  # misc
  empirical_bayes = "https://en.wikipedia.org/wiki/Empirical_Bayes_method",
  # Additional sources for ORs December 2020 update
  gottlieb = "https://onlinelibrary.wiley.com/doi/full/10.1111/acem.14104",
  fairhealth = "https://www.prnewswire.com/news-releases/new-fair-health-study-uncovers-relationship-between-covid-19-comorbidities-and-mortality-301171033.html",
  zambrano = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6944e3.htm",
  dun = "https://www.medrxiv.org/content/10.1101/2020.10.27.20220970v2",
  # vaccine efficacy data
  polack_etal_2020 = "https://www.nejm.org/doi/full/10.1056/NEJMoa2034577?query=RP", #pfizer vaccine 1st dose
  moderna_fda_2020 = "https://www.fda.gov/media/144452/download", #moderna vaccine efficacy report
  moderna_eua_2020 = "https://www.fda.gov/media/144637/download", #moderna vaccine factsheet for hcp
  pfizer_eua_2020 = "https://www.fda.gov/media/144413/download", #pfizer vaccine factsheet for hcp
  jandj_eua_2021 = "https://www.fda.gov/media/146217/download",
  bernal_etal_2021 = "https://www.bmj.com/content/373/bmj.n1088", #pfizer and astrazeneca vax against hosp and death
  bernal_etal_2021b = "https://www.medrxiv.org/content/10.1101/2021.05.22.21257658v1", #pfizer and astrazeneca vax against delta variant
  voysey_etal_2021 = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3777268", # astrazeneca vaccine efficacy against symptomatic infections
  # Activity risk level
  bellage_activity_chart = "https://bellage.org/wp-content/uploads/2020/09/BellAge-COVID19-Activity-Risk-Chart.pdf",
  # excerise level and severe covid
  sallis_etal_2021 = "https://www.bmj.com/company/newsroom/physical-inactivity-linked-to-more-severe-covid-19-infection-and-death"
)

# possible input conditions
conditions_list = c("Chronic renal disease" = "renal_disease",
                    "Cardiovascular disease" = "cardiovascular_disease",
                    "Diabetes" = "diabetes",
                    "Hypertension" = "hypertension",
                    "Current or former smoker" = "smoking",
                    "Immunocompromised condition" = "immunocompromised",
                    "Chronic lung disease" = "lung_disease",
                    "Obesity (BMI &ge; 30 kg/m&sup2;)" = "obesity",
                    "Pregnancy" = "pregnancy",
                    "Cancer" = "cancer",
                    "Sickle cell disease" = "sickle_cell",
                    "Down syndrome" = "downsyndrome",
                    "Other chronic condition" = "other")

# activities list  
l_activities_list = c("Indoor socially distanced activities (e.g. grocery store, library, museum)",
                      "Outdoor socially distanced activities")
m_activities_list = c("In person work or school",
                      "Indoor busy or crowed activities (e.g. casino, shopping mall, salon)",
                      "Public transportation/airplane",
                      "Overnight stay at a hotel", 
                      "Outdoor dining",
                      "Seeing doctor or dentist")
h_activities_list = c("Indoor restaurant or bar", 
                      "Nightclub/music concert/movie theater/gym",
                      "Sports stadium",
                      "Religious services (including weddings and funerals)", 
                      "Team sports (e.g. basketball, football)", 
                      "Visiting nursing home or hospital")

# possible exercise_levels
exercise_level_list = c("10 minutes or less" = "lte_10mpw",
                    "11-149 minutes" = "btw_11_149mpw",
                    "150 minutes or more" = "gte_150mpw")
# vaccine doses
vaccine_labels = list(pfizer = "Pfizer-BioNTech",
                      moderna = "Moderna",
                      johnsonandjohnson = "Johnson & Johnson",
                      astrazeneca = "AstraZeneca")

months_last_vaccination_labels = c("Within one month" = "lt_1mo", "1-6 months ago" = "1_6mo", "More than 6 months ago" = "gt_6mo")
