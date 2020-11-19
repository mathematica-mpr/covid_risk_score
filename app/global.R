# helper functions
source("info_html.R")
source("functions.R ")

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

# Global variables can go here

# urls class to store our urls
urls  = list(
  # Mathematica COVID Score API
  covid_score_api =  "api.covid19.mathematica.org/score",
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
  cdc_mm6913e2 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6913e2.htm",
  cdc_ppe = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/diy-cloth-face-coverings.html",
  cdc_prevention = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html",
  cdc_symptoms = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html",
  cdc_flu = "https://www.cdc.gov/flu/about/burden/index.html",
  cdc_hosp_June2020 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6925e1.htm",
  cdc_pregnancy = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-with-medical-conditions.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fneed-extra-precautions%2Fgroups-at-higher-risk.html#pregnancy",
  # USA Facts
  usafacts_data = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/",
  # papers
  caramelo_etal_2020 = "https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1",
  russel_etal_2020 = "https://cmmid.github.io/topics/covid19/global_cfr_estimates.html",
  verity_etal_2020 = "https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext",
  open_safely = "https://www.medrxiv.org/content/10.1101/2020.05.06.20092999v1",
  wolfer_etall_2020 = "https://www.nature.com/articles/s41586-020-2196-x",
  covid_symptom_study = "https://covid19.joinzoe.com/us/about", 
  menni_etall_2020 = "https://www.nature.com/articles/s41591-020-0916-2",
  simonnet_etall_2020 = "https://onlinelibrary.wiley.com/doi/full/10.1002/oby.22831?af=R", 
  # misc
  empirical_bayes = "https://en.wikipedia.org/wiki/Empirical_Bayes_method"
)

# possible input conditions
conditions_list = c("Chronic renal disease" = "renal_disease",
                    "Cardiovascular disease" = "cardiovascular_disease",
                    "Diabetes" = "diabetes",
                    "Hypertension" = "hypertension",
                    "Current or former smoker" = "smoking",
                    "Immunocompromised condition <sub class = 'text-info'>(the hospitalization risk for immune disease is slightly overestimated due to lack of mutually adjusted odds ratios)</sub>" = "immunocompromised",
                    "Chronic lung disease" = "lung_disease",
                    "Obesity (BMI &ge; 30 kg/m&sup2;)" = "obesity",
                    "My chronic condition is not listed <sub class = 'text-info'>(selecting this will unselect other conditions due to lack of mutually adjusted odds ratios)</sub>" = "other")