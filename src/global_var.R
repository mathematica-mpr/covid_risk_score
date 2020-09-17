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

# Risk adjustment to susceptibility for comorbidities

## UPDATEs to aOR June 2020
# For more info on assumptions and methodology, see 'doc/June2020_new_comorbidity_OR.xlsx'
# hospitalization aOR: Killerby et al
# death aOR: OPENSAFELY
#     Notes: "Other chronic conditions" remains the pre-June 2020 numbers and is only used if no other condition.
# ICU aOR: Simonnet et al

# The first is hospitalization odds ratio, second is ICU odds ratio, third is death odds ratio
# renal disease hosp aOR from Killerby figure, ICU crude OR from CDC MMWR 69(13), death aOR from OPENSAFELY
renal_or    = c(2.6, 5.82, 1.72) 
# heart disease hosp aOR from Killerby figure, ICU crude OR from CDC MMWR 69(13), death aOR from OPENSAFELY
cvd_or      = c(1.4, 4.88, 1.27)
# diabetes hosp aOR from Killerby text, ICU aOR from Simonnet, death aOR from OPENSAFELY
diabetes_or = c(3.1, 1.60, 1.79)
# hypertension hosp aOR from Killerby text, ICU aOR from Simonnet, death aOR from OPENSAFELY (0.95 brought up to 1)
hyper_or    = c(1.1, 2.29, 1)
# smoking hosp aOR from Killerby text, ICU crude OR from CDC MMWR 69(13), death aOR from OPENSAFELY
smoker_or   = c(2.3, 2.64, 1.12) 
# immuno disease hosp crude OR from CDC MMWR 69(13), ICU crude OR from CDC MMWR 69(13), death aOR from OPENSAFELY
immune_or   = c(2.58, 2.86, 1.69) 
# lung disease hosp aOR from Killerby figure (0.7 brought up to 1), ICU crude OR from CDC MMWR 69(13), death aOR from OPENSAFELY
lung_or     = c(1, 2.83, 1.78)
# obesity hosp aOR from Killerby text, ICU aOR from Simonnet, death aOR from OPENSAFELY
obesity_or  = c(1.9, 3.41, 1.46) 
# other hosp crude OR, ICU crude OR and death crude OR are from CDC MMWR 69(13)
other_or    = c(4.21, 3.33, 6.11) 


#Male aOR for hospitalization comes from Killerby et al, https://www.cdc.gov/mmwr/volumes/69/wr/mm6925e1.htm
#Male aOR for death comes from OPENSAFELY https://www.medrxiv.org/content/10.1101/2020.05.06.20092999v1.full.pdf
#Male aOR for ICU comes from Simonnet et al https://onlinelibrary.wiley.com/doi/full/10.1002/oby.22831?af=R
male_or     = c(2.4, 2.83, 1.99)

# Prob(ICU | hosp) for different comorbidities, see 'doc/June2020_new_comorbidity_OR.xlsx'
renal_cp = 0.37
cvd_cp = 0.35
diabetes_cp = 0.37
hyper_cp = 0.32 # hypertension is in other category
smoker_cp = 0.36
immune_cp = 0.39
lung_cp = 0.38
obesity_cp = 0.32 # obesity is in other category
other_cp = 0.32
none_cp = 0.25

#NYC county fips code
#NY_fips_ls<-c("36005", "36047", "36061", "36081", "36085")
KC_fips_ls <- c("29037", "29047","29095", "29165")

# urls class to store our urls
urls  = list(
  # CCDC
  ccdc_vol2_2020 = "https://www.unboundmedicine.com/medline/citation/32064853/[The_epidemiological_characteristics_of_an_outbreak_of_2019_novel_coronavirus_diseases__COVID_19__in_China]_",
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
  conjugate_priors = "https://en.wikipedia.org/wiki/Conjugate_prior"
)
