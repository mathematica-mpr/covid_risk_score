
# disclaimer popup
disclaimerpopupHTML <- function(){
  # return
  tagList(
    tags$p("This tool works best on Google Chrome and mobile.", class = "text-warning"),
    tags$p("We do not retain any information that you provide in connection with your use of the tool."), 
    tags$p("Your use of this tool is subject to these ", tags$a("Terms of Use.", href=urls$terms_of_use)),
    tags$p(style="color:#DF691A", "THE INFORMATION PROVIDED BY THIS TOOL IS NOT MEDICAL ADVICE AND CANNOT BE 
             USED TO DIAGNOSE OR TREAT ANY MEDICAL CONDITION.  See FAQ for more information.", class = "text-warning"),
    tags$p("The 19andMe app and API are no longer being updated as of March 31st, 2023. 
           For demonstration purposes, the tool returns risk scores as of April 6th, 2022.", class = "text-warning")
  )
}

# render the Methodology
renderMethodsHtml <- function() {
  # return
  tagList(
    tags$p(""),
    tags$p('Our "Risk Score" visualization is the quantity {Exposure * Susceptibility}, normalized by the average disease burden of flu for the average American, logarithmically scaled.'),
    tags$p('In the 2018-2019 flu season, the US had 35 million cases, 0.5 million hospitalizations, and almost 35,000 deaths (',
           tags$a('Source: CDC', href = urls$cdc_flu),
           ').'),
    tags$p("Exposure represents how likely it is that you've been infected with the virus. It's a function of the prevalence of active cases in your
             community and ",
           tags$a("transmissibility estimates.", href = urls$cdc_mm6909e1),
           "You can reduce your exposure by ",
           tags$a("social distancing, practicing good hygiene, and closely following the directives of your local public health officials.",
                  href = urls$cdc_prevention),
           "Your personal susceptibility to COVID-19 is quantified by {P(hospitalization) + P(ICU) + P(death)}.",
           "Please remember that even if your personal susceptibility is low, you can still help by preventing spread to others."
    ),
    tags$p(""),
    tags$h4("Exposure:"),
    tags$ol(
      tags$li(
        "To estimate the number of people in your county currently infected with COVID-19, we use the county-level daily case counts reported by ",
        tags$a("the USAFacts published data on COVID-19 cases & deaths ", href = urls$usafacts_data),
        " and the average length of sickness reported by ", 
        tags$a("Wolfel et al (2020)", href = urls$wolfer_etall_2020), " and the ",
        tags$a("COVID-19 Symptom Study. ", href = urls$covid_symptom_study), 
        "USAFacts reports all Kansas City cases under Jackson County, MO even though three other counties overlap Kansas City, ", 
        "so we report cases for all four of these counties aggregated together into 'Kansas City and surrounding counties'."),
      tags$li(
        "Due to rapid spread and insufficient testing during the COVID-19 pandemic, there are likely additional unreported cases beyond the 
        officially reported cases. We use the methodology reported by ",  tags$a("Russell et al (2020)", href = urls$russel_etal_2020), 
        "and the infection fatality rate used in the economic analysis by ",
        tags$a("Xue et al (2022)", href=urls$xue_etal_2022),
        "to estimate the number of cases in your county that were not reported."),
      
      tags$ul(tags$li("The methodology from ", tags$a("Russell et al (2020)", href = urls$russel_etal_2020),
                      " uses the county-level case fatality rate (CFR) to estimate the percentage of cases that are not reported. ",
                      "Because under-reporting has decreased since the beginning of the pandemic (in part due to increased access to testing), ",
                      " we use a 90-day moving window for cases and deaths to calculate the county-level CFR. ",
                      "Because the CFR is noisy for counties with few cases, we use an ", 
                      tags$a("empirical Bayes", href = urls$empirical_bayes), " adjustment to shrink the county CFR ",
                      "towards the state CFR.",
                      "The result is that for counties with many cases, we essentially use the county-level CFR, ",
                      "but for counties with only a few cases we use a CFR that is partway between the county and state CFRs. ")),
      tags$li(
        "Estimations of the probability of having COVID-19 given symptoms are calculated using a logistic regression model published on Nature Medicine developed by ",
        tags$a("Menni et al (2020).", href = urls$menni_etall_2020),
        "This is the largest study so far using self-reported symptoms of more than 2.6 million participants to predict probable infection of COVID-19. "
      ),
      tags$li("The effects of wearing masks and hand hygiene on reducing the spread of SARS-CoV-2 and similar respiratory viruses were obtained from two systematic review and meta-analysis studies: ",
              tags$a("Chu et al (2020) ", href = urls$chu_etal_2020),
              "and ",
              tags$a("Jefferson et al (2008). ", href = urls$jefferson_etal_2008),
              "Without randomized trials, these systematic appraisals of the current best available evidence are useful to inform interim guidance. "),
      tags$li("The efficacy data of the Pfizer-BioNTech, Moderna, Johnson and Johnson, and AstraZeneca COVID-19 vaccines against infection were obtained from 
              FDA Emergency Use Authorization fact sheets and peer-reviewed journal articles",
              tags$a("FDA (2020a)", href = urls$pfizer_eua_2020), ", ", tags$a("FDA (2020b)", href = urls$moderna_eua_2020), ", ",
              tags$a("FDA (2021)", href = urls$jandj_eua_2021), ", and ", tags$a("Voysey et al (2021)", href = urls$voysey_etal_2021),
              ". We adjusted these vaccine efficacy rates to account for the Delta variant by using 
               a recent large-scale study in the United Kingdom on vaccine efficacy the B.1.617.2 (Delta) variant completed by ",
              tags$a("Bernal et al (2021b)", href = urls$bernal_etal_2021b),
              ". Then we estimated vaccine effectiveness over time based on a systematic review and meta-regression completed by ", 
              tags$a("Feilkin et al (2021)", href = urls$feilkin_etal_2021), "."),
      tags$li("Activity risk levels for COVID-19 are based on a professional review panel completed by ", 
              tags$a("BellAge", href = urls$bellage_activity_chart), 
              ". To provide 19 and Me users with a general idea of how different activities affect risk of COVID-19 through community exposure,
               we grouped the activities into three categories -- high, medium, and low risk -- and assigned an equivalent
               number of direct contacts for each activity risk categories. 
               Activities in the low risk category have a value of 0.5 direct contacts, medium is
               1.5 direct contacts and high is 3 direct contacts. For each activity checked, the equivalent number of direct contacts is added to the number of direct 
               contacts used to compute the risk score. For example, if two low risk activities are selected, the 19 and Me calculator 
               adds 1 direct contact to the person's risk level." )
    ), # end of ol
    tags$h4("Susceptibility:"),
    tags$ol(
      tags$li(
        "Estimates of the probability of hospitalization, ICU admission, and death among all infections, stratified by age groups, were calculated using the ",
        tags$a("CDC's COVID-19 Case Surveillance Public Use Data. ", href = urls$cdc_case_surv), 
        "The calculation includes data from a recent 6-month period and ",
        "rates are adjusted downward to account for under-reporting of cases (the national estimate of the percentage of cases that go unreported ", 
        "for this time period was calculated using the same methodology describe in 'Exposure')."
      ),
      tags$li("Estimations of risk factors associated with sex and underlying medical conditions were obtained from multiple studies. ", 
              "Odds ratios are adjusted for age, sex, and other underlying conditions. ", 
              "When an odds ratio is below 1 with a confidence interval containing 1, we round up to 1 so that no chronic conditions will decrease the COVID-19 risk score",
              "When no odds ratio is available for a given condition and outcome, ",
              "we use the same odds ratio as for another outcome (ex. use the same odds ratio for hospitalization and ICU risk). ",
              "In selecting studies to include, we prioritize large, US-based studies that are peer-reviewed and published in distinguished journals like Lancet, Nature, NEJM. ",
              "There is a temporal aspect because the earliest studies are likely selected because it was the only one available at the time. ",
              "Later during our monthly review and update process, if the more recent studies are considerably different from the current parameter, we would update its value. ",
              tags$a("Gottlieb et al (2020) ", href = urls$gottlieb),
              "was selected for risk factors associated with hospitalizations and ICU admissions because it is a large Chicago-based study", 
              " that has accounted for the coexistence of multiple risk factors. ",
              tags$a("Zambrano et al (2020) ", href = urls$zambrano),
              "was selected because it was the most recent CDC MMWR that reports increased risks of severe illness associated with pregnancy.",
              tags$a("OpenSAFELY (2020)", href = urls$open_safely), " was a large UK study based on 17 million adults' primary care records. ",
              "The risk of mortality associated with various age groups in the US follows a comparable pattern to that reported by OpenSAFELY ", 
              tags$a("(Jin et al 2020). ", href = urls$jin_etal_2020),
              "The risk factors of mortality were also complemented by ",
              tags$a("FAIR Health, ", href = urls$fairhealth), 
              "and ",
              tags$a("CDC MMWR (2020). ", href = urls$cdc_mm6913e2),
              "We keep the list of comorbidities included in the app to be consistent with the CDC list of medical conditions that increase the risk of severe illness ",
              tags$a("(CDC 2021). ", href = urls$cdc_medicalconditions)
      ),
      tags$li("Odds ratios for hospitalization, ICU admission, and death associated with exercise level were obtained from ",
              tags$a("Sallis et al (2021)", href = urls$sallis_etal_2021), "."
      ),
      tags$li("Based on a recent large scale study in older adults in England ",
              tags$a("(Bernal et al 2021a), ", href = urls$bernal_etal_2021a),
              "we estimated the effectiveness of vaccine against emergency hospital admissions and mortality. 
              This is an imperfect proxy, and we will keep monitoring the literature and update the calculation as more data on other population segments become available. 
              Then we estimated vaccine effectiveness over time based on a systematic review and meta-regression completed by ", 
              tags$a("Feilkin et al (2021)", href = urls$feilkin_etal_2021), "."),
    ), #end of ol
    tags$br(),
    tags$p("If you have additional suggestions about the app, data sets, or features, Please let us know at", 
           tags$a("covid.risk.score@gmail.com", href="mailto:covid.risk.score@gmail.com"), 
           "or visit us on ", tags$a("GitHub", href="https://github.com/mathematica-mpr/covid_risk_score"))
  ) # end of ul
}

# helper function for rendering FAQ's
faqQuestion<- function(string) {
  return (tags$li(tags$b(tags$span(style="color:#DF691A", string))))
}

# render the FAQ's
renderFaqHtml <- function() {
  # return
  tagList(
    tags$h3("Frequently Asked Questions:"),
    faqQuestion("I understand that the information provided by the tool is not medical advice and cannot be 
                used to diagnose or treat any medical condition, so how should I best use the information provided 
                by the tool?"),
    tags$p("This tool provides you with an estimation of your personal susceptibility or risk of contracting 
           COVID-19 based on the information you input into the tool.  We believe that having this information 
           can help you make better decisions when going about your daily activities.  Whatever your personal 
           risk of contracting COVID-19 may be, you should always follow the ", 
           tags$a("CDC’s guidelines", href="https://www.cdc.gov/coronavirus/2019-ncov/communication/guidance-list.html?Sort=Date%3A%3Adesc"), 
           " and any other guidelines provided by your state or local public health officials.  It is also very important to remember that even 
           if your risk is low, following the ",  
           tags$a("CDC’s guidelines", href="https://www.cdc.gov/coronavirus/2019-ncov/communication/guidance-list.html?Sort=Date%3A%3Adesc"), 
           " will help prevent spreading COVID-19 to others."),
    faqQuestion("Are my data captured by the app?"),
    tags$p("No, we do not collect or store any data you put in. We want this app to be a tool that can serve you."),
    faqQuestion("Why is my score so high?"),
    tags$p("We wanted our tool to be sensitive to the wide variety of circumstances encountered in the US right now;",
           "as a result, it's calibrated around a score of 50. A score of 50 is defined as an equal disease burden as ",
           "the flu, estimated based on total number of flu cases, hospitalizations, ICU admission, and deaths in the ",
           "2018-2019 flu season. For every 10x change in (Exposure*Susceptibility), the score will change by 50/3.",
           "Thus, for two users, one with a score of 20, and one with a score of 70, the user with a score of 70 is",
           "1000x more likely to have been exposed to COVID-19 and experience a serious consequence (hospitalization, ",
           "ICU admission, or death)."),
    faqQuestion("My family is sheltering in place with me. Should I count them as exposure risks?"),
    tags$p("As long as your family has been sheltering in place with you, you should be able to think of your family",
           "as a single \"user\" of the tool. However, bear in mind that their exposure risks become yours, and vice",
           "versa."),
    faqQuestion("My county only has a few (tens, hundreds, thousands) of cases. Why is my exposure risk so high?"),
    tags$p("Probably the most difficult/controversial/inaccurate part of our calculator is our estimation of the",
           "underreporting factor, the factor we use to estimate the true, larger, community prevalence of COVID-19",
           "in your community. In some places, our tool may be overestimating this factor, and in some places, it",
           "may be underestimating. Even so, it's probably good enough to get you a ballpark estimate of your risk."),
    tags$p("If your community has seen a huge increase in testing, has a \"test positive\" rate < 5%, and if you",
           "feel like anyone that wants to be tested is being tested promptly, then I think there is reason to",
           "believe that the authorities are tracking most of the community cases of COVID-19 in your area.",
           "Unfortunately, that is not true of most of the US at present."),
    faqQuestion("My specific medical condition isn't listed. What do I do?"),
    tags$p("Try using \"other conditions\" to get a catch-all estimate of your susceptibility."),
    faqQuestion("How is my sex assigned at birth used in risk score calculations?"),
    tags$p("For exposure risk, the ", tags$a("Menni et al (2020)", href = urls$menni_etall_2020), " model included 
           self-reported 'sex at birth' as a binary independent variable with 1 indicative of male participants and 0 representing 
           females. Therefore for the app, if sex assigned at birth selected is 'Other' or 'Prefer not to say', 
           for the estimations of probability of symptomatic COVID-19, we code these inputs as having a 'sex at birth' equal to 0.5."),  
    tags$p("For susceptibility, we used the original data from ", tags$a("Verity et al (2020)", href = urls$verity_etal_2020), 
           " for people at different age groups. If the sex assigned at birth selected is 'Male' or 'Female', then we modify the estimates from ",
           tags$a("Verity et al (2020)", href = urls$verity_etal_2020),  "by male and female odds ratio from this preprint by ", 
           tags$a("Caramelo et al (2020)", href = urls$caramelo_etal_2020), 
           "and if the sex assigned at birth selected is 'Other' or 'Prefer not to say', then we do not modify the estimates." ),
    faqQuestion("Why is race not in your app?"),
    tags$p("While we acknowledge people from different race groups experience different levels of adverse health outcomes due to COVID-19",
           ", we think race is an 'indicator', not mechanistically causal. There are other exogenous variables that better explain the health outcomes, ",
           "such as access to health care, nutrition, residential condition, occupation, etc. We will try to incorporate these other features when data become available."),
    faqQuestion("When you report 'probability of catching COVID-19 through community transmission', over what period of time does this refer to? Is this XX% chance per day?"),
    tags$p("We calculate the probability of community transmission as a function of the number of close contacts in a week and",
           "prevalence in your local community, so this is a weekly probability."),
    faqQuestion("Why is pregnancy added to the list of underlying conditions"),
    tags$p("CDC recently revised its recommendations and started to suggest pregnancy increases the risks",
           "of severe COVID-19 illness. Therefore we revised the list of medical conditions. ",
           "Odds ratio related to pregnancy was obtained from ",
           tags$a("Zambrano et al (2020) .", href = urls$zambrano)),
    faqQuestion("Are vaccinated individuals less likely to transmit COVID-19 to others around them?"),
    tags$p("According to the ", tags$a("CDC (2021) , ", href = urls$cdc_vax_science_brief), " data from multiple studies in different countries suggest that people vaccinated with Pfizer-BioNTech COVID-19 vaccine
            who develop COVID-19 have a lower viral load than unvaccinated people. This observation may indicate reduced transmissibility", 
            "as viral load has been identified as a key driver of transmission. ", "However, infections in fully vaccinated persons are more commonly
           observed with the Delta variant than with other SARS-CoV-2 variants. Infections with the Delta variant in vaccinated persons potentially have reduced transmissibility than infections in unvaccinated persons", 
           "although additional studies are needed. "),
    faqQuestion("How are you accounting for the dominance of the Delta variant?"),
    tags$p("Delta variant has changed the pandemic in two ways: increased transmission and decreased vaccination effectiveness. ",
           "Therefore, we have updated the transmissibility parameter in the model to account for the more contagious Delta variant. ",
           tags$a("The CDC (2021)", href = urls$cdc_delta_variant), " estimates that the Delta variant is more than 2x as contagious as previous variants. ",
           "Vaccines in the US are still highly effective, including against the Delta variant, but they are not 100% effective. ",
           "We modified the algorithm to account for reduced vaccine protection against Delta variant infections, using the recent data from ",
           "a recent study from ", tags$a("Bernal et al (2021b) .", href = urls$bernal_etal_2021b)),
    faqQuestion("When was the most recent update to the app and what is new?"),
    tags$p("We are no longer updating the COVID-19 data behind this app nor the algorithm used for risk score estimation. ",
           "Visit the \"Change Log\" tab to see the past updates to the algorithm."),
    faqQuestion("Why is the tool calculating risk scores as of April 6th 2022?"),
    tags$p("On March 31, 2023, the 19andMe api was discontinued and is no longer being maintained.  ",
           "For demonstration purposes, the tool returns risk scores as of April 6th, 2022.  ", 
           "We choose the date April 6th 2022 for two reasons.  ",
           "First, at-home testing becomes widely available in the spring of 2022.  ",
           "Second, since spring of 2022 since the official case counts became less reliable.  ",
           "Missouri is the first state that stopped reporting case data to ", tags$a("USAFacts", href = urls$usafacts_data), ", and they stopped on April 6th, 2022.  ",
           "Therefore, freezing the tool as of April 6th 2022 will allow most users from anywhere in the country to test out the full functionality of the tool.  "),
    tags$p("The 19andMe team at ", tags$a("Mathematica", href = "https://www.mathematica.org/"), " thanks you for your support through using, testing, and improving the tool.  ", 
           "We hope the tool has helped you through the COVID-19 public health emergency by empowering you with up-to-date information and personalized risk scores.")
  )# end of tag list
}

# function makes CHANGELOG API call -------------------------------------------------
renderChangelogHtml <- function() {
  changelog_r <- GET(urls$covid_change_log_api, add_headers("x-api-key" = Sys.getenv("X_API_KEY")))
  changelog_md <- content(changelog_r, "text", encoding = "UTF-8") %>% gsub("\n###", "\n\n###", .) %>% gsub("##", "###", .)
  changelog_html <- HTML(markdown::markdownToHTML(text = changelog_md, fragment.only = T))
  return (changelog_html)
}

