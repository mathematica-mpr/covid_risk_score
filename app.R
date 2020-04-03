library(shiny)
library(shinythemes)
library(shinycssloaders)
source("src/helper_county.R")
# Global variables can go here
nppl <- 20
prob_flu<- 35.5/327.2/26 #assume 26 weeks of flu season

# susceptibility data for US, https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm
susceptibility_total_cases = 4226
total_hospitalized = 508
total_icu = 121
age_list =   c(0,     20,    45,    55,    65,    75,    85)
hosp_list =  c(2.05,  17.55, 24.75, 25.3,  36.05, 44.6,  50.8) / 100
icu_list =   c(0,     3.1,   7.9,   7.95,  13.45, 20.75, 17.65) / 100
death_list = c(0,     0.15,  0.65,  2.0,   3.8,   7.4,   18.85) / 100

# odds ratios, https://www.cdc.gov/mmwr/volumes/69/wr/mm6913e2.htm
diabetes_or = c(5.00, 4.57)
lung_or     = c(2.53, 2.83)
cvd_or      = c(6.60, 4.88)
immune_or   = c(2.58, 2.86)
renal_or    = c(10.17, 5.82)
pregnant_or = c(1.23, 0.42)
neuro_or    = c(6.18, 2.30)
liver_or    = c(2.44, 3.05)
other_or    = c(4.21, 3.33)
smoker_or   = c(2.67, 2.64)
all_conditions_death_or = 27.84
# OR source: https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1
male_or     = c(1.8518, 1.85)

css <- HTML(".html-widget.gauge svg {height: 66%; width: 66%; display: block; margin-left: auto;margin-right: auto; margin-bottom:-10%;}
            .irs-bar {background: #DF691A;}
            .irs-single {background: #DF691A;}
            .irs-bar-edge {background: #DF691A;}
            .irs-from {background: #DF691A;}
            .irs-to {background: #DF691A;}")
#yellow = #F0AD4E

# Define the UI
ui <- fluidPage(theme=shinytheme("superhero"),
  # google analytics tracking
  tags$head(includeHTML("google-analytics.html")),
  titlePanel("COVID-19 Risk Score Calculator"),
  tags$head(tags$style(css)),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("DISCLAIMER: this tool is NOT a qualified source of medical knowledge, NOR should it be used to inform policy decisions.", class = "text-danger"),
      helpText("Please answer a few questions to see your COVID-19 risk score.", class = "lead"),
      #textInput('fips', label =  '5-digit FIPS code of your county', fips),
      textInput('zip', label =  "What is your 5-digit zip code?"),
      textInput('age', label =  "What is your age?"),
      radioButtons('gender', "What is your gender?", c("Female" = "female", "Male" = "male")),
      sliderInput('nppl', 
                  'How many people do you see in person in a week?', 
                  min = 0, max = 100, value = nppl, step =1),
      #sliderInput('fac_underreport', "Choose what percentage of cases are tested?", min = 0.01, max = 1, value = 0.15, step = 0.01),
      checkboxInput('is_sick', 
                    HTML(paste0(
                      "I have ", 
                      tags$a(
                        "flu-like symptoms",
                        href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html"))
                      )),
      checkboxInput('has_preexisting', 
                    HTML(paste0(
                      "I have ", 
                      tags$a(
                        "underlying medical complications",
                        href = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-at-higher-risk.html")
                    ))),
      conditionalPanel(
        condition = "input.has_preexisting == true",
        checkboxInput('has_diabetes', "Diabetes"),
        checkboxInput('has_lung', "Chronic lung disease/moderate-to-severe asthma"),
        checkboxInput('has_cvd', "Cardiovascular disease"),
        checkboxInput('has_immune', "Immunocompromised condition"),
        checkboxInput('has_renal', "Chronic renal disease"),
        checkboxInput('is_pregnant', "Pregnancy"),
        checkboxInput('has_neuro', "Neurologic disorder"),
        checkboxInput('has_liver', "Chronic liver disease"),
        checkboxInput('has_other', "Other chronic disease"),
        checkboxInput('is_smoker', "Current or former smoker")
      ),
      actionButton('go', "Calculate", class = "btn-primary"),
      width = 3
    ),
    #OUTPUT
    mainPanel(
      tabsetPanel(
        tabPanel("Score",
                 fluidRow(withSpinner(gaugeOutput("gauge", height = '600%'), type = 1)),
                 fluidRow(column(9, offset = 1, htmlOutput("res")))
        ),
        #tabPanel("Map"),
        tabPanel("Method", htmlOutput("methods")),
        tabPanel("FAQ", htmlOutput("faq"))
      ),
      width = 9
    )
  )
)

# Define the server code
server <- function(input, output) {
  temp<- eventReactive(input$go, {
    #read in FIPS or get it from ZIP
    fips<-get_fips_from_zip(input$zip)
    validate(need(!is.na(fips), "Sorry, we don't have data for your region."))
    #get county-level characteristics
    county_pop <- get_county_pop(fips)
    county_name <- get_county_name(fips)
    county_casecount <- get_county_casecount(fips, latest_day)
    county_underreport <- calc_county_underreport(fips)
    
    unlist(list("fips" = fips,
                "county_pop" = county_pop,
                "county_name" = county_name,
                "county_casecount" = county_casecount,
                "county_underreport" = county_underreport
    ))
  })
  
  temp2<-reactive({
    temp<-temp()
    county_casecount<-temp['county_casecount']%>%as.numeric()
    county_pop<-temp['county_pop']%>%as.numeric()
    county_underreport<-temp['county_underreport']%>%as.numeric()
    total_covid_count = county_casecount/county_underreport
    if(input$is_sick){
      # if you're already sick with flu-like symptoms, your likelihood of having covid is P(C19) / (P(C19) + P(flu))
      total_covid_probability = total_covid_count / county_pop
      exposure_risk = total_covid_probability / (total_covid_probability + prob_flu)
    } else if (input$nppl>0) {
      # ASSUMPTION: diagnosed cases are not active
      active_casecount = total_covid_count - county_casecount
      # ASSUMPTION: active community case count cannot be less than 10% of reported cases
      if (active_casecount < 0.1 * county_casecount) {
        active_casecount = 0.1 * county_casecount
      }
      exposure_risk <- 1-(1-active_casecount/county_pop)^input$nppl
    } else{
      exposure_risk <- 0
    }
    
    # susceptibility calculations
    risk2odds<-function(prob) {
      return (prob / (1 - prob))
    }
    odds2risk<-function(odds) {
      return (odds / (1 + odds))
    }
    age = as.numeric(input$age)
    validate(need(age >= 0, "Invalid age."))
    age_index = max(which(age_list <= age))
    hosp_prob = hosp_list[age_index]
    icu_prob = icu_list[age_index]
    death_prob = death_list[age_index]
    hosp_odds = risk2odds(hosp_prob)
    icu_odds = risk2odds(icu_prob)
    death_odds = risk2odds(death_prob)
    
    if (input$has_diabetes) {
      hosp_odds = hosp_odds * diabetes_or[1]
      icu_odds = icu_odds * diabetes_or[2]
    }
    if (input$has_lung) {
      hosp_odds = hosp_odds * lung_or[1]
      icu_odds = icu_odds * lung_or[2]
    }
    if (input$has_cvd) {
      hosp_odds = hosp_odds * cvd_or[1]
      icu_odds = icu_odds * cvd_or[2]
    }
    if (input$has_immune) {
      hosp_odds = hosp_odds * immune_or[1]
      icu_odds = icu_odds * immune_or[2]
    }
    if (input$has_renal) {
      hosp_odds = hosp_odds * renal_or[1]
      icu_odds = icu_odds * renal_or[2]
    }
    if (input$is_pregnant) {
      hosp_odds = hosp_odds * pregnant_or[1]
      icu_odds = icu_odds * pregnant_or[2]
    }
    if (input$has_neuro) {
      hosp_odds = hosp_odds * neuro_or[1]
      icu_odds = icu_odds * neuro_or[2]
    }
    if (input$has_liver) {
      hosp_odds = hosp_odds * liver_or[1]
      icu_odds = icu_odds * liver_or[2]
    }
    if (input$has_other) {
      hosp_odds = hosp_odds * other_or[1]
      icu_odds = icu_odds * other_or[2]
    }
    if (input$is_smoker) {
      hosp_odds = hosp_odds * smoker_or[1]
      icu_odds = icu_odds * smoker_or[2]
    }
    if (input$gender == "male") {
      hosp_odds = hosp_odds * male_or[1]
      icu_odds = icu_odds * male_or[2]
    }
    
    if (input$has_preexisting) {
      death_odds = death_odds * all_conditions_death_or
    }

    hosp_risk = odds2risk(hosp_odds)
    icu_risk = odds2risk(icu_odds)
    death_risk = odds2risk(death_odds)

    g<-function(exposure, hospitalization, icu, death){
      x = exposure * (hospitalization + icu + death)
      # a mapping function to better visualize probability
      normalized<-log10(x/prob_flu)*20+50 
      # 50 means equal likelihood of flu
      # 0 means 1/10 probability of flu
      # 90 means 100 times probability of flu
      return(normalized)
    }
    score<-if_else(exposure_risk>0, g(exposure_risk, hosp_risk, icu_risk, death_risk), 1)
    unlist(list("exposure_risk" = exposure_risk,
                "hosp_risk" = hosp_risk,
                "icu_risk" = icu_risk,
                "death_risk" = death_risk,
                "score" = score))
  })
  
  output$gauge <-renderGauge({
    temp2<-temp2()
    score<-temp2['score']%>%as.numeric()
    gauge(case_when(score<1 ~ 1,
                score>100 ~ 100,
                TRUE ~round(score)), 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "")
  })
  
  output$res <-renderUI({
    temp <-temp()
    temp2 <- temp2()
    county_casecount<-temp['county_casecount']%>%as.numeric()
    county_pop<-temp['county_pop']%>%as.numeric()
    county_underreport<-temp['county_underreport']%>%as.numeric()
    exposure_risk<-temp2['exposure_risk']%>%as.numeric()
    
    formatDynamicString <- function(string) {
      return (tags$b(tags$span(style="color:#F0AD4E",string)))
    }
    formatNumber<-function(number, unit) {
      return (formatDynamicString(HTML(paste0(formatC(signif(number,digits=2), digits=2,format="fg"), unit))))
    }
    formatPercent<-function(probability) {
      return (formatNumber(100 * probability, "%"))
    }
    
    prob_flu_string = formatPercent(prob_flu)
    county_underreport_string = formatNumber(1/county_underreport, "x")
    risk_string = formatPercent(exposure_risk)

    sickness_html = tags$p(HTML(paste0(
      "Your estimated probability of COVID-19 exposure through community transmission is ", risk_string, '. ',
      "For comparison, ", prob_flu_string, ' of Americans catch the flu every week during flu season.')))
    
    score<- temp2['score']%>%as.numeric()
    score = max(score, 0)
    score = min(score, 100)
    score_string = tags$p(HTML(paste0(
      "Your risk score is ",
      formatDynamicString(round(score)), 
      case_when(
        score<30 ~ paste0(
          ", which is (relatively) safe. Even so, it's a good time to make sure that you're ",
          tags$a("prepared! ", href = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/get-your-household-ready-for-COVID-19.html")),
        score>70 ~ paste0(
          ", which is quite serious. Avoiding exposure, practicing good hygiene, and making sure you have ",
          tags$a("a plan in place ", href = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html"), 
          "are critically important for you."),
        TRUE ~ paste0(
          ". Please take the time to review ",
          tags$a("this page", href = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html"),
          " to make sure you're well prepared in the days to come.")
      )
    )))
    
    if (input$is_sick == TRUE) {
      sickness_html = tags$p(HTML(paste0(
        "Since you're already sick, please immediately consult ", 
        tags$a("the CDC's instructions",
          href = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html"),
        ". The probability that you could have COVID-19 is ", risk_string, '. ')))
    }
    
    tagList(
      tags$p(""),
      tags$p(HTML(paste0(
        'We found data from ', formatDynamicString(temp['county_name']), ' for your zip code.',
        ' This county has ', formatDynamicString(temp['county_casecount']%>%as.numeric()), ' cases out of a population of ', 
        formatDynamicString(format(county_pop%>%as.numeric(), big.mark = ',')), " as of ", formatDynamicString(latest_day), 
        ", and we estimated that your county's specific under-reporting factor is ", 
        county_underreport_string, '. '))),
      sickness_html,

      tags$p(HTML(paste0(
        "If you were to get sick from COVID-19, your risk of hospitalization is ", 
        formatPercent(temp2["hosp_risk"]),
        ", your risk of requiring an ICU is ",
        formatPercent(temp2["icu_risk"]),
        ", and your risk of dying is ",
        formatPercent(temp2["death_risk"]), "."
      ))),
      score_string,
    )
  })
  
  output$methods <-renderUI({
    tagList(
      tags$p(""),
      tags$p('Our "Risk Score" visualization is the quantity {Exposure * Susceptibility}, logarithmically scaled.'),
      tags$p("Exposure represents how likely it is that you've come into contact with the virus. You can help decrease this factor by ",
        tags$a("social distancing, practicing good hygiene, and closely following the directives of your local public health officials.",
               href = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html"),
        "Your personal susceptibility to COVID-19 is quantified by {P(hospitalization) + P(ICU) + P(death)}.",
        "Please remember that even if your personal susceptibility is low, you can still help by preventing spread to others."
      ),
      tags$p(""),
      tags$h3("Assumptions:"),
      tags$li(
        "To calculate exposure, we used ",
        tags$a("the New York Times's published data on COVID-19 cases & deaths", 
               href="https://www.nytimes.com/article/coronavirus-county-data-us.html"),
        "to estimate the prevalence of infected people within your county.",
      ),
      tags$li(
        "Due to rapid spread and insufficient testing during the COVID-19 pandemic, there are likely additional unreported cases beyond the officially reported cases.",
        "We followed methodology reported by",
        tags$a("Russell et al (2020)", href="https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html"),
        "to calculate the percentage of cases that are currently known, and presumably quarantined, versus the number of cases still distributed throughout the community."),
      tags$li("Other methods of becoming infected (e.g. touching an infected surface) are not accounted for by this calculator."),
      tags$li(
        "Estimations of US hospitalization, ICU and morbidity data by age were obtained from",
        tags$a("the CDC Morbidity and Mortality Weekly Report (MMWR), March 26th.", href = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm"),
      ),
      tags$li("Estimations of risk factors associated with underlying medical conditions were obtained from",
        tags$a("the CDC MMWR, March 31st,", href = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm"),
        "and gender from this preprint by", 
        tags$a("Caramelo et al (2020).", href = "https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1")
      ),
      tags$p(""),
      tags$p("We'll be doing our best to update these assumptions as additional knowledge about the virus becomes available."),
    )
  })
  
  faqQuestion<- function(string) {
    return (tags$li(tags$b(tags$span(style="color:#DF691A", string))))
  }
  output$faq <- renderUI({
    tagList(
      tags$h3("Frequently Asked Questions:"),
      faqQuestion("Why is my score so high?"),
      tags$p("We wanted our tool to be sensitive to the wide variety of circumstances encountered in the US right now;",
             "as a result, it's calibrated around a score of 50. A score of 50 is defined as an Exposure = 0.42% (the",
             "frequency by which the average American catches the flu in any given week), and whose susceptibility",
             "score is 100%. For every 10x change in (Exposure*Susceptibility), the score will change by 20.",
             "Thus, for two users, one with a score of 50, and one with a score of 90, the user with a score of 90 is",
             "either 100x more likely to have been exposed to COVID-19, or would be 100x more likely to experience a",
             "serious consequence (hospitalization, ICU admission, or death)."),
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
      faqQuestion("My hospitalization/ICU/death risk seems out of whack."),
      tags$p("A lot is still unknown about the disease, and data sets are sparse, so our susceptibility scores are",
             "good for ballpark estimates only. We'll update our tool with better numbers as they become available."),
      faqQuestion("I have suggestion X, or know of data set Y, or want feature Z..."),
      tags$p("Let us know at", tags$a("covid.risk.score@gmail.com", href="mailto:covid.risk.score@gmail.com"), "!")
    )
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)