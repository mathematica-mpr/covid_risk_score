library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyBS)
source("src/helper_county.R")
source("src/global_var.R")

# Define the UI
ui <- fluidPage(theme=shinytheme("superhero"),
  # google analytics tracking
  tags$head(includeHTML("google-analytics.html")),
  titlePanel("COVID-19 Risk Score Calculator"),
  includeCSS("src/style.css"),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("DISCLAIMER: this tool is NOT a qualified source of medical knowledge, NOR should it be used to inform policy decisions.", class = "text-danger"),
      helpText("Please answer a few questions to see your COVID-19 risk score.", class = "lead"),
      #textInput('fips', label =  '5-digit FIPS code of your county', fips),
      # collapsible UI to streamline input. Everything is open by default.
      bsCollapse(
        id = "collapse_main",
        multiple = TRUE,
        open = "Demographic Information",
        bsCollapsePanel(
          title = "Demographic Information",
          textInput('zip', label = "What is your 5-digit zip code?"),
          uiOutput("zipcontrol"),
          textInput('age', label = "What is your age?"),
          radioButtons('gender', "What is your gender?", c("Female" = "female", "Male" = "male"), inline=TRUE)
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "Pre-existing Conditions",
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
            checkboxGroupInput("conditions", "Conditions",
                               c("Diabetes" = "is_diabetes",
                                 "Hypertension" = "is_hyper",
                                 "Chronic lung disease or asthma" = "is_lung",
                                 "Cardiovascular disease" = "is_cvd",
                                 "Immunocompromised condition" = "is_immune",
                                 "Chronic renal disease" = "is_renal",
                                 "Other chronic disease" = "is_other",
                                 "Current or former smoker" = "is_smoker"
                               )),
          )
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "Behavioral Input",
          sliderInput('nppl', 
                      'How many people do you come into close contact with?', 
                      min = 0, max = 100, value = 0, step =1),
          checkboxInput('is_roommate', "I live with other people."),
          conditionalPanel(
            condition = "input.is_roommate == true",
            sliderInput('nppl2', 
                        'How many people do your other household members come into close contact with?', 
                        min = 0, max = 100, value = 0, step =1)),
          checkboxInput("hand", HTML(paste0(
            "I perform hand hygiene according to ",
            tags$a(
              "CDC guidance",
              href = "https://www.cdc.gov/handhygiene/providers/guideline.html")
          ))),
          checkboxInput("ppe", HTML(paste0(
            "I wear personal pertection equipment according to ",
            tags$a(
              "CDC recommendation",
              href = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/respirator-use-faq.html")
          )))
        ) # bsCollapsePanel
      ), # bsCollapse
      actionButton('go', "Calculate", class = "btn-primary"),
      width = 3
    ), # sidebarPanel
    # OUTPUT
    mainPanel(
      tabsetPanel(
        tabPanel("Score",
                 fluidRow(withSpinner(gaugeOutput("gauge", height = '600%'), type = 1)),
                 fluidRow(column(9, offset = 1, htmlOutput("res")))
        ),
        # tabPanel("Map"),
        tabPanel("Method", htmlOutput("methods")),
        tabPanel("FAQ", htmlOutput("faq"))
      ),
      width = 9
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage

# Define the server code
server <- function(input, output, session) {
  temp<- eventReactive(input$go, {
    fips<-get_fips_from_zip(input$zip)
    if(length(fips)  >1){
      output$zipcontrol <- renderUI({
        fips<-get_fips_from_zip(input$zip)
        fips_names<-lapply(fips, get_county_name)%>%unlist()
        print(fips)
        radioButtons("fips",label = "Choose a county and resubmit", choiceNames = fips_names, choiceValues  = fips, selected = NULL)
       })
      fips<-input$fips
    }
    validate(need(!is.na(fips), "Check input!"))
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
    total_covid_count = county_casecount*county_underreport
    
    #risk calculator
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
      prev_active<-active_casecount/county_pop #prevalence of active cases
      exposure_risk <- 1-(1-prev_active*transmissibility)^(input$nppl+input$nppl2*transmissibility_household)
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
    
    # exposure modifier
    if(input$hand){
      exposure_risk<-odds2risk(risk2odds(exposure_risk)*hand_or)
    }
    
    if(input$ppe){
      exposure_risk<-odds2risk(risk2odds(exposure_risk)*ppe_or)
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
    
    for (condition_id in input$conditions) {
      # remove "is_" prefix
      condition_root = substr(condition_id, 4, nchar(condition_id))
      hosp_odds = hosp_odds * eval(parse(text=paste0(condition_root, "_or[1]")))
      icu_odds = icu_odds * eval(parse(text=paste0(condition_root, "_or[2]")))
      death_odds = death_odds * eval(parse(text=paste0(condition_root, "_or[3]")))
    }
    if (input$gender == "male") {
      hosp_odds = hosp_odds * male_or[1]
      icu_odds = icu_odds * male_or[2]
      death_odds = death_odds * male_or[3]
    }
    
    if (!input$has_preexisting) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "conditions", selected = character(0))
    }

    hosp_risk = odds2risk(hosp_odds)
    icu_risk = odds2risk(icu_odds)
    death_risk = odds2risk(death_odds)

    g<-function(exposure, hospitalization, icu, death){
      x = exposure * (hospitalization + icu + death) 
      
      # a mapping function to better visualize probability
      normalized<-log10(x/prob_flu)*50+100 
      print(paste0("risk score is ", normalized))
      # 100 means equal likelihood of flu
      # 50 means 1/10 probability of flu
      # 0 means 1/100 times probability of flu
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
    county_underreport_string = formatNumber(county_underreport, "x")
    risk_string = formatPercent(exposure_risk)

    sickness_html = tags$p(HTML(paste0(
      "Your estimated probability of COVID-19 exposure through community transmission is ", risk_string, '. ',
      "For comparison, ", prob_flu_string, ' of Americans catch the flu every week during flu season.')))
    
    score<- temp2['score']%>%as.numeric()
    score = max(score, 1)
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
        ", or walk through their ",
        tags$a("self-checker",
               href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/index.html#cdc-chat-bot-open"),
        ". The probability that you could have COVID-19 is ", risk_string, '. ')))
    }
    
    tagList(
      tags$p(""),
      tags$p(HTML(paste0(
        'We found data from ', formatDynamicString(temp['county_name']), ' for your zip code.',
        ' This county has ', formatDynamicString(format(county_casecount, big.mark=",")), ' cases out of a population of ', 
        formatDynamicString(format(county_pop, big.mark = ',')), " as of ", formatDynamicString(latest_day), 
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