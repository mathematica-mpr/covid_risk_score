library(shiny)
source("src/global_var.R")


risk2odds<-function(prob) {
  return (prob / (1 - prob))
}

odds2risk<-function(odds) {
  return (odds / (1 + odds))
}

logodds2risk <- function(logodds){
  return(exp(logodds)/(1 + exp(logodds)))
  }

calculateRisk <- function(input, county_data) {
  casecount_newer <- county_data$casecount_newer
  casecount_older<-county_data$casecount_older
  population<-county_data$population
  underreport_factor<-county_data$underreport_factor
  total_covid_count_newer = casecount_newer * underreport_factor
  total_covid_count = total_covid_count_newer + casecount_older
  #risk calculator
  if(input$is_sick){
    # if you're already sick with symptoms, odds of covid come from https://www.nature.com/articles/s41591-020-0916-2
    community_exposure_risk = total_covid_count / population
    # min input age is 18
    age <- as.numeric(input$age) %>% ifelse(.<18, 18,.)
    sex <- ifelse(input$gender == "male", 1, 0)
    sympt_covid_logodds <- (-1.32) - (0.01*age) + (0.44*sex) + (1.75*"is_loss_smell_taste" %in% input$symptoms)
    + (0.31*"is_cough" %in% input$symptoms) + (0.49*"is_fatigue" %in% input$symptoms) + (0.39*"is_skip_meal" %in% input$symptoms)
    sympt_odds <- risk2odds(logodds2risk(sympt_covid_logodds))
    
    exposure_risk <- odds2risk(risk2odds(community_exposure_risk)*sympt_odds)
    sympt_covid_risk <- odds2risk(sympt_odds)
  } else {
    # ASSUMPTION: diagnosed cases are not active and undiagnosed cases get better in 2 weeks
    active_casecount = total_covid_count_newer - casecount_newer
    
    # ASSUMPTION: active community case count cannot be less than 10% of reported cases
    if (active_casecount < 0.1 * casecount_newer) {
      active_casecount = 0.1 * casecount_newer
    }
    prev_active<-active_casecount/population #prevalence of active cases
    exposure_risk <- 1-(1-prev_active*transmissibility_household)^(input$nppl+input$nppl2*transmissibility_household)
    sympt_covid_risk <- 0
    
  } 
  
  # exposure modifier
  if(input$hand){
    exposure_risk<-odds2risk(risk2odds(exposure_risk)*hand_or)
  }
  
  if(input$ppe){
    exposure_risk<-odds2risk(risk2odds(exposure_risk)*ppe_or)
  }
  
  #susceptibility calculation
  age = as.numeric(input$age)
  age_index = max(which(age_list <= age))
  hosp_prob = hosp_list_female[age_index] # start with female and multiply up if user inputs male
  icu_prob = icu_list_female[age_index]
  death_prob = death_list_female[age_index]
  hosp_odds = risk2odds(hosp_prob)
  icu_odds = risk2odds(icu_prob)
  death_odds = risk2odds(death_prob)
  
  #If user input more than two comorbidities, only the first two are considered.
  if (length(input$conditions)>0){
    for (i in 1:length(input$conditions)) { # loop over indices so we can truncate at 2 for icu
      condition_id = input$conditions[i]
      # remove "is_" prefix
      condition_root = substr(condition_id, 4, nchar(condition_id))
      
      # hosp OR are mutually adjusted except for immuno and other - for these 2 only adjust if they are only condition
      if ( (!condition_root %in% c("immune","other")) | length(input$conditions)==1 ){
        hosp_odds = hosp_odds * eval(parse(text=paste0(condition_root, "_or[1]")))
      }
      # ICU OR are not mutually adjusted, so use first 2 only
      if (i<=2){
        icu_odds = icu_odds * eval(parse(text=paste0(condition_root, "_or[2]")))
      }
      # Death OR are mutually adjusted except for other - for this one only adjust if it is only condition
      if ( (!condition_root %in% c("other")) | length(input$conditions)==1 ){
        death_odds = death_odds * eval(parse(text=paste0(condition_root, "_or[3]")))
      }
    }
  }
  if (input$gender == "male") {
    hosp_odds = hosp_odds * male_or[1] # base odds are the female odds, multiplied by male_or if male
    icu_odds = icu_odds * male_or[2]
    death_odds = death_odds * male_or[3]
  }
  
  hosp_risk = odds2risk(hosp_odds)
  icu_risk = odds2risk(icu_odds)
  death_risk = odds2risk(death_odds)
  
  g<-function(exposure, hospitalization, icu, death){
    # hospitalization, icu, death are probability
    # exposure: the probability of exposure
    
    x = exposure * (hospitalization + icu + death) 
    x_flu = prob_flu * (hosp_flu + icu_flu + death_flu)
    # a mapping function to better visualize probability
    normalized<-log10(x/x_flu)*50/3+50
    # 50 means equal disease burden as flu
    # 100 means 1000 times worse than flu
    # 0 means 1/1000 times the disease burden of flu
    return(normalized)
  }
  score<-if_else(exposure_risk>0, g(exposure_risk, hosp_risk, icu_risk, death_risk), 1)
  return (list(county_data = county_data,
               sympt_covid_risk = sympt_covid_risk,
               exposure_risk = exposure_risk,
               hosp_risk = hosp_risk,
               icu_risk = icu_risk,
               death_risk = death_risk,
               score = score))
}

formatDynamicString <- function(string) {
  return (tags$b(tags$span(style="color:#F0AD4E",string)))
}
formatNumber<-function(number, unit) {
  return (formatDynamicString(HTML(paste0(formatC(signif(number,digits=2), digits=2,format="fg"), unit))))
}
formatPercent<-function(probability) {
  return (formatNumber(100 * probability, "%"))
}

renderOutputIntroHtml <- function() {
  tagList(
    tags$h3("The risk score for people with similar characteristics and behaviors as you is")
  )
}

renderLocationHtml <- function(risk) {
  county_data = risk$county_data
  underreport_factor_string = formatNumber(county_data$underreport_factor, "x")
  div(
    title = "Location",
    tags$p(div('We found data from ', formatDynamicString(county_data$name), ' for your zip code. As of ', 
               formatDynamicString(latest_day), ', this county has ', formatDynamicString(format(county_data$casecount, big.mark=",")), 
               ' total confirmed COVID-19. We estimated that  out of the total confirmed cases',
               formatDynamicString(format(round(county_data$casecount_newer + county_data$casecount_older), big.mark =",")), 
               'of people are still sick. Many people who contract COVID-19 are not tested, and therefore not reported. 
               We estimate that your county has an under-reporting factor of ', underreport_factor_string, 
               '. Accounting for the under-reporting factor and average lenght of sickness, we estimate there are ',
               formatDynamicString(format(round(county_data$casecount_newer*county_data$underreport_factor + county_data$casecount_older), big.mark =",")),
               ' sick people distributed through the county who are not officially reported.'
    ))
  )
}

renderScoreHtml <- function(risk) {
  score<- risk$score
  score = max(score, 1)
  score = min(score, 100)
  
  tags$p(HTML(paste0(
    "The risk score for people with similar characteristics and behaviors as you is ",
    formatDynamicString(round(score)), 
    case_when(
      score<30 ~ paste0(
        ", which is (relatively) safe. Even so, it's a good time to make sure that you're ",
        tags$a("prepared! ", href = urls$cdc_get_ready)),
      score>70 ~ paste0(
        ", which is quite serious. Avoiding exposure, practicing good hygiene, and making sure you have ",
        tags$a("a plan in place ", href = urls$cdc_prevention), 
        "are critically important for you."),
      TRUE ~ paste0(
        ". Please take the time to review ",
        tags$a("this page", href = urls$cdc_prevention),
        " to make sure you're well prepared in the days to come.")
    )
  )))
}

renderExposureHtml <- function(risk, is_sick) {
  prob_flu_string = formatPercent(prob_flu)
  risk_string = formatPercent(risk$sympt_covid_risk)
  
  sickness_html = tags$p(HTML(paste0(
    "Among people who are the same age, sex, and health status as you, and have behaviors and levels of interaction with others that are similar to yours, the estimated probability of catching COVID-19 through community transmission in a week is ", 
    risk_string, '. ',
    "For comparison, ", prob_flu_string, ' of Americans catch the flu every week during flu season.')))
  
  if (is_sick == TRUE) {
    sickness_html = tags$p(HTML(paste0(
      "Since you are experiencing symptoms correlated to COVID-19, please immediately consult ", 
      tags$a("the CDC's instructions", href = urls$cdc_if_sick),
      ", or walk through their ",
      tags$a("self-checker", href = urls$cdc_chatbot),
      ". The probability that you have symptomatic COVID-19 is ", risk_string, '. ')))
  }
  return (sickness_html)
}

renderSusceptibilityHtml <- function(risk) {
  tags$p(HTML(paste0(
    "Among people who are the same age, sex, and health status as you and get sick from COVID-19, the risk of hospitalization is ", 
    formatPercent(risk$hosp_risk),
    ", the risk of requiring an ICU is ",
    formatPercent(risk$icu_risk),
    ", and the risk of dying is ",
    formatPercent(risk$death_risk), "."
  )))
}

renderProtectionHtml <- function(risk, hand, ppe){
  
  risk_hand_0<-risk$exposure_risk
  risk_hand_1<-if_else(hand == TRUE,
                       # default is hand washing, H1 is no hand washing, divide OR
                       odds2risk(risk2odds(risk_hand_0)/hand_or),
                       # default is no hand washing, H1 is with hand washing, multiply OR
                       odds2risk(risk2odds(risk_hand_0)*hand_or))
  risk_hand_delta<-if_else(hand == TRUE,
                           # default is hand washing, H1 is no hand washing, H0/H1
                           abs(risk_hand_0/risk_hand_1-1),
                           # default is no hand washing, H1 is hand washing, H1/H0
                           abs(risk_hand_1/risk_hand_0-1))
  prob_hand_string<- formatPercent(risk_hand_delta)

  risk_ppe_0<-risk$exposure_risk
  risk_ppe_1<-if_else(ppe == TRUE,
                       # default is wearing PPE, H1 is no PPE, divide OR
                       odds2risk(risk2odds(risk_ppe_0)/ppe_or),
                       # default is no PPE, H1 is with PPE, multiply OR
                       odds2risk(risk2odds(risk_ppe_0)*ppe_or))
  risk_ppe_delta<-if_else(ppe == TRUE,
                          # default is hand washing, H1 is no hand washing, H0/H1
                          abs(risk_ppe_0/risk_ppe_1-1),
                          # default is no hand washing, H1 is hand washing, H1/H0
                          abs(risk_ppe_1/risk_ppe_0-1))
  prob_ppe_string<- formatPercent(risk_ppe_delta)
  
  
  if (hand == TRUE){
    hand_html = HTML(paste0(
      "Good to know you wash your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      ". In general, this lowers people's risk of being exposed to COVID-19 by ", prob_hand_string, " . "))
  } else{
    hand_html = HTML(paste0(
      "We recommend you wash your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      ". In general, this would lower people's risk of being exposed to COVID-19 by ", prob_hand_string, " . "))
  }
  
  if (ppe == TRUE){
    ppe_html = HTML(paste0(
      "Good to know you wear personal protection equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe),
      ". In general, this lowers people's risk of being exposed to COVID-19 by ", prob_ppe_string, " . "))
  } else{
    ppe_html = HTML(paste0(
      "We recommend you wear personal protection equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe),
      ". In general, this would lower people's risk of being exposed to COVID-19 by ", prob_ppe_string, " . "))
  }
  
  return(tags$p(hand_html, ppe_html))
}

renderResultsHtml <- function(risk, is_sick, hand, ppe) {
  
  # return
  tagList(
    tags$p(""),
    renderLocationHtml(risk),
    renderExposureHtml(risk, is_sick),
    renderSusceptibilityHtml(risk),
    renderProtectionHtml(risk, hand, ppe),
    renderScoreHtml(risk)
    # fluidRow(column(width = 4,
    #                 offset = 2,
    #                 tags$a(href=urls$facebook_button,
    #                        "Share on Facebook",
    #                        class="btn btn-facebook btn-block"),
    #                 includeScript(urls$facebook_widget)),
    #          # column(width = 3,
    #          #        offset = 1,
    #          #        tags$a(href=urls$w_button, "Share on WhatsApp", class="btn btn-success btn-block")
    #          #        ),
    #          column(width = 4,
    #                 offset = 2,
    #                 tags$a(href=urls$twitter_button, "Share on Twitter", class="btn btn-info btn-block"),
    #                 includeScript(urls$twitter_widget)))
    
  )
}
