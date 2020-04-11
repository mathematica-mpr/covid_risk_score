library(shiny)
source("src/global_var.R")


risk2odds<-function(prob) {
  return (prob / (1 - prob))
}

odds2risk<-function(odds) {
  return (odds / (1 + odds))
}


calculateRisk <- function(input, county_data) {
  casecount<-county_data$casecount
  population<-county_data$population
  underreport_factor<-county_data$underreport_factor
  total_covid_count = casecount * underreport_factor
  
  #risk calculator
  if(input$is_sick){
    # if you're already sick with flu-like symptoms, your likelihood of having covid is P(C19) / (P(C19) + P(flu))
    total_covid_probability = total_covid_count / population
    exposure_risk = total_covid_probability / (total_covid_probability + prob_flu)
  } else {
    # ASSUMPTION: diagnosed cases are not active
    active_casecount = total_covid_count - casecount
    
    # ASSUMPTION: active community case count cannot be less than 10% of reported cases
    if (active_casecount < 0.1 * casecount) {
      active_casecount = 0.1 * casecount
    }
    prev_active<-active_casecount/population #prevalence of active cases
    exposure_risk <- 1-(1-prev_active*transmissibility_household)^(input$nppl+input$nppl2*transmissibility_household)
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
  hosp_prob = hosp_list[age_index]
  icu_prob = icu_list[age_index]
  death_prob = death_list[age_index]
  hosp_odds = risk2odds(hosp_prob)
  icu_odds = risk2odds(icu_prob)
  death_odds = risk2odds(death_prob)
  
  #If user input more than two comorbidities, only the first two are considered.
  for (condition_id in input$conditions[1:min(length(input$conditions), 2)]) {
    # remove "is_" prefix
    condition_root = substr(condition_id, 4, nchar(condition_id))
    hosp_odds = hosp_odds * eval(parse(text=paste0(condition_root, "_or[1]")))
    icu_odds = icu_odds * eval(parse(text=paste0(condition_root, "_or[2]")))
    death_odds = death_odds * eval(parse(text=paste0(condition_root, "_or[3]")))
  }
  if (input$gender == "male") {
    hosp_odds = hosp_odds * male_or[1] # should be hosp_odds(female) * male_or[1]
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
    tags$h3("Your Risk Score is")
  )
}

renderLocationHtml <- function(risk) {
  county_data = risk$county_data
  underreport_factor_string = formatNumber(county_data$underreport_factor, "x")
  div(
      title = "Location",
      tags$p(div('We found data from ', formatDynamicString(county_data$name), ' for your zip code.',
                 ' This county has ', formatDynamicString(format(county_data$casecount, big.mark=",")), ' cases out of a population of ', 
                 formatDynamicString(format(county_data$population, big.mark = ',')), " as of ", formatDynamicString(latest_day), 
                 ", and we estimated that your county under-reports by a factor of ", 
                 underreport_factor_string, '. '
      ))
    )
}

renderScoreHtml <- function(risk) {
  score<- risk$score
  score = max(score, 1)
  score = min(score, 100)
  
  tags$p(HTML(paste0(
    "Your risk score is ",
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
  risk_string = formatPercent(risk$exposure_risk)
  
  sickness_html = tags$p(HTML(paste0(
    "Your estimated probability of catching COVID-19 through community transmission is ", risk_string, '. ',
    "For comparison, ", prob_flu_string, ' of Americans catch the flu every week during flu season.')))
  
  if (is_sick == TRUE) {
    sickness_html = tags$p(HTML(paste0(
      "Since you're already sick, please immediately consult ", 
      tags$a("the CDC's instructions", href = urls$cdc_if_sick),
      ", or walk through their ",
      tags$a("self-checker", href = urls$cdc_chatbot),
      ". The probability that you could have COVID-19 is ", risk_string, '. ')))
  }
  return (sickness_html)
}

renderSusceptibilityHtml <- function(risk) {
  tags$p(HTML(paste0(
    "If you were to get sick from COVID-19, your risk of hospitalization is ", 
    formatPercent(risk$hosp_risk),
    ", your risk of requiring an ICU is ",
    formatPercent(risk$icu_risk),
    ", and your risk of dying is ",
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
      ". This has contributed to a ", prob_hand_string, "reduction of your risk to be exposed to COVID-19. "))
  } else{
    hand_html = HTML(paste0(
      "If you washed your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      ", your risk of being exposed to COVID-19 would drop by ", prob_hand_string, " . "))
  }
  
  if (ppe == TRUE){
    ppe_html = HTML(paste0(
      "Good to know you wear personal protection equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe),
      ". This has contributed to a ", prob_ppe_string, "reduction of your risk to be exposed to COVID-19. "))
  } else{
    ppe_html = HTML(paste0(
      "If you wear personal protection equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe),
      ", your risk of being exposed to COVID-19 would drop by ", prob_ppe_string, " . "))
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
