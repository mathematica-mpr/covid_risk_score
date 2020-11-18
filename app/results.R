
risk2odds<-function(prob) {
  return (prob / (1 - prob))
}

odds2risk<-function(odds) {
  return (odds / (1 + odds))
}

bool2char <- function(bol){
  # convert boolean to char
  # bol : boolean
  stringr::str_to_sentence(bol)
}

calculateRisk <- function(input) {
  
  request_body <- list(
    "zip" = input$zip,
    "age"= as.numeric(input$age),
    "sex" = input$sex,
    "symptoms" = as.list(input$symptoms),
    "nppl" = as.numeric(input$nppl),
    "is_roommate"= bool2char(input$is_roommate),
    "nppl2" = as.numeric(input$nppl2),
    "hand"= bool2char(input$hand),
    "ppe"= bool2char(input$ppe),
    "conditions" = as.list(input$conditions))
  
  resp <- POST(urls$covid_score_api, add_headers("x-api-key" = Sys.getenv("X_API_KEY")), body = request_body, encode = "json")
  api_return <- content(resp)

  return (api_return)
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
  underreport_factor_string = formatNumber(risk$underreport_factor, "x")
  div(
    title = "Location",
    tags$p(div('We found data from ', formatDynamicString(risk$county), ' for your zip code. As of ', 
               formatDynamicString(risk$latest_day), ', this county had', formatDynamicString(format(round(risk$cases_past14d), big.mark =",")),
               ' new reported cases in the last 14 days and ',
               formatDynamicString(format(risk$cumulative_cases, big.mark=",")), 
               ' total reported cases of COVID-19. Many people who contract COVID-19 are not tested, and therefore not reported. 
               We estimate that your county has an under-reporting factor of ', underreport_factor_string, 
               '. Taking into account the under-reporting factor, incubation period, and time from symptom onset to recovery, we estimate there are ',
               formatDynamicString(format(round(risk$est_current_sick), big.mark =",")),
               ' total sick people distributed throughout the county, including those who are not officially reported.'
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

renderExposureHtml <- function(risk, symptoms) {
  prob_flu_string = formatPercent(risk$flu_risk_natl_avg)
  risk_string = formatPercent(risk$exposure_risk)
  sympt_covid_string = formatPercent(risk$sympt_covid_risk)
  exposure_text = paste0(
    "Among people who are the same health status as you and have behaviors and levels of interaction 
    with others that are similar to yours, the estimated probability of catching COVID-19 through community transmission in a week is ", 
    risk_string, '. ', "For comparison, ", prob_flu_string, ' of Americans catch the flu every week during flu season.')
  sickness_text = (paste0(
    "Based on the symptom(s) you selected, the probability that you have symptomatic COVID-19 is ", sympt_covid_string,
    ". If you are experiencing symptoms associated with COVID-19, please immediately consult ", 
    tags$a("the CDC's instructions", href = urls$cdc_if_sick),
    ", or walk through their ",
    tags$a("self-checker", href = urls$cdc_chatbot), '.'))
  
  if (!is.null(symptoms)) {
    total_risk_html = tags$p(HTML(paste0(exposure_text, "<br> <br>", sickness_text)))
  } else {
    total_risk_html = tags$p(HTML(paste0(exposure_text)))
  }
  
  return (total_risk_html)
}

renderSusceptibilityHtml <- function(risk) {
  tags$p(HTML(paste0(
    "Among people who are the same age, sex, and health status as you and get sick from COVID-19, the risk of hospitalization is ", 
    formatPercent(risk$hosp_risk),
    ", the risk of requiring an ICU is ",
    formatPercent(risk$icu_risk),
    ", and the risk of not surviving is ",
    formatPercent(risk$death_risk), "."
  )))
}

renderProtectionHtml <- function(risk, hand, ppe){
  
  prob_hand_string<- formatPercent(risk$risk_reduction_handwash)
  prob_ppe_string<- formatPercent(risk$risk_reduction_ppe)

  if (hand == TRUE ){
    hand_html = HTML(paste0(
      "Good to know you wash your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      "."))
  } else{
    hand_html = HTML(paste0(
      "We recommend you wash your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      "."))
  }
  if (risk$exposure_risk >0){
    # exposure reduction hand text for users with exposure risk of over 0
    hand_delta_html = HTML(paste0("In general, hand washing reduces people's risk of being exposed to COVID-19 by ", 
                                  prob_hand_string, " . "))
  } else{
    # exposure reduction hand text for users with exposure risk less than or equal to 0
    hand_delta_html = HTML(paste0("In general, hand washing reduces people's risk of being exposed to COVID-19, 
                                  if they do come into close contact with others. "))
  }
  
  if (ppe == TRUE){
    ppe_html = HTML(paste0(
      "Good to know you wear personal protection equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe), " . "))
  } else {
    ppe_html = HTML(paste0(
      "We recommend you wear personal protection equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe), " . "))
  }
  
  if (risk$exposure_risk >0){
    # exposure reduction ppe text for users with exposure risk of over 0
    ppe_delta_html = HTML(paste0("In general, wearing personal protection equipment reduces people's risk of being 
                                 exposed to COVID-19 by ", prob_ppe_string, " . "))
  } else {
    # exposure reduction ppe text for users with exposure risk less than or equal to 0
    ppe_delta_html = HTML(paste0("In general, wearing personal protection equipment reduces people's risk of being 
                                 exposed to COVID-19, if they do come into close contact with others. "))
  }
  
  return(tags$p(hand_html, hand_delta_html, ppe_html, ppe_delta_html))
}

renderResultsHtml <- function(risk, symptoms, hand, ppe) {
  
  # return
  tagList(
    tags$p(""),
    renderLocationHtml(risk),
    renderExposureHtml(risk, symptoms),
    renderSusceptibilityHtml(risk),
    renderProtectionHtml(risk, hand, ppe),
    renderScoreHtml(risk)
  )
}
