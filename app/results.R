
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
  
  request_url <- "api.covid19.mathematica.org/score"
  
  request_body <- list(
    "zip" = input$zip,
    "age"= input$age,
    "sex" = input$sex,
    "symptoms" = list(input$symptoms),
    "nppl" = input$nppl,
    "is_roommate"= bool2char(input$is_roommate),
    "nppl2" = input$nppl2,
    "hand"= bool2char(input$hand),
    "ppe"= bool2char(input$ppe),
    "conditions" = list(input$conditions)
  )
  
  resp <- POST(request_url, add_headers("x-api-key" = Sys.getenv("X_API_KEY")), body = request_body, encode = "json")
  api_return <- content(resp)
  results <- api_return$results
  return (results)
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
    tags$p(div('We found data from ', formatDynamicString(risk$name), ' for your zip code. As of ', 
               formatDynamicString(risk$latest_day), ', this county had', formatDynamicString(format(round(risk$moving_casecount), big.mark =",")),
               ' new reported cases in the last 14 days and ',
               formatDynamicString(format(risk$n_case_today, big.mark=",")), 
               ' total reported cases of COVID-19. Many people who contract COVID-19 are not tested, and therefore not reported. 
               We estimate that your county has an under-reporting factor of ', underreport_factor_string, 
               '. Taking into account the under-reporting factor, incubation period, and time from symptom onset to recovery, we estimate there are ',
               formatDynamicString(format(round(risk$est_unreported_sick), big.mark =",")),
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

renderExposureHtml <- function(risk, symptoms) {
  prob_flu_string = formatPercent(prob_flu)
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
