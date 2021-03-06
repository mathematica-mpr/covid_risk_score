
# converts boolean into string -------------------------------------------------
bool2char <- function(bol){
  # convert boolean to char
  # bol : boolean
  stringr::str_to_sentence(bol)
}

# function makes API call ------------------------------------------------------
calculateRisk <- function(input) {
  request_body <- list(
    "zip" = input$zip,
    "age"= as.numeric(input$age),
    "sex" = input$sex,
    "symptoms" = as.list(input$symptoms),
    "direct_contacts" = as.numeric(input$direct_contacts),
    "live_w_others"= input$live_w_others,
    "indirect_contacts" = as.numeric(input$indirect_contacts),
    "hand"= bool2char(input$hand),
    "ppe"= bool2char(input$ppe),
    "conditions" = as.list(input$conditions),
    "activities_high" = length(input$h_activities),
    "activities_medium" = length(input$m_activities),
    "activities_low" = length(input$l_activities),
    "exercise_level" = input$exercise_level)
  
  if (input$has_vaccine){
    request_body$vaccine = input$vaccine
    request_body$doses = as.numeric(input$doses)
    request_body$days_since_last_dose = input$days_since_last_dose
  }
  
  resp <- POST(urls$covid_score_api, add_headers("x-api-key" = Sys.getenv("X_API_KEY")), body = request_body, encode = "json")
  #resp <- POST(urls$covid_score_api_dev, body = request_body, encode = "json")
  api_return <- content(resp)

  return (api_return)
}

# formate string, numbers, or percent ------------------------------------------
formatResultsHeader <- function(string){
  return (tags$h4(tags$span(style="color:#F0AD4E",string)))
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

# function to create location HTML output --------------------------------------
renderLocationHtml <- function(risk) {
  underreport_factor_string = formatNumber(risk$underreport_factor, "x")
  latest_day_string <- format(as.Date(risk$latest_day, "%m/%d/%Y"), "%B %d, %Y")
  cases_past14d_string <- format(round(risk$cases_past14d), big.mark =",")
  cumulative_cases_string <- format(risk$cumulative_cases, big.mark=",")
  est_current_sick_string <- format(round(risk$est_current_sick), big.mark =",")
  
  proportion_sick <- risk$est_current_sick/risk$population
  prob_group50 <- 1-((1-proportion_sick)^50)
  prob_group10 <- 1-((1-proportion_sick)^10)
  
  div(
    title = "Location",
    tags$p(div('We found data from ', formatDynamicString(risk$county), ' for your zip code. As of ', 
               formatDynamicString(latest_day_string), ', this county had', formatDynamicString(cases_past14d_string),
               ' new reported cases in the last 14 days and ',
               formatDynamicString(cumulative_cases_string), 
               ' total reported cases of COVID-19. Many people who contract COVID-19 are not tested, and therefore not reported. 
               We estimate that your county has an under-reporting factor of ', underreport_factor_string, 
               '.'
               
    ),
    tags$p(),
    tags$p("Taking into account the under-reporting factor and average time from symptom onset to recovery, we estimate that:"),
    tags$p(tags$ul(
      tags$li(div('There are ', formatDynamicString(format(round(risk$est_current_sick), big.mark =",")),
                  ' total sick people distributed throughout the county, including those who are not officially reported.')),
      tags$li(div("1 in every ", 
                  formatDynamicString(format(round(1/(proportion_sick)), big.mark =",")),
                  " people in your county is currently infected with COVID-19.")),
      tags$li(div("In a group of 50 people, there is a ", formatPercent(prob_group50),
                  " chance that at least one person has COVID-19.")),
      tags$li(div("In a group of 10 people, there is a ", formatPercent(prob_group10),
                  " chance that at least one person has COVID-19.")))
    ))
  )
}

# function to create risk_score HTML output ------------------------------------
renderScoreHtml <- function(risk) {
  score<- risk$risk_score
  
  tags$p(HTML(paste0(
    "The risk score for people with similar characteristics and behaviors as you is ",
    formatDynamicString(round(score)), 
    case_when(
      score<30 ~ paste0(
        ", which is (relatively) safe. Even so, it's a good time to make sure that you know how to ",
        tags$a("protect yourself & others. ", href = urls$cdc_get_ready)),
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

renderVaccinesHtml <- function(go, has_vaccine, vaccine, doses, days){
  validate(need(go, ""))
  
  # Case where not vaccinated
  if (!has_vaccine){
    # no doses
    text <- tags$p("There are currently", total_vaccines_str, "vaccines against COVID-19 authorized for use in the United States. ",
           "All of the approved vaccines are safe and highly effective at preventing symptomatic COVID-19. ", 
           tags$a("Click here ", href=urls$cdc_vaccines), "for more information and to check when you might be eligible for vaccination.")
  } else if (doses < vaccines[[vaccine]][["doses"]]){
    # some but not all doses
    text <- tags$p("Congratulations on receiving your first dose of the ", vaccine_labels[vaccine], " COVID-19 vaccine!",
           "Be sure to get your second dose of the vaccine ", vaccines[[vaccine]]$days_between_doses,
           " days after the first. ", tags$a("Click here ", href=urls$cdc_vaccines), 
           "for more information about the United States' vaccination program.")
  } else {
    # all doses
    if (days<vaccines[[vaccine]]$days_after_final_dose){
      # before efficacy threshold
      text <- tags$p("Congratulations on receiving", 
                     ifelse(vaccines[[vaccine]]$doses==1, "the ", "both doses of the "), 
                     vaccine_labels[vaccine], " COVID-19 vaccine!",
                     "This vaccine reaches its full ", formatPercent(vaccines[[vaccine]]$efficacy),
                     " efficacy at around ", vaccines[[vaccine]]$days_after_final_dose,
                     ifelse(vaccines[[vaccine]]$doses==1, "days after vaccination. ", "days after the second dose. "), 
                     "Your immunity will build up over the next few days. ",
                     tags$a("Click here ", href=urls$cdc_vaccines), 
                     "for more information about the United States' vaccination program.")
    } else {
      # after efficacy threshold
      text <- tags$p("Congratulations on receiving", 
                     ifelse(vaccines[[vaccine]]$doses==1, "the ", "both doses of the "), 
                     vaccine_labels[vaccine], " COVID-19 vaccine!",
                     "This vaccine reduces your risk of contracting symptomatic COVID-19 by ",
                     formatPercent(vaccines[[vaccine]]$efficacy), ". ",
                     tags$a("Click here ", href=urls$cdc_vaccines), 
                     "for more information about the United States' vaccination program.")
    }
  }
  
  div(formatResultsHeader("Vaccine information"), text, 
      tags$p("After you have been vaccinated, be sure to follow the ",
             tags$a("CDC guidance for fully vaccinated individuals ", href=urls$cdc_vaccinated_guidance),
             "to protect your family, friends, and community.",  
             "Early evidence from vaccine trials suggests that some or all of the vaccines may provide additional protection against hospitalization and death, ", 
             "however this conclusion is only preliminary due to the small number of individuals in the treatment groups who contracted COVID-19."
      ))
}

# function to create exporsure risk HTML output --------------------------------
renderExposureHtml <- function(risk, symptoms) {
  prob_flu_string = formatPercent(risk$flu_risk_natl_avg)
  risk_string = formatPercent(risk$exposure_risk)
  sympt_covid_string = formatPercent(risk$sympt_covid_risk)
  exposure_text = paste0(
    "Among people in your county who have behaviors and levels of interaction 
    with others that are similar to yours, the estimated probability of catching COVID-19 through community transmission in a week is ", 
    risk_string, '. ', "For comparison, ", prob_flu_string, ' of Americans catch the flu every week during flu season.')
  sickness_text = (paste0(
    "Based on the symptom(s) you selected, the probability that your symptoms indicate COVID-19 is ", sympt_covid_string,
    ". If you are experiencing symptoms associated with COVID-19, please consult the ", 
    tags$a("CDC's information on COVID-19 testing", href = urls$cdc_test_info),
    " or visit the website for your state or local health department ",
    "for information about getting tested for COVID-19."))
  
  if (!is.null(symptoms)) {
    total_risk_html = div(tags$p(HTML(exposure_text)), tags$p(HTML(sickness_text)))
  } else {
    total_risk_html = tags$p(HTML(paste0(exposure_text)))
  }
  
  return (total_risk_html)
}

# function to create susceptibility HTML output --------------------------------------
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

# function to create hand and ppe HTML output ----------------------------------
renderProtectionHtml <- function(risk, hand, ppe){
  
  prob_hand_string<- formatPercent(risk$risk_reduction_handwash)
  prob_ppe_string<- formatPercent(risk$risk_reduction_ppe)

  if (hand == TRUE ){
    hand_html = HTML(paste0(
      "Good to know that you wash your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      "."))
  } else{
    hand_html = HTML(paste0(
      "We recommend that you wash your hands per ", 
      tags$a("CDC guidance", href = urls$cdc_hand_hygiene),
      "."))
  }
  if (risk$exposure_risk >0){
    # exposure reduction hand text for users with exposure risk of over 0
    hand_delta_html = HTML(paste0("In general, handwashing reduces people's risk of being exposed to COVID-19 by ", 
                                  prob_hand_string, " . "))
  } else{
    # exposure reduction hand text for users with exposure risk less than or equal to 0
    hand_delta_html = HTML(paste0("In general, handwashing reduces people's risk of being exposed to COVID-19, 
                                  if they do come into close contact with others. "))
  }

  if (ppe == TRUE){
    ppe_html = HTML(paste0(
      "Good to know that you wear personal protective equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe), " . "))
  } else {
    ppe_html = HTML(paste0(
      "We recommend that you wear personal protective equipment per ", 
      tags$a("CDC guidelines", href = urls$cdc_ppe), " . "))
  }
  
  if (risk$exposure_risk >0){
    # exposure reduction ppe text for users with exposure risk of over 0
    ppe_delta_html = HTML(paste0("In general, wearing personal protective equipment reduces people's risk of being 
                                 exposed to COVID-19 by ", prob_ppe_string, " . "))
  } else {
    # exposure reduction ppe text for users with exposure risk less than or equal to 0
    ppe_delta_html = HTML(paste0("In general, wearing personal protective equipment reduces people's risk of being 
                                 exposed to COVID-19, if they do come into close contact with others. "))
  }
  
  return(tags$p(hand_html, hand_delta_html, ppe_html, ppe_delta_html))
}

# function combines score page HTML outputs ------------------------------------
renderResultsHtml <- function(risk, symptoms, hand, ppe) {
  
  # return
  tagList(
    formatResultsHeader("County prevalence"),
    renderLocationHtml(risk),
    formatResultsHeader("Risk of contracting COVID-19"),
    renderExposureHtml(risk, symptoms),
    renderProtectionHtml(risk, hand, ppe),
    formatResultsHeader("Risk of adverse outcomes from COVID-19"),
    renderSusceptibilityHtml(risk)
  )
}
