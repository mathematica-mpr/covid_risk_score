library(shiny)
library(shinythemes)
library(shinycssloaders)
source("src/helper_county.R")
# Global variables can go here
zip <- "94587"
nppl <- 20
prob_flu<- 35.5/327.2/26 #assume 26 weeks of flu season

css <- HTML(".html-widget.gauge svg {height: 66%; width: 66%; display: block; margin-left: auto;margin-right: auto; margin-bottom:-10%;}
            .irs-bar {background: #DF691A;}
            .irs-single {background: #DF691A;}
            .irs-bar-edge {background: #DF691A;}
            .irs-from {background: #DF691A;}
            .irs-to {background: #DF691A;}")

# Define the UI
ui <- fluidPage(theme=shinytheme("superhero"),
  titlePanel("COVID-19 Risk Score Calculator"),
  tags$head(tags$style(css)),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("Answer a few questions to see your COVID-19 risk score:", class = "lead"),
      #textInput('fips', label =  '5-digit FIPS code of your county', fips),
      textInput('zip', label =  "What is your 5-digit zip code?", zip),
      sliderInput('nppl', 
                  'How many people do you see in person in a week? (Try different #\'s after you hit "Calculate")', 
                  min = 0, max = 100, value = nppl, step =1),
      #sliderInput('fac_underreport', "Choose what percentage of cases are tested?", min = 0.01, max = 1, value = 0.15, step = 0.01),
      checkboxInput('is_sick', "Do you have flu-like symptoms?"),
      #checkboxInput('in_hosp', "Do you work in a hospital?"),
      checkboxInput('in_highriskzone', "Do you live in or have you visited in the past two weeks an area where the transmission is widespread?"),
      actionButton('go', "Calculate my risk score", class = "btn-primary"),
      width =3
    ),
    #OUTPUT
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(withSpinner(gaugeOutput("gauge", height = '600%'), type = 1)),
                 fluidRow(column(9, offset = 1, htmlOutput("res")))),
        #tabPanel("Map"),
        tabPanel("Methodology",
                 htmlOutput("methods"))),
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
    if (input$nppl>0){
      total_covid_count = county_casecount/county_underreport
      # ASSUMPTION: diagnosed cases are not active
      active_casecount = total_covid_count - county_casecount
      # ASSUMPTION: active community case count cannot be less than 10% of reported cases
      if (active_casecount < 0.1 * county_casecount) {
        active_casecount = 0.1 * county_casecount
      }
      if(input$is_sick | input$in_highriskzone){
        # if you're already sick with flu-like symptoms, your likelihood of having covid is P(C19) / (P(C19) + P(flu))
        total_covid_probability = total_covid_count / county_pop
        risk = total_covid_probability / (total_covid_probability + prob_flu)
      } else {
        risk <- 1-(1-active_casecount/county_pop)^input$nppl
      }
    } else{
      risk <- 0
    }
    g<-function(x){
      # a mapping function to address nonlinearity between probability and score
      normalized<-log10(x/prob_flu)*30+30 
      # 25 means equal likelihood of flu
      # 0 means 1/10 probability of flu
      # 100 means 1000 times probability of flu
      return(normalized)
    }
    score<-if_else(risk>0, g(risk), 1)
    unlist(list("risk" = risk,
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
    risk<-temp2['risk']%>%as.numeric()
    
    prob_flu_string = tags$b(formatC(signif(100 * prob_flu,digits=2), digits=2,format="fg"))
    county_underreport_string = tags$b(formatC(signif((1/county_underreport),digits=2), digits=2,format="fg"))
    risk_string = tags$b(formatC(signif(100 * risk,digits=2), digits=2,format="fg"))

    sickness_html_string = tags$p("Your estimated probability of COVID-19 exposure through community transmission is ",
                                  risk_string, '%.',
                                  "For comparison, ", prob_flu_string, '% of Americans catch the flu every week during flu season.')
    if (input$is_sick == TRUE) {
      sickness_html_string = tags$p("Since you're already sick, please immediately consult ", 
                             tags$a("the CDC's guidelines. ", 
                                    href = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html"),
                             "The probability that you could have COVID-19 is ", risk_string, '%. ')
    }
    
    tagList(
      tags$p(""),
      tags$p('We found data from ', tags$b(temp['county_name']), ' for your zip code.',
          'This county has ', tags$b(temp['county_casecount']%>%as.numeric()), ' cases out of a population of ', 
          tags$b(format(county_pop%>%as.numeric(), big.mark = ',')), 
          ", and we estimated that your county's sepcific under-reporting factor is ", 
          county_underreport_string, 'x. '),
      sickness_html_string,
      tags$p("On a scale of 0  (low risk) to 100 (high risk), your risk score is ", tags$b(round(temp2['score']%>%as.numeric())), '.')
    )
  })
  
  output$methods <-renderUI({
    tagList(
      tags$p(""),
      div(
        "We used published ",
        tags$a("county-level data of COVID-19 cases & deaths", 
               href="https://www.nytimes.com/article/coronavirus-county-data-us.html"),
        " to estimate the prevalence of infected people within your county. Based on this likely prevalence, and the amount of social distancing you're able to accomplish, we can determine the likelihood you'll be exposed to COVID-19."
      ),
      tags$p(""),
      tags$h3("Assumptions:"),
      tags$li(
        "Above and beyond the official cases reported by your county, there are additional unreported cases of COVID-19. We followed methodology reported",
        tags$a("by Russell et al (2020)", href="https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html"),
        "to calculate the percentage of cases that are currently detected. We then estimate the number of cases distributed throughout your community."),
      tags$li("Other methods of becoming infected (e.g. touching an infected surface) are not accounted for by this calculator."),
      tags$p(""),
      tags$p("We'll be doing our best to update these assumptions as additional knowledge about the virus becomes available."),
    )
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)