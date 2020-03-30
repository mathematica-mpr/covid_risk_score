library(shiny)
library(shinythemes)
library(shinycssloaders)
source("src/helper_county.R")
# Global variables can go here
fips <- ''
zip <- "94587"
nppl <- 20

css <- HTML(".html-widget.gauge svg {height: 350px;width: 900px;}")

# Define the UI
ui <- fluidPage(theme=shinytheme("superhero"),
  titlePanel("COVID-19 Risk Score Calculator"),
  tags$head(tags$style(css)),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("Answer a few questions to see your COVID-19 risk score:", class = "lead"),
      textInput('fips', label =  '5-digit FIPS code of your county', fips),
      textInput('zip', label =  "If you don't know your county FIPS code, what's your 5-digit zip code?", zip),
      numericInput('nppl', 'How many social interactions will you have over the next week?', nppl),
      #sliderInput('fac_underreport', "Choose what percentage of cases are tested?", min = 0.01, max = 1, value = 0.15, step = 0.01),
      checkboxInput('is_sick', "Are you sick already?"),
      #checkboxInput('in_hosp', "Do you work in a hospital?"),
      checkboxInput('in_highriskzone', "Do you live in or have you visited in the past two weeks an area where the transmission is widespread?"),
      actionButton('go', "Calculate my risk score", class = "btn-primary"),
      width =3
    ),
    #OUTPUT
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 fluidRow(withSpinner(gaugeOutput("gauge"), type = 1), style = "height:350px;"),
                 fluidRow(textOutput("res"), style = "width:800px")),
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
    #validate input types
    validate(
      need(input$fips!="" | input$zip!="", 'Provide at least one of the two: FIPS and zip code.'),
      need(input$nppl, 'Please provide the number of contacts.')
    )
    #read in FIPS or get it from ZIP
    if(input$fips!=""){
      fips<-input$fips
    }else{
      fips<-get_fips_from_zip(input$zip)
    }
    #get county-level characteristics
    county_pop <- get_county_pop(fips)
    county_name <- get_county_name(fips)
    county_casecount <- get_county_casecount(fips, latest_day)
    county_underreport <- calc_county_underreport(fips)
    if (input$nppl>0){
      risk <- 1-(1-get_county_casecount(fips, latest_day)/get_county_pop(fips)/calc_county_underreport(fips))^input$nppl
    } else{
      risk <- 0
    }
    g<-function(x){
      # a mapping function to address nonlinearity between probability and score
      assertthat::assert_that(x>=0 && x<=1)
      return((log(x)+8)/8)
    }
    score<-if_else(risk>0, g(risk)*100, 0)
    
    unlist(list("fips" = fips,
                "county_pop" = county_pop,
                "county_name" = county_name,
                "county_casecount" = county_casecount,
                "county_underreport" = county_underreport,
                "risk"= risk,
                "score" = score
    ))
  })
  output$gauge <-renderGauge({
    temp <- temp()
    score <- round(as.numeric(temp['score']))
    gauge(score, 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "")
  })
  output$res <-renderText({
    temp <- temp()
    paste('You live in county:', temp['county_name'], '.',
          'Your county has', temp['county_casecount'], 'cases out of a population of', 
          format(temp['county_pop']%>%as.numeric(), big.mark = ','), '.',
          'We estimated the under-reporting factor is', scales::percent(temp['county_underreport']%>%as.numeric()), '.',
          "The probability of you contracting COVID-19 is", scales::percent(temp['risk']%>%as.numeric()), '.',
          "On a scale of 0 to 100, your risk score is", round(temp['score']%>%as.numeric()), '.')
  })
  output$methods <-renderUI({
    tagList(tags$p(""),
            div(
              "We used published ",
              tags$a("county-level data of COVID-19 cases & deaths", href="https://www.nytimes.com/article/coronavirus-county-data-us.html"),
              " to estimate the prevalence of infected people within your county. Based on this likely prevalence, and the amount of social distancing you're able to accomplish, we can determine the likelihood you'll be exposed to COVID-19."
            ),
            tags$p(""),
            tags$h3("Assumptions:"),
            tags$li(
              "Above and beyond the official cases reported by your county, there are ",
              tags$a("additional unreported cases of COVID-19", href="https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html"),
              "distributed throughout your community."),
            tags$li("Other methods of becoming infected (e.g. touching an infected surface) are not accounted for by this calculator."),
            tags$p(""),
            tags$p("We'll be doing our best to update these assumptions as additional knowledge about the virus becomes available."),
    )
#      
#      "",
#      "Sources:",
#      "County-level COVID-19 data: ",
#      "Under-reporting factor: "
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)