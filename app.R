library('shiny')
source("src/helper_county.R")
# Global variables can go here
fips <- '06001'
nppl <- 20

css <- HTML("
.html-widget.gauge svg {
  height: 400px;
  width: 800px;
}")

# Define the UI
ui <- fluidPage(
  titlePanel("COVID-19 Risk Score Calculator"),
  tags$head(tags$style(css)),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("Answer a few questions to see your COVID-19 risk score:"),
      textInput('fips', label =  '5-digit FIPS code of your county', fips),
      numericInput('nppl', 'Number of contacts you have had in the past two weeks', nppl),
      sliderInput('fac_underreport', "Choose what percentage of cases are tested?", min = 0.01, max = 1, value = 0.15, step = 0.01),
      checkboxInput('is_sick', "Are you sick already?"),
      checkboxInput('in_hosp', "Do you work in a hospital?"),
      checkboxInput("in_highriskzone", "Do you live in or have you visited in the past two weeks an area where the transmission is widespread?")
    ),
    #OUTPUT
    mainPanel(
      gaugeOutput("gauge"),
      textOutput('res')
    )
  )
)


# Define the server code
server <- function(input, output) {
  temp<- reactive({
    fips<-input$fips
    county_pop <- get_county_pop(fips)
    county_name <- get_county_name(fips)
    county_casecount <- get_county_casecount(fips, Sys.Date()-1)
    county_underreport <- calc_county_underreport(fips)
    risk <- 1-(1-get_county_casecount(fips, Sys.Date()-1)/get_county_pop(fips)/calc_county_underreport(fips))^input$nppl
    g<-function(x){
      # a mapping function to address nonlinearity between probability and score
      assertthat::assert_that(x>0 && x<1)
      return((log(x)+8)/8)
    }
    score<-if_else(risk>0, g(risk), 0)
    
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
    gauge(temp['score']%>%as.numeric()%>%round(2), 
          min = 0, max = 1, 
          sectors = gaugeSectors(success = c(0, 0.3),
                                 warning = c(0.3, 0.7),
                                 danger = c(0.7, 1)))
  })
  output$res <-renderText({
    temp <- temp()
    paste('You live in county:', temp['county_name'], ".",
          'Your county has', temp['county_casecount'], 'cases out of a population of', temp['county_pop'], '.',
          'We estimated the under-reporting factor is', temp['county_underreport'], '.',
          "The probability of you contracting COVID-19 is", temp['risk'], '.',
          "On a scale of 0 to 1, your risk score is", temp['score'], '.')
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)