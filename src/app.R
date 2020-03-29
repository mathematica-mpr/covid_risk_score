library(shiny)
source('helper_county.R')
# Global variables can go here
fips <- '06001'
nppl <- 20


# Define the UI
ui <- fluidPage(
  titlePanel("COVID-19 Risk Score Calculator"),
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
      textOutput('res')
    )
  )
)


# Define the server code
server <- function(input, output) {
  output$res <-renderText({
    fips = input$fips
    county_pop = get_county_pop(fips)
    county_name = get_county_name(fips)
    county_pop = get_county_pop(fips)
    county_casecount = get_county_casecount(fips, Sys.Date()-1)
    county_underreport = calc_county_underreport(fips)
    res = 1-(1-county_casecount/county_pop/county_underreport)^input$nppl
    paste('You live in county:', county_name, ".\n\n",
          'Your county has', county_casecount, 'cases out of a population of', county_pop, '.\n\n',
          'We recommend you set the under-reporting factor to', county_underreport, '.\n\n',
          "Your COVID-19 risk score is", res, '.')
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)