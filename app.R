library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
source("src/helper_county.R")
source("src/helper_input.R")
source("src/info_html.R")
source("src/global_var.R")
source("src/results.R")

# Define the UI
ui <- fluidPage(
  useShinyjs(),
  theme=shinytheme("superhero"),
  titlePanel("COVID-19 Risk Score Calculator"),
  # google analytics tracking
  tags$head(includeHTML("src/google-analytics.html")),
  # sharethis
  tags$head(includeHTML("src/sharethis.html")),
  
  includeCSS("src/style.css"),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      helpText("DISCLAIMER: this tool is NOT a qualified source of medical knowledge, NOR should it be used to inform policy decisions.", class = "text-warning"),
      helpText("Please answer a few questions to see your COVID-19 risk score.", class = "lead"),
      # in helper_input.R
      collapseStory(),
      actionButton('go', "Calculate", class = "btn btn-primary btn-block"),
      tags$div(style = "margin:6px;"),
      tags$div(class="sharethis-inline-share-buttons"),
      width = 3
    ), # sidebarPanel
    
    # OUTPUT
    mainPanel(
      tabsetPanel(
        type = c("pills"),
        tabPanel("Score",
                 fluidRow(withSpinner(gaugeOutput("gauge", height = '600%'), type = 1)),
                 fluidRow(column(9, offset = 1, htmlOutput("res")))),
        # tabPanel("Map"),
        tabPanel("Method", htmlOutput("methods")),
        tabPanel("FAQ", htmlOutput("faq")),
        tabPanel("About us", htmlOutput("about"))
      ),
      width = 9
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage

# Define the server code
server <- function(input, output, session) {
  getCountyData<- eventReactive(input$go, {
    #deal with zipcode mapping to >1 counties
    fips<-get_fips_from_zip(input$zip)
    if (length(fips)  >1){
      output$zipcontrol <- renderUI({
        fips<-get_fips_from_zip(input$zip)
        fips_names<-lapply(fips, get_county_name)%>%unlist()
        radioButtons("fips",label = "Choose a county and resubmit", choiceNames = fips_names, choiceValues  = fips, selected = NULL)
       })
      fips<-input$fips
    }
    validate(need(!is.na(fips), "Check input!"))
    #fix NYC, all NYC borough data uses NY county
    if (fips%in%NY_fips_ls){
      population <- NY_fips_ls%>%map(~get_county_pop(.))%>%unlist()%>%sum()
      name <- "New York City (5 Boroughs)"
      casecount <- get_county_casecount("36061", latest_day)
      underreport_factor <- calc_county_underreport("36061")
    } else{
      #get county-level characteristics
      population <- get_county_pop(fips)
      name <- get_county_name(fips)
      casecount <- get_county_casecount(fips, latest_day)
      underreport_factor <- calc_county_underreport(fips)
    }
    
    return (list(fips = fips,
                 population = population,
                 name = name,
                 casecount = casecount,
                 underreport_factor = underreport_factor))
  })
  
  updateRisk <- reactive({
    county_data<-getCountyData()
    if (!input$has_preexisting) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "conditions", selected = character(0))
    }
    
    # in results.R
    return (calculateRisk(input, county_data))
  })
  
  output$gauge <-renderGauge({
    risk<-updateRisk()
    gauge(case_when(risk$score<1 ~ 1,
                    risk$score>100 ~ 100,
                TRUE ~round(risk$score)), 
          min = 0, max = 100, 
          sectors = gaugeSectors(success = c(0, 30),
                                 warning = c(30, 70),
                                 danger = c(70, 100)),
          label = "")
  })
  
  output$res <-renderUI({
    risk <- updateRisk()
    # in src/results.R
    renderResultsHtml(risk, input$is_sick)
  })
  
  output$methods <-renderUI({
    # in src/info_html.R
    renderMethodsHtml()
  })
  
  output$faq <- renderUI({
    # in src/info_html.R
    renderFaqHtml()
  })
  
  output$about <- renderUI({
    # in src/info_html.R
    renderAboutHtml()
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)