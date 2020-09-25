library(shiny)
library(shinycssloaders)
library(shinythemes)
source("src/global_var.R")
source("src/helper_input.R")
source("src/info_html.R")
source("src/results.R")

# Define the UI
ui <- fluidPage(
  theme=shinytheme("superhero"),
  titlePanel(fluidRow(column(width = 9, "19 and Me: COVID-19 Risk Score Calculator"),
                      column(width = 3, img(src = 'MathematicaLogo_White_smaller.png',class = "pull-right"))),
             windowTitle = "19 and Me: COVID-19 Risk Calculator"),
  # google analytics tracking
  tags$head(includeHTML("src/google-analytics.html")),
  
  includeCSS("src/style.css"),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      # in helper_input.R
      collapseStory(),
      width = 4
    ), # sidebarPanel
    
    # OUTPUT
    mainPanel(
      tabsetPanel(
        type = c("pills"),
        tabPanel("Score",
                 fluidRow(column(width = 8, htmlOutput("output_intro"))),
                 fluidRow(column(width = 8, withSpinner(gaugeOutput("gauge", height = '600%'), type = 1))),
                 fluidRow(column(width = 8,htmlOutput("res")))
                 ),
        # tabPanel("Map"),
        tabPanel("Method", htmlOutput("methods")),
        tabPanel("FAQ", htmlOutput("faq"))
      ),
      width = 8
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage

# Define the server code
server <- function(input, output, session) {

  disclaimer_message <- modalDialog(
    title = "Disclaimer",
    disclaimerpopupHTML()
    )
  
  # Show the model on start up ...
  showModal(disclaimer_message)

  
  validate_age<-function(){
    age<-input$age%>%as.numeric()
    validate(need(!is.na(age), "Age must be a number."))
    validate(need(age >= 0, "Age can't be negative."))
    validate(need(age < 123, "According to the Guinness World Records, age must be smaller than 123."))
  }
  
  validate_nppl<-function(){
    #if user checks "I live with others", then input$nppl has to be greater than 0
    if(input$is_roommate){
      validate(need(input$nppl>=1, "Since you live with other people, you must have come into close contact with one or more people."))
    }
  }
  
  hit_api<- eventReactive(input$go, {
    #validate number of people for close contact
    validate_nppl()
    output$output_intro <-renderUI({
      # in src/results.R
      renderOutputIntroHtml()
      })
    return(calculateRisk(input))
    })

  
  # Sidebar Collapse updates
  updateInputCollapse1 <- eventReactive(input$next0, {
    updateCollapse(session, id = "collapse_main", open = "1. About You", close = "Introduction")
  })
  updateInputCollapse2 <- eventReactive(input$next1, {
    validate_age()
    updateCollapse(session, id = "collapse_main", open = "2. Pre-existing Conditions", 
                   close = "1. About You")
  })
  updateInputCollapse3 <- eventReactive(input$next2, {
    updateCollapse(session, id = "collapse_main", open = "3. Your Behavior", 
                   close = "2. Pre-existing Conditions")
  })
  updateInputCollapses <- function() {
    updateInputCollapse1()
    updateInputCollapse2()
    updateInputCollapse3()
  }
  
  updateRisk <- reactive({
    updateInputCollapses()
    if (!input$is_sick) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "symptoms", selected = character(0))
    }
    
    if (!input$has_preexisting) {
      # clear the conditional panel's UI when unchecked
      updateCheckboxGroupInput(session, "conditions", selected = character(0))
    }
    risk_data <- hit_api()
    
    # in results.R
    return (risk_data)
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
    renderResultsHtml(risk, input$symptoms, input$hand, input$ppe)
  })
  
  output$methods <-renderUI({
    # in src/info_html.R
    renderMethodsHtml()
  })
  
  output$faq <- renderUI({
    # in src/info_html.R
    renderFaqHtml()
  })
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)