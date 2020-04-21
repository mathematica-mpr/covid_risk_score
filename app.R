library(shiny)
library(shinycssloaders)
library(shinythemes)
source("src/helper_county.R")
source("src/helper_input.R")
source("src/info_html.R")
source("src/global_var.R")
source("src/results.R")

# Define the UI
ui <- fluidPage(
  theme=shinytheme("superhero"),
  titlePanel(title=div("COVID-19 Risk Score Calculator", 
                       img(src="MathematicaLogo_White_smaller.png", 
                           style = "float:right;"))),
  # google analytics tracking
  tags$head(includeHTML("src/google-analytics.html")),
  
  includeCSS("src/style.css"),
  #INPUT
  sidebarLayout(
    sidebarPanel(
      #textInput('fips', label =  '5-digit FIPS code of your county', fips),
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
  
  validate_fips<-function(){
    #make sure zipcode is a five-digit number
    validate(need(!is.na(input$zip%>%as.numeric()), "zip code must only contain numbers."))
    validate(need(input$zip%>%as.numeric()<=99999, "zip code must be 5 digits."))
    
    fips<-get_fips_from_zip(input$zip)
     #deal with foreign zipcode
    if (length(fips) ==0){
      output$zipcontrol <- renderUI({
        textInput("fips0",label = "Please supply the FIPS code of your county")
      })
      fips<-input$fips0
      updateTextInput(session, "fips0", value = fips)
    } else if(length(fips)  >1){
        #deal with zipcode mapping to >1 counties 
      output$zipcontrol <- renderUI({
        fips<-get_fips_from_zip(input$zip)
        fips_names<-lapply(fips, get_county_name)%>%unlist()
        radioButtons("fips",label = "Choose a county and resubmit", choiceNames = fips_names, choiceValues  = fips, selected = NULL)
      })
      fips<-input$fips
      updateRadioButtons(session, "fips", selected = fips)
    }
    validate(need(!is.na(fips), "If your zip code matches multiple counties, please select the correct county and continue. If you don't see any choices, please input your county 5-digit FIPS code to proceed."))
    
    
  }
  
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
  
  getCountyData<- eventReactive(input$go, {
    #validate number of people for close contact
    validate_nppl()
    output$output_intro <-renderUI({
      # in src/results.R
      renderOutputIntroHtml()
    })
    fips<-get_fips_from_zip(input$zip)
    
    # if zip code matches multiple counties, read the input$fips
    if(length(fips)>1){
      fips <-input$fips
    } else if (is_empty(fips)){
      fips <-input$fips0
    }
    
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

  # Sidebar Collapse updates
  updateInputCollapse1 <- eventReactive(input$next0, {
    updateCollapse(session, id = "collapse_main", open = "1. About You", close = "Introduction")
  })
  updateInputCollapse2 <- eventReactive(input$next1, {
    validate_fips()
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
    renderResultsHtml(risk, input$is_sick, input$hand, input$ppe)
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