library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
source("src/helper_county.R")
source("src/global_var.R")
source("src/info_html.R")
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
      helpText("DISCLAIMER: this tool is NOT a qualified source of medical knowledge, NOR should it be used to inform policy decisions.", class = "text-danger"),
      helpText("Please answer a few questions to see your COVID-19 risk score.", class = "lead"),
      #textInput('fips', label =  '5-digit FIPS code of your county', fips),
      # collapsible UI to streamline input. Everything is open by default.
      bsCollapse(
        id = "collapse_main",
        multiple = TRUE,
        open = "1. Demographic Information",
        bsCollapsePanel(
          title = "1. Demographic Information",
          textInput('zip', label = "What is your 5-digit zip code?"),
          uiOutput("zipcontrol"),
          textInput('age', label = "What is your age?"),
          radioButtons('gender', "What is your gender?", c("Female" = "female", "Male" = "male"), inline=TRUE)
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "2. Pre-existing Conditions",
          checkboxInput('is_sick', div("I have ", tags$a("flu-like symptoms", href = urls$cdc_symptoms))),
          checkboxInput('has_preexisting', div("I have ", tags$a("underlying medical complications", 
                                                                 href = urls$cdc_high_risk))),
          conditionalPanel(
            condition = "input.has_preexisting == true",
            checkboxGroupInput("conditions", "Conditions",
                               c("Diabetes" = "is_diabetes",
                                 "Hypertension" = "is_hyper",
                                 "Chronic lung disease or asthma" = "is_lung",
                                 "Cardiovascular disease" = "is_cvd",
                                 "Immunocompromised condition" = "is_immune",
                                 "Chronic renal disease" = "is_renal",
                                 "Other chronic disease" = "is_other",
                                 "Current or former smoker" = "is_smoker"
                               )),
          )
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "3. Behavioral Input",
          sliderInput('nppl', 
                      'How many people do you come into close contact (> 10 min, < 6 feet) with?', 
                      min = 0, max = 100, value = 0, step =1),
          checkboxInput('is_roommate', "I live with other people."),
          conditionalPanel(
            condition = "input.is_roommate == true",
            sliderInput('nppl2', 
                        'How many people do your other household members come into close contact with?', 
                        min = 0, max = 100, value = 0, step =1)),
          checkboxInput("hand", div("I perform hand hygiene according to ", 
                                            tags$a("CDC guidance", href = urls$cdc_hand_hygiene))),
          checkboxInput("ppe", div("I wear personal protection equipment consistent with ",
                                           tags$a("CDC guidelines", href = urls$cdc_ppe)))
        ) # bsCollapsePanel
      ), # bsCollapse
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