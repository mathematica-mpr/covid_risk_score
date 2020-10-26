# Define the UI
ui <- fluidPage(
  theme=shinytheme("superhero"),
  titlePanel(fluidRow(column(width = 9, "19 and Me: COVID-19 Risk Score Calculator"),
                      column(width = 3, img(src = 'MathematicaLogo_White_smaller.png',class = "pull-right"))),
             windowTitle = "19 and Me: COVID-19 Risk Calculator"),
  # google analytics tracking
  tags$head(includeHTML("google-analytics.html")),
  
  includeCSS("style.css"),
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
                 uiOutput("select_county"),
                 uiOutput("zipcontrol"),
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
