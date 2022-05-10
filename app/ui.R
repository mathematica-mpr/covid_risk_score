# Define the UI
ui <- fluidPage(
  theme=shinytheme("superhero"),
  titlePanel(fluidRow(column(width = 9, "19 and Me: COVID-19 Risk Score Calculator"),
                      column(width = 3, img(src = 'MathematicaLogo_White_smaller.png',class = "pull-right"))),
             windowTitle = "19 and Me: COVID-19 Risk Calculator"),
  # google analytics tracking
  tags$head(includeHTML("google-analytics.html")),
  
  includeCSS("style.css"),
  includeScript('app.js'),
  
  #INPUT --------------------------------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # collapsible UI to streamline input. Everything is open by default.
      bsCollapse(
        id = "collapse_main",
        multiple = TRUE,
        open = c("Introduction"),
        bsCollapsePanel(
          title = "Introduction",
          tags$p("This tool synthesizes reported COVID-19 geographic case data and rapidly evolving
               scientific research to help you ballpark how much risk this disease poses to you."),
          tags$p("We believe people make the right decisions when empowered with neither fear, nor 
               complacency, but with accurate data."),
          tags$p("Please note: many 
               very important aspects of this disease are either unknown or estimated with large 
               uncertainty. With that said, our guiding philosophy is that an imperfect estimate 
               is better than no estimate."),
          tags$p("This tool works best on Google Chrome and mobile.", class = "text-warning"),
          tags$p("We do not retain any information that you provide in connection with your use of the tool."),
          tags$p("Your use of this tool is subject to these ", tags$a("Terms of Use.", href=urls$terms_of_use)),
          tags$p(style="color:#DF691A", "THE INFORMATION PROVIDED BY THIS TOOL IS NOT MEDICAL ADVICE AND CANNOT BE 
             USED TO DIAGNOSE OR TREAT ANY MEDICAL CONDITION.  See FAQ for more information.", class = "text-warning"),
          actionButton('next0', "Next", class = "btn btn-info btn-block")
        ),
        bsCollapsePanel(
          title = "1. About You",
          radioButtons('country', "What is your country?", 
                       c("United States" = "us", "Belgium" = "be"), selected = "us", inline=TRUE),
          conditionalPanel(
            condition = "input.country == 'us'",
            div(class = "questiontext", "What is your 5-digit zip code?")),
          conditionalPanel(
            condition = "input.country == 'be'",
            div(class = "questiontext", "What is your 4-digit postal code?")),
          textInput('zip', label = NULL),
          textInput('age', label = "What is your age?"),
          radioButtons('sex', "What sex were you assigned at birth?", 
                       c("Male" = "male", "Female" = "female",  "Other" = "sex_other", "Prefer not to say" = "sex_other"), inline=TRUE),
          actionButton('next1', "Next", class = "btn btn-info btn-block")
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "2. Your Health Status",
          checkboxInput('is_sick', div("I have ", tags$a("potential symptoms of COVID-19", href = urls$cdc_symptoms))),
          # COVID-19 symptoms input
          conditionalPanel(
            condition = "input.is_sick == true",
            checkboxGroupInput("symptoms", "Symptoms",
                               c("Loss of smell and taste" = "loss_smell_taste",
                                 "Severe or significant persistent cough" = "severe_cough",
                                 "Severe fatigue" = "severe_fatigue",
                                 "Loss of appetite, skipped meals" = "loss_appetite",
                                 "My symptoms are not listed here" = "other" 
                               ))),
          hr(),
          # Pre-Existing conditions inputs
          checkboxInput('has_preexisting', div("I have ", tags$a("underlying medical complications", 
                                                                 href = urls$cdc_high_risk))),
          conditionalPanel(
            condition = "input.has_preexisting == true",
            checkboxGroupInput("conditions", "Conditions", # this is written this way to allow html math in obesity test
                               choiceNames = lapply(names(conditions_list), HTML),
                               choiceValues = unname(conditions_list)
            ),
          ),
          hr(),
          # Excerise Level
          radioButtons('exercise_level', "On average how many minutes per week do you engage in moderate to strenuous exercises (like brisk walking)?", 
                       exercise_level_list, inline=TRUE, selected = "btw_11_149mpw"), 
          # Next Buttion
          actionButton('next2', "Next", class = "btn btn-info btn-block")
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "3. Your Behavior",
          radioButtons('live_w_others', "Do you live with other people?", 
                       list("Yes"="True", "No"="False"), inline=TRUE, selected = "False"),
          sliderInput('direct_contacts', 
                      'Direct exposure: how many people (include your household members) do you come into close contact (> 10 min, < 6 feet) with in a week?', 
                      min = 0, max = 100, value = 1, step =1),
          conditionalPanel(
            condition = "input.live_w_others == 'True'",
            sliderInput('indirect_contacts', 
                        'Indirect exposure: how many people in total do your household members come into close contact with in a week? (Do not include contact between household members in this count.)', 
                        min = 0, max = 100, value = 0, step =1)),
          hr(),
          checkboxInput("hand", div("I perform hand hygiene according to ", 
                                    tags$a("CDC guidance", href = urls$cdc_hand_hygiene))),
          checkboxInput("ppe", div("I wear personal protection equipment consistent with ",
                                   tags$a("CDC guidelines", href = urls$cdc_ppe))),
          # activities risk
          hr(),
          div(class = "questiontext", "In the previous week, I participated in the following activities:"),
          checkboxGroupInput("l_activities", "Low-risk activities:",
                             choices = l_activities_list,
                             inline = TRUE,
                             selected = NULL),
          checkboxGroupInput("m_activities", "Medium-risk activities:",
                             choices = m_activities_list,
                             inline = TRUE,
                             selected = NULL),
          checkboxGroupInput("h_activities", "High-risk activities:",
                             choices = h_activities_list,
                             inline = TRUE,
                             selected = NULL),
          actionButton('next3', "Next", class = "btn btn-info btn-block")
        ), # bsCollapsePanel
        bsCollapsePanel(
          title = "4. Your Vaccination Status",
          checkboxInput('has_vaccine', div("I have received at least one dose of a COVID-19 vaccine")),
          conditionalPanel(
            condition = "input.has_vaccine == true",
            radioButtons('vaccine', "Which of the available COVID-19 vaccines did you receive most recently, including the booster?",
                         choiceNames = unname(vaccine_labels), choiceValues = names(vaccine_labels),
                         inline=TRUE),
            radioButtons('months_last_vaccination', "When was your most recent COVID-19 vaccine, including the booster?", 
                         months_last_vaccination_labels, inline=TRUE),
            ),
          actionButton('go', "Calculate", class = "btn btn-primary btn-block")
        ) # bsCollapsePanel
      ), # bsCollapse
      width = 4
    ), # sidebarPanel
    
    # OUTPUT--------------------------------------------------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = c("pills"),
        tabPanel("Score",
                 uiOutput("select_county"),
                 uiOutput("zipcontrol"),
                 fluidRow(column(width = 8, htmlOutput("output_intro"))),
                 fluidRow(column(width = 8, withSpinner(gaugeOutput("gauge", height = '600%'), type = 1))),
                 fluidRow(column(width = 8,htmlOutput("score_info"))),
                 fluidRow(column(width = 8,htmlOutput("vaccines"),style = "background-color:#4E5D6C;")),
                 fluidRow(column(width = 8,htmlOutput("res")))
        ),
        # tabPanel("Map"),
        tabPanel("Method", htmlOutput("methods")),
        tabPanel("FAQ", htmlOutput("faq")),
        tabPanel("Change Log", htmlOutput("changelog"))
      ),
      width = 8
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage
