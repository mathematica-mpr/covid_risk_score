library(shiny)
library(shinyBS)
source("src/global_var.R")

collapseStory <- function() {

  #textInput('fips', label =  '5-digit FIPS code of your county', fips),
  # collapsible UI to streamline input. Everything is open by default.
  bsCollapse(
    id = "collapse_main",
    multiple = TRUE,
    open = c("1. About You", "2. Pre-existing Conditions", "3. Your Behavior"),
    bsCollapsePanel(
      title = "1. About You",
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
      title = "3. Your Behavior",
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
  ) # bsCollapse
}