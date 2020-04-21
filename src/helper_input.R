library(shiny)
library(shinyBS)
source("src/global_var.R")

collapseStory <- function() {

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
      tags$p("Your use of this tool is subject to these ", tags$a("Terms of Use", href="")),
      tags$p(style="color:#DF691A", "THE INFORMATION PROVIDED BY THIS TOOL IS NOT MEDICAL ADVICE AND CANNOT BE 
             USED TO DIAGNOSE OR TREAT ANY MEDICAL CONDITION.  See FAQ for more information.", class = "text-warning"),
      actionButton('next0', "Next", class = "btn btn-info btn-block")
    ),
    bsCollapsePanel(
      title = "1. About You",
      textInput('zip', label = "What is your 5-digit zip code?"),
      uiOutput("zipcontrol"),
      textInput('age', label = "What is your age?"),
      radioButtons('gender', "Are you female or male?", c("Female" = "female", "Male" = "male"), inline=TRUE),
      actionButton('next1', "Next", class = "btn btn-info btn-block")
    ), # bsCollapsePanel
    bsCollapsePanel(
      title = "2. Pre-existing Conditions",
      checkboxInput('is_sick', div("I have ", tags$a("flu-like symptoms", href = urls$cdc_symptoms))),
      checkboxInput('has_preexisting', div("I have ", tags$a("underlying medical complications", 
                                                             href = urls$cdc_high_risk))),
      conditionalPanel(
        condition = "input.has_preexisting == true",
        checkboxGroupInput("conditions", "Conditions",
                           c("Chronic renal disease" = "is_renal",
                             "Cardiovascular disease" = "is_cvd",
                             "Diabetes" = "is_diabetes",
                             "Hypertension" = "is_hyper",
                             "Current or former smoker" = "is_smoker",
                             "Immunocompromised condition" = "is_immune",
                             "Chronic lung disease or asthma" = "is_lung",
                             "Other chronic disease" = "is_other"
                           ))
      ),
      actionButton('next2', "Next", class = "btn btn-info btn-block")
    ), # bsCollapsePanel
    bsCollapsePanel(
      title = "3. Your Behavior",
      sliderInput('nppl', 
                  'Direct exposure: how many people (include your household members) do you come into close contact (> 10 min, < 6 feet) with in a week?', 
                  min = 0, max = 100, value = 1, step =1),
      checkboxInput('is_roommate', "I live with other people."),
      conditionalPanel(
        condition = "input.is_roommate == true",
        sliderInput('nppl2', 
                    'Indirect exposure: how many people in total do your other household members come into close contact with in a week?', 
                    min = 0, max = 100, value = 0, step =1)),
      checkboxInput("hand", div("I perform hand hygiene according to ", 
                                tags$a("CDC guidance", href = urls$cdc_hand_hygiene))),
      checkboxInput("ppe", div("I wear personal protection equipment consistent with ",
                               tags$a("CDC guidelines", href = urls$cdc_ppe))),
      actionButton('go', "Calculate", class = "btn btn-primary btn-block")
    ) # bsCollapsePanel
  ) # bsCollapse
}