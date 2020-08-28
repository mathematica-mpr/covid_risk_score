library(shiny)
library(shinyBS)
source("src/global_var.R")

conditions_list = c("Chronic renal disease" = "is_renal",
                    "Cardiovascular disease" = "is_cvd",
                    "Diabetes" = "is_diabetes",
                    "Hypertension" = "is_hyper",
                    "Current or former smoker" = "is_smoker",
                    "Immunocompromised condition" = "is_immune",
                    "Chronic lung disease or asthma" = "is_lung",
                    "Obesity (BMI &ge; 30 kg/m&sup2;)" = "is_obesity",
                    "Other chronic disease" = "is_other")

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
      tags$p("We do not retain any information that you provide in connection with your use of the tool."),
      tags$p("Your use of this tool is subject to these ", tags$a("Terms of Use.", href="https://19andme-pub-0812304701987.s3.amazonaws.com/COVID-19+Risk+Calculator+Terms+of+Use+-+042220.pdf")),
      tags$p(style="color:#DF691A", "THE INFORMATION PROVIDED BY THIS TOOL IS NOT MEDICAL ADVICE AND CANNOT BE 
             USED TO DIAGNOSE OR TREAT ANY MEDICAL CONDITION.  See FAQ for more information.", class = "text-warning"),
      actionButton('next0', "Next", class = "btn btn-info btn-block")
    ),
    bsCollapsePanel(
      title = "1. About You",
      textInput('zip', label = "What is your 5-digit zip code?"),
      uiOutput("zipcontrol"),
      textInput('age', label = "What is your age?"),
      radioButtons('sex', "What sex were you assigned at birth?", 
                   c("Male" = "male", "Female" = "female",  "Other" = "sex_other", "Prefer not to say" = "sex_other"), inline=TRUE),
      actionButton('next1', "Next", class = "btn btn-info btn-block")
    ), # bsCollapsePanel
    bsCollapsePanel(
      title = "2. Pre-existing Conditions",
      checkboxInput('is_sick', div("I have ", tags$a("potential symptoms of COVID-19", href = urls$cdc_symptoms))),
      
      conditionalPanel(
        condition = "input.is_sick == true",
        checkboxGroupInput("symptoms", "Symptoms",
                           c("Loss of smell and taste" = "is_loss_smell_taste",
                             "Severe or significant persistent cough" = "is_cough",
                             "Severe fatigue" = "is_fatigue",
                             "Loss of appetite, skipped meals" = "is_skip_meal",
                             "My symptoms are not listed here" = "is_other" 
                             ))),
      hr(),
      
      checkboxInput('has_preexisting', div("I have ", tags$a("underlying medical complications", 
                                                             href = urls$cdc_high_risk))),
      
      conditionalPanel(
        condition = "input.has_preexisting == true",
        checkboxGroupInput("conditions", "Conditions", # this is written this way to allow html math in obesity test
                           choiceNames = lapply(names(conditions_list), HTML),
                           choiceValues = unname(conditions_list)
        ),
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
      #if ppe is true, then offer more choices                        
      conditionalPanel(
        condition = "input.ppe == true",
        radioButtons("ppe_type", "What kind of PPE?",
                           c("N95 respirators or similar" = "n95",
                             "Surgical or similar masks (eg, 12-16 layer cotton or gauze masks)" = "surgical",
                             "Other masks (cotton, fleece, bandana)" = "othermasks",
                             "Eye protection (eg, goggles or face shields)" = "eyeprotection"
                             ))),
      actionButton('go', "Calculate", class = "btn btn-primary btn-block")
    ) # bsCollapsePanel
  ) # bsCollapse
}