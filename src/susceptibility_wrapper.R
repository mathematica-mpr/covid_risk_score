# This script contains a single function to calculate the probability of hospitalzation, ICU and deaths given user input.

source("./global_var.R")


risk2odds<-function(prob) {
    return (prob / (1 - prob))
}

odds2risk<-function(odds) {
    return (odds / (1 + odds))
}

calculate_susceptibility <- function (input){
    #input has the following fields:
    # age: numeric 
    # sex: string, expecting either "male" or "female", it can have "other", see line 40 if you want to modify the code to allow "other"
    # conditions: list of strings, expecting the following values:
    # "is_renal", if user selects "Chronic renal disease"
    # "is_cvd", if user selects "Cardiovascular disease"
    # "is_diabetes", if user selects "Diabetes"
    # "is_hyper", if user selects "Hypertension"
    # "is_smoker", if user selects "Current or former smoker"
    # "is_immune", if user selects "Immunocompromised condition"
    # "is_lung", if user selects "Chronic lung disease or asthma",
    # "is_other", if user selects "Other chronic disease",
    # "is_obese", if user selects "Obesity",
    # If user has no comorbidities, conditions will be NULL
    
    #check input validity
    assertthat::assert_that(is.numeric(input$age), msg = "Age must be numeric.")
    assertthat::assert_that(tolower(input$sex) %in% c("male", "female", "other"), msg = "Sex must be one of the three: male, female, other")
    
    #Baseline probability of hosp, ICU, death based on age
    age = as.numeric(input$age)
    age_index = max(which(age_list <= age))
    hosp_prob = hosp_list[age_index]
    icu_prob = icu_list[age_index]
    death_prob = death_list[age_index]
    hosp_odds = risk2odds(hosp_prob)
    icu_odds = risk2odds(icu_prob)
    death_odds = risk2odds(death_prob)
    
    #Update odds based on sex
    #This will consider "sex: other" the same as "female", if you want to consider them the same as "male", change the condition inside if() to 
    #input$sex !='female'
    if (input$sex=="male") {
        hosp_odds = hosp_odds * male_or[1] # should be hosp_odds(female) * male_or[1], but we are not doing it yet, so we slightly overpredict
        icu_odds = icu_odds * male_or[2]
        death_odds = death_odds * male_or[3]
    }
    
    #Update odds based on comorbidities
    #If user input more than two comorbidities, only the first two are considered.
    #[IMPORTANT]:The order of comorbidities can not be changed, renal disease has to be the first because its OR is the highest.
    for (condition_id in input$conditions[1:min(length(input$conditions), 2)]) {
        # remove "is_" prefix
        condition_root = substr(condition_id, 4, nchar(condition_id))
        hosp_odds = hosp_odds * eval(parse(text=paste0(condition_root, "_or[1]")))
        icu_odds = icu_odds * eval(parse(text=paste0(condition_root, "_or[2]")))
        death_odds = death_odds * eval(parse(text=paste0(condition_root, "_or[3]")))
    }
    
    #Convert odds back to probabilities
    hosp_risk = odds2risk(hosp_odds)
    icu_risk = odds2risk(icu_odds)
    death_risk = odds2risk(death_odds)
    
    return(list(hosp_risk = hosp_risk,
                icu_risk = icu_risk,
                death_risk = death_risk))
}

# examples
input1 <- list(age = 50, sex = "male", conditions = NULL)
calculate_susceptibility(input1)

input2 <- list(age = 65, sex = "male", conditions = list("is_cvd", "is_hyper"))
calculate_susceptibility(input2)

input3 <- list(age = 57, sex = "other", conditions = list("is_other"))
calculate_susceptibility(input3)

input4 <- list(age = 32, sex = "male", conditions = list("is_obese"))
calculate_susceptibility(input4)
