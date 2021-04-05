# API Helper Functions


request_url <- "api.covid19.mathematica.org/score"


# libraries
library(tidyverse)
library(magrittr)
library(httr)
library(future.apply)

# change booleans
bol2char <- function(x){
  # converts x from 1, 0 to True False
  
  ifelse(x == 1, 'True', 'False')
}

na_input <- function(x){
  # takes in val and converts to empty list if NA
  if (length(x) >1){x}
  else if(is.na(x)){list()} 
  else {as.list(x)}
}


list2_df <- function(list_out){
  # api output as list from
  # https://stackoverflow.com/questions/47720363/how-to-convert-a-nested-lists-to-dataframe-in-r
  # returns df
  temp <- unique(unlist(lapply(list_out, names)))
  mydf <- setNames(object = data.frame(lapply(temp, function(nm)
    unlist(lapply(list_out, function(x) x[[nm]])))), nm = temp)
  df_out <- tibble::rownames_to_column(mydf, "persons")
  
  return(df_out)
}

calculateRisk <- function(input) {
  #makes API Calls
  
  request_body <- list(
    'zip' = input[['zip']],
    'age' = as.numeric(input[['age']]),
    'sex' = gsub("is_", "", input[['sex']]),
    'symptoms' = na_input(input[['symptoms']]),
    'nppl' = as.numeric(input[['nppl']]),
    'is_roommate' = bol2char(input[['is_roommate']]),
    'nppl2' = as.numeric(input[['nppl2']]),
    'hand' = bol2char(input[['hand']]),
    'ppe' = bol2char(input[['ppe']]),
    'conditions' = na_input(input[['conditions']])
  )
  #print(request_body)
  #print(jsonlite::toJSON(request_body , auto_unbox = TRUE))
  resp <- POST(request_url, add_headers("x-api-key" = Sys.getenv("X_API_KEY")), body = request_body, encode = "json")
  api_return <- content(resp)
  
  #print(api_return)
  results_all <- api_return$results  
  
  # specify fips
  results  <- results_all[lapply(results_all, '[[', 'fips') == input$fips] %>% unlist
  
  #if there results fips does not match with fips input and there is only one output result then chose that output
  if (is.null(results) & length(results_all) == 1){
    results  <- results_all[[1]] %>% unlist
    #if there results fips does not match with fips input and the county is "Jackson County, MO" then pick that one
  } else if (sum(lapply(results_all, '[[', 'name') == 'Jackson County, MO')>= 1){
    results  <- results_all[[1]] %>% unlist
  }
  
  return(results)
}
