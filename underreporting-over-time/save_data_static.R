library(tidyverse)
library(httr)
library(lubridate)

process_usafacts_data <- function(dat_raw, type){
  stopifnot(type %in% c("cases","deaths"))
  dat_raw %>% .$content %>% rawToChar() %>% read_csv()  %>%
    gather(date, !!sym(type), names(.)[grepl("^\\d",names(.))]) %>%
    set_names(tolower(gsub("[^A-Za-z]","",names(.)))) %>% 
    rename(fips=countyfips, county=countyname) %>%
    mutate(fips = str_pad(fips, width=5, pad="0"),
           date = ymd(date)) %>%
    select_if(names(.) %in% c("date","fips","cases","deaths","state"))
}

save_data_static <- function(outpath){ 
  # code from county_helper.R on USAFacts branch 
  usafacts_cases_url <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  usafacts_deaths_url <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
  
  cases<-GET(usafacts_cases_url) %>% process_usafacts_data(type="cases")
  deaths<-GET(usafacts_deaths_url) %>% process_usafacts_data(type="deaths") 
  df <- full_join(cases, deaths, by=c("date","fips", "state")) %>%
    filter(fips!="00000") 
  stopifnot(!is.na(cases), !is.na(deaths)) 
  
  df %>% saveRDS(file.path(outpath, 
                           paste0("usafacts_", gsub("-","",Sys.Date()),".RDS")))
}

if (F){
  save_data_static("C:\\Users\\elipman\\OneDrive - Mathematica\\Documents\\19andMe\\Paper\\Data")
}