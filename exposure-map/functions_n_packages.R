library(tidyverse)
library(lubridate)
library(yaml)
library(httr)
library(tidycensus)
library(lme4)
library(fitdistrplus)


process_usafacts_data <- function(dat_raw, type){
  stopifnot(type %in% c("cases","deaths"))
  dat_raw %>% .$content %>% rawToChar() %>% read_csv()  %>%
    gather(date, !!sym(type), names(.)[grepl("^\\d",names(.))]) %>%
    set_names(tolower(gsub("[^A-Za-z]","",names(.)))) %>% 
    rename(fips=countyfips, county=countyname) %>%
    mutate(fips = str_pad(fips, width=5, pad="0"),
           date = ymd(date)) %>%
    select_if(names(.) %in% c("date","fips","cases","deaths", "state"))
}

get_data <- function(){
  httr::set_config(config(ssl_verifypeer = 0L))
  usafacts_cases_url <- 'https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv'
  usafacts_deaths_url <- 'https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv'
  
  cases<-GET(usafacts_cases_url) %>% process_usafacts_data(type="cases")
  deaths<-GET(usafacts_deaths_url) %>% process_usafacts_data(type="deaths") 
  df <- full_join(cases, deaths, by=c("date","fips","state")) 
  stopifnot(!is.na(cases), !is.na(deaths))
  
  data("fips_codes")
  fips_list <- paste0(fips_codes$state_code, fips_codes$county_code)
  df <- df %>% filter(fips %in% fips_list) #exclude 00000, 00001, 06000
  
  return(df)
}

shape_data <- function(dat){
  dat %>% 
    gather(var,count,c("cases","deaths")) %>%
    mutate(d_label = case_when(var=="deaths" & date == max(date) ~ "deaths_today",
                               var=="deaths" & date == max(date)-90 ~ "deaths_90",
                               var=="cases" & date == max(date)-13 ~ "cases_today",
                               var=="cases" & date == max(date)-13-90 ~ "cases_90")) %>%
    filter(!is.na(d_label)) %>% dplyr::select(fips,state,count,d_label) %>%
    spread(d_label, count) %>%
    mutate(cases = cases_today - cases_90, deaths = deaths_today-deaths_90, CFR = deaths/cases) 
    #mutate(cases = cases_today, deaths = deaths_today, CFR = deaths/cases) 
}

calc_adjusted_CFR <- function(dat, fips){
  fit <-  glmer(cbind(deaths, cases-deaths) ~ (1|state) + (1|fips),  
                data = dat, family="binomial")
  fitted(fit)
}

#rounds all values to two decimal places
round2 <- function(x, n=2) {
  # x: data frame 
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}
