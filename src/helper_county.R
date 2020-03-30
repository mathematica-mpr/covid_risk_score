#load dependencies
# if(!require(pacman)){
#   install.packages('pacman')
# }
# library(pacman)
#install.packages(c("shiny", "tidyverse", "tidycensus", "assertr", "flexdashboard"))
library(shiny)
library(tidyverse)
library(tidycensus)
library(assertr)
library(flexdashboard)
census_api_key("341b9e8939115fe9fcd94897d70d826fe1b945be")

#FIPS data from censue
data("fips_codes")
fips_codes<-fips_codes%>%
  mutate(fips = paste0(state_code, county_code))

#Read in NYT covid-19 county-level data
# path is relative to app.R
dir <- "data/covid-19-data/"
#NYT county-level data
df<-read_csv(file.path(dir, "us-counties.csv"))%>%
  select(-c(state,county))

latest_day = df$date%>%max()

#utiity function
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}


augment_nyt_covid<-function(df){
  #Augment NYT data to a list of data organized by date
  start_date <- range(df$date)[1]
  end_date<- range(df$date)[2]
  df_ls<-#repeat fips code df N times, N equals number of days since 2020-01-21
    purrr::map_dfr(seq_len(as.numeric(end_date-start_date)+1), ~fips_codes)%>%
    mutate(date = rep(seq.Date(start_date, end_date, by= 'day'), each = dim(fips_codes)[1]))%>%
    #join NYT data to the repeated fips data by fips and date
    left_join(df, by = c("date", "fips"))%>%
    #recode NA as zero, cuz if not in NYT data then county has zero
    mutate(cases = if_else(is.na(cases), 0, cases),
           deaths = if_else(is.na(deaths), 0, deaths))%>%
    #check the number of cases is greater than number of deaths, or both are zero
    verify((cases>deaths)|(deaths == 0 && cases ==0))%>%
    named_group_split(date)
  return(df_ls)
}

df_ls<-augment_nyt_covid(df)

get_county_name<-function(key){
  #Looks up county name
  #input
  ##key: string, five digit
  #output
  ##ctyname: string, county name
  
  res<-fips_codes%>%
    filter(fips==key)%>%
    select(county, state)%>%
    paste(., collapse = ', ')
  return(res)
}
assertthat::assert_that(get_county_name("06001")=="Alameda County, CA")

get_county_pop<-function(key){
  #looks up county population
  #input
  ##key: string, five digit
  #output:
  ##ctypop: numeric, county population
  census_pop<-get_estimates(geography = "county", product = "population")%>%
    filter(variable == "POP")
  res<-census_pop%>%
    filter(GEOID==key)%>%
    pull(value)
  return(res)
}
assertthat::assert_that(get_county_pop("06001")>1.66*10^6)

get_county_casecount<-function(key, date){
  #looks up county case count on a given date
  #input
  ##key: string, five digit
  ##date: Date, "YYYY-MM-DD"
  #output
  ##casecount: numeric, county case count on a given day

  res<-df_ls[[as.character(date)]]%>%
    filter(fips == key)%>%
    pull(cases)
  return(res)
}
assertthat::assert_that(get_county_casecount("06001", as.Date("2020-03-27"))>220)

get_county_deathcount<-function(key, date){
  #looks up county death count on a given date
  #input
  ##key: string, five digit
  ##date: Date, "YYYY-MM-DD"
  #output
  ##deathcount: numeric, county death count on a given day
  res<-df_ls[[as.character(date)]]%>%
    filter(fips == key)%>%
    pull(deaths)
  return(res)
}
assertthat::assert_that(get_county_deathcount("06001", as.Date("2020-03-27"))>1)
assertthat::assert_that(get_county_deathcount("06001", as.Date("2020-03-27"))<
                          get_county_casecount("06001", as.Date("2020-03-27")-13))

calc_county_underreport<-function(fips){
  #calculate the underreporting factor using case fatality rate, following methods in (TW Russell, 2020)
  true_mortality_rate <-0.0138 # a big study from China
  #number of death today
  n_death_today<-get_county_deathcount(fips, latest_day)
  #number of infected cases 13 days ago
  n_case_13d<-get_county_casecount(fips, latest_day-13)
  #calculate case fatality rate
  cfr <- n_death_today/n_case_13d
  #calculate underreporting rate
  fac_underreport<-true_mortality_rate/cfr
  #if cfr is zero, the underreporting factor is NA, then force it to be US average, 0.17.
  return(if_else(!is.na(fac_underreport), fac_underreport, 0.17))
}
assertthat::assert_that(calc_county_underreport("06001")<=1)
assertthat::assert_that(calc_county_underreport("36067")<=1)


get_fips_from_zip<-function(zip){
  #get FIPS code given zip code, using crosswalk from census
  crosswalk<-"https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt"%>%
    read.table(header=T, sep=",")%>%
    select(ZCTA5, GEOID)%>%
    mutate_all(.funs = stringr::str_pad, width = 5, pad = "0")
  fips<-crosswalk%>%
    filter(ZCTA5 == zip)%>%
    pull(GEOID)
  return(fips)
}


