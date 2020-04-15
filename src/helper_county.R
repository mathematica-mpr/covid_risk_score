library(shiny)
library(tidyverse)
library(tidycensus)
library(assertr)
library(flexdashboard)
library(httr)

httr::set_config(config(ssl_verifypeer = 0L))

#Population data from census
census_pop<-read_csv("data/county_pop.csv")
#FIPS data from census
data("fips_codes")
fips_codes<-fips_codes%>%
  mutate(fips = paste0(state_code, county_code))

#Read in NYT covid-19 county-level data
#NYT county-level data
nyt_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
GET(nyt_url, write_disk("data/us-counties.csv", overwrite=TRUE))

df <- read_csv("data/us-counties.csv") %>%
  mutate(fips = case_when(county == "New York City" & state == "New York" ~ "36061",
                          county == "Kansas City" & state == "Missouri" ~ "29095",
                          TRUE ~ fips))%>%
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
us_death_today<-df_ls[[latest_day%>%as.character()]]%>%pull(deaths)%>%sum(na.rm = T)
us_case_13d<-df_ls[[(latest_day-13)%>%as.character()]]%>%pull(cases)%>%sum(na.rm = T)

###########county specific############3
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
  res<-census_pop%>%
    filter(fips==key)%>%
    pull(POPESTIMATE2019)
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
  if(length(res)==1){
    return(res)
  }else{
    return(max(res, na.rm = TRUE))
  }
  
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
  if(length(res)==1){
    return(res)
  }else{
    return(max(res, na.rm = TRUE))
  }
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
  #calculate underreporting factor as cfr/true mortality
  fac_underreport <- n_death_today/n_case_13d/true_mortality_rate
  #if cfr is zero, the underreporting factor is NA, then force it to be US average
  us_average<-us_death_today/us_case_13d/true_mortality_rate
  return(case_when(fac_underreport==0 ~ us_average,
                   is.na(fac_underreport) ~ us_average,
                   fac_underreport>10 ~ 10,
                   TRUE ~ fac_underreport))
}
assertthat::assert_that(calc_county_underreport("06001")>=1)
assertthat::assert_that(calc_county_underreport("36067")>=1)
assertthat::assert_that(calc_county_underreport("30031")>=1)

get_fips_from_zip<-function(zip){
  #HUD_API_KEY <- Sys.getenv("HUD_API_KEY")
  HUD_API_KEY <- paste(readLines("doc/HUD_API_KEY.txt"), collapse=" ")
  query<-paste0("https://www.huduser.gov/hudapi/public/usps?type=2&query=", zip)
  resp<-GET(url = query, add_headers(Authorization = paste("Bearer", HUD_API_KEY)))
  result <- httr::content(resp, as = "parsed")
  fips<-sapply(result$data$results, '[[', 'geoid')
  return(fips)
}
