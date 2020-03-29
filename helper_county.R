get_county_name<-function(fips){
  #Looks up county name
  #input
  ##fips: string, five digit
  #output
  ##ctyname: string, county name
  
  
  
}

get_county_pop<-function(fips){
  #looks up county population
  #input
  ##fips: string, five digit
  #output:
  ##ctypop: numeric, county population
  
}

get_county_casecount<-function(fips, date){
  #looks up county case count on a given date
  #input
  ##fips: string, five digit
  ##date: Date, "YYYY-MM-DD"
  #output
  ##casecount: numeric, county case count on a given day
}

get_county_deathcount<-function(fips, date){
  #looks up county death count on a given date
  #input
  ##fips: string, five digit
  ##date: Date, "YYYY-MM-DD"
  #output
  ##casecount: numeric, county death count on a given day
}

calc_county_underreport<-function(fips){
  #calculate the underreporting factor using case fatality rate, following methods in (, 2020)
  true_mortality_rate <-0.0158 # a big study from China
  #number of death today
  n_death_today<-get_county_deathcount(fips, Sys.Date())
  #number of infected cases 14 days ago
  n_case_14d<-get_county_casecount(fips, Sys.Date()-14)
  #calculate case fatality rate
  cfr <- n_death_today/n_case_14d
  #calculate underreporting rate
  fac_underreport<-true_mortality_rate/cfr
  return(fac_underreport)
}


  
}

