solve_for_p_female <- function(OR, p_overall){
  if (p_overall==0){return(0)}
  # solve for female suspetibility rate using male/female OR and overall suseptibility 
  
  # p = .5(p_m+p_f) -> p_m = 2p - p_f
  # OR = (2p-p_f)(1-p_f) / (1-2p+p_f)p_f
  # rearrange to quadratic to get the following coefficients
  
  A = OR - 1
  B = OR*(1-2*p_overall) + (1+2*p_overall)
  C = -2*p_overall
  
  p_female = ( -1*B + sqrt(B^2 - 4*A*C) )/(2*A)
  
  # chk 
  p_male = 2*p_overall - p_female
  stopifnot(all.equal(OR, risk2odds(p_male)/risk2odds(p_female)))
  
  return(p_female)
}

hosp_list_female <- map_dbl(hosp_list, solve_for_p_female, OR=male_or[1])
icu_list_female <- map_dbl(icu_list, solve_for_p_female, OR=male_or[2])
death_list_female <- map_dbl(death_list, solve_for_p_female, OR=male_or[3])


if (F){
  # check how the rates differ by sex and from overall
  hosp_list_male = odds2risk(risk2odds(hosp_list_female) * male_or[1])
  hosp_list_male2 = 2*hosp_list - hosp_list_female
  rbind(hosp_list_female, hosp_list, hosp_list_male, hosp_list_male2)
  
  icu_list_male = odds2risk(risk2odds(hosp_list_female) * male_or[2])
  icu_list_male2 = 2*hosp_list - hosp_list_female
  rbind(icu_list_female, hosp_list, hosp_list_male, hosp_list_male2)
  
  death_list_male = odds2risk(risk2odds(hosp_list_female) * male_or[3])
  death_list_male2 = 2*hosp_list - hosp_list_female
  rbind(death_list_female, hosp_list, hosp_list_male, hosp_list_male2)
}
