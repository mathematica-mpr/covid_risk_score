### This script prepares Nexoid data for input to the 19andme API.
### Data source:  ./data-wrangling-nexoid/api-output/
### This set of files is too large for the github repo, but is available in the
### transfer folder:  N:/Transfer/JStarling/19andme/api-output

### All the API outputs are located here: N:\Transfer\JStarling\19andme\api_output

### "csv_part_##.csv" are the 71 parts which make up the api results information for if I 
### could assign one api country output to the person. 

### "two_results_no_match.csv" is a data frame with results information for people who have 
### two county fips returned and neither of the counties returned matches the person's nexoid fips. 
### Each person appears twice in the data frame

### "all_api_output.csv" is "csv_part_##.csv" and "two_results_no_match.csv" combined

### "error_message.csv" is a data frame with the person number and the API error message returned

##############################################################################
### Workspace prep.
##############################################################################

rm(list=ls())
library(tidyverse)
library(data.table)
library(stringr)
library(foreach)

# File for the API inputs.  This is the Nexoid data, with columns for the 19andme mappings.
file_api_inputs = paste0('data-wrangling-nexoid/nexoid_data_forapi.csv')

# Files for the API outputs.
files_all = paste0('data-wrangling-nexoid/api_output/', list.files('data-wrangling-nexoid/api_output/'))

files_no_issues = files_all[str_detect(files_all, 'csv_part_')]
files_two_results = files_all[str_detect(files_all, 'two_results')]
files_errs = files_all[str_detect(files_all, 'err')]

##############################################################################
# Data read and clean.
##############################################################################

#-----------------------------------------------------------------------------
# Read all data files.
#-----------------------------------------------------------------------------

# Read the Nexoid data file which corresponds to what was input into the 19andme API.
df_api_inputs = data.frame(fread(file_api_inputs))

# Read observations with no errors or mis-matches from API.
df_api = foreach(i=1:length(files_no_issues), .combine=rbind) %do% {
  data.frame(fread(files_no_issues[i]))
}

# Read observations with two rows returned, or errors, from API.
df_api_two_results = data.frame(fread(files_two_results))
df_api_errs = data.frame(fread(files_errs))  
  
dim(df_api_inputs)
dim(df_api) + dim(df_api_two_results) + dim(df_api_errs)  
# These row counts likely will not match; we removed observations where the Nexoid zip does not exist in the 19andme API.

#-----------------------------------------------------------------------------
# Investigate and clean up records with error messages.
#-----------------------------------------------------------------------------

# All API errors are bad zip code requests;  remove these people from the df_api_inputs dataframe.
table(df_api_errs$message)
people_to_remove <- df_api_errs$person
df_api_inputs <- df_api_inputs %>% dplyr::filter(!(person %in% people_to_remove))

#-----------------------------------------------------------------------------
# Investigate and clean up records with two matches, where 
# neither fips matches the Nexoid fips.  Each person appears in this file twice.
#-----------------------------------------------------------------------------

# Number of people impacted.
dim(df_api_two_results)[1] / 2
people_to_remove <- unique(df_api_two_results$person)
df_api_inputs <- df_api_inputs %>% dplyr::filter(!(person %in% people_to_remove))

##############################################################################
# Combine Nexoid API inputs and API output.
##############################################################################

dim(df_api_inputs)
dim(df_api)

# Append API output colnames with "api" for easy identification.
colnames(df_api)[which(colnames(df_api)!='person')] = paste0('api_', colnames(df_api)[which(colnames(df_api)!='person')])

# We will start with everybody that we have API output for, and pull in the input
# information.
df_comb <- left_join(df_api, df_api_inputs, by='person')

# Limit range of scores to (1,100). (Until API patch in)
df_comb$api_score = ifelse(df_comb$api_score<1, 1, 
                           ifelse(df_comb$api_score>100, 100, df_comb$api_score))

# Remove any records with NA nexoid risk scores.
df_comb <- df_comb %>% filter(!is.na(nexoid_risk_infection), !is.na(nexoid_risk_mortality))

# Write output.
fwrite(df_comb, 'data-clean/data_clean_nexoid_all.csv', row.names=F)

# For github storage, split output into separate data files.  (Try 20 files.)
setDT(df_comb)  # Convert df to data.table.

# Create variable for splitting into 10 output files.
df_comb$file_num = rep(1:10, each=ceiling(nrow(df_comb)/10))[1:nrow(df_comb)]

# Save individual files.
df_split = split(df_comb, f=df_comb$file_num)
lapply(names(df_split), function(x){ fwrite(df_split[[x]], file=paste0("data-clean/data_clean_nexoid_", x, ".csv"))})
  
