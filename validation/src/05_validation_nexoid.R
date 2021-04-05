
rm(list = ls())
library(tidyverse)
library(data.table)
library(stringr)
library(foreach)
library(maps)     # To get all fips, even those we don't have data for.
library(usmap)    # To create county-level map.
library(ggthemes)
library(ggsci)
library(lubridate)
library(prodlim)  # For finding matching rows.
library(metR)     # For geom_arrow.
library(cowplot)

# Workspace prep ===================================================================================

# Set wd() to make for easy transition to Rmd file later.
setwd("C:/Users/JStarling/repos/covid_risk_score_validation/src")

# Load maps info.
data(county.fips)

# Load data.
all_files = paste0('../data-clean/', list.files('../data-clean/'))
all_files = all_files[!str_detect(all_files, '_all')]

df <- foreach(i=1:length(all_files), .combine=rbind) %do% {
  data.frame(fread(all_files[i]))
}

# Preview data.
summary(df)
dim(df)
head(df)

# Utility function for creating Nexoid labels based on quantiles.
# Nexoid's Low and Lowest are grouped together, as are High and Highest.
nexoid_quantiles <- function(x, probs = c(0, .075, .075+.15, .075+.15+.55, .075+.15+.55+.15, 1)){
  
  qu <- quantile(x, probs=probs)
  labels=c('Low', 'Low', 'Middle', 'High', 'High')
  risk_bands = ifelse(x<=qu[2], labels[1],
                      ifelse(x<=qu[3], labels[2], 
                             ifelse(x<=qu[4], labels[3],
                                    ifelse(x<=qu[5], labels[4], labels[5]))))
  risk_bands = factor(risk_bands, levels=c('Low','Middle','High'))
}


# Scale P(exposure) and P(death).
range(df$api_exposure_risk); range(df$nexoid_risk_infection)
range(df$api_death_risk); range(df$nexoid_risk_mortality)

df <- df %>% mutate(
  nexoid_risk_infection = nexoid_risk_infection/100,
  nexoid_risk_mortality = nexoid_risk_mortality/100,
  death_diff = api_death_risk - nexoid_risk_mortality,
  exp_diff = api_exposure_risk - nexoid_risk_infection,
  death_diff_ratio = api_death_risk / nexoid_risk_infection)

# Calculate number of weeks, since P(exposure) for Nexoid is for all time, and for 19andme is in a week.
nex_weeks = as.double(difftime(max(df$survey_date), min(df$survey_date), unit='weeks'))
df <- df %>% 
  mutate(nexoid_risk_infection_wk = nexoid_risk_infection/nex_weeks) %>%
  mutate(exp_diff_wk = api_exposure_risk - nexoid_risk_infection_wk)

# Keep the last 90-day slice of data only.
df <- df %>% 
  mutate(survey_date = as_date(survey_date)) %>%
  dplyr::filter(survey_date >= max(survey_date)-90)

# Add indicator for whether there are any pre-existing conditions, and the number of conditions.
df <- df %>% mutate(conditions_any = ifelse(conditions=='', 0, 1)) %>%
  rowwise() %>% 
  mutate(conditions_num = sum(is_renal, is_cvd, is_diabetes, is_hyper, is_immune, is_lung, is_smoker, is_other, is_obesity))
  
# Compare P(death) =================================================================================

# Overall comparison -------------------------------------------------------------------------------

# Categorize death rates exceeding 10% in either direction.
df <- df %>% mutate(death_check = factor(ifelse(death_diff>.1, '19andMe 10%+ higher', 
                                                ifelse(-death_diff>.1, '19andMe 10%+ lower', 
                                                       "within 10%")),
                                         levels=c('within 10%', '19andMe 10%+ lower', '19andMe 10%+ higher')))
table(df$death_check)
my_props = round(prop.table(table(df$death_check)),4)
my_props

# Calculate proportions in each category, and spearman ranked correlation coefficient.
spearman_cor = round(cor(df$nexoid_risk_mortality, df$api_death_risk, method='spearman'),3)

# Plot 19andMe versus Nexoid death risks.
ggplot(df, aes(x=nexoid_risk_mortality, y=api_death_risk, colour=death_check)) + 
  geom_point(alpha=.5) + 
  coord_cartesian(xlim=c(0,.75), ylim=c(0,.75)) + 
  geom_abline(intercept=0, slope=1, colour='red') + 
  labs(x='Nexoid', y='19andMe', title='P(Death)') + 
  scale_colour_colorblind(name='') +
  labs(x='Nexoid', y='19andme', 
       title=paste0('P(Death): Spearman ranked corr ', spearman_cor))+
  theme_bw()

# Note: The ratios really don't seem to be the way to go here.  If you look at the colour here 
# for ratio more than two, it captures a lot of small values where the ratio is large, but practical
# magnitude of the difference is very small.  (I.e. P(death)=.8 versus .4 - we wouldn't care about this.)
ggplot(df, aes(x=nexoid_risk_mortality, y=api_death_risk, colour=factor(abs(death_diff_ratio)>2))) + 
  geom_point(alpha=.5) + 
  coord_cartesian(xlim=c(0,.75), ylim=c(0,.75)) + 
  geom_abline(intercept=0, slope=1, colour='red') + 
  labs(x='Nexoid', y='19andMe', title='P(Death)') + 
  scale_colour_colorblind(name='Ratio of death rates >2') +
  labs(x='Nexoid', y='19andme', 
       title=paste0('P(Death): Spearman ranked corr ', spearman_cor))+
  theme_bw()


# Check patterns in covariates ---------------------------------------------------------------------

# Age
ggplot(df %>% mutate(age_cat = factor(ifelse(age<60, 'Under 60', age), levels=c('Under 60', '60', '70', '80'))), 
       aes(x=nexoid_risk_mortality, y=api_death_risk, colour=death_check)) + 
  geom_point() + 
  coord_cartesian(xlim=c(0,.75), ylim=c(0,.75)) + 
  geom_abline(intercept=0, slope=1, colour='red') + 
  labs(x='Nexoid', y='19andMe', title='Mortality risk by age.') + 
  facet_wrap(~age_cat, ncol=2) + 
  scale_colour_colorblind() + 
  theme_bw()

# The 10% diff cases show up only in the 60+ population.  Let's dig in.
df_60plus = df %>% filter(age>=60)

# Let's see if race plays a factor.
ggplot(df_60plus, aes(x=race)) + 
  geom_bar() + 
  facet_wrap(~death_check,ncol=1, scales='free_y') + 
  theme_bw()

# Conclusion:
# The cases where 19andme is estimating lower are all black patients over 60.
xtabs(~age+death_check, df %>% filter(race=='black'))


# For the 'lower than 10%' patients, what happens if you find similar patients who are not black in the Nexoid dataset, and
# see how their risk estimates change?

temp <- df_60plus %>% 
  mutate(nppl_range = cut(nppl, breaks=c(0,5,10,20,100), include.lowest=T),
         nppl2_range = cut(nppl2, breaks=c(0,5,10,20,100), include.lowest=T)) %>%
  select(person, age, sex, conditions_any, race, symptoms, nppl_range, is_roommate, nppl2_range, hand, ppe, api_death_risk, nexoid_risk_mortality, death_check)

temp1 <- temp %>% filter(death_check=='19andMe 10%+ lower')
temp2 <- temp %>% filter(death_check=='within 10%', race!='black') 
match = row.match(temp1 %>% select(-person,  -api_death_risk, -nexoid_risk_mortality, -death_check, -race), 
                  temp2 %>% select(-person,  -api_death_risk, -nexoid_risk_mortality, -death_check, -race))
temp1$nexoid_death_risk_similar_person = temp2$nexoid_risk_mortality[match]
temp1$diff_adj = temp1$api_death_risk - temp1$nexoid_death_risk_similar_person

ggplot(temp1, aes(x=nexoid_risk_mortality, y=api_death_risk)) +
  geom_point(data=df, colour='grey80', alpha=.2) + 
  geom_point(size=2, colour='#E69F00') +
  geom_point(aes(x=nexoid_death_risk_similar_person, colour=factor(diff_adj>.1)), size=2) + #), colour='#56B4E9', size=2) +
  coord_cartesian(xlim=c(0,.75), ylim=c(0,.75)) +
  geom_abline(intercept=0, slope=1, colour='red') +
  geom_segment(aes(x=.45, xend=.3, y=.37, yend=.37), 
               arrow = arrow(length = unit(0.25, "cm")), colour='black') +
  labs(x='Nexoid', y='19andMe', title='P(Death)') +
  scale_colour_manual(name='', values=c('#000000', '#56B4E9')) +
  guides(colour=F) + 
  labs(x='Nexoid', y='19andme',
       title=paste0('P(Death): Spearman ranked corr ', spearman_cor),
       subtitle=c('Nexoid P(death) for similar, non-black patients.')) +
  theme_bw()



# What about the cases where 19andme is estimating higher death rates?
xtabs(~age+death_check+race, df)

# Most patients in this category are white, and the race distribution is similar to the non-descrepancy cases.
# Let's continue investigating.

# Number of pre-existing conditions looks to matter - higher for patients where there are 10%+ differences.
ggplot(df_60plus, aes(x=factor(conditions_num))) + 
  geom_bar() + 
  facet_wrap(~death_check, ncol=1, scales='free_y') +
  labs(x='Number of pre-existing conditions', y='Count', title='Pre-existing conditions for patients age 60 or older.') + 
  theme_bw()

# Working does not look impactful.
ggplot(df_60plus, aes(x=working)) + 
  geom_bar() + 
  facet_wrap(~death_check, ncol=1, scales='free_y') +
  labs(x='Employment status', y='Count', title='Employment status for patients age 60 or older.') + 
  theme_bw()

# Covid symptoms.  Test for different proportions of symptoms.
x = c(sum(df_60plus$death_check=='within 10%' & df_60plus$symptoms=="is_other"), sum(df_60plus$death_check=='19andMe 10%+ higher' & df_60plus$symptoms=="is_other"))
n = c(sum(df_60plus$death_check=='within 10%'), sum(df_60plus$death_check=='19andMe 10%+ higher'))
pval = prop.test(x=x, n=n)$p.value


ggplot(df_60plus, aes(x=death_check, fill=factor(symptoms))) +
  geom_bar(position='fill') + 
  scale_fill_manual(name='Symptoms', values=c('grey80', 'grey20'), labels=c('none', 'is_other')) + 
  labs(x='', y='Count', title='Covid-19 Symptoms', subtitle='(p<0.001 for differences)') +
  theme_bw()

# Let's investigate the specific pre-existing conditions to see if we can narrow it down to a particular
# one which looks different among the risk diff bands in the over-60 crowd.
plot_grid(
  # Heart disease (is_cvd)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_cvd))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Heart disease', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Diabetes (is_diabetes)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_diabetes))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Diabetes', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Hypertension (is_hyper)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_hyper))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Hypertension', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Kidney disease (is_renal)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_renal))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Kidney disease', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Immune disease (is_immune)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_immune))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Immune disease', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Lung disease (is_lung)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_lung))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Lung disease', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Obesity (is_obesity)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_obesity))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Obesity', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  # Smoker (is_smoker)
  ggplot(df_60plus, aes(x=death_check, fill=factor(is_smoker))) +
    geom_bar(position='fill') + 
    scale_fill_manual(name='Smoker', values=c('grey80', 'grey20'), labels=c('No','Yes')) + 
    labs(x='', y='Count') +
    theme_bw(),
  ncol=2
)
  

# P(death) Conclusions:
# 1. For patients under 60, all estimated death risks are within 10% for 19andMe vs Nexoid.
# 2. For patients over 60, 19andMe is giving lower estimates for Black patients, as we do not consider race.
# 3. For patients over 60, 19andMe is estimating higher death risks when there are multiple pre-existing conditions or 
#    when there are symptoms.  (is_other is only symptom we are able to check against.)

# Compare P(exposure) =================================================================================


# Overall comparison -------------------------------------------------------------------------------

# Nexoid is looking at cumulative risk over all time, whereas 19andMe is looking at risk in a week.
# Try comparing adjusted Nexoid risk.

# Categorize death rates exceeding 10% in either direction.
df <- df %>% mutate(exp_check = factor(ifelse(exp_diff_wk>.01, '19andMe 1%+ higher', 
                                                ifelse(-exp_diff_wk>.01, '19andMe 1%+ lower', 
                                                       "within 1%")),
                                         levels=c('within 1%', '19andMe 1%+ lower', '19andMe 1%+ higher')))
table(df$exp_check)
my_props = round(prop.table(table(df$exp_check)),4)
my_props

# Calculate proportions in each category, and spearman ranked correlation coefficient.
spearman_cor = round(cor(df$nexoid_risk_infection_wk, df$api_exposure_risk, method='spearman'),3)

# Plot 19andMe versus Nexoid exposure risks.
ggplot(df, aes(x=nexoid_risk_infection_wk, y=api_exposure_risk, colour=exp_check)) + 
  geom_point(alpha=.5) + 
  coord_cartesian(xlim=c(0,.15), ylim=c(0,.15)) + 
  geom_abline(intercept=0, slope=1, colour='red') + 
  labs(x='Nexoid', y='19andMe', title='P(Exposure)') + 
  scale_colour_colorblind(name='') +
  labs(x='Nexoid', y='19andme', 
       title=paste0('P(Exposure): Spearman ranked corr ', spearman_cor))+
  theme_bw()

print(my_props)


#- The 1% where 19andme is over 1% higher ----------------------------------------------------------

# Number of primary contacts 
ggplot(df, aes(x=nppl)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept=9), colour='red') +
  geom_vline(aes(xintercept=17), colour='red') +
  facet_wrap(~ifelse(exp_check=="19andMe 1%+ higher","19andMe 1%+ higher","Other"), ncol=1, scales="free_y") + 
  theme_bw()

# Number of secondary contacts.
ggplot(df, aes(x=nppl2)) + 
  geom_histogram() + 
  facet_wrap(~ifelse(exp_check=="19andMe 1%+ higher","19andMe 1%+ higher","Other"), ncol=1, scales="free_y") + 
  theme_bw()

# Handwashing and PPE proportions.
df %>% dplyr::group_by('group'=exp_check) %>%
  dplyr::summarise(hand=sum(hand)/n(), ppe=sum(ppe)/n()) %>%
  pivot_longer(-group, names_to='variable', values_to='prop') %>%
  ggplot(aes(x=variable, y=prop)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~group) + 
  labs(x='', y='Proportion following CDC guidelines') +
  theme_bw()
  
#- The 19% where 19andme is over 1% lower ----------------------------------------------------------

# Symptoms raises Nexoid exposure risk by 73%.  This seems strange that symtpoms would modify P(exposure).
df %>% dplyr::group_by('group'=ifelse(exp_check=="19andMe 1%+ lower","19andMe 1%+ lower","Other")) %>%
  dplyr::summarise(covid_symptoms = sum(is_other)/n(), 
                   health_worker=sum(health_worker)/n(),
                   nursing_home=sum(nursing_home)/n(),
                   public_transit=sum(public_transport_count>0)/n(),
                   working_outside_home=sum(working %in% c('travel critical', 'travel non critical'))/n(),
                   conditions = sum(is_diabetes | is_renal | is_lung | is_cvd) / n()
                   ) %>%
  pivot_longer(-group, names_to='variable', values_to='prop') %>%
  ggplot(aes(x=group, y=prop)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~variable,ncol=2) + 
  labs(x='Exposure risk group', y='Proportion') +
  theme_bw()  


