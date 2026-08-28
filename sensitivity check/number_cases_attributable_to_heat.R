#setwd()

library(fixest)
library(splines2)
library(tidyverse)
library(broom.mixed)

# calculate the number of cases attributable to heat
#based on counterfactual prediction  -

#load data
load("toy_data.RData")
weekcounts_temp <- toy_data

#load functions
source("functions.R")

start2 <- Sys.time()

#model formulas main and for age
formula_main <- c("N_hospitalization ~ relevel(gisd_const, ref='1') * (temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                  temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                  temp_cat_18 + temp_cat_21 + temp_cat_24 +
                  
                  temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                  temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                  temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1) +
                  
                  geschlecht + age_cat + 
                  relevel(gisd_const, ref='1') + relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +
                  bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52)) +
                  offset(log(N_population.corr))| year + AGS_N3_21")

formula_age <- c("N_hospitalization ~ relevel(gisd_const, ref='1') * (temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                  temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                  temp_cat_18 + temp_cat_21 + temp_cat_24 +
                  
                  temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                  temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                  temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1) +
                  
                  geschlecht + 
                  relevel(gisd_const, ref='1') + relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +
                  bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52)) +
                  offset(log(N_population.corr))| year + AGS_N3_21")

#fit main model, and by settlement and age
M1.2 <-  fepois(fml = as.formula(formula_main),
                data = weekcounts_temp,
                cluster = ~ AGS_N3_21) #is county only cluster or both cluster and FE?
M1.2_settlement <- fepois(fml = as.formula(formula_main),
                data = weekcounts_temp,
                split = ~ settlement_bin,
                cluster = ~ AGS_N3_21) #is county only cluster or both cluster and FE?
M1.2_agegroup <- fepois(fml = as.formula(formula_age),
                data = weekcounts_temp,
                split = ~ age_cat,
                cluster = ~ AGS_N3_21) #is county only cluster or both cluster and FE?

##counterfactual prediction
## predict on data that has no days above 24°C
newdats <- weekcounts_temp %>% mutate(temp_cat_24 = 0, temp_cat_24_lag_week1 = 0)

## calculate excess cases total, by gisd categories, by settlement and by age
excess_total <- cf_pred(weekcounts_temp, newdats, M1.2)
excess_bygisd <- cf_pred(weekcounts_temp, newdats, M1.2, by_gisd=TRUE)
excess_settlement <- cf_pred(weekcounts_temp, newdats, M1.2_settlement, by_gisd=TRUE, strata="settlement_bin")
excess_age_cat <- cf_pred(weekcounts_temp, newdats, M1.2_agegroup, by_gisd=TRUE, strata="age_cat")

#rename strata
excess_settlement <- excess_settlement %>% rename(strata = settlement_bin)
excess_age_cat <- excess_age_cat %>% rename(strata = age_cat)

#combine all into one excess table
excess <- bind_rows(excess_total, excess_bygisd, excess_settlement, excess_age_cat)
excess$strata <- ifelse(is.na(excess$strata), "Total", as.character(excess$strata))
excess$gisd_const <- ifelse(is.na(excess$gisd_const), "Total", as.character(excess$gisd_const))

#make sure GISD and strata factor orderings are correct
excess$gisd_const <- factor(excess$gisd_const, levels = c("Total", "1", "2", "3"),
                        labels =  c("Total", "Low", "Medium", "High"))
excess$strata <- factor(excess$strata, levels = c("Total", "rural", "urban",
                                                  "<30 years", "30 to 65 years", ">65 years"))

#get the average across all years
excess_avg <- excess %>% 
  group_by(strata, gisd_const) %>%
  summarise(avg = round(mean(total_excess),2),
            min = paste0(round(min(total_excess),2)," (",year[which.min(total_excess)],")"),
            max = paste0(round(max(total_excess),2)," (",year[which.max(total_excess)],")"))

#save table
write.csv(excess_avg, file="Tables/N_attributable_heat_cutoff_above24degrees.csv")
