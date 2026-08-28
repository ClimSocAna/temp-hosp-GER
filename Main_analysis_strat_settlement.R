#setwd()

library(marginaleffects)
library(fixest)
library(tidyverse)
library(broom.mixed)
library(boot)
library(parallel)

#load data
load("toy_data.RData")
weekcounts_temp <- toy_data

#source functions 
source("functions.R")

#specify temperature categories for calculating marginal effects later on
temp_cats <- c("temp_cat_min","temp_cat_neg6","temp_cat_neg3",
               "temp_cat_zero","temp_cat_3","temp_cat_6",
               "temp_cat_16","temp_cat_18","temp_cat_21","temp_cat_24")

start2 <- Sys.time()

formula_gisd <- c("N_hospitalization ~ relevel(gisd_const, ref='1') * 
                    (temp_cat_min + temp_cat_neg6 + temp_cat_neg3 +
                    temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 +
                    temp_cat_18 + temp_cat_21 + temp_cat_24 +
                    
                    temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 +
                    temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + 
                    temp_cat_16_lag_week1 + temp_cat_18_lag_week1 + temp_cat_21_lag_week1 +
                    temp_cat_24_lag_week1) +

                    geschlecht + age_cat +

                    relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +

                    bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52))+
                    offset(log(N_population.corr)) | year + AGS_N3_21")

#fit model
M1_gisd <-  fepois(fml = as.formula(formula_gisd),
                data = weekcounts_temp,
                split = ~ settlement_bin,
                cluster = ~ AGS_N3_21) 

time2 <- Sys.time() - start2
print(time2) 

# summarise results ------------------------------------------------------------------
M1_gisd.urban <- tidy(M1_gisd[[1]])
M1_gisd.urban$settlement_group <- "urban"
M1_gisd.rural <- tidy(M1_gisd[[2]])
M1_gisd.rural$settlement_group <- "rural"

M1_gisd.clean <- rbind(M1_gisd.urban, M1_gisd.rural)
write.csv(M1_gisd.clean, file="Tables/coef.table_FE_GISD_stratified.csv")

rm(M1_gisd.clean, M1_gisd.rural, M1_gisd.urban)
gc()
 
set.seed(1234)

#assign number of cores
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 1))

#create seperate newdats
newdat <- datagrid(model = M1_gisd[[1]], 
                   year = levels(weekcounts_temp$year), #real data: every 2 years seq(2005,2021,2)
                   AGS_N3_21 = unique(weekcounts_temp$AGS_N3_21),
                   geschlecht = levels(weekcounts_temp$geschlecht),
                   age_cat = levels(weekcounts_temp$age_cat),
                   OAD_ratio_tert = levels(weekcounts_temp$OAD_ratio_tert),
                   Lebenserwartung_tert = levels(weekcounts_temp$Lebenserwartung_tert),
                   gisd_const = levels(weekcounts_temp$gisd_const),
                   week = c(4,12,20,28,36,44,52), #every 2 months essentially
                   N_population.corr=10000)

rm(M1_gisd); gc()

#get prediction by age_cat to use for calculating relative change later
mean(predict(M1_gisd[[1]], newdat),na.rm=T)
mean(predict(M1_gisd[[2]], newdat),na.rm=T)

# run bootstrap -----------------------------------------------------------
boot_res <- boot(data = weekcounts_temp, statistic = boot_fun, R = 1000, newdatasets = newdat,
                 formula = formula_gisd, temp_names = temp_cats, strata_by = "settlement_bin",
                 strata = weekcounts_temp$settlement_bin, #to make sure it samples right
                 parallel = "multicore",
                 ncpus = n_cores)
estimates <- boot_res$t0

# CI
ci_mat <- sapply(seq_along(estimates), function(i) {
  ci <- boot.ci(boot_res, type = "perc", index = i)
  if (!is.null(ci$percent)) {
    c(lower = ci$percent[4], upper = ci$percent[5])
  } else {
    c(lower = NA, upper = NA)
  }
})

# Combine into data frame
ci_df <- as.data.frame(t(ci_mat))

# Create full names in row order
ci_df$estimates <- estimates
ci_df$name <- names(estimates)
ci_df <- ci_df[,c(4,3,1,2)]

write.csv(ci_df, "Tables/cumulativeME_FE_GISDsettlement_bin.csv")