#setwd()

library(marginaleffects)
library(fixest)
library(splines2)
library(tidyverse)
library(broom.mixed)
library(boot)
library(parallel)

# for running this analyses on HPC -------------------------------------------
#read in some stuff from slurm so we can use the same script for multiple checks 
#N_lags 
N_lag <- Sys.getenv("N_lag")

#default is 1 (main analysis)
if (N_lag == "") {N_lag <- "1"}

#temp def 
temp_def <- Sys.getenv("temp_def")

#default is mean (main analysis)
if (temp_def == "") {temp_def <- "mean"}

#strata 
strata <- Sys.getenv("strata")

#default is none (main analysis)
if (strata == "") {strata <- "none"}

#load data
load("toy_data.RData")
weekcounts_temp <- toy_data

#load functions
source("code/functions.R")

# check fit ---------------------------------------------------------------
#original model formula
formula_gisd_org <- c("N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 +
                                                                           temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 +
                                                                           temp_cat_18 + temp_cat_21 + temp_cat_24 +

                                                                           temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 +
                                                                           temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 +
                                                                           temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +

                     geschlecht + age_cat +

                     relevel(gisd_const, ref='1') + relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +

                     bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52))+
                     offset(log(N_population.corr)) | year + AGS_N3_21")

#formula with 2 week lags
formula_gisd_2l <- c("N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 +
                                                                           temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 +
                                                                           temp_cat_18 + temp_cat_21 + temp_cat_24 +

                                                                           temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 +
                                                                           temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 +
                                                                           temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                                                                           
                                                                           temp_cat_min_lag_week2 + temp_cat_neg6_lag_week2 + temp_cat_neg3_lag_week2 +
                                                                           temp_cat_zero_lag_week2 + temp_cat_3_lag_week2 + temp_cat_6_lag_week2 + temp_cat_16_lag_week2 +
                                                                           temp_cat_18_lag_week2 + temp_cat_21_lag_week2 + temp_cat_24_lag_week2 +

                     geschlecht + age_cat +

                     relevel(gisd_const, ref='1') + relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +

                     bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52))+
                     offset(log(N_population.corr)) | year + AGS_N3_21")

#formula with 3 week lags
formula_gisd_3l <- c("N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 +
                                                                           temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 +
                                                                           temp_cat_18 + temp_cat_21 + temp_cat_24 +

                                                                           temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 +
                                                                           temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 +
                                                                           temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                                                                           
                                                                           temp_cat_min_lag_week2 + temp_cat_neg6_lag_week2 + temp_cat_neg3_lag_week2 +
                                                                           temp_cat_zero_lag_week2 + temp_cat_3_lag_week2 + temp_cat_6_lag_week2 + temp_cat_16_lag_week2 +
                                                                           temp_cat_18_lag_week2 + temp_cat_21_lag_week2 + temp_cat_24_lag_week2 + 
                                                                           
                                                                           temp_cat_min_lag_week3 + temp_cat_neg6_lag_week3 + temp_cat_neg3_lag_week3 +
                                                                           temp_cat_zero_lag_week3 + temp_cat_3_lag_week3 + temp_cat_6_lag_week3 + temp_cat_16_lag_week3 +
                                                                           temp_cat_18_lag_week3 + temp_cat_21_lag_week3 + temp_cat_24_lag_week3 +

                     geschlecht + age_cat +

                     relevel(gisd_const, ref='1') + relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +

                     bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52))+
                     offset(log(N_population.corr)) | year + AGS_N3_21")

# decide on which model formula to use based on slurm input ------------------------------------
if (tolower(temp_def) == "mean") {
  
  ## stuff we need later for the function
  temp_cats <- c("temp_cat_min","temp_cat_neg6","temp_cat_neg3",
                  "temp_cat_zero","temp_cat_3","temp_cat_6",
                  "temp_cat_16","temp_cat_18","temp_cat_21","temp_cat_24")
  
  if (tolower(N_lag) == "1") {
  model_formula <- formula_gisd_org
  } else if (tolower(N_lag) == "2") {
    model_formula <- formula_gisd_2l
  } else if (tolower(N_lag) == "3") {
    model_formula <- formula_gisd_3l
  } else {
    stop("N_lag wrongly defined")
  }
} else if (tolower(temp_def) == "max") {
  ## stuff we need later for the function
  temp_cats <- c("temp_cat_maxt_min","temp_cat_maxt_neg6","temp_cat_maxt_neg3",
                  "temp_cat_maxt_zero","temp_cat_maxt_3","temp_cat_maxt_6",
                  "temp_cat_maxt_16","temp_cat_maxt_18","temp_cat_maxt_21","temp_cat_maxt_24")
  
  #replace mean temp with max temp
  model_formula <- c("N_hospitalization ~ temp_cat_maxt_min + temp_cat_maxt_neg6 + temp_cat_maxt_neg3 +
                                                                           temp_cat_maxt_zero + temp_cat_maxt_3 + temp_cat_maxt_6 + temp_cat_maxt_16 +
                                                                           temp_cat_maxt_18 + temp_cat_maxt_21 + temp_cat_maxt_24 +

                                                                           temp_cat_maxt_min_lag_week1 + temp_cat_maxt_neg6_lag_week1 + temp_cat_maxt_neg3_lag_week1 +
                                                                           temp_cat_maxt_zero_lag_week1 + temp_cat_maxt_3_lag_week1 + temp_cat_maxt_6_lag_week1 + temp_cat_maxt_16_lag_week1 +
                                                                           temp_cat_maxt_18_lag_week1 + temp_cat_maxt_21_lag_week1 + temp_cat_maxt_24_lag_week1 +

                     geschlecht + age_cat +

                     relevel(gisd_const, ref='1') + relevel(OAD_ratio_tert, ref='(30.7,34.4]') + relevel(Lebenserwartung_tert, ref='(78.5,82.5]') +

                     bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52))+
                     offset(log(N_population.corr)) | year + AGS_N3_21")
  } else {
  stop("temp_def wrongly defined")
}


# fit model -------------------------------------------------------------------------
M1_gisd <-  fepois(fml = as.formula(model_formula),
                                       data = weekcounts_temp,
                                       cluster = ~ AGS_N3_21) 
# calculate marginal coefs ------------------------------------------------------------
#code is simpler than for the main analysis because we are not aggregating over
#all combinations of the covariates and are showing relative effects
slopes_FE <- vector("list", length(temp_cats))

for (t in 1:length(temp_cats)) {
  
  if (tolower(N_lag) == "1") {
    hyp_string <- paste0(temp_cats[t], " + ",
                         temp_cats[t], "_lag_week1 = 0")
    
  } else if (tolower(N_lag) == "2") {
    hyp_string <- paste0(temp_cats[t], " + ",
                         temp_cats[t], "_lag_week1 + ",
                         temp_cats[t], "_lag_week2 = 0")
    
  } else if (tolower(N_lag) == "3") {
    hyp_string <- paste0(temp_cats[t], " + ",
                         temp_cats[t], "_lag_week1 + ",
                         temp_cats[t], "_lag_week2 + ",
                         temp_cats[t], "_lag_week3 = 0")
    
  } else {
    stop("N_lag wrongly defined")
  }
  
  slopes_FE[[t]] <- hypotheses(M1_gisd, hyp_string)
  slopes_FE[[t]]$term <- temp_cats[t]
  
  print(paste("Finished temp_cat:", t))
  gc()
}

slopes_FE_df <- do.call(rbind, slopes_FE)

write.csv(slopes_FE_df, paste0("Tables/check_tempdef_",temp_def, "lag_", N_lag, "ME.csv"))
rm(slopes, slopes_FE);gc()

# cumulative coefficients ----------------------------------------------------------
rm(M1_gisd); gc()
set.seed(1234)

#assign number of cores
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 1))

# run bootstrap -----------------------------------------------------------
boot_res <- boot(data = weekcounts_temp, statistic = boot_fun, R = 1000, newdatasets = newdat,
                 formula = model_formula, temp_names = temp_names, lags = N_lag,
                 by_gisd = F,
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

write.csv(ci_df, paste0("Tables/check_tempdef_",temp_def, "lag_", N_lag, "bootstrap_est.with.ci_cumulativeME.csv"))
