#setwd()

library(glmmTMB)
library(tidyverse)
library(splines2)
library(broom.mixed)

#load data
load("toy_data.RData")
weekcounts_temp <- toy_data

#### FOR TRAIN/TEST TO OBTAIN RMSE USE:
# #load data
# load("Data/Train_data_modelcomparison.RData")
# load("Data/Test_data_modelcomparison.RData")
# 
# weekcounts_temp <- train
####

# simple Poisson hierarchical model_temp bins ----------------------------------------
start2 <- Sys.time()

#we exclude temp 9 which represents 9to16degrees which is seens as the "Comfort zone"
 M1.2 <- glmmTMB(N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                       temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                       temp_cat_18 + temp_cat_21 + temp_cat_24 +
                   
                       temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                       temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                       temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                       
                        geschlecht + age_cat + 
                       (1 | AGS_N3_21) + (1 | year) + 
                       bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52)) + 
                       offset(log(N_population.corr)),
                     family = poisson,
                     data = weekcounts_temp,
                 #to address convergence issues
                 control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e4),profile=T)) 
time2 <- Sys.time() - start2
print(time2) 

#save coefs
M1.2$model <- "GLMM_tempbins"
results_GLMM <- M1.2         
write.csv(results_GLMM, file="output_glmm_tempbins_modelspecifications.csv")

#### FOR TRAIN/TEST TO OBTAIN RMSE USE:
# ## test how well it predicts out of sample
# test$predicted <- GLMMM_pred <- predict(M1.2, newdat=test, type="response")
# 
# RMSE <- sqrt(mean((test$N_hospitalization - test$predicted)^2))
# RMSE     
####