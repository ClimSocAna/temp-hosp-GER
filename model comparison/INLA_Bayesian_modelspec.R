setwd("/home/gueltzowm/scratch/hospitalization_and_climate")


library(tidyverse)
library(splines2)
library(sf)
library(spdep)
library(INLA)

#number of cores for running in parralel 
n_cores=10

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

# Read the shapefile
shape_data <- st_read("Kreise/VG5000_KRS.shp")
shape_data <- rename(shape_data, AGS_N3_21=AGS)
shape_data$AGS_N3_21 <- factor(as.numeric(shape_data$AGS_N3_21))

##create W for the CAR Models (Conditional autoregressive models)
# CAR models the correlation between neighboring units
# W represents adjacency matrix where entries {i,i} are zero and the off-diagonal elements are 1 if regions i and j are neighbors and 0 otherwise.
# https://mc-stan.org/learn-stan/case-studies/icar_stan.html

# estimate first order adjacency matrix
shape_nb <- poly2nb(shape_data)
names(shape_nb) <- shape_data$AGS_N3_21
nb2INLA("Data/shape_INLA_adj.graph", shape_nb)

H <- inla.read.graph(filename = "Data/shape_INLA_adj.graph")
image(inla.graph2matrix(H), xlab = "", ylab = "")

zim_adj <- paste("Data/shape_INLA_adj.graph")

#transform AGS variable to be a count from 1 to 400
#so that it matches with the INLA graph needed for the BYM 
weekcounts_temp$AGS_num <- factor(weekcounts_temp$AGS_N3_21, labels = c(1:400))
test$AGS_num <- factor(test$AGS_N3_21, labels = c(1:400))

#even though the levels are the same across these two, we need to assign values
values <- as.factor(unique(c(levels(weekcounts_temp$AGS_num), levels(names(H$nbs))))) 


# prep --------------------------------------------------------------------
#formula
formula <- N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
  temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
  temp_cat_18 + temp_cat_21 + temp_cat_24 +
  
  temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
  temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
  temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
  
  age_cat + geschlecht +
  f(AGS_num, values = values, model = "bym", graph = H, scale.model = T) + # CAR spatial structure
  f(year, model = "iid") + # Random intercept for year
  bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52)) + #lets assume biweekly variation
  offset(log(N_population.corr))

#specify priors, we assume the same for all temperature variables 
priors <- list(
  mean = list(
    temp_cat_min = 0,
    temp_cat_neg6 = 0,
    temp_cat_neg3 = 0,
    temp_cat_zero = 0,
    temp_cat_3 = 0,
    temp_cat_6 = 0,
    temp_cat_16 = 0,
    temp_cat_18 = 0,
    temp_cat_21 = 0,
    temp_cat_24 = 0,
    
    temp_cat_min_lag_week1 = 0,
    temp_cat_neg6_lag_week1 = 0,
    temp_cat_neg3_lag_week1 = 0,
    temp_cat_zero_lag_week1 = 0,
    temp_cat_3_lag_week1 = 0,
    temp_cat_6_lag_week1 = 0,
    temp_cat_16_lag_week1 = 0,
    temp_cat_18_lag_week1 = 0,
    temp_cat_21_lag_week1 = 0,
    temp_cat_24_lag_week1 = 0,
    
    geschlechtmale = 0,
    age_cat30to65years = 0,
    `age_cat>65years` = 0
  ),
  prec = list(
    temp_cat_min = 1 / (2.5^2),
    temp_cat_neg6 = 1 / (2.5^2),
    temp_cat_neg3 = 1 / (2.5^2),
    temp_cat_zero = 1 / (2.5^2),
    temp_cat_3 = 1 / (2.5^2),
    temp_cat_6 = 1 / (2.5^2),
    temp_cat_16 = 1 / (2.5^2),
    temp_cat_18 = 1 / (2.5^2),
    temp_cat_21 = 1 / (2.5^2),
    temp_cat_24 = 1 / (2.5^2),
    
    temp_cat_min_lag_week1 = 1 / (2.5^2),
    temp_cat_neg6_lag_week1 = 1 / (2.5^2),
    temp_cat_neg3_lag_week1 = 1 / (2.5^2),
    temp_cat_zero_lag_week1 = 1 / (2.5^2),
    temp_cat_3_lag_week1 = 1 / (2.5^2),
    temp_cat_6_lag_week1 = 1 / (2.5^2),
    temp_cat_16_lag_week1 = 1 / (2.5^2),
    temp_cat_18_lag_week1 = 1 / (2.5^2),
    temp_cat_21_lag_week1 = 1 / (2.5^2),
    temp_cat_24_lag_week1 = 1 / (2.5^2),
    
    geschlechtmale = 1 / (0.75^2),
    age_cat30to65years = 1 / (1^2),
    `age_cat>65years` = 1 / (1.5^2)
  ))

# ONLY NEEDED FOR TRAIN/TEST TO OBTAIN RMSE -------------------------------------------------------------------
# #create training data stack
# stack_train <- inla.stack(
#   data = list(N_hospitalization = weekcounts_temp$N_hospitalization),  # observed response
#   A = list(1),                       # link matrix 
#   effects = list(
#     data.frame(
#       intercept = 1,
#       AGS_num = weekcounts_temp$AGS_num,
#       year = weekcounts_temp$year,
#       week = weekcounts_temp$week,
#       geschlecht = weekcounts_temp$geschlecht,
#       age_cat = weekcounts_temp$age_cat,
#       temp_cat_min = weekcounts_temp$temp_cat_min,
#       temp_cat_neg6 = weekcounts_temp$temp_cat_neg6,
#       temp_cat_neg3 = weekcounts_temp$temp_cat_neg3,
#       temp_cat_zero = weekcounts_temp$temp_cat_zero,
#       temp_cat_3 = weekcounts_temp$temp_cat_3,
#       temp_cat_6 = weekcounts_temp$temp_cat_6,
#       temp_cat_16 = weekcounts_temp$temp_cat_16,
#       temp_cat_18 = weekcounts_temp$temp_cat_18,
#       temp_cat_21 = weekcounts_temp$temp_cat_21,
#       temp_cat_24 = weekcounts_temp$temp_cat_24,
#       
#       temp_cat_min_lag_week1 = weekcounts_temp$temp_cat_min_lag_week1,
#       temp_cat_neg6_lag_week1 = weekcounts_temp$temp_cat_neg6_lag_week1,
#       temp_cat_neg3_lag_week1 = weekcounts_temp$temp_cat_neg3_lag_week1,
#       temp_cat_zero_lag_week1 = weekcounts_temp$temp_cat_zero_lag_week1,
#       temp_cat_3_lag_week1 = weekcounts_temp$temp_cat_3_lag_week1,
#       temp_cat_6_lag_week1 = weekcounts_temp$temp_cat_6_lag_week1,
#       temp_cat_16_lag_week1 = weekcounts_temp$temp_cat_16_lag_week1,
#       temp_cat_18_lag_week1 = weekcounts_temp$temp_cat_18_lag_week1,
#       temp_cat_21_lag_week1 = weekcounts_temp$temp_cat_21_lag_week1,
#       temp_cat_24_lag_week1 = weekcounts_temp$temp_cat_24_lag_week1,
#       
#       N_population.corr = weekcounts_temp$N_population.corr)
#   ),
#   tag = "train"
# )
# 
# # Build a prediction stack
# stack_pred <- inla.stack(
#   data = list(N_hospitalization = NA),
#   A = list(1),
#   effects = list(
#     data.frame(
#       intercept = 1,
#       AGS_num = test$AGS_num,
#       year = test$year,
#       week = test$week,
#       geschlecht = test$geschlecht,
#       age_cat = test$age_cat,
#       temp_cat_min = test$temp_cat_min,
#       temp_cat_neg6 = test$temp_cat_neg6,
#       temp_cat_neg3 = test$temp_cat_neg3,
#       temp_cat_zero = test$temp_cat_zero,
#       temp_cat_3 = test$temp_cat_3,
#       temp_cat_6 = test$temp_cat_6,
#       temp_cat_16 = test$temp_cat_16,
#       temp_cat_18 = test$temp_cat_18,
#       temp_cat_21 = test$temp_cat_21,
#       temp_cat_24 = test$temp_cat_24,
#       
#       temp_cat_min_lag_week1 = test$temp_cat_min_lag_week1,
#       temp_cat_neg6_lag_week1 = test$temp_cat_neg6_lag_week1,
#       temp_cat_neg3_lag_week1 = test$temp_cat_neg3_lag_week1,
#       temp_cat_zero_lag_week1 = test$temp_cat_zero_lag_week1,
#       temp_cat_3_lag_week1 = test$temp_cat_3_lag_week1,
#       temp_cat_6_lag_week1 = test$temp_cat_6_lag_week1,
#       temp_cat_16_lag_week1 = test$temp_cat_16_lag_week1,
#       temp_cat_18_lag_week1 = test$temp_cat_18_lag_week1,
#       temp_cat_21_lag_week1 = test$temp_cat_21_lag_week1,
#       temp_cat_24_lag_week1 = test$temp_cat_24_lag_week1,
#       
#       N_population.corr = test$N_population.corr)
#   ),
#   tag = "pred"
# )



# Bayesian non-informative prior (for fixed effects) with spatial autocorrelation prior ----------------------------------------
# Fit the model using INLA 
#see https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html; https://rpubs.com/keniajin/CAR_model_inla 
start <- Sys.time()

## FOR CALCUATING RMSE WITH TRAIN/TEST SPLIT:
# replace data argument with inla.stack.data(stack_train)
# and include control.predictor = list(A = inla.stack.A(stack_train), compute = TRUE)

INLA_tempbins_autocorr <- inla(formula,
  control.compute=list(config = TRUE,dic=TRUE, waic=TRUE),
  data = weekcounts_temp,
  num.threads = 10,
  family = "poisson")

time <- Sys.time() - start
print(time)

# Print summary
results <- summary(INLA_tempbins_autocorr)
results

#save summary as table
res1 <- results[[3]]
res2 <- results[[4]]
res2$kld <- NA

res <- rbind(res1, res2)
write.csv(res, file="Tables/output_INLA_tempbins_autocorr.csv")
gc() 

#### FOR TRAIN/TEST TO OBTAIN RMSE USE:
# # Obtain predicted values
# obs <- test$N_hospitalization
# # rm obs from data
# test$N_hospitalization <- NA
# 
# stack_all <- inla.stack(stack_train, stack_pred)
# 
# #re run inla model
# fit_pred <- inla(
#   formula,
#   data = inla.stack.data(stack_all1),
#   num.threads = n_cores,
#   
#   control.predictor = list(
#     A = inla.stack.A(stack_all1),
#     compute = TRUE,
#     link = 1
#   ),
#   family="poisson",
#   control.fixed = INLA_tempbins_autocorr$control.fixed,              
#   control.inla = INLA_tempbins_autocorr$control.inla,                
#   control.compute = list(config = TRUE)          
# )
# 
# # Index for predictions
# index_pred <- inla.stack.index(stack_pred, tag = "pred")$data
# 
# pred_mean_response <- fit_pred$summary.fitted.values$mean[index_pred]
# 
# # Compute RMSE
# rmse_manual <- sqrt(mean((obs - pred_mean_response)^2))
# rmse_manual
##

# Informative prior --------------------------------------------------------------
start <- Sys.time()

## FOR CALCUATING RMSE WITH TRAIN/TEST SPLIT:
# replace data argument with inla.stack.data(stack_train)
# and include control.predictor = list(A = inla.stack.A(stack_train), compute = TRUE)
INLA_tempbins_autocorr_informedprior <- inla(formula,
                                             control.compute=list(config = TRUE,dic=TRUE, waic=TRUE),
                                             control.fixed = priors, #include priors
                                             data = weekcounts_temp,
                                             num.threads = n_cores,
                                             family = "poisson")


time <- Sys.time() - start
print(time)

# Print summary
results <- summary(INLA_tempbins_autocorr_informedprior)
results

#save summary as table
res1 <- results[[3]]
res2 <- results[[4]]
res2$kld <- NA

res <- rbind(res1, res2)
write.csv(res, file="Tables/output_INLA_tempbins_autocorr_informedprior.csv")
gc()

#### FOR TRAIN/TEST TO OBTAIN RMSE USE:
# # Obtain predicted values
# obs <- test$N_hospitalization
# #rm obs from data
# test$N_hospitalization <- NA
# 
# stack_all_inf <- inla.stack(stack_train, stack_pred)
# 
# #re run INLA model
# fit_pred_inf <- inla(
#   formula,
#   data = inla.stack.data(stack_all_inf),
#   num.threads = n_cores,
#   
#   control.predictor = list(
#     A = inla.stack.A(stack_all_inf),
#     compute = TRUE,
#     link = 1
#   ),
#   family="poisson",
#   control.fixed = INLA_tempbins_autocorr_informedprior$control.fixed,             
#   control.inla = INLA_tempbins_autocorr_informedprior$control.inla,               
#   control.compute = list(config = TRUE)          
# )
# 
# # Index for predictions
# index_pred <- inla.stack.index(stack_pred, tag = "pred")$data
# 
# pred_mean_response <- fit_pred_inf$summary.fitted.values$mean[index_pred]
# 
# # Compute RMSE
# rmse_manual <- sqrt(mean((obs - pred_mean_response)^2))
# rmse_manual
##