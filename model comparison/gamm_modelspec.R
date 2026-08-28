#setwd()

library(tidyverse)
library(mgcv)
library(sf)
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

# Read the shapefile
shape_data <- st_read("VG5000_KRS.shp")
shape_data <- rename(shape_data, AGS_N3_21=AGS)
shape_data$AGS_N3_21 <- as.numeric(shape_data$AGS_N3_21)

#function to transform shapefile geometry into a list for each county ( so we can use it in the gam model)
get_polygon_list <- function(sf_obj) {
  polys <- lapply(seq_len(nrow(sf_obj)), function(i) {
    geom <- sf_obj$geometry[i]
    
    if (st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON")) {
      coords <- do.call(rbind, lapply(st_geometry(geom), function(poly) {
        st_coordinates(poly)[, 1:2]  # Extract x, y
      }))
      
      # Separate multiple polygons with NA rows
      return(rbind(coords, c(NA, NA)))
    } else {
      return(NULL)  # Skip non-polygon geometries
    }
  })
  
  names(polys) <- sf_obj$county_name  # Ensure names match counties
  return(polys)
}

poly_info <- get_polygon_list(shape_data)
names(poly_info) <- shape_data$AGS

rm(shape_data);gc()

# GAMM model ----------------------------------------
start3 <- Sys.time()

GAMM.2 <- bam(N_hospitalization ~ 
                temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                temp_cat_18 + temp_cat_21 + temp_cat_24 +
                
                temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                
                s(AGS_N3_21, bs = "re") + # Random effect for AGS
                s(year, bs = "re") +  # Random effect for year
                s(week, bs="cc",k=12) +  # Smooth temporal trend
                
                geschlecht + age_cat + offset(log(N_population.corr)),  
              family = poisson(link = "log"), 
              method = "REML", 
              data = weekcounts_temp)
time3 <- Sys.time() - start3
print(time3)

#save coefficients
GAMM.2 <- tidy(GAMM.2, parametric=T)
gc()

#### FOR TRAIN/TEST TO OBTAIN RMSE USE:
# ## test how well it predicts out of sample
# test$predicted <- GAMM_pred_tempbins <- predict(GAMM.2, newdata = test, type = "response")
# 
# #calculate RMSE
# RMSE <- sqrt(mean((test$N_hospitalization - test$predicted)^2))
# RMSE     
####

# GAMM model with autocorrelation----------------------------------------
#we could technically also provide the neighborhood structure here with nb
#but technically the s() function should be able to calculate it from the polygons that we provded
start3 <- Sys.time()

GAMM.2_autocorr <- bam(N_hospitalization ~ 
                         temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                         temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                         temp_cat_18 + temp_cat_21 + temp_cat_24 +
                         
                         temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                         temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                         temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                         
                         s(AGS_N3_21, bs = "mrf", xt=list(polys=poly_info)) + # Spatial autcorr effect
                         s(year, bs = "re") +  # Random effect for year
                         s(week,bs="cc",k=12) +  # Smooth temporal trend
                         geschlecht + age_cat + offset(log(N_population.corr)),  
                       family = poisson(link = "log"), 
                       method = "REML",
                       data = weekcounts_temp)
time3 <- Sys.time() - start3
print(time3)

#save coefficients
GAMM.2_autocorr <- tidy(GAMM.2_autocorr, parametric=T)
gc()

#### FOR TRAIN/TEST TO OBTAIN RMSE USE:
## test how well it predicts out of sample
# test$predicted <- GAMM_pred_autcorr_tempbins <- predict(GAMM.2_autocorr, newdata = test, type = "response")
# write.csv(GAMM_pred_autcorr_tempbins, file="Models/predictions_test.sample__GAMM_autocorr_tempbins_base.csv")
# 
# #calculate RMSE
# RMSE <- sqrt(mean((test$N_hospitalization - test$predicted)^2))
# RMSE  
####

# save results ------------------------------------------------------------------
#create a table to save the output
GAMM.2$model <- "GAM_tempbins"
GAMM.2_autocorr$model <- "GAM_tempbins_autocorrelation"
results_GAMM2 <- bind_rows(GAMM.2, GAMM.2_autocorr)          
write.csv(results_GAMM2, file="output_gamm.tempbins_modelspecification.csv")

