
# preprocess temp data ----------------------------------------------------
#function to aggregate meteorological information to district level
preprocess <- function(file_path, measure, shape_file){
  get_measure <- terra::rast(file_path)
  
  # make sure time is in col name
  times <- terra::time(get_measure)
  names(get_measure) <- as.character(times)
  
  e           <- terra::ext(vect(shape_file))
  cropped     <- terra::crop(get_measure, e)
  
  # Resample weights to match the value raster's grid
  weight_aligned <- terra::resample(HSL_GER, cropped, method = "sum")
  
  extract     <- exact_extract(cropped, shape_file,
                               weights = weight_aligned,
                               fun="weighted_mean")
}

#rename columns 
rename_cols <- function(df, prefix) {
  names(df) <- paste0(prefix, ".", gsub("weighted_mean\\.", "", names(df)))
  df
}

# bootstrap cumulative ME -------------------------------------------------
#bootstrap function to calculate cumulative marginal effects of each temperature bin
#aggregated over a set of covariate values
#needs the following
#data = weekcounts_temp (or other dataset name)
#newdatasets needs dataframe used for prediction, e.g. different combinations of covariate values
#strata_by ="none", "age_cat" or "settlement_bin"
#formula = formula_gisd or other model formula
#temp_names=c("temp_cat_min","temp_cat_24") or other temperature bin names 
#by_gisd = T - calculate cumulative marginal effect by GISD categories
#lags - set to 1 which is default, can also be 2 or 3
##only relevant for sensitivity check so only implemented for by_gisd=F

boot_fun <- function(data, indices, newdatasets, formula, temp_names, strata_by, lags = "1", by_gisd=T) {
  d <- data[indices, ]
  
  if (strata_by == "none"){
    mod_b <- fepois(fml = as.formula(formula),
                    data = d,
                    cluster = ~ AGS_N3_21)
    b <- coef(mod_b)
    newdatasets$mu <- predict(mod_b, newdata = newdatasets, type="response")
    
    i <- 1
    
    if (by_gisd == FALSE){
      out <- vector(length = length(temp_names))
      
      if (lags == "1") {
        for (t in temp_names){ 
          beta_comb <- b[t] + b[paste0(t,"_lag_week1")] 
          newdatasets$me <- beta_comb * newdatasets$mu
          out[i] <- mean(newdatasets$me)
          i <- i + 1
        }
      } else if (lags == "2") {
        for (t in temp_names){ 
          beta_comb <- b[t] + b[paste0(t,"_lag_week1")] + b[paste0(t,"_lag_week2")] 
          newdatasets$me <- beta_comb * newdatasets$mu
          out[i] <- mean(newdatasets$me)
          i <- i + 1
        }
      } else if (lags == "3") {
        for (t in temp_names){ 
          beta_comb <- b[t] + b[paste0(t,"_lag_week1")] + b[paste0(t,"_lag_week2")] + b[paste0(t,"_lag_week3")] 
          newdatasets$me <- beta_comb * newdatasets$mu
          out[i] <- mean(newdatasets$me)
          i <- i + 1
        }
      }
      
      names(out) <- temp_names
    } else if (by_gisd == TRUE) {
      out <- matrix(NA, nrow = length(temp_names), ncol = 3)
      
      for (t in temp_names){
        #get beta combinations for each gisd, keeping in mind to add the right interaction terms
        beta_comb <- ifelse(newdatasets$gisd_const == 2,
                            sum(b[t], b[paste0(t,"_lag_week1")], b[grep(paste0("2:",t), names(b))]),
                            ifelse(newdatasets$gisd_const == 3,
                                   sum(b[t], b[paste0(t,"_lag_week1")],b[grep(paste0("3:",t), names(b))]),
                                   sum(b[t], b[paste0(t,"_lag_week1")]))) #gisd 1 is reference category so we dont add itneraction terms
        
        newdatasets$me <- beta_comb * newdatasets$mu
        out_temp <- newdatasets %>% group_by(gisd_const) %>% summarise(estimate=mean(me)) %>% ungroup()
        out[i, ] <- out_temp$estimate
        i <- i + 1
      } 
      out <- as.data.frame(out)
      colnames(out) <- c("gisd1","gisd2","gisd3")
      out$tempcats <- temp_names
      
      #pivot longer so that boot can use it
      out <- out %>% pivot_longer(cols = 1:3)
      out$label <- paste0(out$tempcats, "_", out$name)
      
      # Sort by label to ensure consistent order
      out <- out[order(out$label), ]
      
      # Return as a named numeric vector
      out <- setNames(out$value, out$label)
    } else {return("by_gisd is not correctly specificed.")}
    
  } else {
    strata_by_levels <- length(unique(data[[strata_by]]))
    
    mod_b <- fepois(fml = as.formula(formula),
                    data = d,
                    split = as.formula(paste0("~",strata_by)),
                    cluster = ~ AGS_N3_21)
    
    #prep out file
    out <- matrix(NA, nrow = length(temp_names), ncol = 3)
    out_list <- rep(list(out), strata_by_levels)
    names(out_list) <- levels(data[[strata_by]])
    
    for (s in 1:strata_by_levels){
      if (strata_by == "settlement_bin"){
        ags_select <- unique(data$AGS_N3_21[data[[strata_by]] == levels(data[[strata_by]])[s]])
        newdataset_temp <- newdatasets[newdatasets$AGS_N3_21 %in% ags_select,]
      } else {
        newdataset_temp <- newdatasets
      }
      b <- coef(mod_b[[s]])
      
      #do a prediction for each gisd
      mu <- predict(mod_b[[s]], newdata = newdataset_temp, type="response")
      
      i <- 1
      
      for (t in temp_names){ 
        #get beta combinations for each gisd, keeping in mind to add the right interaction terms
        beta_comb <- ifelse(newdataset_temp$gisd_const == 2,
                            sum(b[t], b[paste0(t,"_lag_week1")], b[grep(paste0("2:",t), names(b))]),
                            ifelse(newdataset_temp$gisd_const == 3,
                                   sum(b[t], b[paste0(t,"_lag_week1")],b[grep(paste0("3:",t), names(b))]),
                                   sum(b[t], b[paste0(t,"_lag_week1")]))) #gisd 1 is reference category so we dont add itneraction terms
        
        newdataset_temp$me <- beta_comb * mu
        
        out <- newdataset_temp %>% group_by(gisd_const) %>% summarise(estimate=mean(me)) %>% ungroup()
        
        out_list[[s]][i, ] <- out$estimate
        i <- i + 1
      }
      
      rownames(out_list[[s]]) <- temp_names
      rm(newdataset_temp); gc()
    }
    
    out <- do.call(rbind.data.frame, out_list)
    
    colnames(out) <- c("gisd1","gisd2","gisd3")
    out$tempcats <- rownames(out) 
    
    #pivot longer so that boot can use it
    out <- out %>% pivot_longer(cols = 1:3)
    out$label <- paste0(out$tempcats, "_", out$name)
    
    # Sort by label to ensure consistent order
    out <- out[order(out$label), ]
    
    # Return as a named numeric vector
    out <- setNames(out$value, out$label)
  }
  gc()
  return(out)
}

# plot function for stratified analyses -----------------------------------------------------------
##makes plotting main results for stratified analyses easier
##needs dataset, strata and color vector (length 3)

plot_function <- function(dat, strata, colors){
  
  p1 <- dat %>% 
    ggplot(aes(x = term, y = estimates, color=gisd_const, group=gisd_const)) +
    
    #estimates
    geom_point(position = position_dodge(width = 0.3)) +
    geom_line(aes(linetype = "absolute change"), position = position_dodge(width = 0.3)) +
    
    #relative change
    geom_line(aes(y = rel_change, color=gisd_const, group=gisd_const, linetype="relative change"), position = position_dodge(width = 0.3)) +  
    geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(width = 0.3))+
    
    #make it pretty
    scale_color_manual(values=colors)+
    scale_linetype_manual(name = "Type",
                          values = c("absolute change" = "solid", "relative change" = "dotted")) +
    scale_y_continuous(name = "Emergency hospitalization case per 10,000",
                       sec.axis = sec_axis(~ . /10, name = "Relative change in %",
                                           labels = scales::percent_format(scale=1))) +
    geom_hline(yintercept = 0) +
    labs(x="Temperature bins (°C)", y="Emergency hospitalization case per 10,000") +
    guides(color = guide_legend(title="")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          panel.border = element_blank()) + 
    panel_border() +
    #facet
    facet_wrap( vars({{ strata }}), nrow=2 )
  
  return(p1)
} 


# counterfactual prediction -----------------------------------------------
## function for calculating the number of cases attributable to a certain temperature bin per year
## in our case we use temp > 24°C
## needs data, newdat = original dataframe but temp_bin and lag_temp_bin of interest set to 0
## by_gisd - get estimates by GISD categories or not?
## strata - no strata, by age_cat or settlement_bin?
cf_pred <- function(data, newdat, model, by_gisd=FALSE, strata=NULL) {
  
  if (is.null(strata)){
    
    data$pred_obs <- predict(model)
    data$pred_cf <- predict(model, newdat)
    
    if (by_gisd==FALSE){
      excess_cases <- data %>% 
        group_by(year) %>% 
        summarise(total_cases_obs = sum(N_hospitalization),
                  total_cases_nc = sum(pred_obs),
                  total_cases_cf = sum(pred_cf),
                  total_excess = sum(pred_obs - pred_cf))
    } else {
      
      excess_cases <- data %>% 
        group_by(year, gisd_const) %>% 
        summarise(total_cases_obs = sum(N_hospitalization),
                  total_cases_nc = sum(pred_obs),
                  total_cases_cf = sum(pred_cf),
                  total_excess = sum(pred_obs - pred_cf))
      
    }
    return(excess_cases)
    
  }  else if (strata == "settlement_bin"){
    
    df_list <- split(data, data[[strata]])
    newdat_list <- split(newdat, newdat[[strata]])
    
    df_list[[1]]$pred_obs <- predict(model[[1]])
    df_list[[1]]$pred_cf <- predict(model[[1]], newdat_list[[1]])
    
    df_list[[2]]$pred_obs <- predict(model[[2]])
    df_list[[2]]$pred_cf <- predict(model[[2]], newdat_list[[2]])
    
    data <- rbind(df_list[[1]], df_list[[2]])
    
  } else if (strata == "age_cat"){
    
    df_list <- split(data, data[[strata]])
    newdat_list <- split(newdat, newdat[[strata]])
    
    df_list[[1]]$pred_obs <- predict(model[[1]])
    df_list[[1]]$pred_cf <- predict(model[[1]], newdat_list[[1]])
    
    df_list[[2]]$pred_obs <- predict(model[[2]])
    df_list[[2]]$pred_cf <- predict(model[[2]], newdat_list[[2]])
    
    df_list[[3]]$pred_obs <- predict(model[[3]])
    df_list[[3]]$pred_cf <- predict(model[[3]], newdat_list[[3]])
    
    data <- rbind(df_list[[1]], df_list[[2]], df_list[[3]])
    
  }
  excess_cases <- data %>% 
    group_by(year, gisd_const, .data[[strata]]) %>% 
    summarise(total_cases_obs = sum(N_hospitalization),
              total_cases_nc = sum(pred_obs),
              total_cases_cf = sum(pred_cf),
              total_excess = sum(pred_obs - pred_cf))
  return(excess_cases)
}
