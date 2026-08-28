#setwd()

library(tidyverse)
library(ggplot2)

# create Table ------------------------------------------------------------
GLMM <- read.csv("output_glmm_tempbins_modelspecifications_1206.csv")
GAMM_tempbins_ac <- read.csv("output_gamm.tempbins_autocorr_modelspecifications_1206.csv")
GAMM_tempbins <- read.csv("output_gamm.tempbins_modelspecification_1206.csv")
FE <- read.csv("Tables/output_FE_tempbins_modelspecifications_finalmodel_1206.csv")
INLA_tempbins_ac <- read.csv("Tables/output_INLA_tempbins_autocorr_1206.csv")
INLA_tempbins_ac_inf <- read.csv("Tables/output_INLA_tempbins_autocorr_informedprior_1206.csv")

#combine INLA in one and remove
INLA_tempbins_ac$model <- "Bayesian_autocorr"
INLA_tempbins_ac_inf$model <- "Bayesian_autocorr_inf"

INLA_models <- rbind(INLA_tempbins_ac, INLA_tempbins_ac_inf)
rm(INLA_tempbins_ac, INLA_tempbins_ac_inf);gc()

#adapt GAMM to fit format of GLMM
GLMM <- GLMM %>% 
  mutate(component = NULL,
         model = gsub("\\_.*","",model))

GAMM_tempbins <- GAMM_tempbins %>% 
  mutate(effect = "fixed",
         model = gsub("_tempbins","",model))

GAMM_tempbins_ac <- GAMM_tempbins_ac %>% 
  mutate(effect = "fixed",
         model = gsub("_tempbins","",model))

FE <- FE %>% 
  mutate(model = "FE")

INLA_models <- INLA_models %>% 
  rename(term = X,
         estimate = mean,
         std.error = sd,
         lb = X0.025quant,
         ub = X0.975quant) 

#join everything into one
models_output <- bind_rows(GLMM, FE, 
                       GAMM_tempbins, GAMM_tempbins_ac)
models_output <- models_output[order(models_output$model), ]

models_output$estimate <- as.numeric(models_output$estimate)
models_output$std.error <- as.numeric(models_output$std.error)

models_output$lb <- models_output$estimate - 1.96*models_output$std.error
models_output$ub <- models_output$estimate + 1.96*models_output$std.error

#add in bayesian model
models_output <- bind_rows(models_output, INLA_models)

models_output$estimate_exp <- exp(models_output$estimate)
models_output$lb_exp <- exp(models_output$lb)
models_output$ub_exp <- exp(models_output$ub)

models_output$model <- factor(models_output$model, levels=c("GLMM","FE","GAM","GAM_autocorrelation",  "Bayesian_autocorr","Bayesian_autocorr_inf"))
models_output$term <- gsub("TRUE", "T", models_output$term)

models_output

# plot coefficient --------------------------------------------------------
#for plotting let's create an indicator on whether we are looking at the lag or not
models_output$t <- ifelse(grepl("lag_week1", models_output$term) == T, "lagged",
                                 "same week")
models_output$t <- factor(models_output$t, levels = c("same week", "lagged"))

#now remove the lag from the terms
models_output$term <- gsub("_lag_week1", "", models_output$term)

#include reference cat
#take any temp cat
reference <- models_output %>% filter(term == "temp_cat_18")
#change name and set values to zero
reference <- reference %>% mutate(term="temp_cat_9 (ref)",
                                  estimate=1, std.error=1, statistic=1, p.value=1, lb=1, ub=1,
                                  estimate_exp=1, lb_exp=1, ub_exp=1)

models_output <- rbind(models_output, reference)

#order everything so that we get the right order
models_output <- models_output %>% filter(term == "temp_cat_min" | term ==  "temp_cat_neg6"  | 
                                            term ==  "temp_cat_neg3"  |term ==    "temp_cat_zero"
                                          | term == "temp_cat_3"| term ==   "temp_cat_6" |
                                            term=="temp_cat_9 (ref)"| term == "temp_cat_16" |
                                            term == "temp_cat_18" | term == "temp_cat_21" | term == "temp_cat_24")

models_output$term <- factor(models_output$term, levels=c("temp_cat_min", "temp_cat_neg6","temp_cat_neg3", "temp_cat_zero","temp_cat_3" ,          
                                                          "temp_cat_6","temp_cat_9 (ref)","temp_cat_16", "temp_cat_18" , "temp_cat_21", "temp_cat_24"),
                             labels = c("<-6", "-6;-3", "-3;0", "0;3", "3;6", "6;9",
                                        "9;16", "16;18", "18;21", "21;24", ">24"))


#plot exponentiated coefs
coefsexp <- models_output %>%
  ggplot(aes(y=estimate_exp, x=term, color=model)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(aes(ymin=lb_exp, ymax=ub_exp), position = position_dodge2(width = 0.3)) +
  theme_minimal() +
  geom_hline(yintercept = 1)+
  scale_color_viridis_d()+
  labs(x="Temperature ranges (°C)", y="Point-change in Hospitalization Rate") +
  guides(color = guide_legend(title="Model")) +
  theme(legend.position = "bottom") +
  facet_wrap(~t,nrow = 3) 

coefsexp

