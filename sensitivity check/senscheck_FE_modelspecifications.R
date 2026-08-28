#setwd()

library(marginaleffects)
library(fixest)
library(splines2)
library(tidyverse)
library(broom.mixed)
library(performance)

#load data
load("toy_data.RData")
weekcounts_temp <- toy_data

# Base model ---> least conservative-----------------------------------
M1 <-  fepois(fml = N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                  temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                  temp_cat_18 + temp_cat_21 + temp_cat_24 +
                  
                  temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                  temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                  temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                 
                relevel(gisd_const, ref="1") + relevel(OAD_ratio_tert, ref="(30.7,34.4]") + relevel(Lebenserwartung_tert, ref="(78.5,82.5]") +
                 geschlecht + age_cat + 
                  bSpline(week, periodic = T, knots = seq(1, 52, length.out = 12)[-c(1,12)], Boundary.knots = c(1, 52)) +
                  offset(log(N_population.corr))| AGS_N3_21 + year,
                data = weekcounts_temp,
                cluster = ~ AGS_N3_21) 

summary(M1)
perform_M1 <- performance(M1)
perform_M1$Model <- "base (least conservative)"

perform_M1
Base <- tidy(M1)

# Medium conservative ----------------------------------------
## create year_AGS and week_year variable, remove B spline
weekcounts_temp$year_AGS <- as.factor(paste0(weekcounts_temp$year,"_",weekcounts_temp$AGS_N3_21))
weekcounts_temp$week_year <- as.factor(paste0(weekcounts_temp$week,"_",weekcounts_temp$year))
weekcounts_temp$year <- as.numeric(as.character(weekcounts_temp$year))

#fit
M1.2 <-  fepois(fml = N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                  temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                  temp_cat_18 + temp_cat_21 + temp_cat_24 +
                  
                  temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                  temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                  temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                  
                  relevel(gisd_const, ref="1") + relevel(OAD_ratio_tert, ref="(30.7,34.4]") + relevel(Lebenserwartung_tert, ref="(78.5,82.5]") +
                  geschlecht + age_cat + 
                  offset(log(N_population.corr))| year_AGS + week_year + interaction(AGS_N3_21, year),
                data = weekcounts_temp,
                cluster = ~ AGS_N3_21) 

summary(M1.2)
performance(M1.2) #RMSE lower than for base model

perform_M1.2 <- performance(M1.2)
perform_M1.2$Model <- "Medium conservative (FE for AGS_year, week_year, int(AGS,year))"

medium_cons <- tidy(M1.2)

# Most conservative model ----------------------------------------
#include week_AGS FEs
weekcounts_temp$week_AGS <- as.factor(paste0(weekcounts_temp$week,"_",weekcounts_temp$AGS_N3_21))

#fit
M1.3 <-  fepois(fml = N_hospitalization ~ temp_cat_min + temp_cat_neg6 + temp_cat_neg3 + 
                  temp_cat_zero + temp_cat_3 + temp_cat_6 + temp_cat_16 + 
                  temp_cat_18 + temp_cat_21 + temp_cat_24 +
                  
                  temp_cat_min_lag_week1 + temp_cat_neg6_lag_week1 + temp_cat_neg3_lag_week1 + 
                  temp_cat_zero_lag_week1 + temp_cat_3_lag_week1 + temp_cat_6_lag_week1 + temp_cat_16_lag_week1 + 
                  temp_cat_18_lag_week1 + temp_cat_21_lag_week1 + temp_cat_24_lag_week1 +
                  
                  relevel(gisd_const, ref="1") + relevel(OAD_ratio_tert, ref="(30.7,34.4]") + relevel(Lebenserwartung_tert, ref="(78.5,82.5]") +
                  geschlecht + age_cat + 
                  offset(log(N_population.corr))| year_AGS + week_year + interaction(AGS_N3_21, year) + week_AGS,
                data = weekcounts_temp,
                cluster = ~ AGS_N3_21) 

summary(M1.3)
performance(M1.3)

perform_M1.3 <- performance(M1.3)
perform_M1.3$Model <- "most conservative (additional FE for week_AGS)"

mostconservative <- tidy(M1.3)

# save model performance comparison ----------------------------------------
model_performances <- rbind(perform_M1[,-c(3:6)], perform_M1.2, perform_M1.3)
model_performances <- model_performances[,c(7,1:6)]

write.csv(model_performances, file="Tables/check_FE_modelspecifications.csv")

# plot coefficients ----------------------------------------
Base$model <- "base, least conservative"
medium_cons$model <- "medium conservative"
mostconservative$model <- "most conservative"

models_output <- rbind(Base, medium_cons, mostconservative)

models_output$lb <- models_output$estimate - 1.96*models_output$std.error
models_output$ub <- models_output$estimate + 1.96*models_output$std.error

models_output$estimate_exp <- exp(models_output$estimate)
models_output$lb_exp <- exp(models_output$lb)
models_output$ub_exp <- exp(models_output$ub)

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


coefsexp <- models_output %>%
  ggplot(aes(y=estimate_exp, x=term, color=model, group=model)) +
  geom_point() +
  geom_linerange(aes(ymin=lb_exp, ymax=ub_exp)) +
  theme_minimal() +
  geom_hline(yintercept = 1)+
  scale_color_manual(values = c("#414487FF","#4169E1", "#00BFFF") )+
  labs(x="Temperature ranges (°C)", y="%-change in Hospitalization Rate") +
  guides(color = guide_legend(title="Model")) +
  theme(legend.position = "bottom") +
  facet_wrap(~t,nrow = 3) 

coefsexp



