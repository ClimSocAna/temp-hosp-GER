#setwd()

library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)
library(sf)
library(rmapshaper)

#load functions
source("functions.R")

# FIGURE 1 ----------------------------------------------------------------
#load data
load("toy_data.RData")
weekcounts_temp <- toy_data

#get date
weekcounts_temp$date <- as.Date(paste(weekcounts_temp$year,weekcounts_temp$week,1), format= "%Y %U %u")
weekcounts_temp$month <- month(weekcounts_temp$date)

plot_df <- weekcounts_temp %>%
  group_by(date) %>%
  ##since we are not interested in the different groups for now, we'll now aggregate everything by date
  summarise(tot_hosp = sum(N_hospitalization),
            hosp_rate = sum(N_hospitalization)/sum(N_population.corr),
            pop = sum(N_population.corr)) %>%
  mutate(month = month(date),
         #create season variable
         season = ifelse(month == 1| month == 2, "winter",
                         ifelse(month == 12, "winter2",
                                ifelse(month >= 3 & month <=5, "spring",
                                       ifelse(month >= 6 & month <=8, "summer",
                                              ifelse(month >= 9 & month <=11, "autumn",NA))))))

#create background shading
background <- weekcounts_temp %>%
  group_by(year, season) %>%
  summarize(xmin = min(date), xmax = max(date), .groups = 'drop') %>%
  mutate(ymin = -Inf, ymax = Inf)

descrplot <- ggplot() +
  geom_line(data=plot_df, mapping=aes(x=date, y=hosp_rate*10000), color="black") +
  geom_rect(data=background, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = season), alpha = 0.35) +
  scale_fill_manual(values = c("spring" = "#A3C585",
                               "summer" = "#EF8354",
                               "autumn" = "#8B5A2B",
                               "winter" = "#118AB2",
                               "winter" = "#118AB2")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y", # Format date labels as Month Year (e.g., Jan 2020)
               expand = expansion(mult = c(0.01, 0.01))) + # Remove padding around the x-axis
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x.top = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom", legend.title = element_blank())+
  labs(x="", y="Weekly Emergency Hospitalization Rate per 10,000")

##plot hosp map
# make plot df
plot_df1 <- weekcounts_temp %>%
  group_by(AGS_N3_21) %>%
  summarise(mean = sum(N_hospitalization)/sum(N_population.corr)*10000) %>% 
  mutate(extreme_temp = "Average")

# Read the shapefile
shape_data <- st_read("Kreise/KRS_ew_21.shp")
shape_data <- rename(shape_data, AGS_N3_21=AGS)
shape_data$AGS_N3_21 <- factor(as.numeric(shape_data$AGS_N3_21))
shape_data <- rmapshaper::ms_simplify(shape_data, keep=0.05)

#join
plot_df2 <- left_join(shape_data, plot_df1, by="AGS_N3_21")

##Hospitalization rate 
plot1 <- plot_df2 %>%
  filter(!is.na(extreme_temp)) %>%
  ggplot() +
  geom_sf(aes(fill = mean)) +
  scale_fill_viridis_c(na.value = "white", name = "Rate per 10,000", option="mako", direction=-1) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "bottom") 

Fig1 <- (descrplot + plot1) + 
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(tag_levels = 'A') 

ggsave(filename = "Figures/Fig1.svg", Fig1, width = 10, height = 5, dpi=300)

# FIGURE 2: TOTAL & By GISD -----------------------------------------------------------------
slopes_total <- read.csv("Tables/cumulativeME.csv")
slopes_gisd <- read.csv("Tables/cumulativeME_GISD.csv")

slopes_total$gisd_const <- "total"

slopes_total$group <- "Main effects only"
slopes_gisd$group <- "Effect modification by GISD"

#split term variable
slopes_gisd <- slopes_gisd %>%
  separate(name, 
         into = c("name", "gisd_const"), 
         sep = "_(?=[^_]+$)") 

slopes <- bind_rows(slopes_total, slopes_gisd)
slopes$group <- factor(slopes$group, levels = c("Main effects only","Effect modification by GISD"))

slopes <- slopes %>% rename(term = name)

#include reference cat
#take any temp cat
reference <- slopes %>% filter(term == "temp_cat_18")
#change name and set values to zero
reference <- reference %>% mutate(term="temp_cat_9 (ref)",
                                  estimates=0, lower=0, upper=0)


slopes <- rbind(slopes, reference)

#order everything
slopes$term <- factor(slopes$term, levels=c("temp_cat_min", "temp_cat_neg6","temp_cat_neg3", "temp_cat_zero","temp_cat_3" ,          
                                                    "temp_cat_6","temp_cat_9 (ref)","temp_cat_16", "temp_cat_18" , "temp_cat_21", "temp_cat_24"),
                          labels = c("<-6", "-6;-3", "-3;0", "0;3", "3;6", "6;9",
                                     "9;16", "16;18", "18;21", "21;24", ">24"))

slopes$gisd_const <- factor(slopes$gisd_const, levels = c("total","gisd1","gisd2", "gisd3"),
                        labels = c("Total","Low Deprivation","Medium Deprivation", "High Deprivation"))

## simple calculation to obtain relative change by dividing estimates
## 987.89 is the predicted overall hospitalization rate for the model on the total sample
slopes$rel_change <- slopes$estimate/987.89*100

## plot
my_colors <- c(
  "Total" = "#414487FF",
  "Low Deprivation" = "#3CB44B",
  "Medium Deprivation" = "#B0B0B0",
  "High Deprivation" = "#9370DB"
)

##we dont want total to show so we need to make an adjustment and use scale_manual differently to the function
p1 <- slopes %>% 
  ggplot(aes(x = term, y = estimates, color=gisd_const, group=gisd_const)) +
  
  #estimates
  geom_point(position = position_dodge(width = 0.3)) +
  geom_line(aes(linetype = "absolute change"), position = position_dodge(width = 0.3)) +
  
  #relative change
  geom_line(aes(y = rel_change*10, color=gisd_const, group=gisd_const, linetype="relative change"),
            position = position_dodge(width = 0.3)) +  
  geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(width = 0.3))+
  
  #make it pretty
  scale_color_manual(values = my_colors,
                     breaks = c("Low Deprivation","Medium Deprivation","High Deprivation")) +
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
  facet_wrap(vars(group), nrow=2)

#ggsave("Figures/main_relchange_GISD.svg", plot=p1, dpi=300, height = 5, width = 6)

# FIGURE 3: BY GISD AND SETTLEMENT -----------------------------------------------------------------
slopes <- read.csv("Tables/cumulativeME_FE_GISDsettlement_bin.csv")
 
# #split name variable
slopes <- slopes %>%
  # First, split at the dot
  separate(name, into = c("settlement", "rest"), sep = "\\.") %>%
  # Then split the rest at the last underscore
  separate(rest, into = c("term", "gisd_const"), sep = "_(?=[^_]+$)")

slopes$settlement <- factor(slopes$settlement, levels=c("urban","rural"),
                            labels = c("Urban", "Rural"))

#include reference cat
#take any temp cat
reference <- slopes %>% filter(term == "temp_cat_18")
#change name and set values to zero
reference <- reference %>% mutate(term="temp_cat_9 (ref)",
                                  estimates=0, lower=0, upper=0)

slopes <- rbind(slopes, reference)

#order everything so that we get the right order
slopes$term <- factor(slopes$term, levels=c("temp_cat_min", "temp_cat_neg6","temp_cat_neg3", "temp_cat_zero","temp_cat_3" ,          
                                                    "temp_cat_6","temp_cat_9 (ref)","temp_cat_16", "temp_cat_18" , "temp_cat_21", "temp_cat_24"),
                          labels = c("<-6", "-6;-3", "-3;0", "0;3", "3;6", "6;9",
                                     "9;16", "16;18", "18;21", "21;24", ">24"))

slopes$gisd_const <- factor(slopes$gisd_const, levels = c("gisd1","gisd2", "gisd3"),
                            labels = c("Low Deprivation","Medium Deprivation", "High Deprivation"))

## simple calculation to obtain relative change by dividing estimates 
## 104.22 and 97.54 are the predicted overall hospitalization rate for rual and urban sample
slopes$rel_change <- ifelse(slopes$settlement == "Rural", slopes$estimate/104.22*100,
                            ifelse(slopes$settlement == "Urban", slopes$estimate/97.54*100, NA))

# ME line PLOT ------------------------------------------------------------
my_colors2 <- c("#3CB44B", "#B0B0B0", "#9370DB")

p2 <- plot_function(dat=slopes, strata = settlement, colors = my_colors2)

ggsave("Figures/main_rel.change_GISD.settlement.svg", plot=p2, dpi=300, height = 6, width = 6)

# age ---------------------------------------------------------------------
slopes <- read.csv("Tables/cumulativeME_FE_GISDagestrat_bin.csv")

# #split name variable
slopes <- slopes %>%
  # First, split at the dot
  separate(name, into = c("age_cat", "rest"), sep = "\\.") %>%
  # Then split the rest at the last underscore
  separate(rest, into = c("term", "gisd_const"), sep = "_(?=[^_]+$)")

#slopes <- slopes %>% rename(term = name)

#include reference cat
#take any temp cat
reference <- slopes %>% filter(term == "temp_cat_18")
#change name and set values to zero
reference <- reference %>% mutate(term="temp_cat_9 (ref)",
                                  estimates=0, lower=0, upper=0)

slopes <- rbind(slopes, reference)

#order everything so that we get the right order
slopes$term <- factor(slopes$term, levels=c("temp_cat_min", "temp_cat_neg6","temp_cat_neg3", "temp_cat_zero","temp_cat_3" ,          
                                            "temp_cat_6","temp_cat_9 (ref)","temp_cat_16", "temp_cat_18" , "temp_cat_21", "temp_cat_24"),
                      labels = c("<-6", "-6;-3", "-3;0", "0;3", "3;6", "6;9",
                                 "9;16", "16;18", "18;21", "21;24", ">24"))

slopes$age_cat <- factor(slopes$age_cat, levels = c("<30 years", "30 to 65 years", ">65 years"),
                         labels = c("<30", "30 to 65", ">65 years"))
slopes$gisd_const <- factor(slopes$gisd_const, levels = c("gisd1","gisd2", "gisd3"),
                            labels = c("Low Deprivation","Medium Deprivation", "High Deprivation"))

## simple calculation to obtain relative change by dividing estimates 
## 50.53, 57.66 and 191.64 are the predicted overall hospitalization rate for each age group
slopes$rel_change <- ifelse(slopes$age_cat == "<30", slopes$estimate/50.53*100,
                            ifelse(slopes$age_cat == "30 to 65", slopes$estimate/57.66*100,
                                   ifelse(slopes$age_cat == ">65 years", slopes$estimate/191.64 *100,NA)))

# ME line PLOT ------------------------------------------------------------
p3 <- plot_function(dat=slopes, strata = age_cat, colors = my_colors2)

#adapt legend position so it's inside the plot
p3_final <- p3 + theme(
  legend.position = "inside",
  legend.position.inside = c(0.75,0.2),
  legend.box = "vertical") 

ggsave("Figures/main_agestrat_w.rel.change.svg", plot=p3_final, dpi=300, height = 6, width = 10)
