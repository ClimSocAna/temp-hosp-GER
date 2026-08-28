#setwd()

library(tidyverse)
library(data.table)
library(sf)
library(terra)
library(exactextractr)
library(readxl)

#load functions
source("functions.R")

# AGS harmonization file: includes updated AGS codes for 2005-2021 -------------------------------------------

#load in table with harmonized AGS codes
#contains original AGS codes and new AGS codes with year that the code was changed
harm_vb_ags <- read.table("Data/harmonized_ags_2005to2021.txt",sep=";",header=T)

#update file
harm_vb_ags_unique <- harm_vb_ags %>% 
  #adapt format
  mutate(pat_ags5 = as.numeric(substr(AGS_orig,1,nchar(AGS_orig)-3))) %>%
  rename(year = Year) %>%
  # keep only Kreis level
  select(pat_ags5, year, AGS_N3_21) %>%
  distinct(pat_ags5, year, .keep_all = TRUE)  %>%
  #identify those AGS that dont have 2011
  group_by(pat_ags5) %>%
  mutate(has_2011 = any(year == 2011)) %>%
  ungroup()

#for those who do not have an observation in 2011, copy the observation from 2010
rows_to_add <- harm_vb_ags_unique %>%
  filter(!has_2011, year == 2010) %>%
  mutate(year = 2011)

# Combine the original data with the new rows
harm_vb_ags_unique_update <- rbind(harm_vb_ags_unique[,-4], rows_to_add[,-4])
harm_vb_ags_unique_update <- harm_vb_ags_unique_update[order(harm_vb_ags_unique_update$year,
                                                             harm_vb_ags_unique_update$pat_ags5), ]

# hospitalization counts --------------------------------------------------

## hospitalization counts were aggregated by AGS (district), week, gender/sex, age group using STATA
## exclusion criteria before aggregating can be found in Methods section

#load each year and combine into one
weekcounts <- vector(mode = "list",length = 17)

for (d in 2005:2021){
   weekcounts[[d-2004]] <- readxl::read_excel(paste0("Data/hosp_",d,"_weekcounts.xlsx"))
   }

weekcounts <- do.call(rbind.data.frame, weekcounts)

#make sure data types are correct
weekcounts$pat_ags5 <- as.numeric(weekcounts$pat_ags5)
weekcounts$aufn_anl_notfall <- as.factor(weekcounts$aufn_anl_notfall)
weekcounts$age_cat <- as.factor(weekcounts$age_cat)
weekcounts$count_var <- as.integer(weekcounts$count_var)

## we have city region codes 11001 to 11012 for Berlin, combine into one for Berlin (11000)
weekcounts$pat_ags5 <- ifelse(weekcounts$pat_ags5 > 11000 & weekcounts$pat_ags5 <= 11012, 11000, weekcounts$pat_ags5) 

#remove everyone from year 2004
## these are only cases that were admitted at the end of 2004 but released in 2005
weekcounts <- weekcounts[weekcounts$year_according_to_aufndat != 2004, ]

#correct year variable if admission year does not match release year
weekcounts$year <- ifelse(weekcounts$year_according_to_aufndat != weekcounts$year_according_to_dataset,
                          weekcounts$year_according_to_aufndat, weekcounts$year_according_to_dataset)

#aggregate across pat_ags5 (districts)
weekcounts <- weekcounts %>% group_by(pat_ags5, age_cat, geschlecht, week_aufn, year, aufn_anl_notfall) %>% 
  summarise(count = sum(count_var)) %>%
  ungroup()

#join the AGS2021 codes
weekcounts <- left_join(weekcounts, harm_vb_ags_unique_update, by=c("year","pat_ags5"))

## there are some AGS that are coded wrong based on the year
check_NAs <- weekcounts[is.na(weekcounts$AGS_N3_21),]
AGS_to_fix <- unique(check_NAs$pat_ags5) 
years_to_fix <- check_NAs %>% distinct(pat_ags5, year)

#these pat_ags codes that couldnt be harmonized is bc they already include the changed AGS (that was harmonized the year after)
#we can therefore just fill the missing AGS with the pat_ags in this case
weekcounts$AGS_N3_21 <- ifelse(is.na(weekcounts$AGS_N3_21), weekcounts$pat_ags5, weekcounts$AGS_N3_21)

#exclude routine hospitalizations
weekcounts <- weekcounts[weekcounts$aufn_anl_notfall==1,]

# add population counts -------------------------------------------------------
## combine hosp counts with population counts

## obtained from https://genesis.destatis.de/datenbank/online/statistic/12411/table/12411-0018

#load pop count data by year, age groups, gender and AGS5
#skip first 4 rowsbecause they just have information about the data
pop_counts_byAGS5 <- fread("Data/pop_counts_byAGS.age.sex.2005.to.2024.csv",skip = 4, fill=T, na.strings = "-")
pop_counts_byAGS5 <- as.data.frame(pop_counts_byAGS5)

#clean dataset
#remove the last few rows because they just have some information about the data
#every second row is "e"'s. remove those
pop_counts_byAGS5 <- pop_counts_byAGS5[-c(9999:nrow(pop_counts_byAGS5)),]
pop_counts_byAGS5 <- pop_counts_byAGS5[, c(1:3, seq(4, ncol(pop_counts_byAGS5), by = 2))]

#make new variable that has gender and age in one
#and make that the column names so we can reshape it after
new_names <- paste0(pop_counts_byAGS5[1,4:37], "_", pop_counts_byAGS5[2,4:37])
colnames(pop_counts_byAGS5) <- c("year","pat_ags5","Kreis",new_names)

#remove first two rows because that information is now in the column header
pop_counts_byAGS5 <- pop_counts_byAGS5[-c(1,2),]

#pivot
pop_counts_byAGS5 <- pop_counts_byAGS5 %>%
  pivot_longer(cols = 4:37) %>% 
  separate_wider_delim(name, names = c("geschlecht","age"), delim="_") 

#put datasets in correct format so we can merge them and there is no confusion what level is what sex
weekcounts$geschlecht <- factor(weekcounts$geschlecht, labels = c("female","male"))
weekcounts$age_cat <- factor(weekcounts$age_cat,
                             levels = c("<30 years", "30 to 65 years", ">65 years"))
pop_counts_byAGS5$geschlecht <- as.factor(pop_counts_byAGS5$geschlecht)
pop_counts_byAGS5$geschlecht <- factor(pop_counts_byAGS5$geschlecht, labels = c("female","male"))
pop_counts_byAGS5$year       <- as.numeric(sub("31.12.", "", pop_counts_byAGS5$year))

#we want age to be categorized into <30 years, 30 to 65 years, >65 years
pop_counts_byAGS5$age_cat    <- as.factor(ifelse(pop_counts_byAGS5$age %in% c("unter 3 Jahre", "3 bis unter 6 Jahre",
                                                                              "6 bis unter 10 Jahre", "10 bis unter 15 Jahre",
                                                                              "15 bis unter 18 Jahre", "18 bis unter 20 Jahre",
                                                                              "20 bis unter 25 Jahre", "25 bis unter 30 Jahre"), "<30 years",
                                       ifelse(pop_counts_byAGS5$age %in% c("30 bis unter 35 Jahre", "35 bis unter 40 Jahre",
                                                                           "40 bis unter 45 Jahre", "45 bis unter 50 Jahre",
                                                                           "50 bis unter 55 Jahre", "55 bis unter 60 Jahre",
                                                                           "60 bis unter 65 Jahre"), "30 to 65 years",
                                              ifelse(pop_counts_byAGS5$age %in% c("65 bis unter 75 Jahre", "75 Jahre und mehr"),
                                                     ">65 years", NA))))

#add AGS codes 
pop_counts_byAGS5 <- left_join(pop_counts_byAGS5, harm_vb_ags_unique[,c("pat_ags5", "year", "AGS_N3_21")],
                               by=c("year","pat_ags5"))

#aggregate by age groups and ags 21
pop_counts_byAGS5_agegroups <- pop_counts_byAGS5 %>%
  group_by(year, AGS_N3_21, geschlecht, age_cat) %>%
  summarise(pop_counts = sum(as.numeric(value))) %>%
  ungroup()

## attach to weekcounts
weekcounts_w.popcounts <- left_join(weekcounts, pop_counts_byAGS5_agegroups, by = c("year", "AGS_N3_21", "geschlecht", "age_cat"))

#rename 
weekcounts_w.popcounts <- weekcounts_w.popcounts %>%
  rename(N_hospitalization = count,
         N_population = pop_counts,
         week = week_aufn) 

#sort
weekcounts_v3 <- weekcounts_w.popcounts[,c("year", "AGS_N3_21", "week", "age_cat",
                                           "geschlecht", "aufn_anl_notfall", 
                                           "N_hospitalization", "N_population")]

rm(pop_counts_byAGS5_agegroups, weekcounts_w.popcounts, weekcounts); gc()

# add German Socioeconomic Index of Deprivation (GISD) ------------------------
#load("Data/GISD.RData")

#download GISD on Kreislevel from 1998 until 2019

# URL of the raw file from GitHub
url <- "https://raw.githubusercontent.com/robert-koch-institut/German_Index_of_Socioeconomic_Deprivation_GISD/refs/heads/main/GISD_Release_aktuell/Bund/GISD_Bund_Kreis.csv"

# Read the CSV directly into R
GISD <- read_csv(url,show_col_types = FALSE)

#prep to join with weekly counts
GISD <- GISD %>% rename(AGS_N3_21 = kreis_id)
GISD$AGS_N3_21 <- as.numeric(GISD$AGS_N3_21)

#join with our weekly counts
weekcounts_with_GISD <- left_join(weekcounts_v3, GISD, by=c("AGS_N3_21","year"))

rm(weekcounts_v3, GISD); gc()

# Temperature Information from EOBS -------------------------------------------
# load shapefile for German districs (obtained from https://www.bkg.bund.de/)
shape_data <- st_read("Data/VG5000_KRS.shp")
#shape_data <- st_read("/home/gueltzowm/scratch/DRG2/data/vg5000_ebenen_1231/VG5000_KRS.shp")

# shapefile is in meters but temperature file is long/langitude
# transform shapefile
new_shp <- st_transform(shape_data, crs = "+proj=longlat +datum=WGS84")

## load human settlement data (1x1km grid, -200 = NA, 2015)
#obtained from https://human-settlement.emergency.copernicus.eu/ghs_pop2023.php 
HSL <- terra::rast("Data/GHS_POP_E2015_GLOBE_R2023A_54009_1000_V1_0.tif")

# transform shapefile in native Mollweide so it matches HSL
new_shp_moll <- st_transform(new_shp, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m")
e_moll       <- terra::ext(vect(new_shp_moll))       
HSL_crop     <- terra::crop(HSL, e_moll)
HSL_GER      <- project(HSL_crop, "EPSG:4326", method = "bilinear")

#-200 is NA
HSL_GER[HSL_GER == -200] <- NA

# aggregate meteorological information to district level
mean.temp_95to10 <- preprocess(file_path='Data/tg_ens_mean_0.1deg_reg_1995-2010_v30.0e.nc',
                               measure="tg",
                               shape_file=new_shp)
mean.temp_11to24 <- preprocess(file_path='Data/tg_ens_mean_0.1deg_reg_2011-2024_v30.0e.nc',
                               measure="tg",
                               shape_file=new_shp)

max.temp_95to10 <- preprocess(file_path='Data/tx_ens_mean_0.1deg_reg_1995-2010_v30.0e.nc',
                              measure="tx",
                              shape_file=new_shp)
max.temp_11to24 <- preprocess(file_path='Data/tx_ens_mean_0.1deg_reg_2011-2024_v30.0e.nc',
                              measure="tx",
                              shape_file=new_shp)

#all files now have the name weighted mean
#change names beforehand so we can distinguish it
mean.temp_95to10 <- rename_cols(mean.temp_95to10, "mean")
mean.temp_11to24 <- rename_cols(mean.temp_11to24, "mean")
max.temp_95to10  <- rename_cols(max.temp_95to10,  "max")
max.temp_11to24  <- rename_cols(max.temp_11to24,  "max")

#attach AGS information from shapefile
mean.temp_95to10$AGS <- shape_data$AGS

#move AGS to front
mean.temp_95to10 <- mean.temp_95to10[,c(ncol(mean.temp_95to10), 
                                        1:(ncol(mean.temp_95to10)-1))]

#combine
temp <- cbind(mean.temp_95to10, mean.temp_11to24,
              max.temp_95to10, max.temp_11to24)

rm(mean.temp_95to10, mean.temp_11to24, 
   max.temp_95to10, max.temp_11to24)

#put in right format to join with hospitalization data
temp_aggr <- temp %>%
  pivot_longer(
    cols = -1,  # Adjust this range as needed
    names_sep = "\\.",
    names_to = c("measurement","date"),
    values_to = c("temperature")) %>%
  pivot_wider(
    names_from = "measurement",
    values_from = "temperature")

#set date
temp_aggr$date <-  as.Date(temp_aggr$date, format = "%Y-%m-%d")

#extract year
temp_aggr$year <- year(temp_aggr$date)

#calculate weeks
temp_aggr$week <- week(temp_aggr$date)

# set every observation that is week = 53 to week 52
#now week 52 is longer than the rest of the weeks
#but this is how it's treated in the hospitalization file too
temp_aggr$week <- ifelse(temp_aggr$week == 53, 52, temp_aggr$week)

#create temperature bins based on mean
temp_aggr$mean <- as.numeric(temp_aggr$mean)
temp_aggr$temp_cat <- cut(temp_aggr$mean,
                          breaks = c(min(temp_aggr$mean),
                                     -6, -3, 0, 3, 6, 9, 16, 18, 21, 24, max(temp_aggr$mean)),
                          include.lowest = T)
check_cats <- temp_aggr[temp_aggr$mean < -5.9999 & temp_aggr$mean > -6.1,]
levels(temp_aggr$temp_cat)
## categorization is: warmer than -5.9999 is -6 to -3, colder than 6.0000 is -18 to -6

#aggregate
#we want each temperature category variable to be a separate column 
# with the N of days that week that were in each temperature category
temp_aggr_week <- temp_aggr %>%
  group_by(AGS, year, week) %>%
  summarise(mean_temp = mean(mean),
            max_temp = max(max),
            
            temp_cat_min = sum(temp_cat == "[-19.3,-6]"),
            temp_cat_neg6 = sum(temp_cat == "(-6,-3]"),
            temp_cat_neg3 = sum(temp_cat == "(-3,0]"),
            temp_cat_zero = sum(temp_cat == "(0,3]"),
            temp_cat_3 = sum(temp_cat == "(3,6]"),
            temp_cat_6 = sum(temp_cat == "(6,9]"),
            temp_cat_9 = sum(temp_cat == "(9,16]"),
            temp_cat_16 = sum(temp_cat == "(16,18]"),
            temp_cat_18 = sum(temp_cat == "(18,21]"),
            temp_cat_21 = sum(temp_cat == "(21,24]"),
            temp_cat_24 = sum(temp_cat == "(24,31.5]")) %>%
  rename(AGS_N3_21 = AGS) %>%
  ungroup()

#create mean weekly temperature lags and lags of temp bins (4)
temp_aggr_week <- temp_aggr_week %>%
  group_by(AGS_N3_21) %>%
  mutate(across(
    .cols = c("mean_temp","temp_cat_min","temp_cat_neg6","temp_cat_neg3",
              "temp_cat_zero","temp_cat_3","temp_cat_6","temp_cat_9",
              "temp_cat_16","temp_cat_18","temp_cat_21","temp_cat_24"), 
    .fns = list(
      lag_week1 = ~lag(., 1),
      lag_week2 = ~lag(., 2),
      lag_week3 = ~lag(., 3)),
    .names = "{.col}_{.fn}"
  )) %>% 
  ungroup()

#make sure AGS is numeric for merging later
temp_aggr_week$AGS_N3_21 <- as.numeric(temp_aggr_week$AGS_N3_21)

#join temp to data
weekcounts_temp <- left_join(weekcounts_with_GISD, temp_aggr_week, by = c("AGS_N3_21","year","week"))
rm(weekcounts_with_GISD); rm(temp_aggr_week); gc()

#create date variable
weekcounts_temp <- weekcounts_temp %>%
  mutate(date=as.Date(paste(year,week,1), format= "%Y %U %u"),
         month=month(date))

## make sure variable classes are correct
weekcounts_temp <- weekcounts_temp %>%
  mutate(year = as.integer(year), 
         AGS_N3_21 = as.factor(AGS_N3_21),
         week = as.integer(week),
         N_hospitalization = as.integer(N_hospitalization),
         N_population = as.integer(N_population),
         month = as.integer(month))

rm(weekcounts_with_GISD, temp, temp_aggr, HSL, HSL_GER, new_shp, new_shp_moll); gc()

# Regional Characteristics from INKAR -------------------------------------
## SETTLEMENT DATA for 2021
##obtained from https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/downloads/download-referenzen.html 
settle <- read_excel("Data/raumgliederungen-referenzen-2021.xlsx", sheet = "Kreisreferenz")
settle <- settle[-1,c(1,2,19:length(settle))]

#we need only KTU 2021
settle$settlement <- factor(settle$KTU2021, labels = c("kreisfreie Großstadt", "Städtischer Kreis",
                                                 "Ländlicher Kreis mit Verdichtungsansätzen",
                                                 "Dünn besiedelter ländlicher Kreis"))
#edit AGS to be Kreisschlüssel
settle$AGS_N3_21 <- gsub('.{3}$', '', settle$KRS2021)

## LIFE EXPECTANCY DATA
#seperate data sources for 95 to 2017 and 2017-2022
# first source can be found on the INKAR database https://www.inkar.de/
# second source was reported by the Federal Institute for Population
LE_95to17 <- read_excel("Data/INKAR_LE_Kreise_Geschlecht_1995_2017.xlsx", sheet = "Daten")
LE_17to22 <- read_excel("Data/DE_LE_Kreise_Geschl_2017-2022.xlsx")

#preprocessing to make it easier to work with
#clean up colnames
colnames(LE_95to17) <- sub("\\..*", "", colnames(LE_95to17))

#update col names to include year
colnames(LE_95to17) <- paste0(colnames(LE_95to17),"_",LE_95to17[1,]) 

#remove first row and reorder because some variables do not change over time
LE_95to17 <- LE_95to17[-1,c(1:3,73:75,4:72,76:144)]

#transform into long format
LE_95to17_long <- LE_95to17 %>%
  select(-Raumeinheit_NA) %>%
  rename(pat_ags5 = Kennziffer_NA) %>%
  pivot_longer(cols = -c(pat_ags5, Aggregat_NA,`Veränderung Lebenserwartung_2017`,`Veränderung Lebenserwartung Männer_2017`,`Veränderung Lebenserwartung Frauen_2017`),   # Columns to pivot (exclude ID)
               names_to = c("Indicator", "year"),  # Split column names
               names_sep = "_") %>% 
  pivot_wider(names_from = "Indicator", values_from = value) %>%
  mutate(year = as.integer(year),
         pat_ags5 = as.numeric(pat_ags5)) %>%
  filter(year >= 2005)

#create a column for gender 
LE_95to17_long <- LE_95to17_long %>%
  select(-c(3:5)) %>%
  pivot_longer(cols = -c("pat_ags5","Aggregat_NA","year"),
               names_to = c("Indicator", "geschlecht"),
               names_pattern = "^(.*) (.{6})$") %>%
  mutate(geschlecht = ifelse(is.na(geschlecht),"All", geschlecht),
         Indicator = ifelse(is.na(Indicator), lead(Indicator), Indicator)) %>%
  rename(life_exp = value) %>%
  pivot_wider(names_from = "Indicator", values_from = "life_exp") #we want each indicator to have its own column

LE_95to17_long$geschlecht <- factor(LE_95to17_long$geschlecht, labels = c("All", "female", "male"))

#check if Kreise are harmonized
#Eisenach 16066 needs to be included in Wartburgkreis (16063)
#subset harmonization file 
harm_subset <- harm_vb_ags_unique[harm_vb_ags_unique$pat_ags5 == "16063" 
                                  | harm_vb_ags_unique$AGS_N3_21 == "16063"
                                  | harm_vb_ags_unique$AGS_N3_21 == "16066",]

#join with dataset
LE_95to17_long <- left_join(LE_95to17_long, harm_subset, by=c("pat_ags5","year"))

#fix AGS column
LE_95to17_long$AGS_N3_21 <- ifelse(is.na(LE_95to17_long$AGS_N3_21), LE_95to17_long$pat_ags5,
                                   LE_95to17_long$AGS_N3_21)

#check for duplicates
check_duplicates <- LE_95to17_long %>%
  count(AGS_N3_21, year, geschlecht) %>%
  filter(n > 1)

#we have duplicates for 16063 and 16066, take the mean LE here
LE_95to17_long <- LE_95to17_long %>%
  group_by(AGS_N3_21, year, geschlecht) %>%
  summarize(Lebenserwartung = mean(as.numeric(Lebenserwartung)),
            `Restlebenserwartung der 60-jährigen` = mean(as.numeric(`Restlebenserwartung der 60-jährigen`))) %>%
  ungroup()

## check other dataset from 2017
check_AGS <- LE_17to22[(LE_17to22$Region %in% harm_vb_ags_unique$AGS_N3_21) == F,]

#restructure and rename
LE_17to22 <- LE_17to22 %>%
  rename(AGS_N3_21 = Region,
         geschlecht = Sex,
         year = Year,
         life_exp = ex) %>%
  mutate(Indicator = ifelse(Age==0, "Lebenserwartung","Restlebenserwartung ab 65"),
         geschlecht = factor(geschlecht, levels=c("b","f","m"),
                             labels=c("All","female","male"))) %>%
  select(-Age) %>%
  pivot_wider(names_from = "Indicator", values_from = "life_exp") #indicator has its own column

LE_17to22_long <- LE_17to22 %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(year = rep(rep(c(2017:2022), 6),200)) %>%
  filter(year > 2017) #because we have the calculations from other data source 

LE_17to22_long$AGS_N3_21 <- as.character(LE_17to22_long$AGS_N3_21)

#join both LE datasets
LE <- rbind(LE_95to17_long[,1:4], LE_17to22_long[,c(1,3,2,4)])

## OLD AGE DEPENDENCY FROM INKAR DATABASE
INKAR_oldage <- read.csv("Data/INKAR_indicators_oldagedependency.csv", dec=",", fill=T, sep=";", header=F)

#update col names to include year
colnames(INKAR_oldage) <- paste0(INKAR_oldage[1,],"_",INKAR_oldage[2,]) 

INKAR_oldage <- INKAR_oldage[-c(1,2),]

#some variables got a bit messed up because of the different decimal systems, correct:
INKAR_oldage[] <- cbind(INKAR_oldage[,1:3],
                        lapply(INKAR_oldage[,4:87], function(x) gsub("\\.", "", x)))
INKAR_oldage[] <- cbind(INKAR_oldage[,1:3],
                        lapply(INKAR_oldage[,4:87], function(x) as.numeric(gsub(",", ".", x))))

#transform into long format
INKAR_oldage_long <- INKAR_oldage %>%
  select(-Raumeinheit_) %>%
  rename(AGS_N3_21 = Kennziffer_) %>%
  pivot_longer(cols = -c(AGS_N3_21, Aggregat_),   # Columns to pivot (exclude ID)
               names_to = c("Indicator", "year"),  # Split column names
               names_sep = "_") %>% 
  pivot_wider(names_from = "Indicator", values_from = value) %>%
  mutate(year = as.integer(year),
         AGS_N3_21 = as.factor(as.numeric(AGS_N3_21)),
         `Einwohner 65 Jahre und älter` = as.numeric(`Einwohner 65 Jahre und älter`/100),
         `Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre)` = as.numeric(`Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre)`),
         `Bevölkerung gesamt` = as.numeric(`Bevölkerung gesamt`)) %>%
  filter(year >= 2005) 

#calculate population above age 65
INKAR_oldage_long$N_age65plus <- INKAR_oldage_long$`Einwohner 65 Jahre und älter` * INKAR_oldage_long$`Bevölkerung gesamt`

#calculate OAD ratio
INKAR_oldage_long$OAD_ratio_per100 <- (INKAR_oldage_long$N_age65plus / INKAR_oldage_long$`Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre)`)*100

#join with rest of the data
weekcounts_temp <- left_join(weekcounts_temp, LE, by=c("AGS_N3_21","year", "geschlecht")) 
weekcounts_temp <- left_join(weekcounts_temp, INKAR_oldage_long, by=c("AGS_N3_21","year"))
weekcounts_temp <- left_join(weekcounts_temp, settle[,c("AGS_N3_21","settlement")], by=c("AGS_N3_21"))

rm(settle, LE, LE_17to22, LE_17to22_long, LE_95to17, LE_95to17_long,
   INKAR_oldage, INKAR_oldage_long); gc()

# prepare analysis df -----------------------------------------------------
#to make sure that we are calculating the correct hospitalization rate
#we need to do some more pre-processing so that the hosp rate is actually accurate, right now it's not
# since i wanna keep all the other variables make sure to keep them. 
#unique works because every age and gender group is exposed to the same temperature in that region and week

#do this in data.table format so its faster
setDT(weekcounts_temp)
setkey(weekcounts_temp, AGS_N3_21, year, week, geschlecht, age_cat)

# columns to sum
sum_cols <- "N_hospitalization"

# columns to take unique() of
unique_cols <- c("N_population", "gisd_k", "settlement",
                 "OAD_ratio_per100", "Lebenserwartung", "mean_temp",
                 grep("temp_cat", names(weekcounts_temp), value = TRUE))

weekcounts_temp <- weekcounts_temp[
  , c(
    setNames(list(sum(get(sum_cols))), sum_cols),
    lapply(mget(unique_cols), function(x) x[1L])
  ),
  by = .(AGS_N3_21, year, week, geschlecht, age_cat)
]

#not strictly necessary but change it back to data.frame so we dont have any sort of issues
weekcounts_temp <- as.data.frame(weekcounts_temp)

#remove the last week of 2021 bc there are likely many cases here that were admitted in that week but "entlassen" the week after
#which means that they would be in the data for 2022
weekcounts_temp$date <- as.Date(paste(weekcounts_temp$year,weekcounts_temp$week,1), format= "%Y %U %u")
weekcounts_temp <- weekcounts_temp %>%
  filter(date != "2021-12-27")

#year and AGS to factor
weekcounts_temp$year <- as.factor(weekcounts_temp$year)
weekcounts_temp$AGS_N3_21 <- as.factor(weekcounts_temp$AGS_N3_21)
weekcounts_temp$geschlecht <- droplevels(weekcounts_temp$geschlecht)

#calculate correct population counts
weekcounts_temp$N_population.corr <- weekcounts_temp$N_population/52

#categorize covariates into terciles
Lebenserwartung_tert <- quantile(weekcounts_temp$Lebenserwartung, probs = c(0,1/3,2/3,1))
weekcounts_temp$Lebenserwartung_tert <- cut(weekcounts_temp$Lebenserwartung,
                                            breaks = Lebenserwartung_tert, include.lowest=T)

OAD_ratio_tert <- quantile(weekcounts_temp$OAD_ratio_per100, probs = c(0,1/3,2/3,1))
weekcounts_temp$OAD_ratio_tert <- cut(weekcounts_temp$OAD_ratio_per100,
                                      breaks = OAD_ratio_tert, include.lowest=T)

#make sure to take gisd_k that occurs most often in district
weekcounts_temp <- weekcounts_temp %>%
  group_by(AGS_N3_21) %>%
  mutate(gisd_const = (as.integer(names(which.max(table(gisd_k)))))) %>%
  ungroup()

weekcounts_temp$gisd_const <- as.factor(weekcounts_temp$gisd_const)

#binary degree of urbanization variable
weekcounts_temp$settlement_bin <- ifelse(weekcounts_temp$settlement %in% c("kreisfreie Großstadt","Städtischer Kreis"), "urban",
                                         ifelse(weekcounts_temp$settlement %in% c("Ländlicher Kreis mit Verdichtungsansätzen","Dünn besiedelter ländlicher Kreis"), "rural",
                                                NA))
weekcounts_temp$settlement_bin <- factor(weekcounts_temp$settlement_bin, levels = c("urban","rural"))

save(file = "weekcounts_temp_analysis.RData", weekcounts_temp)

# FOR MODEL COMPARISON: train/test data -----------------------------------
## delete 20% of rows in each year/AGS combination
#add row indicator
weekcounts_temp$row <- seq(1:nrow(weekcounts_temp))

# Create empty lists to store results
test_list <- list()
train_list <- list()
i <- 1  # index for storing in list

set.seed(1234)

# Loop over years and AGS groups
for (y in 2005:2021) {
  for (ags in unique(weekcounts_temp$AGS_N3_21)) {
    
    # Filter only once
    temp_subset <- weekcounts_temp[weekcounts_temp$year == y & weekcounts_temp$AGS_N3_21 == ags, ]
    
    # Randomly select rows to delete (20%)
    delete_rows <- sample(temp_subset$row, round(nrow(temp_subset) * 0.2))
    
    # Partition into test and train
    test_list[[i]] <- temp_subset[temp_subset$row %in% delete_rows, ]
    train_list[[i]] <- temp_subset[!(temp_subset$row %in% delete_rows), ]
    
    i <- i + 1
  }
  print(y)
}

# Combine all at once
test <- do.call(rbind, test_list)
train <- do.call(rbind, train_list)

#check if it's a 80/20 split
nrow(test)/nrow(weekcounts_temp)
nrow(train)/nrow(weekcounts_temp)

save(file="Data/Test_data_modelcomparison.RData", test)
save(file="Data/Train_data_modelcomparison.RData", train)

