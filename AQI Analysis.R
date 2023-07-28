#Packages
install.packages("tidyverse")
library(tidyverse)
install.packages("tidyr")
library(tidyr)
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("rio")
library(rio)
install.packages("rmarkdown")
library(rmarkdown)

#Download the years worth of air quality data (by year)
aqi_2022 <- read.csv("annual_aqi_by_county_2022.csv")
aqi_2021 <- read.csv("annual_aqi_by_county_2021.csv")
aqi_2020 <- read.csv("annual_aqi_by_county_2020.csv")
aqi_2019 <- read.csv("annual_aqi_by_county_2019.csv")
aqi_2018 <- read.csv("annual_aqi_by_county_2018.csv")
aqi_2017 <- read.csv("annual_aqi_by_county_2017.csv")
aqi_2016 <- read.csv("annual_aqi_by_county_2016.csv")
aqi_2015 <- read.csv("annual_aqi_by_county_2015.csv")
aqi_2014 <- read.csv("annual_aqi_by_county_2014.csv")
aqi_2013 <- read.csv("annual_aqi_by_county_2013.csv")
aqi_2012 <- read.csv("annual_aqi_by_county_2012.csv")
aqi_2011 <- read.csv("annual_aqi_by_county_2011.csv")
aqi_2010 <- read.csv("annual_aqi_by_county_2010.csv")
aqi_2009 <- read.csv("annual_aqi_by_county_2009.csv")
aqi_2008 <- read.csv("annual_aqi_by_county_2008.csv")
aqi_2007 <- read.csv("annual_aqi_by_county_2007.csv")
aqi_2006 <- read.csv("annual_aqi_by_county_2006.csv")
aqi_2005 <- read.csv("annual_aqi_by_county_2005.csv")
aqi_2004 <- read.csv("annual_aqi_by_county_2004.csv")
aqi_2003 <- read.csv("annual_aqi_by_county_2003.csv")
aqi_2002 <- read.csv("annual_aqi_by_county_2002.csv")
aqi_2001 <- read.csv("annual_aqi_by_county_2001.csv")
aqi_2000 <- read.csv("annual_aqi_by_county_2000.csv")

#Join the Dataframes
air_quality_index <- bind_rows(aqi_2022, aqi_2021, aqi_2020, aqi_2019, aqi_2018,
                               aqi_2017, aqi_2016, aqi_2015, aqi_2014, aqi_2013,
                               aqi_2012, aqi_2011, aqi_2010, aqi_2009, aqi_2008, 
                               aqi_2007, aqi_2006, aqi_2005, aqi_2004, aqi_2003,
                               aqi_2002, aqi_2001, aqi_2000)

export(air_quality_index, "air_quality_index.xlsx") #export to excel

#Check for missing values
air_quality_index %>% 
  summarize(na_state = sum(is.na(State)),
            na_county = sum(is.na(County)),
            na_year = sum(is.na(Year)),
            na_days_with_aqi = sum(is.na(Days.with.AQI)),
            na_good_days = sum(is.na(Good.Days)),
            na_moderate_days = sum(is.na(Moderate.Days)),
            na_sensitive_groups = sum(is.na(Unhealthy.for.Sensitive.Groups.Days)),
            na_unhealty_days = sum(is.na(Unhealthy.Days)),
            na_very_unhealthy_days = sum(is.na(Very.Unhealthy.Days)),
            na_hazardous_days = sum(is.na(Hazardous.Days)),
            na_max_aqi = sum(is.na(Max.AQI)),
            na_90th_percent = sum(is.na(X90th.Percentile.AQI)),
            na_median_aqi = sum(is.na(Median.AQI)),
            na_days_co = sum(is.na(Days.CO)),
            na_days_no2 = sum(is.na(Days.NO2)), 
            na_days_ozone = sum(is.na(Days.Ozone)),
            na_days_pm2.5 = sum(is.na(Days.PM2.5)),
            na_days_pm10 = sum(is.na(Days.PM10)))

#Clean the columns
colnames(air_quality_index) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(air_quality_index)

#Rename the x90th.percentile.aqi to remove the x
air_quality_index <- air_quality_index %>% 
  rename_with(.cols = 12, ~"aqi.90th.percentile")

#Top county's with most unhealthy for sensitive groups days
air_quality_index %>%
  arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
  distinct(state, county, year, .keep_all = TRUE) 

#Top 10 county's with most unhealthy for sensitive groups days for each year
head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2022, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2021, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2020, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2019, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2018, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2017, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2016, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2015, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2014, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2013, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2012, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2011, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2010, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2009, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2008, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2007, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2006, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2005, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2004, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2003, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2002, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2001, .keep_all = TRUE), n=10)

head(air_quality_index %>%
       arrange(desc(unhealthy.for.sensitive.groups.days)) %>%
       distinct(state, county, year=2000, .keep_all = TRUE), n=10)

##average aqi

#Average max aqi per state each year
average_max_aqi <- air_quality_index %>%
  group_by(state, year) %>%
  summarize(average_max_aqi = mean(max.aqi))

View(average_max_aqi)

export(average_max_aqi, "average_max_aqi.xlsx") #export to excel

#Average max aqi per state from 2000 - 2022
average_max_aqi_county <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(average_max_aqi_county = mean(max.aqi))

View(average_max_aqi_county)

export(average_max_aqi_county, "average_max_aqi_county.xlsx") #export to excel


#Average median aqi per state each year
average_median_aqi <- air_quality_index %>%
  group_by(state, year) %>%
  summarize(average_median_aqi = mean(median.aqi))

View(average_median_aqi)

export(average_median_aqi, "average_median_aqi.xlsx") #export to excel

#Average median api per county from 2000 - 2022
average_median_aqi_county <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(average_median_aqi_county = mean(median.aqi))

View(average_median_aqi_county)

export(average_median_aqi_county, "average_median_aqi_county.xlsx") #export to excel

##Days

#Total unhealthy.for.sensitive.groups.days per county each year
total_sensitive_groups_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_sensitive_groups_days = sum(unhealthy.for.sensitive.groups.days))

View(total_sensitive_groups_days)

export(total_sensitive_groups_days, "total_sensitive_groups_days.xlsx") #export to excel

#Total unhealthy.for.sensitive.groups.days per county from 2000 - 2022
total_sensitive_groups_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_sensitive_groups_days_overall = sum(unhealthy.for.sensitive.groups.days))

View(total_sensitive_groups_days_overall)

export(total_sensitive_groups_days_overall, "total_sensitive_groups_days_overall.xlsx") #export to excel

#Total good.days per county each year
total_good_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_good_days = sum(good.days))

View(total_good_days)

export(total_good_days, "total_good_days.xlsx") #export to excel

#Total good.days per county from 2000 - 2022
total_good_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_good_days_overall = sum(good.days))

View(total_good_days_overall)

export(total_good_days_overall, "total_good_days_overall.xlsx") #export to excel

#Total hazardous.days per county each year
total_hazardous_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_hazardous_days = sum(hazardous.days))

View(total_hazardous_days)

export(total_hazardous_days, "total_hazardous_days.xlsx") #export to excel

#Total hazardous.days per county from 2000 - 2022
total_hazardous_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_hazardous_days_overall = sum(hazardous.days))

View(total_hazardous_days_overall)

export(total_hazardous_days_overall, "total_hazardous_days_overall.xlsx") #export to excel

##Pollutants

#Total days per county with carbon monoxide (CO) pollutants per county per year
total_co_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_co_days = sum(days.co))

View(total_co_days)

export(total_co_days, "total_co_days.xlsx") #export to excel

#Total days per county with carbon monoxide (CO) pollutants per county from 2000 - 2022
total_co_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_co_days_overall = sum(days.co))

View(total_co_days_overall)

export(total_co_days_overall, "total_co_days_overall.xlsx") #export to excel

#Total days per county with ozone pollutants per county per year
total_ozone_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_ozone_days = sum(days.ozone))

View(total_ozone_days)

export(total_ozone_days, "total_ozone_days.xlsx") #export to excel


#Total days per county with ozone pollutants per county from 2000 - 2022
total_ozone_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_ozone_days_overall = sum(days.ozone))

View(total_ozone_days_overall)

export(total_ozone_days_overall, "total_ozone_days_overall.xlsx") #export to excel


#Total days per county with nitrogen dioxide pollutants per county per year
total_no2_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_no2_days = sum(days.no2))

View(total_no2_days)

export(total_no2_days, "total_no2_days.xlsx") #export to excel

#Total days per county with nitrogen dioxide pollutants per county from 2000 - 2022
total_no2_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_no2_days_overall = sum(days.no2))

View(total_no2_days_overall)

export(total_no2_days_overall, "total_no2_days_overall.xlsx") #export to excel

#Total days per county with particulate matter (PM10) pollutants per county per year
total_pm10_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_pm10_days = sum(days.pm10))

View(total_pm10_days)

export(total_pm10_days, "total_pm10_days.xlsx") #export to excel

#Total days per county with particulate matter (PM10) pollutants per county from 2000 - 2022
total_pm10_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_pm10_days_overall = sum(days.pm10))

View(total_pm10_days_overall)

export(total_pm10_days_overall, "total_pm10_days_overall.xlsx") #export to excel

#Total days per county with fine particulate matter (PM2.5) pollutants per county per year
total_pm2.5_days <- air_quality_index %>%
  group_by(state, county, year) %>%
  summarize(total_pm2.5_days = sum(days.pm2.5))

View(total_pm2.5_days)

export(total_pm2.5_days, "total_pm2.5_days.xlsx") #export to excel

#Total days per county with fine particulate matter (PM2.5) pollutants per county from 2000 - 2022
total_pm2.5_days_overall <- air_quality_index %>%
  group_by(state, county) %>%
  summarize(total_pm2.5_days_overall = sum(days.pm2.5))

View(total_pm2.5_days_overall)

export(total_pm2.5_days_overall, "total_pm2.5_days_overall.xlsx") #export to excel






