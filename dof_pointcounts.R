library(sf)
library(tidyverse)
library(lubridate)
library(tmap)

### standard squares ####

#read in transect squares
squares <- st_read(dsn = "data/Insects_and_TTT",
                   layer = "transect squares utm32")
head(squares)

tm_shape(squares)+
  tm_fill("kvadratnr")

#### bird data ####

#bird count data
pointcountData <- read.csv("data/Insects_and_TTT/point_count_data.csv",sep=";")
head(pointcountData)
#obsid - unique species observation ID
#tid - survey ID (turID) (route and year)
#pid - point ID (subset of survey ID)
#artnr - species ID code
#antal - number of birds

#survey data (a survey is a route in a specific year)
pointcountInfo <- read.csv("data/Insects_and_TTT/point_count_info.csv",sep=";")
head(pointcountInfo)
table(pointcountInfo$tyear)
#tid - survey ID (turID) (route and year)
#rid - routeID
#sigt -  distance that you can see
#opdateret_af - ignore (operator)
#times start and end time
#antalpktobsandel?? (prop of points survyed %)
#shift from 20 to 10 points

#route data
pointSites <- read.csv("data/Insects_and_TTT/point_site_info.csv",sep=";",dec=",")
#rid - routeID
#oid - observer ID
#season 
#N   SY   TY    V 
#1 2027  145 1645
#SY - late breeding - choose this May/June
#pkt - total number of points per routeID
#lat/lon
#some points are separated several km but not common

#species code
speciesCodes <- read.csv("data/Insects_and_TTT/species_codes.csv",sep=";")
#arttype - ubestemt (exclude these)
#check - how many for different species

### merging all ####

all(pointcountInfo$tid %in% pointcountData$tid)#FALSE
all(pointcountData$tid %in% pointcountInfo$tid)#TRUE
all(pointcountInfo$rid %in% pointSites$rid)#TRUE
all(pointcountData$artnr %in% speciesCodes$artnr)#TRUE

allData <- pointcountData %>%
            inner_join(.,pointcountInfo, by="tid") %>%
            inner_join(.,pointSites, by="rid") %>%
            inner_join(.,speciesCodes, by="artnr")

### subsetting ####

#late breeding
allData <- filter(allData, season=="SY")

### formating ####

allData$Date <- as.Date(allData$tdate)
allData$Year <- year(allData$Date)
table(allData$Year)#2016 onwards

### make spatial ####

spatialPoints <- st_as_sf(allData,
                          coords = c("lon","lat"),
                          crs = 4326)

# plot data
tm_shape(spatialPoints)+
  tm_dots("rid")
