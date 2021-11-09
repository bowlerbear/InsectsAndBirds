library(tidyverse)
library(sf)
library(lubridate)

#### birds ####

#read in transect squares
squares <- st_read(dsn = "data/Insects_and_TTT",
                    layer = "transect squares utm32")
plot(squares)
head(squares)

#group into the kvadratnr??
squares <- squares %>%
            dplyr::group_by(kvadratnr) %>%
            dplyr::summarise()
  
plot(squares)


#also grouped into 5 x 5 km squares (previous atlas data presence/absence)
#1 km sometimes moved due to waterbodies

#read in transects data where birds were counted
transects <- st_read(dsn = "data/Insects_and_TTT",
                     layer = "transects utm32")
plot(transects)
head(transects)

#kvadratnr - grid - same as insect mobile data
#lok_grp_id - regional grouping

#read in bird observation data - counts of birds using distance sampling
data <- read.csv("data/Insects_and_TTT/ttt_data.csv",sep=";")
head(data)
#artnr  -species code
#artnavn - common danish name
#kvadratnr - link to grid above
#type - we should use "sen" (13 May to 22nd June?)
#turid - trip/survey ID
#dato - pull out year

#distance at which birds were seen
#X.0 0 - 25 m
#X.1  25 - 50m
#X.2  50 - 100m
#X.UB - outside distance band
#X.OF - flying over
#id

#distance function: half-normal, 3 standard distance function
#Distance software - not used so far due to hedgerow bias

data$Date <- as.Date(data$dato)
data$Year <- year(data$Date) #2014 to 2018 - many fewer obs in 2018??
table(data$type,data$Year)
#        2014  2015  2016  2017  2018
#sen     6960  9153 10920 11579     0 #late (includes migrants)
#tidlig  6663  6901 11558 10399     0 #early (not includes some migrants)

#vinter  1957  6757  7794  5001  3279

#read in transect info
info <- read.csv("data/Insects_and_TTT/ttt_info.csv",sep=";")
#tidfra - start time
#tidtil - end time
#skydaekke - cloud cover 1 to 3 - low(0-33) to high (66-100)
#regn - rain 1 to 3
#vind - wind 1 to 3
#sigtbarhed - visibility 1 or 2 (which is which?) 1 = good or 2 = moderate (no counts outside)
#obserid - observer ID

#standard filtering??
#species, date or transect??

#nb: some transects are not randomly (e.g. along paths or roads)
#basemap - roads and paths 10 x 10 m grid biased towards bushes rather than open land (not allowed to walk on field)
#overreprenstation of edge species
#e.g., skylark distance function higher at mid

