
library(tidyverse)
library(sf)
library(tmap)


# =================== reference grid, bird atlas =================== 
# transect squares -- reference grid (kvadratnr)
squares32 <- st_read(dsn = "data/Insects_and_TTT", layer = "transect squares utm32")
squares33 <- st_read(dsn = "data/Insects_and_TTT", layer = "transect squares utm33")
## also grouped into 5 x 5 km squares (previous atlas data presence/absence)
## 1 km sometimes moved due to waterbodies

# transects data where birds were counted
transects32 <- st_read(dsn = "data/Insects_and_TTT", layer = "transects utm32")
transects33 <- st_read(dsn = "data/Insects_and_TTT", layer = "transects utm33")

## kvadratnr ----------- grid - same as insect mobile data
## lok_grp_id ---------- regional grouping


# bird atlas
## bird observation data - counts of birds using distance sampling
bird_atlas <- read.csv("data/Insects_and_TTT/ttt_data.csv", sep=";") %>% tibble()
## artnr -------------- species code
## artnavn ------------ common danish name
## kvadratnr ---------- link to grid in transect squares
## type --------------- we should use "sen" (13 May to 22nd June?)
## turid -------------- trip/survey ID
## dato --------------- date; pull out year

# distance at which birds were seen
## X.0 ---------------- 0 - 25 m
## X.1 ---------------- 25 - 50m
## X.2 ---------------- 50 - 100m
## X.UB --------------- outside distance band
## X.OF --------------- flying over
## id

# distance function: half-normal, 3 standard distance function
# distance software - not used so far due to hedgerow bias


# transect info
transect_info <- read.csv("data/Insects_and_TTT/ttt_info.csv", sep=";") %>% tibble()
## tidfra ------------ start time
## tidtil ------------ end time
## skydaekke --------- cloud cover 1 to 3 - low (0-33) to high (66-100)
## regn -------------- rain 1 to 3
## vind -------------- wind 1 to 3
## sigtbarhed -------- visibility 1 or 2 (which is which?) 1 = good or 2 = moderate (no counts outside)
## obserid ----------- observer ID


# standard filtering??
# species, date or transect??

# nb: some transects are not randomly (e.g. along paths or roads)
# basemap - roads and paths 10 x 10 m grid biased towards bushes rather than open land (not allowed to walk on field)
# overreprenstation of edge species
# e.g., skylark distance function higher at mid




# =================== bird point count =================== 
# bird count data
bird_count <- read.csv("data/Insects_and_TTT/point_count_data.csv",sep=";") %>% tibble()
## obsid ----------- unique species observation ID
## tid ------------- survey ID (turID) (route and year)
## pid ------------- point ID (subset of survey ID)
## artnr ----------- species ID code
## antal ----------- number of birds


# survey data (a survey is a route in a specific year)
bird_count_info <- read.csv("data/Insects_and_TTT/point_count_info.csv",sep=";") %>% tibble()
## tid ----------------- survey ID (turID) (route and year)
## rid ----------------- routeID
## skydaekke ----------- cloud cover 1 to 3 - low (0-33) to high (66-100)
## sigt ---------------- distance that you can see
## opdateret_af -------- ignore (operator)
## time1 --------------- start time
## time2 --------------- end time
## antalpktobsandel----- ?? (prop of points survyed %)
## shift from 20 to 10 points


# route data
bird_count_sites <- read.csv("data/Insects_and_TTT/point_site_info.csv", sep=";", dec=",") %>% tibble()
## rid ----------------- routeID
## oid ----------------- observer ID
## season 
### N   SY   TY    V 
### 1 2027  145 1645
### SY - late breeding - choose this May/June
## pkt ----------------- total number of points per routeID
## lat/lon
### some points are separated several km but not common


# species code
species_codes <- read.csv("data/Insects_and_TTT/species_codes.csv",sep=";") %>% tibble()
## arttype ------------- ubestemt (exclude these)




# =================== insect routes =================== 
# read in insekt mobile routes
insect_routes2018 <- st_read(dsn = "data/InsektMobile_routes/ruter_2018_2019_final", layer = "finalRoutes2018")



