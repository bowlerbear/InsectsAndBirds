
library(tidyverse)
library(sf)
library(tmap)


# =================== reference grid, bird atlas =================== 
# transect squares -- reference grid (kvadratnr)
tm_shape(squares32)+
  tm_fill("kvadratnr")

tm_shape(squares33)+
  tm_fill("kvadratnr")


# bird atlas
bird_atlas <- bird_atlas %>% 
  mutate(dato = as.Date(dato),
         year = lubridate::year(dato)) # 2014 to 2018 - many fewer obs in 2018??

table(bird_atlas$type,bird_atlas$year)
#        2014  2015  2016  2017  2018
#sen     6960  9153 10920 11579     0   # late (includes migrants)
#tidlig  6663  6901 11558 10399     0   # early (not includes some migrants)
#vinter  1957  6757  7794  5001  3279





# =================== bird point count =================== 
# species code
## check - how many for different species



# survey data (a survey is a route in a specific year)
table(bird_count_info$tyear)


# merge all bird count datasets
all(bird_count_info$tid %in% bird_count$tid)        #FALSE
all(bird_count$tid %in% bird_count_info$tid)        #TRUE
all(bird_count_info$rid %in% bird_count_sites$rid)  #TRUE
all(bird_count$artnr %in% species_codes$artnr)      #TRUE

bird_count_merged <- bird_count %>%
  inner_join(bird_count_info, by="tid") %>%
  inner_join(bird_count_sites, by="rid") %>%
  inner_join(species_codes, by="artnr")

# subset -- late breeding
bird_count_merged <- filter(bird_count_merged, season=="SY")

# format date and year
bird_count_merged <- bird_count_merged %>% 
  rename(date = tdate) %>% 
  mutate(date = as.Date(date),
         year = lubridate::year(date)) %>% 
  select(-tyear)

table(bird_count_merged$year) # 2016 onwards

# make spatial
bird_count_sf <- st_as_sf(bird_count_merged, coords = c("lon","lat"), crs = 4326)

# plot and check
tm_shape(bird_count_sf)+
  tm_dots("rid")
