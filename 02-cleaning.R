
library(tidyverse)
library(sf)
library(tmap)


# =================== reference grid, bird atlas =================== 
# transect squares -- reference grid (kvadratnr)
tm_shape(squares32) +
  tm_fill("kvadratnr") +
  tm_grid()

tm_shape(squares33) +
  tm_fill("kvadratnr") +
  tm_grid()


# transects data where birds were counted
transects32
transects33


# bird atlas
bird_atlas <- bird_atlas %>% 
  mutate(dato = as.Date(dato),
         year = lubridate::year(dato)) # 2014 to 2018 - many fewer obs in 2018??

table(bird_atlas$type,bird_atlas$year)
#        2014  2015  2016  2017  2018
#sen     6960  9153 10920 11579     0   # late (includes migrants)
#tidlig  6663  6901 11558 10399     0   # early (not includes some migrants)
#vinter  1957  6757  7794  5001  3279


# how many kvadratnr were sampled each year
bird_atlas %>% 
  group_by(year) %>% 
  summarise(n_distinct(kvadratnr)) %>% 
  rename(number_of_unique_kvadratnr = `n_distinct(kvadratnr)`) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr))

# only sen
bird_atlas %>% filter(type=="sen") %>% 
  group_by(year) %>% 
  summarise(n_distinct(kvadratnr)) %>% 
  rename(number_of_unique_kvadratnr = `n_distinct(kvadratnr)`) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr))


# transect info
transect_info <- transect_info %>% 
  mutate(dato = as.Date(dato),
         year = lubridate::year(dato)) 

transect_info %>% 
  group_by(year) %>% 
  summarise(n_distinct(kvadratnr)) %>% 
  rename(number_of_unique_kvadratnr = `n_distinct(kvadratnr)`) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr))
# one more data point in 2016 and 2017 than in bird atlas
# setdiff(), all_equal()



# only sen
transect_info %>% filter(type=="sen") %>% 
  group_by(year) %>% 
  summarise(n_distinct(kvadratnr)) %>% 
  rename(number_of_unique_kvadratnr = `n_distinct(kvadratnr)`) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr))
# one more data point in 2015 and 2017 than in bird atlas





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
tm_shape(bird_count_sf) +
  tm_dots("rid") +
  tm_grid()


# look for the outliers
bird_count_merged %>% ggplot() +
  aes(x="", y=lat) +
  geom_boxplot(fill = "#0c4c8a")

bird_count_merged %>% ggplot() +
  aes(x = lat) +
  geom_histogram(bins = 30L, fill = "#0c4c8a")

bird_count_merged %>% 
  filter(lat < 10) %>% 
  select(lon, lat) %>% 
  unique() # all with 0 lon, 0 lat

bird_count_merged %>% 
  filter(lat < 5) %>% 
  select(rid, year, lon, lat) %>% 
  nrow() # 1066

# remove data points without coordinates
bird_count_merged <- bird_count_merged %>% 
  filter(lat >= 5)

# make spatial
bird_count_sf <- st_as_sf(bird_count_merged, coords = c("lon","lat"), crs = 4326)

# plot and check
tm_shape(bird_count_sf) +
  tm_dots("rid") +
  tm_grid()




# =================== match bird point count to reference grid =================== 
# set crs to UTM42
squares32

# reproject data
squares32_sp <- as(squares32, "Spatial")
squares_sp <- sp::spTransform(squares32_sp, raster::crs(bird_count_sf))
squares_sf <- squares_sp %>% st_as_sf()


# check points grid intersection
## google R match point coordinates to grid
## https://www.andybeger.com/2014/03/29/associating-points-with-polygons-in-r/
## https://gis.stackexchange.com/questions/324378/identify-polygon-grid-in-which-the-point-belongs-to

tm_shape(squares_sf) +
  tm_polygons() +
  tm_shape(bird_count_sf) +
  tm_dots("rid") +
  tm_grid()

raster::extent(squares_sf)
raster::extent(bird_count_sf)

bird_count_sp <- as(bird_count_sf, "Spatial")

# over() to match up points and polygons
intersect_bird_count_squares_sp <- cbind(bird_count_sp, sp::over(bird_count_sp, squares_sp))

intersect_bird_count_squares_sf <- intersect_bird_count_squares_sp %>% st_as_sf()
# intersect_bird_count_squares_sp %>% filter(!is.na(rid))


