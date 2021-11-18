
library(tidyverse)
library(sf)
library(tmap)


# =================== reference grid, bird atlas =================== 
# transect squares -- reference grid (kvadratnr)
squares32_map <- tm_shape(squares32) +
  tm_borders() +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Reference grid: bird atlas transect squares UTM32")
# tmap_save(tm = squares32_map,
#           filename = paste0("output", "/", "transect_squares_1x1_utm32.png"))
# rm(squares32_map)


# transect squares (UTM 33)
squares33_map <- tm_shape(squares33) +
  tm_borders() +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Bird atlas transect squares UTM32")
# tmap_save(tm = squares33_map,
#           filename = paste0("output", "/", "transect_squares_utm33.png"))
# rm(squares33_map)


# transects data where birds were counted
# transects32
# tm_shape(transects32) +
#   tm_lines() +
#   tm_grid()

squares32_transects32_map <- tm_shape(squares32) +
  tm_borders() +
  tm_shape(transects32) +
  tm_lines(col="red", alpha=0.7) +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Bird atlas transect squares and transects UTM32")
# tmap_save(tm = squares32_transects32_map,
#           filename = paste0("output", "/", "transect_squares_and_transects_1x1_utm32.png"))
# rm(squares32_transects32_map)

# transects33


# bird atlas
bird_atlas <- bird_atlas %>% 
  mutate(dato = as.Date(dato),
         year = lubridate::year(dato)) # 2014 to 2018 - many fewer obs in 2018??

table(bird_atlas$type,bird_atlas$year)
#        2014  2015  2016  2017  2018
#sen     6960  9153 10920 11579     0   # late (includes migrants)
#tidlig  6663  6901 11558 10399     0   # early (not includes some migrants)
#vinter  1957  6757  7794  5001  3279

num_type_year_bird_atlas_bar <- bird_atlas %>% 
  count(year, type) %>% 
  ggplot() + 
  geom_bar(aes(x=year, y=n, fill=type), stat="identity", position="dodge") +
  ylab("number") + 
  ggtitle("Number of samples per year, grouped by type, in bird atlas")
# ggsave(filename = paste0("output", "/", "num_type_year_bird_atlas.png"), num_type_year_bird_atlas_bar)
# rm(num_type_year_bird_atlas_bar)


# how many kvadratnr were sampled each year
num_kvadratnr_per_year_bar <- bird_atlas %>% 
  group_by(year) %>% 
  summarise(n_distinct(kvadratnr)) %>% 
  rename(number_of_unique_kvadratnr = `n_distinct(kvadratnr)`) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr)) +
  ylab("number") + 
  ggtitle("Number of unique kvadratnr sampled per year")
# ggsave(filename = paste0("output", "/", "num_unique_kvadratnr_per_year.png"), num_kvadratnr_per_year_bar)
# rm(num_kvadratnr_per_year_bar)

# only sen
num_kvadratnr_per_year_sen_bar <- bird_atlas %>% filter(type=="sen") %>% 
  group_by(year) %>% 
  summarise(n_distinct(kvadratnr)) %>% 
  rename(number_of_unique_kvadratnr = `n_distinct(kvadratnr)`) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr)) +
  ylab("number") + 
  ggtitle("Number of unique kvadratnr sampled per year (only type = sen)")
# ggsave(filename = paste0("output", "/", "num_unique_kvadratnr_per_year_sen.png"), num_kvadratnr_per_year_sen_bar)
# rm(num_kvadratnr_per_year_sen_bar)


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


# subset sen (late summer)
bird_atlas_sen <- bird_atlas %>% filter(type=="sen")
table(bird_atlas_sen$year)




# =================== bird point count =================== 
# species code
## check - how many for different species
species_codes %>% 
  summarise(n_distinct(latin))

species_codes %>% 
  summarise(n_distinct(english))


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
bird_count_map <- tm_shape(bird_count_sf) +
  tm_dots("red") +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Bird count locations")
# tmap_save(tm = bird_count_map,
#           filename = paste0("output", "/", "bird_count_map.png"))
# rm(bird_count_map)





# =================== match bird point count to reference grid =================== 
# reproject from utm to longlat
raster::crs(squares32)

squares32_sp <- as(squares32, "Spatial")
squares_sp <- sp::spTransform(squares32_sp, raster::crs(bird_count_sf))
squares_sf <- squares_sp %>% st_as_sf()
raster::crs(squares_sf)


# check points grid intersection
## google R match point coordinates to grid
## https://www.andybeger.com/2014/03/29/associating-points-with-polygons-in-r/
## https://gis.stackexchange.com/questions/324378/identify-polygon-grid-in-which-the-point-belongs-to


# visualis both points and grid
intersect_bird_count_transect_squares_map <- tm_shape(squares_sf) +
  tm_borders() +
  tm_shape(bird_count_sf) +
  tm_dots(col="red", alpha=0.7) +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Intersection of CS bird counts and bird atlas transect squares", main.title.size = 1)
# tmap_save(tm = intersect_bird_count_transect_squares_map,
#           filename = paste0("output", "/", "intersect_bird_count_transect_squares_1x1.png"))
# rm(intersect_bird_count_transect_squares_map)

# check extents
raster::extent(squares_sf)
raster::extent(bird_count_sf)

# transform to sp to use over() 
bird_count_sp <- as(bird_count_sf, "Spatial")

# over() to match up points and polygons
intersect_bird_count_squares <- sp::over(squares_sp, bird_count_sp)
intersect_bird_count_squares <- intersect_bird_count_squares %>% tibble()
intersect_bird_count_squares %>% nrow() # 12334
intersect_bird_count_squares %>% filter(!is.na(rid)) %>% nrow() # 274 (2.22% intersected)

# make sf 
intersect_bird_count_squares <- intersect_bird_count_squares %>% filter(!is.na(rid))
intersect_bird_count_squares_sf <- intersect_bird_count_squares %>%
  inner_join((bird_count_sites %>% select(rid,lon,lat)), by="rid") %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# visualise the intersected points
intersect_bird_count_transect_squares_map_2 <- tm_shape(squares_sf) +
  tm_borders() +
  tm_shape(bird_count_sf) +
  tm_dots(col="red", alpha=0.7) +
  tm_shape(intersect_bird_count_squares_sf) +
  tm_dots(col="cyan", alpha=0.7) +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Intersection of CS bird counts and bird atlas transect squares", main.title.size = 1) +
  tm_credits(text="Intersected points denoted by cyan", position="left")
# tmap_save(tm = intersect_bird_count_transect_squares_map,
#           filename = paste0("output", "/", "intersect_bird_count_transect_squares_1x1_highlighted.png"))
# rm(intersect_bird_count_transect_squares_map_2)



# change polygon size (5x5 grid) for interaction
# try out buffer with 2km
squares32 %>% st_buffer(dist=2000) %>% 
  tm_shape() +
  tm_borders() +
  tm_grid()

# check area of each polygon
shape_tmp <- raster::shapefile("data/Insects_and_TTT/transect squares utm32.shp")
unique(raster::area(shape_tmp) / 1000000)
rm(shape_tmp)
# all 1 km2


# transform to 5x5km square
squares32 %>% st_centroid() %>% 
  tm_shape() +
  tm_dots() +
  tm_grid()

squares32 %>% st_centroid() %>% 
  st_buffer(dist=2500, endCapStyle="SQUARE") %>% # radius of 2.5km = 5x5km square
  tm_shape() +
  tm_borders() +
  tm_grid()


squares32_5x5 <- squares32 %>% st_centroid() %>% 
  st_buffer(dist=2500, endCapStyle="SQUARE")

squares32_5x5_sp <- as(squares32_5x5, "Spatial")
squares_5x5_sp <- sp::spTransform(squares32_5x5_sp, raster::crs(bird_count_sf))
squares_5x5_sf <- squares_5x5_sp %>% st_as_sf()
raster::crs(squares_5x5_sf)


# over() to match up points and polygons
intersect_bird_count_squares_5x5 <- sp::over(squares_5x5_sp, bird_count_sp)
intersect_bird_count_squares_5x5 <- intersect_bird_count_squares_5x5 %>% tibble()
intersect_bird_count_squares_5x5 %>% nrow() # 12334
intersect_bird_count_squares_5x5 %>% filter(!is.na(rid)) %>% nrow() # 2846 (23.1% intersected)

# make sf 
intersect_bird_count_squares_5x5 <- intersect_bird_count_squares_5x5 %>% filter(!is.na(rid))
intersect_bird_count_squares_5x5_sf <- intersect_bird_count_squares_5x5 %>%
  inner_join((bird_count_sites %>% select(rid,lon,lat)), by="rid") %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# visualise the intersected points
intersect_bird_count_transect_squares_map_5x5 <-tm_shape(squares_5x5_sf) +
  tm_borders() +
  tm_shape(bird_count_sf) +
  tm_dots(col="red", alpha=0.7) +
  tm_shape(intersect_bird_count_squares_5x5_sf) +
  tm_dots(col="cyan", alpha=0.7) +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "Intersection of CS bird counts and bird atlas transect squares", main.title.size = 1) +
  tm_credits(text="Intersected points denoted by cyan", position="left")
# tmap_save(tm = intersect_bird_count_transect_squares_map_5x5,
#           filename = paste0("output", "/", "intersect_bird_count_transect_squares_5x5_highlighted.png"))
# rm(intersect_bird_count_transect_squares_map_5x5)



# =================== insect routes =================== 
insect_routes2018 



