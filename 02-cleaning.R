
library(tidyverse)
library(sf)
library(tmap)


# =================== visualise reference grid of bird atlas =================== 
# plot the grid
# transect squares  (UTM 32)
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
  tm_layout(legend.outside = TRUE, main.title = "Bird atlas transect squares UTM33")
# tmap_save(tm = squares33_map,
#           filename = paste0("output", "/", "transect_squares_utm33.png"))
# rm(squares33_map)




# =================== check reference grid of bird atlas =================== 
# bird atlas
bird_atlas <- bird_atlas %>% 
  mutate(dato = as.Date(dato),
         year = lubridate::year(dato)) # 2014 to 2018 - many fewer obs in 2018??


# number of samples per year grouped by season
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
  theme_bw() +
  ggtitle("Number of samples per year, grouped by type, in bird atlas")
# ggsave(filename = paste0("output", "/", "num_type_year_bird_atlas.png"), num_type_year_bird_atlas_bar)
# rm(num_type_year_bird_atlas_bar)


# subset sen (late summer)
bird_atlas_sen <- bird_atlas %>% filter(type=="sen")
table(bird_atlas_sen$year)


# transect info
transect_info <- transect_info %>% 
  mutate(dato = as.Date(dato),
         year = lubridate::year(dato)) 


# # merge bird atlas sen with transect info
# bird_atlas_sen %>% 
#   inner_join(transect_info %>% filter(type=="sen") %>% select(-c(id, type, dato, year)), by="kvadratnr")


# check no. of kvadratnr sampled each year 
# how many kvadratnr were sampled each year (bird_atlas)
num_kvadratnr_per_year_bar <- bird_atlas %>% 
  group_by(year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr), fill="steelblue") +
  ylab("number") + 
  theme_bw() +
  ggtitle("Number of unique kvadratnr sampled per year (bird atlas)")
# ggsave(filename = paste0("output", "/", "num_unique_kvadratnr_per_year_bird_atlas.png"), num_kvadratnr_per_year_bar)
# rm(num_kvadratnr_per_year_bar)

# subset sen (bird_atlas)
num_kvadratnr_per_year_sen_bar <- bird_atlas %>% 
  filter(type=="sen") %>% 
  group_by(year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr), fill="steelblue") +
  ylab("number") + 
  theme_bw() +
  ggtitle("Number of unique kvadratnr sampled per year (only type == sen) (bird atlas)")
# ggsave(filename = paste0("output", "/", "num_unique_kvadratnr_per_year_sen_bird_atlas.png"), num_kvadratnr_per_year_sen_bar)
# rm(num_kvadratnr_per_year_sen_bar)


# check if the numbers match with that obtained from transect info
# # how many kvadratnr were sampled each year (transect info)
transect_info %>% 
  group_by(year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr))
# one more data point in 2016 and 2017 than in bird atlas
# setdiff(), all_equal()

# subset sen (transect_info)
transect_info %>% 
  filter(type=="sen") %>% 
  group_by(year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr))
# one more data point in 2015 and 2017 than in bird atlas




# =================== assign kvadratnr to bird_count_sites =================== 
# make bird_count_sites spatial
bird_count_sites_sf <- bird_count_sites_sf %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326)

# reproject squares32 from utm to longlat
raster::crs(squares32)
squares_32_1x1_sf <- sf::st_transform(squares32, crs=raster::crs(bird_count_sites_sf))
raster::crs(squares_32_1x1_sf)

# reproject squares33 from utm to longlat
raster::crs(squares33)
squares_33_1x1_sf <- sf::st_transform(squares33, crs=raster::crs(bird_count_sites_sf))
raster::crs(squares_33_1x1_sf)

# combine squares32 and squares33
squares_1x1_sf <- rbind(squares_32_1x1_sf, squares_33_1x1_sf)


# visualise both count sites and grid
bird_count_sites_squares_map <- tm_shape(squares_1x1_sf) +
  tm_borders() +
  tm_shape(bird_count_sites_sf) +
  tm_dots(col="red", alpha=0.7) +
  tm_grid() +
  tm_layout(legend.outside = TRUE, main.title = "CS bird count sites (all) and bird atlas transect squares (all)", main.title.size = 1)
# tmap_save(tm = bird_count_sites_squares_map,
#           filename = paste0("output", "/", "bird_count_sites_and_transect_squares_1x1.png"))
# rm(bird_count_sites_squares_map)


# transform grids from form 1x1km to 5x5km 
# check area of each polygon
shape_tmp <- raster::shapefile("data/Insects_and_TTT/transect squares utm32.shp")
unique(raster::area(shape_tmp) / 1000000) # all 1 km2
rm(shape_tmp)

shape_tmp <- raster::shapefile("data/Insects_and_TTT/transect squares utm33.shp")
unique(raster::area(shape_tmp) / 1000000) # all 1 km2
rm(shape_tmp)


# transform 1x1km to 5x5km square (UTM), UTM to latlon
squares32_5x5_sf <- squares32 %>% 
  st_centroid() %>% 
  st_buffer(dist=2500, endCapStyle="SQUARE") %>% 
  sf::st_transform(crs=raster::crs(bird_count_sites_sf))

squares33_5x5_sf <- squares33 %>% 
  st_centroid() %>% 
  st_buffer(dist=2500, endCapStyle="SQUARE") %>% 
  sf::st_transform(crs=raster::crs(bird_count_sites_sf))

# combine squares32 and squares33
squares_5x5_sf <- rbind(squares32_5x5_sf, squares33_5x5_sf)


# assign kvadratnr to bird_count_sites
intersection_bird_count_sites_squares_sf <- st_join(bird_count_sites_sf, squares_5x5_sf, left=F)  # left=F: inner join
intersection_bird_count_sites_squares_sf <- intersection_bird_count_sites_squares_sf %>% 
  distinct() # subset distinct rows

# check if there is any NA for kvadratnr
intersection_bird_count_sites_squares_sf %>% 
  filter(!is.na(kvadratnr))

# subset the unmatched count sites for visualisation
bird_count_sites_diff_sf <- setdiff(bird_count_sites_sf,intersection_bird_count_sites_squares_sf[,1:8])

# check number of points fall within the grids
bird_count_sites_sf %>% nrow() # 3821
intersection_bird_count_sites_squares_sf %>% nrow() # 3156 (82.6%)

# save only the original columns and kvadratnr
intersection_bird_count_sites_squares_sf <- intersection_bird_count_sites_squares_sf %>% 
  select(c(colnames(bird_count_sites_sf), "kvadratnr"))


# visualise the count sites the fall within the transect grid
intersection_bird_count_sites_squares_map <- tm_shape(squares_5x5_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(bird_count_sites_diff_sf) +
  tm_dots(col="red", alpha=0.7) +
  tm_shape(intersection_bird_count_sites_squares_sf) +
  tm_dots(col="cyan", alpha=0.7) +
  tm_grid() + 
  tm_layout(main.title = "Intersection of CS bird count sites (all) and bird atlas transect squares (all)", main.title.size = 1) +
  tm_credits(text="Intersected points denoted by cyan (82.6% of the sites)", position="left")
# tmap_save(tm = intersection_bird_count_sites_squares_map,
#           filename = paste0("output", "/", "intersection_bird_count_sites_and_transect_squares_5x5.png"))
# rm(intersection_bird_count_sites_squares_map)


# subset reference grid (sen)
squares_5x5_sen_sf <- squares_5x5_sf %>% 
  inner_join(bird_atlas %>% filter(type=="sen") %>% 
               select(c(kvadratnr, type)), by="kvadratnr")

data.table::setDT(squares_5x5_sen_sf)                              # use data.table to speed up looking for distinct rows
squares_5x5_sen_sf <- unique(squares_5x5_sen_sf, by="kvadratnr")   # subset distinct rows
squares_5x5_sen_sf <- squares_5x5_sen_sf %>%                       # change the format back to sf
  tibble() %>% 
  st_as_sf() %>% 
  filter(!is.na(kvadratnr))




# =================== bird point count =================== 
# species code
## check - how many for different species
species_codes %>% 
  summarise(num = n_distinct(latin))

species_codes %>% 
  summarise(num = n_distinct(english))


# survey data (a survey is a route in a specific year)
table(bird_count_info$tyear)


# merge all bird count datasets
all(bird_count_info$tid %in% bird_count$tid)        #FALSE
all(bird_count$tid %in% bird_count_info$tid)        #TRUE
all(bird_count_info$rid %in% bird_count_sites$rid)  #TRUE
all(bird_count$artnr %in% species_codes$artnr)      #TRUE

bird_count_merged <- bird_count %>%
  inner_join(bird_count_info, by="tid") %>%
  inner_join(intersection_bird_count_sites_squares_sf, by="rid") %>%
  inner_join(bird_count_sites %>% select(c(rid, lon, lat)), by="rid") %>% 
  inner_join(species_codes, by="artnr")

# format date and year
bird_count_merged <- bird_count_merged %>%
  rename(date = tdate) %>%
  mutate(date = as.Date(date),
         year = lubridate::year(date)) %>%
  select(-tyear)

table(bird_count_merged$year) # from 2016 - 2021


# subset -- late breeding
bird_count_merged_sen <- bird_count_merged %>% 
  filter(season=="SY") %>% 
  filter(kvadratnr %in% squares_5x5_sen_sf$kvadratnr) # ensure it contains also only the sen subset of kvadratnr


# make bird_count spatial
bird_count_sen_sf <- st_as_sf(bird_count_merged_sen , coords = c("lon","lat"), crs = 4326)

# plot and check
tm_shape(bird_count_sen_sf) +
  tm_dots("rid") +
  tm_grid()

# visualise the bird counts (SY) and the transect grid (sen)
intersection_bird_count_SY_squares_sen_map <- tm_shape(squares_5x5_sen_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(bird_count_sen_sf) +
  tm_dots(col="cyan") +
  tm_grid() + 
  tm_layout(main.title = "Intersection of CS bird count sites (SY) and bird atlas transect squares (sen)", main.title.size = 1)
# tmap_save(tm = intersection_bird_count_SY_squares_sen_map,
#           filename = paste0("output", "/", "intersection_bird_count_sites_SY_and_transect_squares_sen_5x5.png"))
# rm(intersection_bird_count_SY_squares_sen_map)

# no. of kvadratnr sampled each year in bird point count
num_kvadratnr_per_year_bar <- bird_count_merged_sen %>% 
  group_by(year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr), fill="steelblue") +
  ylab("number") + 
  theme_bw() +
  ggtitle("Number of unique kvadratnr sampled per year (only type == sen) (bird point counts)")
# ggsave(filename = paste0("output", "/", "num_unique_kvadratnr_per_year_sen_bird_point.png"), num_kvadratnr_per_year_bar)
# rm(num_kvadratnr_per_year_bar)




# =================== add kvadratnr to insect routes =================== 
# reproject insect_routes from utm to longlat
raster::crs(insect_routes2018)
insect_routes2018_sf <- sf::st_transform(insect_routes2018, crs=raster::crs(bird_count_sen_201819_sf))
raster::crs(insect_routes2018_sf)

raster::crs(insect_routes2019)
insect_routes2019_sf <- sf::st_transform(insect_routes2019, crs=raster::crs(bird_count_sen_201819_sf))
raster::crs(insect_routes2019_sf)

# get centroid for route
insect_routes2018_centroid_sf <- insect_routes2018_sf %>% 
  st_centroid()

insect_routes2019_centroid_sf <- insect_routes2019_sf %>% 
  st_centroid()


# make insect_routes_mapping spatial
insect_routes_mapping_sf <- insect_routes_mapping %>% 
  inner_join(squares_5x5_sen_sf %>% select(-c(flyttet_no, flyttet_os, lok_grp_id, type)), by="kvadratnr") %>% 
  st_as_sf(crs=raster::crs(squares_5x5_sen_sf))

# check how insect routes 2018 and 2019 are laying on insect_routes_mapping
tm_shape(insect_routes_mapping_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(insect_routes2018_centroid_sf) +
  tm_dots(col="red") +
  tm_grid()

tm_shape(insect_routes_mapping_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(insect_routes2019_centroid_sf) +
  tm_dots(col="red") +
  tm_grid()

# => the difference suggests that insect_routes_mapping_sf is probably applicable for the 2018 data only,
# in this case, it's more useful to use the bird atlas transect grid processed above directly.
# Especially when it seems that no information about the kvadratnr of insect 2019 has been provided


# 2018
# assign kvadratnr to insect_routes
intersection_insect_routes2018_squares_sf <- st_join(insect_routes2018_centroid_sf, squares_5x5_sen_sf, left=F)  # left=F: inner join
intersection_insect_routes2018_squares_sf <- intersection_insect_routes2018_squares_sf %>% 
  distinct() # subset distinct rows 

# subset the unmatched count sites for visualisation
insect_routes2018_diff_sf <- setdiff(insect_routes2018_centroid_sf, intersection_insect_routes2018_squares_sf %>% select(colnames(insect_routes2018_centroid_sf)))

# check number of points fall within the grids
insect_routes2018_centroid_sf %>% nrow() # 343
intersection_insect_routes2018_squares_sf %>% nrow() # 328 (95.6%)

# save only the original columns and kvadratnr
intersection_insect_routes2018_squares_sf <- intersection_insect_routes2018_squares_sf %>% 
  select(c(colnames(insect_routes2018_centroid_sf), "kvadratnr"))

# convert centroids back to lines
intersection_insect_routes2018_squares_sf <- insect_routes2018_sf %>% 
  inner_join(intersection_insect_routes2018_squares_sf %>% tibble() %>% select(c(OBJECTID,kvadratnr))) %>% 
  filter(!is.na(kvadratnr))

insect_routes2018_diff_sf <- insect_routes2018_sf %>% 
  inner_join(insect_routes2018_diff_sf %>% tibble() %>% select(c(OBJECTID)))

# plot grid and insect_routes2018
intersection_insect_routes2018_squares_map <- tm_shape(squares_5x5_sen_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(insect_routes2018_diff_sf) +
  tm_lines(col="red", alpha=0.7) +
  tm_shape(intersection_insect_routes2018_squares_sf) +
  tm_lines(col="cyan", alpha=0.7) +
  tm_grid() +
  tm_layout(main.title = "Intersection of insect routes (2018) and bird atlas transect squares (sen)", main.title.size = 1) +
  tm_credits(text="Intersected routes denoted by cyan (95.6% of the routes)", position="left")
# tmap_save(tm = intersection_insect_routes2018_squares_map,
#           filename = paste0("output", "/", "intersection_insect_routes2018_and_transect_squares_5x5.png"))
# rm(intersection_insect_routes2018_squares_map)



# 2019
# assign kvadratnr to insect_routes
intersection_insect_routes2019_squares_sf <- st_join(insect_routes2019_centroid_sf, squares_5x5_sen_sf, left=F)  # left=F: inner join
intersection_insect_routes2019_squares_sf <- intersection_insect_routes2019_squares_sf %>% 
  distinct() # subset distinct rows 

# subset the unmatched count sites for visualisation
insect_routes2019_diff_sf <- setdiff(insect_routes2019_centroid_sf, intersection_insect_routes2019_squares_sf %>% select(colnames(insect_routes2019_centroid_sf)))

# check number of points fall within the grids
insect_routes2019_centroid_sf %>% nrow() # 404
intersection_insect_routes2019_squares_sf %>% nrow() # 386 (95.5%)

# save only the original columns and kvadratnr
intersection_insect_routes2019_squares_sf <- intersection_insect_routes2019_squares_sf %>% 
  select(c(colnames(insect_routes2019_centroid_sf), "kvadratnr"))

# convert centroids back to lines
intersection_insect_routes2019_squares_sf <- insect_routes2019_sf %>% 
  inner_join(intersection_insect_routes2019_squares_sf %>% tibble() %>% select(c(OBJECTID,kvadratnr))) %>% 
  filter(!is.na(kvadratnr))

insect_routes2019_diff_sf <- insect_routes2019_sf %>% 
  inner_join(insect_routes2019_diff_sf %>% tibble() %>% select(c(OBJECTID)))

# plot grid and insect_routes2019
intersection_insect_routes2019_squares_map <- tm_shape(squares_5x5_sen_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(insect_routes2019_diff_sf) +
  tm_lines(col="red", alpha=0.7) +
  tm_shape(intersection_insect_routes2019_squares_sf) +
  tm_lines(col="cyan", alpha=0.7) +
  tm_grid() +
  tm_layout(main.title = "Intersection of insect routes (2019) and bird atlas transect squares (sen)", main.title.size = 1) +
  tm_credits(text="Intersected routes denoted by cyan (95.5% of the routes)", position="left")
# tmap_save(tm = intersection_insect_routes2019_squares_map,
#           filename = paste0("output", "/", "intersection_insect_routes2019_and_transect_squares_5x5.png"))
# rm(intersection_insect_routes2019_squares_map)


# no. of kvadratnr sampled each year in insect routes
# merge the sfs of the two years together
intersection_insect_routes2018_squares_sf$year <- "2018"
intersection_insect_routes2019_squares_sf$year <- "2019"

intersection_insect_routes_squares_sf <- rbind(intersection_insect_routes2018_squares_sf %>% 
                                                 select(c(OBJECTID, kvadratnr, geometry, year)),
                                               intersection_insect_routes2019_squares_sf %>% 
                                                 select(c(OBJECTID, kvadratnr, geometry, year))
                                               )
# have to beware of the OBJECTEDID here:
#  count from 1 in 2018
#  count from 1 again in 2019


num_kvadratnr_per_year_bar <- intersection_insect_routes_squares_sf %>% 
  group_by(year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr), fill="steelblue") +
  ylab("number") + 
  theme_bw() +
  ggtitle("Number of unique kvadratnr sampled per year (only type == sen) (insect routes)")
# ggsave(filename = paste0("output", "/", "num_unique_kvadratnr_per_year_sen_insect_routes.png"), num_kvadratnr_per_year_bar)
# rm(num_kvadratnr_per_year_bar)



# =================== get lists of overlapping kvadratnr and years =================== 
# summary visualisaiton
kvadratnr_year_df <- intersection_insect_routes_squares_sf %>%
  mutate(dataset="insects") %>% select(c(year, kvadratnr, dataset)) %>% 
  rbind(bird_count_merged_sen %>% mutate(dataset="bird_counts") %>% select(c(year, kvadratnr, dataset))) %>% 
  rbind(bird_atlas_sen %>% mutate(dataset="bird_atlas") %>% select(c(year, kvadratnr, dataset)))

kvadratnr_summary_bar <- kvadratnr_year_df %>% 
  group_by(dataset, year) %>% 
  summarise(number_of_unique_kvadratnr = n_distinct(kvadratnr)) %>% 
  ggplot() +
  geom_col(aes(x=year, y=number_of_unique_kvadratnr), fill="steelblue") +
  ylab("number") + 
  facet_wrap(~dataset) +
  theme_bw() +
  ggtitle("Number of unique kvadratnr sampled per year (only type == sen)")
# ggsave(filename = paste0("output", "/", "kvardratnr_summary.png"), kvadratnr_summary_bar)
# rm(kvadratnr_summary_bar)


# save cleaned datasets
saveRDS(intersection_insect_routes_squares_sf, "output/data/cleaned/insect_routes.rds")

saveRDS(bird_count_merged_sen, "output/data/cleaned/bird_counts.rds")

saveRDS(bird_atlas_sen, "output/data/cleaned/bird_atlas.rds")


# check overlappings
bird_count_kvadratnr <- bird_count_merged_sen %>% 
  mutate(dataset="bird_counts") %>% 
  select(year, kvadratnr, dataset) %>% 
  distinct()

bird_atlas_kvadratnr <- bird_atlas_sen %>% 
  mutate(dataset="bird_atlas") %>% 
  select(year, kvadratnr, dataset) %>% 
  distinct()

insects_kvadratnr <- intersection_insect_routes_squares_sf %>% 
  as_tibble() %>% 
  mutate(dataset="insects") %>% 
  select(year, kvadratnr, dataset) %>% 
  distinct()


bird_overlap <- bird_count_kvadratnr %>% 
  inner_join(bird_atlas_kvadratnr, by=c("kvadratnr", "year")) %>% 
  select(year, kvadratnr)

saveRDS(bird_overlap, "output/data/bird_overlap.rds")


insect_bird_overlap <- insects_kvadratnr %>% 
  mutate(year=as.numeric(year)) %>% 
  inner_join(bird_count_kvadratnr, by=c("kvadratnr", "year")) %>% 
  select(year, kvadratnr)

saveRDS(insect_bird_overlap, "output/data/bird_overlap.rds")



