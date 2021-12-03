
library(tidyverse)
library(sf)
library(tmap)


# =================== match bird point count (SY) to reference grid (sen) =================== 
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


bird_count_merged_sen

bird_atlas_sen



