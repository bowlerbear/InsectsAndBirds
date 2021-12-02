

# =================== match bird point count (SY) to reference grid (sen) =================== 
# visualise the bird counts (SY) and the transect grid (sen)
intersection_bird_count_SY_squares_sen_map <- tm_shape(squares_5x5_sen_sf) +
  tm_borders() +
  tm_fill(col="kvadratnr") +
  tm_shape(bird_count_sf) +
  tm_dots(col="cyan", alpha=0.7) +
  tm_grid() + 
  tm_layout(main.title = "Intersection of CS bird count sites (SY) and bird atlas transect squares (sen)", main.title.size = 1)
# tmap_save(tm = intersection_bird_count_SY_squares_sen_map,
#           filename = paste0("output", "/", "intersection_bird_count_sites_SY_and_transect_squares_sen_5x5.png"))
# rm(intersection_bird_count_SY_squares_sen_map)



squares_5x5_sen_sf
bird_count_sf

bird_count_merged

