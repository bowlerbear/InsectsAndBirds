library(sf)
#library(raster)
library(terra)
library(tmap)

#read in dof squares
squares <- st_read(dsn = "data/Insects_and_TTT",
                   layer = "transect squares utm32")
plot(squares)

#group into kvadratnr
squares <- squares %>%
  dplyr::group_by(kvadratnr) %>%
  dplyr::summarise() %>%
  dplyr::ungroup() 

plot(squares)

#buffer the squares
summary(st_area(squares))#area is 1000000
#we want area is increase by 5, so length to increase by 2.236
squares_buffer <- st_buffer(squares,dist=659)
summary(st_area(squares_buffer))


#get land use data
ldir <- "C:/Users/db40fysa/Dropbox/DOF/landuseData/basemap03_2018"
#ldir <- "C:/Users/db40fysa/Dropbox/DOF/landuseData/basemap03_2011_2016_2018"

#only built layer
#r <- rast(paste(ldir,"lu_built_2018.tif",sep="/"))
#unique(r)
#r <- aggregate(r, fact=10,fun="modal")
#plot(r)

#aggregated layer with all classes
r <- rast(paste(ldir,"lu_agg_2018.tif",sep="/"))
r <- aggregate(r, fact=10,fun="modal")
r[r==999999] <- NA
sort(unique(values(r)))#corresponds with table 3.6!!
table(values(r))
plot(r)
crs(r) <- "epsg:32632"


#overlay land map with squares
tm_shape(raster::raster(r))+
  tm_raster()+
tm_shape(squares_buffer)+
  tm_polygons()
#overlap fine

#extract land use for the kvatradnr
squares_buffer_landUse <- raster::extract(raster::raster(r),squares_buffer,df=T,weights=TRUE)
#squares_buffer_landUse2 <- terra::extract(r,vect(squares_buffer))#cant get it to work?
names(squares_buffer_landUse)[2] <- "LandUse"
squares_buffer_landUse <- subset(squares_buffer_landUse, !is.na(LandUse))


#summarize per ID
squares_buffer_landUse_summary <- squares_buffer_landUse %>%
                                    dplyr::group_by(ID,LandUse) %>%
                                    dplyr::summarise(weight=sum(weight))


#see Table 3.6 in TR159.pdf for explanation of land use codes
table(squares_buffer_landUse_summary$LandUse)

#now get proportion of urban areas and agricultural areas
landuse_summary <- squares_buffer_landUse_summary %>%
                    dplyr::group_by(ID) %>%
                    dplyr::summarise(urban = sum(weight[LandUse %in% 110000:150110]),
                                     agri = sum(weight[LandUse %in% 211000:230000]),
                                    forest = sum(weight[LandUse %in% 311000:312000]))  

head(landuse_summary)


#combine with land use data
squares_buffer <- bind_cols(squares_buffer,landuse_summary)

#plot to check
tm_shape(squares_buffer)+
  tm_fill("urban")          

tm_shape(squares_buffer)+
  tm_fill("agri")   

tm_shape(squares_buffer)+
  tm_fill("forest")  

saveRDS(squares_buffer,file="squares_buffer_1km.rds")
saveRDS(squares_buffer,file="squares_buffer_5km.rds")
