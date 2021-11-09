


#read in insekt mobile routes
routes2018 <- st_read(dsn = "data/InsektMobile_routes/ruter_2018_2019_final",
                      layer = "finalRoutes2018")


#read in danish biomass data (not needed for Edwin)
setwd("C:/Users/db40fysa/Nextcloud/mobileInsect-share/13_data/Season2018/danishData")

meta <- read.csv("SamplingEvent_allsamples_2018.csv",sep=";") 
names(meta)[2] <- "SampleID"
data <- read.csv("Biomass_2018_DK.csv",sep=";") 

#quick analysis for the chase lab mtg
insectDF <- inner_join(data,meta) 


#get mean biomass per route (average over times)
insectDF_summary <- insectDF %>%
              dplyr::group_by(DOFAtlasQuadrantID,habitat)%>%
              dplyr::summarise(biomass=median(SampleBiomass..mg.))
  

