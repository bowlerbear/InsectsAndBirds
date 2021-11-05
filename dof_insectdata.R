setwd("C:/Users/db40fysa/Dropbox/DOF/InsektMobile_routes/ruter_2018_2019_final")

#read in insekt mobile routes
routes2018 <- st_read(dsn = getwd(),
                      layer = "finalRoutes2018")