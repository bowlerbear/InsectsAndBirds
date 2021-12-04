#quick analysis for chase lab presentation

library(tidyverse)
library(sf)
library(lubridate)
library(ggthemes)

#### birds ####

#read in transects data where birds were counted
transects <- st_read(dsn = "data/Insects_and_TTT",
                     layer = "transects utm32")
plot(transects)
head(transects)

#read in bird observation data - counts of birds using distance sampling
data <- read.csv("data/Insects_and_TTT/ttt_data.csv",sep=";")
head(data)
info <- read.csv("data/Insects_and_TTT/ttt_info.csv",sep=";")
head(info)

#combine by vadratnr?
names(data)[names(data) %in% names(info)]
allDF <- inner_join(data,info,by=c("kvadratnr","type","dato"))
nrow(allDF)

#subsetting
allDF$Date <- as.Date(allDF$dato)
allDF$Year <- year(allDF$Date)
allDF$Species <- allDF$latin
allDF$Time <- allDF$tidtil - allDF$tidfra

allDF <- allDF %>%
              dplyr::filter(type=="sen") %>% #late summer
              dplyr::filter(Year == 2017) %>% #late summer
              dplyr::filter(!arttype %in% c("hybrid","ubestemt")) %>%#unclear names
              dplyr::filter(Time>15)

#total birds seen
allDF <- allDF %>%
              dplyr::mutate(Count = (X.0 + X.1 + X.2))


#is a species just seen once on a given date and kvadratnr
check <- allDF %>%
          dplyr::group_by(english,latin,Date,kvadratnr) %>%
          dplyr::summarise(nuPoints=length(Count))
summary(check$nuPoints)# all 1!!

#add on land use data
squares_buffer <- readRDS("squares_buffer_1km.rds") %>% 
                    as_tibble() %>%
                    select(-geometry)

allDF <- inner_join(allDF,squares_buffer,by="kvadratnr")

#examine pattern for a few species
sort(unique(allDF$Species))

ggplot(subset(allDF,Species=="Passer domesticus"))+
  geom_point(aes(x=urban,y=Count))

ggplot(subset(allDF,Species=="Regulus regulus"))+
  geom_point(aes(x=urban,y=Count))

#get trait information
load("C:/Users/db40fysa/Dropbox/central European birds/MS development/traitsEUbirds_v2.RData")
traits <- traitsEUbirds %>%
            select(Species, Diet.5Cat, InsectSpecialist, 
                   SeedSpecialist, DietSpecialist,
                   Closed,Open,NumberGrids,Long.distance.migrant,
                   PassNonPass,Weight)

#do these names match?
unique(allDF$Species[allDF$Species %in% traits$Species])
unique(allDF$Species[!allDF$Species %in% traits$Species])#dont recognise these
#sort later

allDF <- allDF %>%
          inner_join(., traits, by = "Species")


#summarise transect data - total number of birds and total number of insectivores
allDF_summary <- allDF %>%
                  dplyr::group_by(Date,kvadratnr,turid,urban,agri,forest) %>%
                  dplyr::summarise(total = sum(Count),
                                   totalInsectivores = sum(Count[InsectSpecialist==1]),
                                   totalSeedeaters = sum(Count[SeedSpecialist==1]),
                                   totalOtherDiet = sum(Count[Diet.5Cat!="Invertebrate"]),
                                   nuSpecies = length(unique(Species[Count>0]))) 
                  
#associations with urban
ggplot(allDF_summary,aes(x=urban+0.001,y=nuSpecies))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=urban+0.001,y=total))+
  geom_point()+
  stat_smooth(method="lm")+
  #scale_x_log10()+
  theme_few()

ggplot(allDF_summary,aes(x=urban+0.001,y=totalInsectivores))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=urban+0.001,y=totalSeedeaters))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=urban+0.001,y=totalOtherDiet))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

#associations with agri
ggplot(allDF_summary,aes(x=agri,y=nuSpecies))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=agri,y=total))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=agri,y=totalInsectivores))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=agri,y=totalSeedeaters))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=agri,y=totalOtherDiet))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

#associations with forest
ggplot(allDF_summary,aes(x=forest,y=nuSpecies))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=forest,y=total))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=forest,y=totalInsectivores))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=forest,y=totalSeedeaters))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_summary,aes(x=forest,y=totalOtherDiet))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_few()


## add insect data - see dof_insectdata.R
allDF_summary$insect_biomass <- insectDF_summary$biomass[match(allDF_summary$kvadratnr,
                                                               insectDF_summary$DOFAtlasQuadrantID)]
allDF_ib <- subset(allDF_summary,!is.na(insect_biomass))
nrow(allDF_ib)

g1 <- ggplot(allDF_ib,aes(x=insect_biomass,y=total))+
  geom_point()+
  scale_x_log10()+
  stat_smooth(method="lm")+
  theme_few()

g2 <- ggplot(allDF_ib,aes(x=insect_biomass,y=totalInsectivores))+
  geom_point()+
  scale_x_log10()+
  stat_smooth(method="lm")+
  theme_few()

cowplot::plot_grid(g1,g2,nrow=1)


#insect plots
ggplot(allDF_ib,aes(x=urban,y=insect_biomass))+
  geom_point()+
  scale_y_log10()+
  stat_smooth(method="lm")+
  theme_few()

ggplot(allDF_ib,aes(x=agri,y=insect_biomass))+
  geom_point()+
  scale_y_log10()+
  stat_smooth(method="lm")+
  theme_few()

#quick SEM
library(piecewiseSEM)

model <- psem(lm(insect_biomass ~ urban, allDF_ib), 
              lm(totalInsectivores ~ urban, allDF_ib)
              )

model <- psem(lm(insect_biomass ~ urban + agri, allDF_ib), 
              lm(totalInsectivores ~ urban + agri, allDF_ib),
              insect_biomass %~~% totalInsectivores
)


model <- psem(lm(insect_biomass ~ urban + agri + totalInsectivores, allDF_ib), 
              lm(totalInsectivores ~ urban + agri + insect_biomass, allDF_ib)
)

model <- psem(lm(insect_biomass ~ urban + agri, allDF_ib), 
              lm(totalSeedeaters ~ urban + agri, allDF_ib),
              insect_biomass %~~% totalSeedeaters
)

summary(model)
plot(model)


#lay with Hmsc
#library(Hmsc)

library(gllvm)

#convert data into community matrix
allDF$insect_biomass <- insectDF_summary$biomass[match(allDF$kvadratnr,
                                                               insectDF_summary$DOFAtlasQuadrantID)]
allDF_ib <- subset(allDF,!is.na(insect_biomass))

#subset allDF to reasonably common species
allDF2 <- allDF_ib %>%
            dplyr::group_by(Species) %>%
            dplyr::summarise(nuObs = sum(Count>0))


birdMatrix <- reshape2::acast(subset(allDF_ib, Species %in% allDF2$Species[allDF2$nuObs>10]), 
                              kvadratnr ~ Species, value.var="Count",fun= function(x) round(mean(x)))
birdMatrix[is.nan(birdMatrix)] <- 0
dim(birdMatrix)

#environmental data
X <- data.frame(Site=row.names(birdMatrix))
X$insect_biomass <- insectDF_summary$biomass[match(X$Site,
                                                       insectDF_summary$DOFAtlasQuadrantID)]
X$insect_biomass <- log(X$insect_biomass+0.01)
X$urban <- allDF$urban[match(X$Site,allDF$kvadratnr)]
X$agri <- allDF$agri[match(X$Site,allDF$kvadratnr)]
X$forest <- allDF$forest[match(X$Site,allDF$kvadratnr)]
X <- X[,-1]

#trait data
TR <- data.frame(Species=colnames(birdMatrix))
TR$Insect <- allDF$InsectSpecialist[match(TR$Species,allDF$Species)]
TR$Seed <- allDF$SeedSpecialist[match(TR$Species,allDF$Species)]
TR$Diet <- allDF$Diet.5Cat[match(TR$Species,allDF$Species)]
TR <- TR[,-1]

#null model
fit1 <- gllvm(birdMatrix, family=poisson())
ordiplot(fit1, biplot=TRUE, ind.spp=15)

#incl environ
fit1 <- gllvm(birdMatrix, X, family=poisson(),num.lv=2,
              formula = ~ urban + agri + forest)
coefplot(fit1,cex.ylab=1.2)

#residual correlation
library(corrplot)
library(gclus)
cr <- getResidualCor(fit1)
corrplot(cr[order.single(cr),order.single(cr)],
         diag=FALSE,
         type="lower",
         method="square",
         tl.cex=0.8,tl.srt=45,tl.col="black")

#add traits
fit_4th <- gllvm(birdMatrix, X, TR, family=poisson(),num.lv=2,
              formula = ~ (urban + agri + forest) + (urban + agri + forest):(Insect + Seed))

library(lattice)
par(mfrow=c(2,1))
coefplot(fit_4th, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourth <- fit_4th$fourth.corner
a <- max( abs(fourth) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4th <- levelplot((as.matrix(fourth)), xlab = "Environmental Variables",
                      ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3,
                      at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))

plot.4th

#with insect biomass - add traits
fit_4th <- gllvm(birdMatrix, X, family=poisson(),num.lv=2,
                 formula = ~ urban + agri + forest + (insect_biomass))
coefplot(fit_4th, mar = c(4, 11, 1, 1), cex.ylab = 0.8)

fit_4th <- gllvm(birdMatrix, X, TR, family=poisson(),num.lv=2,
                 formula = ~ (urban + agri + forest + insect_biomass) + (urban + agri + forest + insect_biomass):(Insect+Seed))
coefplot(fit_4th, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
