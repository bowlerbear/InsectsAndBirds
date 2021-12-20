library(AHMbook)
library(tidyverse)
library(lubridate)

### DOF data #####

species <- c("Alauda arvensis", "Perdix perdix", 
               "Passer domesticus", "Passer montanus",
               "Fringilla coelebs","Hirundo rustica")
myspecies <- species[6]

#read in bird observation data - counts of birds using distance sampling
data <- read.csv("data/Insects_and_TTT/ttt_data.csv",sep=";")

#read in transect info
info <- read.csv("data/Insects_and_TTT/ttt_info.csv",sep=";")
info$Date <- as.Date(info$dato)
info$Year <- year(info$Date)

#start with just the last year of data
infoS <- info %>%
            filter(Year==2016) %>%
            select(-id)

#and just one species
dataS <- data %>% 
          filter(latin==myspecies) %>%
          select(-id)

#join data
allData <- left_join(infoS,dataS)

#which season do we have most data for
table(dataS$type)
allData <- filter(allData, type=="sen")

#plot histogram of detected birds
allData %>% dplyr::select(X.0,X.1,X.2) %>%
            pivot_longer(everything(),names_to="distance", values_to="nu") %>%
            dplyr::filter(!is.na(nu)) %>%
            dplyr::mutate(distance = recode(distance, X.0 = "25", X.1 = "50", X.2 = "100")) %>%
            dplyr::group_by(distance) %>%
            dplyr::summarise(total=sum(nu)) %>%
            dplyr::mutate(distance = factor(distance,levels=c("25","50","100"))) %>%
            ggplot() + geom_col(aes(x=distance,y=total))+ggtitle(myspecies)

#organise data for the distance model

#was a quadrat number visited more than once? no
summaryQuad <- allData %>%
                    dplyr::group_by(kvadratnr) %>%
                    dplyr::summarise(nu = length(unique(dato)))#all only visited once
table(summaryQuad$nu)

# Get number of individuals detected per site
procData <- allData %>% 
                dplyr::select(kvadratnr,X.0,X.1,X.2) %>%
                pivot_longer(starts_with("X."),names_to="distance", values_to="nu") %>%
                dplyr::mutate(distance = 
                                  recode(distance, X.0 = "25", X.1 = "50", X.2 = "100")) %>%
                dplyr::mutate(nu = ifelse(is.na(nu),0,nu),
                              distance = as.numeric(as.character(distance)))
    
### Distance model ####

# library(Distance)
# ds_hn <- ds(procData, transect="line",key="hn")
# ds_hr <- ds(procData, transect="line",key="hr")
# ds_uni <- ds(procData, transect="line",key="unif")
# 
# plot(ds_hn)
# gof_ds(ds_hn)
# summarize_ds_models(ds_hn, ds_hr, ds_uni, output="plain")


### unmarked ####

library(unmarked)

temp <- allData[,c("X.0","X.1","X.2")]
temp[is.na(temp)] <- 0

unmarkDF <- unmarkedFrameDS(y=as.matrix(temp),
                            siteCovs=data.frame(scale(allData[,c("skydaekke","regn","vind")])),
                            dist.breaks=c(0,25,50,100), 
                            tlength=rep(100,nrow(allData)),
                            unitsIn="m", 
                            survey="line")

(fm1 <- distsamp(~1 ~1, data = unmarkDF, keyfun = "halfnorm"))
(fm2 <- distsamp(~1 ~1, data = unmarkDF, keyfun = "hazard"))
(fm3 <- distsamp(~1 ~1, data = unmarkDF, keyfun = "exp"))
(fm4 <- distsamp(~1 ~1, data = unmarkDF, keyfun = "uniform"))

#compare fits
summary(fm1)
hist(fm2, xlab="Distance (m)")	
fmList <- fitList(HN=fm1, HA=fm2, EXP=fm3, UNI=fm4)

#find out best distance model for each of these
(tab <- modSel(fmList, nullmod="HN"))
tab <- as(tab,'data.frame')

ggplot(tab)+
    geom_col(aes(x=model,y=AIC))

#see also 
#gdistsamp

### covariates ####



### JAGS ####

#get other sites
procData_pos <- procData %>%
                    filter(nu!=0) %>%
                    dplyr::group_by(kvadratnr,distance) %>%
                    dplyr::slice(rep(1:n(), each = nu)) %>%
                    select(-nu) %>%
                    add_column(y=1)

#get zero sites
procData_zeros <- procData %>%
                        filter(nu==0) %>%
                        filter(!duplicated(kvadratnr)) %>%
                        select(kvadratnr) %>%
                        dplyr::mutate(distance=NA) %>%
                        filter(!kvadratnr %in% procData_pos$kvadratnr)

#combine all
mydata <- bind_rows(procData_pos,procData_zeros) %>%
            arrange(kvadratnr,distance) 
mydata$siteID <- as.numeric(as.factor(mydata$kvadratnr))
head(mydata)

# ncap = 1 plus number of detected individuals per site
ncap <- table(mydata[,"siteID"])                # ncap = 1 if no individuals captured
sites0 <- mydata$siteID[is.na(mydata$y)]        # sites where nothing detected
ncap[names(ncap) %in% sites0] <- 0              # Fill in 0 for sites with no detections
ncap <- as.vector(ncap)

# Prepare other data
site <- mydata$siteID[!is.na(mydata$y)]         # site ID of each observation
delta <-  c(25,25,50)                           # distance bin width for rect. approx.
midpt <- c(12.5,25,50)                          # make mid-points and chop up data
B <- 100
nD <- length(midpt)                             # Number of distance intervals
y <- mydata$distance[!is.na(mydata$y)]          # distance data
dclass <- ifelse(y==25,1,
                 ifelse(y==50,2,3))             # Observed categorical observations
nind <- length(dclass)                          # Total number of individuals detected

# Bundle and summarize data set
win.data <- list(nsites=length(ncap), site=site, nind=nind, 
                 B=B, nD=nD, midpt=midpt,
                 delta=delta, ncap=ncap, dclass=dclass) 

# BUGS model specification for line-transect HDS 
cat("
    model{
    # Priors
    alpha0 ~ dnorm(0,0.01)
    #alpha1 ~ dunif(-10,10)
    beta0 ~ dnorm(0,0.01)
    #beta1 ~ dunif(-10,10)
    
    for(i in 1:nind){
    
        dclass[i] ~ dcat(fc[site[i],]) # Part 1 of HM
        
    }
    
    for(s in 1:nsites){
    
        # Construct cell probabilities for nD multinomial cells
        for(g in 1:nD){                                 
            log(p[s,g]) <- -midpt[g] * midpt[g] / (2*sigma[s]*sigma[s]) #half normal
            log(p[s,g]) <- 1 - exp (-(1 - exp (- ( power( x/sigma ),(-1 * beta) ) ) ) ) #hazard # p.401 K&R
            pi[s,g] <- delta[g] / B                     
            f[s,g] <- p[s,g] * pi[s,g]
            fc[s,g] <- f[s,g] / pcap[s]
        }
        
        #site-level parameters
        
        pcap[s] <- sum(f[s,])           # Pr(capture): sum of rectangular areas
        ncap[s] ~ dbin(pcap[s], N[s])   # Part 2 of HM
        N[s] ~ dpois(lambda[s])         # Part 3 of HM
        
        #models:
        log(lambda[s]) <- beta0 
        log(sigma[s]) <- alpha0      
    }
    
    # Derived parameters
    
    #Ntotal <- sum(N[])
    #area<- nsites*1*2*B  # Unit length == 1, half-width = B
    #D<- Ntotal/area
    
    }
    ",fill=TRUE, file = "distanceModel.txt")


# Inits
Nst <- ncap + 2
inits <- function(){list(alpha0 = 0, beta0 = 0, N = Nst)}

# Params to save
params <- c("alpha0", "beta0", "Ntotal", "D")

# MCMC settings
ni <- 12000   ;   nb <- 2000   ;   nt <- 1   ;   nc <- 3

# Run JAGS (ART 1 min) and summarize posteriors
library(jagsUI)
myModel <- jags(win.data, inits, params, "distanceModel.txt", 
                n.thin=nt,n.chains=nc, n.burnin=nb, n.iter=ni)
print(myModel, 2)
