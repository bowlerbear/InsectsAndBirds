### example 1 ####
# 8.5.2 Bayesian HDS using data augmentation

library(AHMbook)

# Recreate line transect data set
set.seed(1234)
tmp <- simHDS()                  # Line transect (default)
attach(tmp)

# Data augmentation: add a bunch of "pseudo-individuals"
nz <- 500                        # Augment by 500
nind <- nrow(data)
y <- c(data[,2], rep(0, nz))     # Augmented detection indicator y
site <- c(data[,1], rep(NA, nz)) # Augmented site indicator,
# unknown (i.e., NA) for augmented inds.
d <- c(data[,5], rep(NA,nz))     # Augmented distance data (with NAs)

# Bundle and summarize data set
str( win.data <- list(nsites=nsites, habitat=habitat, wind=wind, B=B, nind=nind, nz=nz, y=y, d=d, site=site) )
win.data$site                    # unknown site cov. for augmented inds.


# BUGS model for line transect HDS (NOT point transects!)
cat("
    model{
    # Prior distributions
    beta0 ~ dunif(-10,10)   # Intercept of lambda-habitat regression
    beta1 ~ dunif(-10,10)   # Slope of log(lambda) on habitat
    alpha0 ~ dunif(-10,10)  # Intercept of log(sigma) (half-normal scale)
    alpha1 ~ dunif(-10,10)  # Slope of log(sigma) on wind
    
    # psi is a derived parameter under DA for stratified populations
    psi <- sum(lambda[]) / (nind+nz)
    
    # 'Likelihood' (sort of...)
    for(i in 1:(nind+nz)){                 # i is index for individuals
    z[i] ~ dbern(psi)                    # Data augmentation variables
    d[i] ~ dunif(0, B)                   # distance uniformly distributed
    p[i] <- exp(-d[i]*d[i]/(2*sigma[site[i]]*sigma[site[i]])) # Det. function
    mu[i] <- z[i]* p[i]                  # 'straw man' for WinBUGS
    y[i] ~ dbern(mu[i])                  # basic Bernoulli random variable
    site[i] ~ dcat(site.probs[1:nsites]) # Population distribution among sites
    }
    
    # Linear models for abundance and for detection
    for(s in 1:nsites){                    # s is index for sites
    # Model for abundance
    # next line not necessary, but allows to make predictions
    N[s] ~ dpois(lambda[s])              # Realized abundance at site s
    log(lambda[s]) <- beta0 + beta1*habitat[s] # Linear model abundance
    site.probs[s] <- lambda[s] / sum(lambda[])
    
    # Linear model for detection
    log(sigma[s]) <- alpha0 + alpha1*wind[s]
    }
    # Derived parameter: total population size across all sites
    Ntotal <- sum(z[])
    area<- nsites*1*2*B   # Unit length == 1, half-width = B
    D<- Ntotal/area
    }
    ",fill=TRUE , file = "model1.txt")


# Inits
zst <- c(rep(1, sum(y)), rep(0, nz)) # ... and for DA variables
inits <- function(){list(beta0=0, beta1=0, alpha0=0, alpha1=0, z=zst)}

# Parameters to save
params <- c("alpha0", "alpha1", "beta0", "beta1", "psi", "Ntotal", "D")

# MCMC settings
ni <- 12000   ;   nb <- 2000   ;   nt <- 2   ;   nc <- 3

# Call BUGS (ART 33 min) ...
bd <- "c:/Program Files/WinBUGS14/" # Never forget this for WinBUGS
#Taken from Kery and Royle

### example 2 ####
# Bayesian HDS using the 3-part conditional multinomial model

library(AHMbook)
set.seed(1234)
tmp <- simHDS(type="line", discard0=FALSE)
attach(tmp)

# Get number of individuals detected per site

# ncap = 1 plus number of detected individuals per site
ncap <- table(data[,1])            # ncap = 1 if no individuals captured
sites0 <- data[is.na(data[,2]),][,1] # sites where nothing detected
ncap[as.character(sites0)] <- 0    # Fill in 0 for sites with no detections
ncap <- as.vector(ncap)

# Prepare other data
site <- data[!is.na(data[,2]),1]   # site ID of each observation
delta <- 0.1                       # distance bin width for rect. approx.
midpt <- seq(delta/2, B, delta)    # make mid-points and chop up data
dclass <- data[,5] %/% delta + 1   # convert distances to cat. distances
nD <- length(midpt)                # Number of distance intervals
dclass <- dclass[!is.na(data[,2])] # Observed categorical observations
nind <- length(dclass)             # Total number of individuals detected

# Bundle and summarize data set
str( win.data <- list(nsites=nsites, nind=nind, B=B, nD=nD, midpt=midpt, delta=delta, ncap=ncap, habitat=habitat, wind=wind, dclass=dclass, site=site) )

# BUGS model specification for line-transect HDS 
cat("
    model{
    # Priors
    alpha0 ~ dunif(-10,10)
    alpha1 ~ dunif(-10,10)
    beta0 ~ dunif(-10,10)
    beta1 ~ dunif(-10,10)
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fc[site[i],]) # Part 1 of HM
    }
    
    for(s in 1:nsites){
    # Construct cell probabilities for nD multinomial cells
    for(g in 1:nD){                 # midpt = mid-point of each cell
    log(p[s,g]) <- -midpt[g] * midpt[g] / (2*sigma[s]*sigma[s])
    pi[s,g] <- delta / B          # probability per interval
    f[s,g] <- p[s,g] * pi[s,g]
    fc[s,g] <- f[s,g] / pcap[s]
    }
    pcap[s] <- sum(f[s,])           # Pr(capture): sum of rectangular areas
    
    ncap[s] ~ dbin(pcap[s], N[s])   # Part 2 of HM
    N[s] ~ dpois(lambda[s])         # Part 3 of HM
    log(lambda[s]) <- beta0 + beta1 * habitat[s] # linear model abundance
    log(sigma[s])<- alpha0 + alpha1*wind[s]      # linear model detection
    }
    # Derived parameters
    Ntotal <- sum(N[])
    area<- nsites*1*2*B  # Unit length == 1, half-width = B
    D<- Ntotal/area
    }
    ",fill=TRUE, file = "model3.txt")


# Inits
Nst <- ncap + 1
inits <- function(){list(alpha0=0, alpha1=0, beta0=0, beta1=0, N=Nst)}

# Params to save
params <- c("alpha0", "alpha1", "beta0", "beta1", "Ntotal","D")

# MCMC settings
ni <- 12000   ;   nb <- 2000   ;   nt <- 1   ;   nc <- 3

# Run JAGS (ART 1 min) and summarize posteriors
library(jagsUI)
out3 <- jags(win.data, inits, params, "model3.txt", n.thin=nt,
             n.chains=nc, n.burnin=nb, n.iter=ni)
print(out3, 2)