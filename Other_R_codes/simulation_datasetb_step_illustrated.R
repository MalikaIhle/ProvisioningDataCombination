# simulation of dataset b - steps illustrated
library(here)

# assumptions

## exponential distribution of IVI
hist(rexp(10000), breaks = 100, xlim = c(0,10), ylim = c(0,1000), xaxt = "n",
     main = "Exponential distribution", xlab = "Interval duration (min)")
     axis(side = 1, at = 0:10, labels = FALSE, tck = -0.01)

## uniform distribution of visit times
hist(runif(10000, 0, 90), breaks = 100,  xlim = c(0,96), ylim = c(0,150), 
     main = "Uniform distribution", 
     xlab = "Time of entrance in the nest, for a nest watch of 90 min", 
     ylab = "Frequency")
     axis(side=1, at=seq(10,90, 20), labels=seq(10,90,20))


# Steps

## generate PR from observed parameters

MY_TABLE_perDVD <- read.csv(paste(here(), "R_Selected&RandomizedData/R_MY_TABLE_perDVD.csv", sep="/")) 


### observed provisioning rate
hist(c(MY_TABLE_perDVD$FVisit1,MY_TABLE_perDVD$MVisit1), 
     breaks = 20, xlim = c(0,80), ylim = c(0,1000), 
     main = "Overdispersed Poisson distribution", 
     xlab = "Observed number of provisioning visits per bird per nest watch")







### expected provisioning rate on the latent scale - without poisson stochastic error
summary(c(MY_TABLE_perDVD$FVisit1,MY_TABLE_perDVD$MVisit1))
sd(c(MY_TABLE_perDVD$FVisit1,MY_TABLE_perDVD$MVisit1))
nPR <- nrow(MY_TABLE_perDVD)
avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the observed scale

# expected mean = observed mean
# expected sd = sqrt(var(observed)-mean(observed))
# in poisson mean = variance, we are trying to remove poisson variance. we can remove the mean (since = variance)
# var(observed) = biological variation + poisson error
# poisson error = mean(observed)

# expected sd = sqrt(sd^2(observed)-mean(observed)) # formula for variance sqrted to get sd
expected_mean <- avPR
expected_sd <- sqrt(sdPR^2-avPR)


meanlogPR <- log(avPR) # pass on the log scale to be able to add Poisson distributed error # not actually correct
sdlogPR <- sqrt(log(1 + sdPR^2/avPR^2)) 


### after extracting mean and SD on the expected scale and logged (with jensen inequality) to input into rlnorm, simulate PR expected
MalePexp <- rlnorm(nPR, meanlog = meanlogPR, sdlog = sdlogPR ) # expected number of visits
FemalePexp <- rlnorm(nPR, meanlog = meanlogPR, sdlog = sdlogPR )

hist(c(MalePexp,FemalePexp), 
     breaks = 20, xlim = c(0,80), ylim = c(0,1000), 
     main = "Log Normal distribution", 
     xlab = "Expected number of provisioning visits per bird per nest watch")

### Draw from such distribution and add stochastic Poisson error
MaleP <- NULL
FemaleP <- NULL

for (i in 1: length(MalePexp)){ # for my analyses I selected videos where both partners visited at least once
  MaleP5 <- rpois(5, MalePexp[i])  # potential realized number of visits including the poisson distributed error
  MaleP[i] <- sample(MaleP5[MaleP5>0],1)  # picking one potential MaleP that is not zero (excluded in observed data)
  FemaleP5 <- rpois(5, FemalePexp[i])
  FemaleP[i] <- sample(FemaleP5[FemaleP5>0],1)
}

hist(c(MaleP,FemaleP), 
     breaks = 20, xlim = c(0,80), ylim = c(0,1000), 
     main = "Overdispersed Poisson distribution", 
     xlab = "Simulated number of provisioning visits per bird per nest watch")

## generate nest visit times
MaleVisits <- NULL
FemaleVisits <- NULL

for (i in 1:length(MaleP)){
MaleVisits[[i]] <- sort(runif(MaleP[i],0,90)) # MaleP is male number of provisioning visits
FemaleVisits[[i]] <- sort(runif(FemaleP[i],0,90))
}

MaleIntervals <- lapply(MaleVisits,function(x)diff(x))
FemaleIntervals <- lapply(FemaleVisits,function(x)diff(x))

hist(unlist(MaleVisits), 
     breaks = 100, xlim = c(0,90), #ylim = c(0,110), 
     main = "Uniform distribution", 
     xlab = "Simulated time of entrance in the nest, for a nest watch of 90 min",
     ylab = "Frequency")
     axis(side=1, at=seq(10,90, 20), labels=seq(10,90,20))


hist(c(unlist(MaleIntervals),unlist(FemaleIntervals)), breaks = 100, #ylim = c(0,600), 
    main = "Exponential distribution", xlab = "Simulated interval durations (min)")








