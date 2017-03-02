#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Joel PICK & Malika IHLE    joel.l.pick@gmail.com   malika_ihle@hotmail.fr
#	 Simulation to help decide which analyses to perfom on provisioning data sparrows
#	 Start : 02/02/2017
#	 last modif : 22/02/2017
#	 commit: one function (create fulldataset) to test all analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{### Packages
library(pbapply)
library(lme4)
library(MASS)
options(scipen=999)

library(tidyr)
library(dplyr) 
library(ggplot2)

}


{## to test the link between A and TP


Generate_data_randomize_them_and_analyse_A <-function(){

{# generate data

avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))
VideoLength <- 90

# Joel suggested to pass on the log scale to be able to add Poisson distributed error, I assume (??)
meanlog <- log(avPR)
sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))

full_dat <- list()

for (i in 1:2000){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog ) # expected number of visits
FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
MaleP <- rpois(1, MalePexp) # realized number of visits including the poisson distributed error
FemaleP <- rpois(1, FemalePexp)
TotalP <- MaleP + FemaleP # total number of visits for that video
DiffP <- abs(MaleP - FemaleP)

MaleVisits <- sort(runif(MaleP,0,VideoLength))
FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
MaleIntervals <- c(diff(MaleVisits),90-MaleVisits[length(MaleVisits)])
FemaleIntervals <- c(diff(FemaleVisits),90-FemaleVisits[length(FemaleVisits)])

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- i
if (TotalP - DiffP >1) {full_dat[[i]] <- dat} # for my analyses I selected videos where both partners visited at least once

}

full_dat <- do.call(rbind,full_dat)

}

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(10,Randomize_Data_WithinFile_and_Calculate_A_S_fun(full_dat),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

}

{# create table long

median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer), S= apply(out_Ssim_within_df[,-1],1,median_integer))
			

sumarize_one_DVD <- function(x){

MaleP <-nrow(x[x$Sex==1,])
FemaleP <-nrow(x[x$Sex==0,])
TotalP <- MaleP+FemaleP
DiffP <- abs(MaleP-FemaleP)

A <- sum(diff(x$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
S <- sum(diff(x$Sex)!=0 & diff(x$Visits) <= 0.5)

summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),TotalP,DiffP,MaxA,A,S))

summary_DVD

}


summary_all_DVD <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))

SimOut <- merge (x=SimOut, y= summary_all_DVD[,c('DVDRef', 'TotalP', 'DiffP', 'MaxA')], by='DVDRef', all.x=TRUE)
SimOut$Type <- 'a_Sim'
summary_all_DVD$Type <- 'z_Obsv'

head(summary_all_DVD)
head(SimOut)

fulldat_long <- rbind(summary_all_DVD,SimOut)
fulldat_long$rowID <- seq(1:nrow(fulldat_long))

}

{# analyse 

modA <- glm(A~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=fulldat_long, family = 'poisson') 
#modA_off <- glm(A~ Type*scale(TotalP) + Type*scale(DiffP) + offset(log(MaxA))+ (1|DVDRef), data=fulldat_long, family = 'poisson')
#modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP)+ (1|DVDRef), data=fulldat_long, family = 'binomial') 
modS <- glm(S~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=fulldat_long, family = 'poisson') 
modTotalP <- glmer(TotalP ~ scale(A)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 
modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 

}

{# results
results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*A'), modA = rep(NA,3),modA_off= rep(NA,3), modAoutofAmax = rep(NA,3),modS = rep(NA,3), modP=rep(NA,3), modP_AMax= rep(NA,3))
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[5,4],3)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[6,4],3)
#results$modA_off[results$Factor=='Type*TotalP'] <- round(summary(modA_off)$coef[5,4],3)
#results$modA_off[results$Factor=='Type*DiffP'] <- round(summary(modA_off)$coef[6,4],3)
#results$modAoutofAmax[results$Factor=='Type*TotalP'] <- round(summary(modAoutofAmax)$coef[5,4],3)
#results$modAoutofAmax[results$Factor=='Type*DiffP'] <- round(summary(modAoutofAmax)$coef[6,4],3)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[5,4],3)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[6,4],3)
results$modP[results$Factor=='Type*A'] <- round(summary(modTotalP)$coef[4,4],3)
results$modP_AMax[results$Factor=='Type*A'] <- round(summary(modTotalP_AMax)$coef[4,4],3)
}

return(list(results))

}


n <- 100
all_results_A <- pbreplicate(n,Generate_data_randomize_them_and_analyse_A())
all_results_A_Sign <- lapply(all_results_A, function(x){x[,-1] <0.05})
all_results_A_Sign_compiled <- Reduce('+',all_results_A_Sign)/n*100
results_A_PercentageFactorSignificant <- data.frame(Factor=all_results_A[[1]]$Factor,all_results_A_Sign_compiled)
results_A_PercentageFactorSignificant

# modA <- glm(A~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'poisson')
# modA_off <- glm(A~ Type*TotalP + Type*DiffP + offset(log(MaxA)), data=fulldat_long, family = 'poisson')
# modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'binomial') 
# modTotalP <- glmer(TotalP ~ scale(A)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 
# modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 

# n <- 100
       # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1 Type*TotalP   13       22            90   NA        NA
# 2  Type*DiffP    0       wrong         68   NA        NA
# 3      Type*A   NA       NA            NA   73         0


# n <- 100
       # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1 Type*TotalP    4       13            91   NA        NA
# 2  Type*DiffP    0        0            63   NA        NA
# 3      Type*A   NA       NA            NA   83         0


# n <- 20
       # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1 Type*TotalP    0       10            80   NA        NA
# 2  Type*DiffP    0        0            45   NA        NA
# 3      Type*A   NA       NA            NA   90         0


# n <- 100
       # Factor modA modA_off modAoutofAmax modS modP modP_AMax
# 1 Type*TotalP   10       NA            NA    0   NA        NA
# 2  Type*DiffP    0       NA            NA    0   NA        NA
# 3      Type*A   NA       NA            NA   NA   78         0

}

{## to test the link between A and TP, while having CN correlated to TP
 

{## exploration on how to simulate correlated normally distributed TP and CN, and change it to counts

SelectedData_folder <- "R_Selected&SimulatedData"

MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD.csv", sep="/")) 

# descriptive statistics of the observed data
cov(MY_TABLE_perDVD$MFVisit1,MY_TABLE_perDVD$DVDInfoChickNb) # 7.21
var(MY_TABLE_perDVD$MFVisit1)#  204
var(MY_TABLE_perDVD$DVDInfoChickNb) # 1.1 
mean(MY_TABLE_perDVD$MFVisit1) # 31
mean(MY_TABLE_perDVD$DVDInfoChickNb) #2.8

# simulate 2 normally distributed variables with mean, variance, covariance specified
mean_TP_CN <- c(31,2.8) # mean(MY_TABLE_perDVD$MFVisit1) , mean(MY_TABLE_perDVD$DVDInfoChickNb)
Var_Covar_TP_CN <- matrix(c(204,8,8,1.1),2,2) # var(MY_TABLE_perDVD$MFVisit1) ,cov(MY_TABLE_perDVD$DVDInfoChickNb,MY_TABLE_perDVD$MFVisit1), var(MY_TABLE_perDVD$DVDInfoChickNb)
rawvars <- mvrnorm(n=3000, mu=mean_TP_CN, Sigma=Var_Covar_TP_CN)

# we get ~ the specification we put in
mean(rawvars[,1])
mean(rawvars[,2])
var(rawvars[,1])
var(rawvars[,2])
cov(rawvars)
cor(rawvars) # correlation between TP and CN of 0.55 

# excluded videos where no chicks or no visits (like with observed data)
rawvars <- rawvars[rawvars[,1]>0 & rawvars[,2]>0,] 

# how does removing zeros affects the match between specification in and out
mean(rawvars[,1]) 
mean(rawvars[,2])
var(rawvars[,1]) # variance in TP reduced a lot
var(rawvars[,2]) 
cov(rawvars) # variance in TP reduced 
cor(rawvars) # correlation between TP and CN of 0.5 (in data = 0.48)

hist(rawvars[,1]) # this looks normally distributed
hist(MY_TABLE_perDVD$MFVisit1) # this looks poisson distributed
hist(rawvars[,2]) # this looks normally distributed
hist(MY_TABLE_perDVD$DVDInfoChickNb) # this looks poisson distributed

# change continuous variable into count, adding poisson distributed error to TP
poissonvars <- matrix(nrow=nrow(rawvars), ncol=2)

for (i in 1:nrow(rawvars)){
poissonvars[i,1] <- rpois(1, rawvars[i,1]) # this overdisperse the data (like in observed data)
poissonvars[i,2] <- round(rawvars[i,2]) # this does not overdisperse the data (observed data are underdispersed)
}

# excluded videos where no chicks or no visits (like with observed data)
poissonvars <- poissonvars[poissonvars[,1]>0 & poissonvars[,2]>0,]
 
mean(poissonvars[,1])
mean(poissonvars[,2])
var(poissonvars[,1]) # variance in TP increases back to ~ observed data
var(poissonvars[,2])
cov(poissonvars)
cor(poissonvars) # correlation between TP and CN of 0.45 (in data = 0.48)

hist(poissonvars[,1])
hist(MY_TABLE_perDVD$MFVisit1)
hist(poissonvars[,2])
hist(MY_TABLE_perDVD$DVDInfoChickNb)

}


Generate_data_withCN_randomize_them_and_analyse <-function(){

{# generate data

{## summary table with TotalP, ChickNb, MaleP, FemaleP for 2000 observations 

# simulate 2 normally distributed variables TP and CN with mean, variance, covariance specified
mean_TP_CN <- c(31,2.8) # average total provisioning visits , average chick number
Var_Covar_TP_CN <- matrix(c(204,8,8,1.1),2,2) # var TP ,cov(CN,TP), var(CN)
rawvars <- mvrnorm(n=4000, mu=mean_TP_CN, Sigma=Var_Covar_TP_CN) # simulate a lot of observartions to be able to remove those that aren;t strictly positive
rawvars <- rawvars[rawvars[,1]>0 & rawvars[,2]>0,] 

# change those continuous variables into count, adding poisson distributed error to TP
poissonvars <- matrix(nrow=nrow(rawvars), ncol=2)

for (i in 1:nrow(rawvars)){
poissonvars[i,1] <- rpois(1, rawvars[i,1]) # this overdisperse the data (like in observed data)
poissonvars[i,2] <- round(rawvars[i,2]) # this does not overdisperse the data (observed data are underdispersed)
}

# excludes videos where no chicks or less than 2 visits (at least one per parents; like with observed data, which is needed to calculate alternation)
full_dat <- as.data.frame(poissonvars[poissonvars[,1]>1 & poissonvars[,2]>0,])
colnames(full_dat) <- c("TotalP","ChickNb")

# create a large pool of potential Male Nb of visits
MalePexp <- rlnorm(2*nrow(full_dat), meanlog = log(15), sdlog = sqrt(log(1 + 8^2/15^2)) ) # expected number of visits, in observed data, average per indiv is 15, sd is 8 (on the expected scale)

# realized number of visit with poisson distributed error
MaleP <- NULL
for (i in 1:length(MalePexp)){
MaleP[i] <- rpois(1, MalePexp[i]) 
}

# pick a possible realized Male number of visit below the total number of visit simulated for that observation and superior to 0 (like in the observed data)
for (i in 1:nrow(full_dat)){
full_dat$MaleP[i] <- sample(MaleP[MaleP < full_dat$TotalP[i] & MaleP > 0],1) 
}


# deduce female number of visit, difference in visit numbers, and alternation max
full_dat$FemaleP <- full_dat$TotalP - full_dat$MaleP
full_dat$DiffP <- abs(full_dat$MaleP - full_dat$FemaleP)

full_dat <- head(full_dat,2000) # select the first 2000 observations that meet criteria of the observed data

for (i in 1:nrow(full_dat)){
if (full_dat$MaleP[i] == full_dat$FemaleP[i]) {full_dat$MaxA[i] <- full_dat$TotalP[i] - full_dat$DiffP[i] -1} 
else {full_dat$MaxA[i] <- full_dat$TotalP[i] - full_dat$DiffP[i]}}


full_dat$DVDRef <- seq(1:nrow(full_dat))

}

{## simulating 2000 observations with the characteristic defined above - calculate A observed for each

VideoLength <- 90
raw_dat <- list()

	# x <- split(full_dat, full_dat$DVDRef)[[1]]

create_rawData_perDVD <- function (x) {

MaleVisits <- sort(runif(x$MaleP,0,VideoLength))
FemaleVisits <- sort(runif(x$FemaleP,0,VideoLength))
MaleIntervals <- c(diff(MaleVisits),90-MaleVisits[length(MaleVisits)])
FemaleIntervals <- c(diff(FemaleVisits),90-FemaleVisits[length(FemaleVisits)])

if (length(MaleVisits) > 0 & length(FemaleVisits) >0){
dat <- data.frame(rbind(cbind(Tstart = MaleVisits, 
								Sex = rep(1, length(MaleVisits)),
								Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,
								Sex = rep(0, length(FemaleVisits)), 
								Interval=FemaleIntervals)))
dat$DVDRef <- rep(x$DVDRef, nrow(dat))
dat <- dat[order(dat$Tstart),]
return(dat)
}


}


raw_dat <- do.call(rbind,lapply(split(full_dat, full_dat$DVDRef),create_rawData_perDVD))

sumarize_one_DVD <- function(x){
A <- sum(diff(x$Sex)!=0)
S <- sum(diff(x$Sex)!=0 & diff(x$Visits) <= 0.5)
return(c(unique(x$DVDRef), A, S))
}

summary_obs <- do.call(rbind,lapply(split(raw_dat, raw_dat$DVDRef),sumarize_one_DVD))
colnames(summary_obs) <- c('DVDRef', 'A','S')

full_dat <- merge(x=full_dat, y=summary_obs, by='DVDRef')

}

}

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(10,Randomize_Data_WithinFile_and_Calculate_A_S_fun(raw_dat),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(raw_dat$DVDRef), head(A_S_within_randomization,length(unique(raw_dat$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(raw_dat$DVDRef), tail(A_S_within_randomization,length(unique(raw_dat$DVDRef))))

}

{# create table long

median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer),S= apply(out_Ssim_within_df[,-1],1,median_integer))

SimOut <- merge (x=SimOut, y= full_dat[,c('DVDRef','TotalP', 'DiffP','ChickNb', 'MaleP', 'FemaleP', 'MaxA')], by='DVDRef', all.x=TRUE) # all meta data from full_dat but the data 'A'
SimOut$Type <- 'a_Sim'
full_dat$Type <- 'z_Obsv'

fulldat_long <- rbind(full_dat,SimOut) # first half: A is observed, second half, A is from randomization
fulldat_long$rowID <- seq(1:nrow(fulldat_long))

}

{# analyse 

modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
#modA_off <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + offset(log(MaxA)) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa"))
#modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb), data=fulldat_long, family = 'binomial') 
modS <- glmer(S~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
#modTotalP <- glmer(TotalP ~ scale(A)*Type + scale(ChickNb) + (1|DVDRef), family = 'poisson', data=fulldat_long,control=glmerControl(optimizer = "bobyqa")) 
modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + scale(ChickNb) + (1|DVDRef) , family = 'poisson', data=fulldat_long,control=glmerControl(optimizer = "bobyqa")) 

}

{# results
results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*ChickNb', 'Type*A'), modA = rep(NA,4),modA_off= rep(NA,4), modAoutofAmax = rep(NA,4),modP=rep(NA,4), modP_AMax= rep(NA,4))
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[6,4],4)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[7,4],4)
results$modA[results$Factor=='Type*ChickNb'] <- round(summary(modA)$coef[8,4],4)
# results$modA_off[results$Factor=='Type*TotalP'] <- round(summary(modA_off)$coef[6,4],4)
# results$modA_off[results$Factor=='Type*DiffP'] <- round(summary(modA_off)$coef[7,4],4)
# results$modA_off[results$Factor=='Type*ChickNb'] <- round(summary(modA_off)$coef[8,4],4)
# results$modAoutofAmax[results$Factor=='Type*TotalP'] <- round(summary(modAoutofAmax)$coef[6,4],4)
# results$modAoutofAmax[results$Factor=='Type*DiffP'] <- round(summary(modAoutofAmax)$coef[7,4],4)
# results$modAoutofAmax[results$Factor=='Type*ChickNb'] <- round(summary(modAoutofAmax)$coef[8,4],4)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[6,4],4)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[7,4],4)
results$modS[results$Factor=='Type*ChickNb'] <- round(summary(modS)$coef[8,4],4)
# results$modP[results$Factor=='Type*A'] <- round(summary(modTotalP)$coef[5,4],4)
results$modP_AMax[results$Factor=='Type*A'] <- round(summary(modTotalP_AMax)$coef[5,4],4)

}


return(list(results))
}


n <-100

all_results <- pbreplicate(n,Generate_data_withCN_randomize_them_and_analyse())
all_results_Sign <- lapply(all_results, function(x){x[,-1] <0.05})
all_results_Sign_compiled <- Reduce('+',all_results_Sign)/n*100
results_PercentageFactorSignificant <- data.frame(Factor=all_results[[1]]$Factor,all_results_Sign_compiled)
results_PercentageFactorSignificant


# modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) # [,1] 
# modA_off <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + offset(log(MaxA)) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa"))
# modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb), data=fulldat_long, family = 'binomial') # [,2] 
# modTotalP <- glmer(TotalP ~ scale(A)*Type + scale(ChickNb) + (1|DVDRef), family = 'poisson', data=fulldat_long,control=glmerControl(optimizer = "bobyqa")) # [,4] 
# modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + scale(ChickNb) + (1|DVDRef) , family = 'poisson', data=fulldat_long,control=glmerControl(optimizer = "bobyqa")) # [,3] 


#n <- 100
		# Factor modA modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP   59       76            99   NA        NA
# 2   Type*DiffP    0       wrong         99   NA        NA
# 3 Type*ChickNb    0       wrong          0   NA        NA
# 4       Type*A   NA       NA            NA   46        18


#n <- 100
       # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP   54       72           100   NA        NA
# 2   Type*DiffP    1        1            98   NA        NA
# 3 Type*ChickNb    0        0             0   NA        NA
# 4       Type*A   NA       NA            NA   46        14


#n <- 20
        # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP   60       80           100   NA        NA
# 2   Type*DiffP    0        0           100   NA        NA
# 3 Type*ChickNb    0        0             0   NA        NA
# 4       Type*A   NA       NA            NA   50        10


#n <- 100
        # Factor modA modA_off modAoutofAmax modP modP_AMax modS
# 1  Type*TotalP   62       NA            NA   NA        NA    0
# 2   Type*DiffP    0       NA            NA   NA        NA    0
# 3 Type*ChickNb    0       NA            NA   NA        NA    0
# 4       Type*A   NA       NA            NA   NA        12   NA

}

{## Bebbington & Hatchwell 2015 analyses on A/(TP-1), while having CN correlated to TP

Generate_data_randomize_them_and_analyse_ben <-function(){

{# generate data

{## summary table with TotalP, ChickNb, MaleP, FemaleP for 2000 observations 

# simulate 2 normally distributed variables TP and CN with mean, variance, covariance specified
mean_TP_CN <- c(31,2.8) # average total provisioning visits , average chick number
Var_Covar_TP_CN <- matrix(c(204,8,8,1.1),2,2) # var TP ,cov(CN,TP), var(CN)
rawvars <- mvrnorm(n=4000, mu=mean_TP_CN, Sigma=Var_Covar_TP_CN) # simulate a lot to be able to remove those with zeros
rawvars <- rawvars[rawvars[,1]>0 & rawvars[,2]>0,] 

# change those continuous variables into count, adding poisson distributed error to TP
poissonvars <- matrix(nrow=nrow(rawvars), ncol=2)

for (i in 1:nrow(rawvars)){
poissonvars[i,1] <- rpois(1, rawvars[i,1]) # this overdisperse the data (like in observed data)
poissonvars[i,2] <- round(rawvars[i,2]) # this does not overdisperse the data (observed data are underdispersed)
}

# excluded videos where no chicks or less than 2 visits (at least one per parents; like with observed data, needed to calculate alternation)
full_dat <- as.data.frame(poissonvars[poissonvars[,1]>1 & poissonvars[,2]>0,])
colnames(full_dat) <- c("TotalP","ChickNb")

# create a large pool of potential Male Nb of visits
MalePexp <- rlnorm(2*nrow(full_dat), meanlog = log(15), sdlog = sqrt(log(1 + 8^2/15^2)) ) # expected number of visits, in observed data, average per indiv is 15, sd is 8

# realized number of visit with poisson distributed error
MaleP <- NULL
for (i in 1:length(MalePexp)){
MaleP[i] <- rpois(1, MalePexp[i]) 
}

# pick a possible realized Male number of visit below the total number of visit simulated for that observation and superior to 0 (like in the observed data)
for (i in 1:nrow(full_dat)){
full_dat$MaleP[i] <- sample(MaleP[MaleP < full_dat$TotalP[i] & MaleP > 0],1) 
}


# deduce female number of visit, difference in visit numbers, and alternation max
full_dat$FemaleP <- full_dat$TotalP - full_dat$MaleP
full_dat$DiffP <- abs(full_dat$MaleP - full_dat$FemaleP)

full_dat <- head(full_dat,2000)

full_dat$DVDRef <- seq(1:nrow(full_dat))

}

{## simulating 2000 observations with the characteristic defined above - calculate A observed for each

VideoLength <- 90
raw_dat <- list()

	# x <- split(full_dat, full_dat$DVDRef)[[1]]

create_rawData_perDVD <- function (x) {

MaleVisits <- sort(runif(x$MaleP,0,VideoLength))
FemaleVisits <- sort(runif(x$FemaleP,0,VideoLength))
MaleIntervals <- c(diff(MaleVisits),90-MaleVisits[length(MaleVisits)])
FemaleIntervals <- c(diff(FemaleVisits),90-FemaleVisits[length(FemaleVisits)])

if (length(MaleVisits) > 0 & length(FemaleVisits) >0){
dat <- data.frame(rbind(cbind(Tstart = MaleVisits, 
								Sex = rep(1, length(MaleVisits)),
								Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,
								Sex = rep(0, length(FemaleVisits)), 
								Interval=FemaleIntervals)))
dat$DVDRef <- rep(x$DVDRef, nrow(dat))
dat <- dat[order(dat$Tstart),]
return(dat)
}


}


raw_dat <- do.call(rbind,lapply(split(full_dat, full_dat$DVDRef),create_rawData_perDVD))

sumarize_one_DVD <- function(x){
x <- x[order(x$Tstart),]

A <- sum(diff(x$Sex)!=0)/(nrow(x)-1)*100 # A score

x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
S <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])/ (nrow(x))*100
return(c(unique(x$DVDRef), A, S))
}

summary_A_obs <- do.call(rbind,lapply(split(raw_dat, raw_dat$DVDRef),sumarize_one_DVD))
colnames(summary_A_obs) <- c('DVDRef', 'Ascore', 'Sscore')

full_dat <- merge(x=full_dat, y=summary_A_obs, by='DVDRef')

}

}

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
Asim <- sum(diff(x$Sex)!=0)/(nrow(x)-1)*100  # A score
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])/ (nrow(x))*100
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(10,Randomize_Data_WithinFile_and_Calculate_A_S_fun(raw_dat),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(raw_dat$DVDRef), head(A_S_within_randomization,length(unique(raw_dat$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(raw_dat$DVDRef), tail(A_S_within_randomization,length(unique(raw_dat$DVDRef))))

}

{# calculate deviation from randomization

#median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
#SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer))

SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], MeanASim = rowMeans(out_Asim_within_df[,-1]))
full_dat <- merge (x=full_dat, y=SimOut , by='DVDRef', all.x=TRUE)
full_dat$Adev <- full_dat$Ascore - full_dat$MeanASim

}

{# analyse 

modA <- lm(Ascore~ scale(TotalP) + scale(DiffP) + scale(ChickNb), data=full_dat) 
modTotalP <- lm(TotalP ~ scale(Adev) + scale(ChickNb), data=full_dat) 
modS <- glm(round(Sscore) ~ scale(TotalP)+ scale(Ascore) + scale(ChickNb), data=full_dat, family='poisson')

}

{# results
results <- data.frame(Factor = c('TotalP', 'DiffP', 'ChickNb', 'Adev', 'A'), modA = rep(NA,5), modP=rep(NA,5), modS= rep(NA,5))
results$modA[results$Factor=='TotalP'] <- round(summary(modA)$coef[2,4],3)
results$modA[results$Factor=='DiffP'] <- round(summary(modA)$coef[3,4],3)
results$modA[results$Factor=='ChickNb'] <- round(summary(modA)$coef[4,4],3)
results$modP[results$Factor=='Adev'] <- round(summary(modTotalP)$coef[2,4],3)
results$modP[results$Factor=='ChickNb'] <- round(summary(modTotalP)$coef[3,4],3)
results$modS[results$Factor=='TotalP'] <- round(summary(modS)$coef[2,4],3)
results$modS[results$Factor=='A'] <- round(summary(modS)$coef[3,4],3)
results$modS[results$Factor=='ChickNb'] <- round(summary(modS)$coef[4,4],3)
}

return(list(results))

}


n <- 100

all_results_ben <- pbreplicate(n,Generate_data_randomize_them_and_analyse_ben())
all_results_ben_Sign <- lapply(all_results_ben, function(x){x[,-1] <0.05})
all_results_ben_Sign_compiled <- Reduce('+',all_results_ben_Sign)/n*100
results_PercentageFactorSignificant <- data.frame(Factor=all_results_ben[[1]]$Factor,all_results_ben_Sign_compiled)
results_PercentageFactorSignificant

# modA <- lm(Ascore~ scale(TotalP) + scale(DiffP) + scale(ChickNb), data=full_dat) 
# modTotalP <- lm(TotalP ~ scale(Adev) + scale(ChickNb), data=full_dat) 
# modS <- glm(round(Sscore) ~ scale(TotalP)+ scale(Ascore) + scale(ChickNb), data=full_dat, family='poisson')

# n <- 100
   # Factor modA modP modS
# 1  TotalP  100   NA  100
# 2   DiffP  100   NA   NA
# 3 ChickNb    3  100   17
# 4    Adev   NA    3   NA
# 5       A   NA   NA  100

# n <- 100
   # Factor modA modP modS
# 1  TotalP  100   NA  100
# 2   DiffP  100   NA   NA
# 3 ChickNb    5  100   22
# 4    Adev   NA    7   NA
# 5       A   NA   NA  100




}

{## Randomized real dataset instead of generating data

{# Get real data

{### Get raw data from R_ExtractedData

{# output csv files

# source('Compilation_provisioning_DataExtraction.R')
# or :

ExtractedData_folder <- "R_ExtractedData"

MY_tblParentalCare <- read.csv(paste(ExtractedData_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblBroods <- read.csv(paste(ExtractedData_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
MY_tblDVDInfo <- read.csv(paste(ExtractedData_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(ExtractedData_folder,"R_MY_RawFeedingVisits.csv", sep="/")) # OF directly followed by IN are merged into one feeding visits ; will be used for simulation


}

{# input txt files  !!! needs updating if specific data change !!!

input_folder <- "R_input"

sys_LastSeenAlive <- read.table(file= paste(input_folder,"sys_LastSeenAlive_20160503.txt", sep="/"), sep='\t', header=T)	## !!! to update when new pedigree !!! (and other corrections potentially)
sys_LastSeenAlive$LastYearAlive <- substr(sys_LastSeenAlive$LastLiveRecord, 7,10)

pedigree <-  read.table(file= paste(input_folder,"Pedigree_20160309.txt", sep="/"), sep='\t', header=T)  ## !!! to update when new pedigree !!! 

FedBroods <-  read.table(file= paste(input_folder,"FedBroods.txt", sep="/"), sep='\t', header=T)  ## from Ian Cleasby 20160531

tblChicks <-  read.table(file= paste(input_folder,"R_tblChicks.txt", sep="/"), sep='\t', header=T)  ## to update if consider new year of data

}


}

{### select valid video files for studying behavioural compatibility in chick provisioning

list_non_valid_DVDRef <- 
c(
MY_tblParentalCare$DVDRef[!(MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 10 files with no visits at all + 2 files with no feeding visits at all
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)],# 6 - where 0 chicks
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$ChickAge >5 & MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef) ],# 171 - where still brooding (age <=5) and with chicks and with feeding visit
MY_tblParentalCare$DVDRef[(MY_tblParentalCare$MVisit1 ==0 | MY_tblParentalCare$FVisit1 ==0 )& MY_tblDVDInfo$DVDInfoChickNb > 0 & MY_tblDVDInfo$ChickAge >5  & (MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 153 - one sex did not visit for feeding despite having chicks above age 5
MY_tblDVDInfo$DVDRef[ !MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef],# 2 DVD where both parents unidentified
MY_tblDVDInfo$DVDRef[MY_tblDVDInfo$BroodRef %in% unlist(FedBroods)] # 106 extra files for 48 broods (the 49th: 980 already excluded as only female visited) fed by Ian 
)


length(unique(list_non_valid_DVDRef)) # 450 

MY_tblDVDInfo <- MY_tblDVDInfo[ ! MY_tblDVDInfo$DVDRef %in% list_non_valid_DVDRef,]
MY_tblParentalCare <- MY_tblParentalCare[ ! MY_tblParentalCare$DVDRef %in% list_non_valid_DVDRef,]
MY_RawFeedingVisits  <- MY_RawFeedingVisits[ ! MY_RawFeedingVisits$DVDRef %in% list_non_valid_DVDRef,]

MY_tblChicks <- tblChicks[tblChicks$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

MY_tblChicks_byRearingBrood <- as.data.frame(tblChicks %>% group_by(RearingBrood) %>% summarise(sd(AvgOfMass),sd(AvgOfTarsus), n(), sum(CrossFosteredYN)))
colnames(MY_tblChicks_byRearingBrood) <- c("RearingBrood","sdMass", "sdTarsus", "NbChicksMeasured", "NbChicksMeasuredCrossFostered")
MY_tblChicks_byRearingBrood$MixedBroodYN <- MY_tblChicks_byRearingBrood$NbChicksMeasured != MY_tblChicks_byRearingBrood$NbChicksMeasuredCrossFostered
head(MY_tblChicks_byRearingBrood)

MY_tblChicks_byRearingBrood <- MY_tblChicks_byRearingBrood[MY_tblChicks_byRearingBrood$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

MY_tblParentalCare <- dplyr::rename(MY_tblParentalCare,VisitRateDifference= DiffVisit1Rate)
MY_tblParentalCare <- dplyr::rename(MY_tblParentalCare, TotalProRate = MFVisit1RateH)

{# fill in manually the data where Julia deleted it 
# unfortunately this list is not exhaustive and migth even be arguable 
# they were deleted from the dataset by Julia because the genetic parents did not match the social parents
#  but the social parents most likely did raise that one chick ! and since I am looking at social fitness...

MY_tblBroods[MY_tblBroods$BroodRef == 1152,] 
MY_tblBroods$HatchingDate <- as.character(MY_tblBroods$HatchingDate)
MY_tblBroods$HatchingDate[MY_tblBroods$BroodRef == 1152] <- "2010-05-18" # couldn't add it because it is a new factor level...
MY_tblBroods$BreedingYear[MY_tblBroods$BroodRef == 1152] <- 2010
MY_tblBroods$HatchingDayAfter0401[MY_tblBroods$BroodRef == 1152] <- 47
MY_tblBroods$NbHatched[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$Nb3[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$NbRinged[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$DadAge[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$MumAge[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$ParentsAge[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$PairIDYear <- as.character(MY_tblBroods$PairIDYear )
MY_tblBroods$PairIDYear[MY_tblBroods$BroodRef == 1152] <- "4573475420010" # couldn't add it because it is a new factor level...
MY_tblBroods$PairIDYear <- as.factor(MY_tblBroods$PairIDYear)
MY_tblBroods$AvgMass[MY_tblBroods$BroodRef == 1152] <- 23.3
MY_tblBroods$MinMass[MY_tblBroods$BroodRef == 1152] <- 23.3
MY_tblBroods$AvgTarsus[MY_tblBroods$BroodRef == 1152] <- 17.3

MY_tblBroods[MY_tblBroods$BroodRef == 457,] 
MY_tblBroods$NbRinged[MY_tblBroods$BroodRef == 457] <- 1

MY_tblBroods[MY_tblBroods$BroodRef==969,] # could have 2 hatchling and 1 ringed - not sure
MY_tblBroods$NbRinged[MY_tblBroods$BroodRef == 969] <- 1
MY_tblBroods$NbHatched[MY_tblBroods$BroodRef == 969] <- 2




}

}

{### Descriptive statistics

{### sample sizes
nrow(MY_tblParentalCare) # 1662 DVD files ; = length(unique(MY_RawFeedingVisits$DVDRef)) = nrow(MY_tblDVDInfo) 
length(unique(MY_tblDVDInfo$BroodRef)) # 910 broods videotaped at least once
range(table(MY_tblDVDInfo$BroodRef)) # range from 1 to 3
mean(table(MY_tblDVDInfo$BroodRef)) # on average 1.8 videos per brood watched


}

{# the typical sparrow

cor.test(MY_tblBroods$ParentsAge,MY_tblBroods$PairBroodNb) # cor = 0.54, p < 0.0001 
summary(MY_tblBroods$MBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])
summary(MY_tblBroods$FBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])
summary(MY_tblBroods$PairBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])

summary(MY_tblBroods$ParentsAge[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])


Mums <- MY_tblBroods %>% group_by(SocialMumID)%>% summarise(n_distinct(SocialDadID))
Dads <- MY_tblBroods %>% group_by(SocialDadID)%>% summarise(n_distinct(SocialMumID))
summary(Mums[!is.na(Mums$SocialMumID),2])
summary(Dads[!is.na(Dads$SocialDadID),2])
table(unlist(Mums[!is.na(Mums$SocialMumID),2]))
table(unlist(Dads[!is.na(Dads$SocialDadID),2]))


# female divorce more than male, in majority for the Exes ??? polyandrous females ?
summary(MY_tblBroods$FwillDivorce)
summary(MY_tblBroods$FwillDivorceforEx)
summary(MY_tblBroods$MwillDivorce)
summary(MY_tblBroods$MwillDivorceforEx)


}

{# first interval after setting up camera
outTsartMin <- do.call(rbind, by(MY_RawFeedingVisits, MY_RawFeedingVisits$DVDRef, function(x) x[which.min(x$TstartFeedVisit), c('DVDRef','TstartFeedVisit')] ))
summary(outTsartMin$TstartFeedVisit)

t.test(MY_RawFeedingVisits$Interval,outTsartMin$TstartFeedVisit)
}

}

head(MY_tblBroods) # even those where one parent unknown, needed divorce question
head(MY_tblDVDInfo) 
head(MY_tblParentalCare)
head(MY_RawFeedingVisits) # even those where one parent unknown, needed for simulation
head(MY_tblChicks)
head(MY_tblChicks_byRearingBrood)

RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		

}

head(RawInterfeeds,40)

Randomize_real_data_re_randomize_them_and_analyse <-function(){

{### ONE Randomization Within nest watch, within individual: to get SimData as the new observed data

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData <- do.call(rbind,lapply(split(RawInterfeeds, RawInterfeeds$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

}

NreplicatesWithinFileRandomization <- 10

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData2 <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData2) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A2 <- do.call(rbind,lapply(X=split(SimData2,SimData2$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S2 <- do.call(rbind,lapply(X=split(SimData2,SimData2$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A2, SimData_S2)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S_fun(SimData),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

out_Asim_within_df$SimMean <- rowMeans(out_Asim_within_df[,2:(NreplicatesWithinFileRandomization)])
out_Ssim_within_df$SimMean <- rowMeans(out_Ssim_within_df[,2:(NreplicatesWithinFileRandomization)])

}

out <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), A=SimData_A,MeanAsim=out_Asim_within_df$SimMean,S=SimData_S,MeanSsim=out_Ssim_within_df$SimMean)
head(out)

{### create MY_TABLE_perDVD: select those where both parents known + add expected alternation from simulation
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

{# merge tables

{# add MaxA

MY_tblParentalCare$MaxA <- NA

for (i in 1:nrow(MY_tblParentalCare))
{
if((MY_tblParentalCare$FVisit1[i] - MY_tblParentalCare$MVisit1[i])==0)
{
MY_tblParentalCare$MaxA[i] <- min(MY_tblParentalCare$FVisit1[i],MY_tblParentalCare$MVisit1[i])*2-1}

else{
MY_tblParentalCare$MaxA[i] <- min(MY_tblParentalCare$FVisit1[i],MY_tblParentalCare$MVisit1[i])*2 
}

}
}

MY_tblParentalCare$TotalP <- MY_tblParentalCare$FVisit1+ MY_tblParentalCare$MVisit1
MY_TABLE_perDVD <- merge(MY_tblParentalCare[,c("DVDRef", "MaxA","TotalP","VisitRateDifference")],out, by="DVDRef")
MY_TABLE_perDVD <- merge(MY_TABLE_perDVD,
MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","DVDInfoAge", "ChickAgeCat","RelTimeHrs")], by="DVDRef")

names(MY_TABLE_perDVD)[names(MY_TABLE_perDVD) == 'VisitRateDifference'] <- 'DiffP'
names(MY_TABLE_perDVD)[names(MY_TABLE_perDVD) == 'DVDInfoChickNb'] <- 'ChickNb'

MY_TABLE_perDVD <- merge(MY_TABLE_perDVD, 
MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401",
"SocialMumID","SocialDadID","DadAge","MumAge","ParentsAge",
"MBroodNb","MPriorResidence","MnextNBsame", "MwillDivorce",
"FBroodNb","FPriorResidence","FnextNBsame","FwillDivorce",
"PairID","PairBroodNb","PairIDYear", "NbRinged","AvgMass", "MinMass", "AvgTarsus")], by= "BroodRef")

MY_TABLE_perDVD <- merge(MY_TABLE_perDVD, 
MY_tblChicks_byRearingBrood[,c("RearingBrood", "sdMass", "sdTarsus", "MixedBroodYN")], 
by.x="BroodRef", by.y="RearingBrood", all.x=TRUE)


MY_TABLE_perDVD$BroodRef <- as.factor(MY_TABLE_perDVD$BroodRef)
MY_TABLE_perDVD$SocialDadID <- as.factor(MY_TABLE_perDVD$SocialDadID)
MY_TABLE_perDVD$SocialMumID <- as.factor(MY_TABLE_perDVD$SocialMumID)
MY_TABLE_perDVD$PairID <- as.factor(MY_TABLE_perDVD$PairID)
MY_TABLE_perDVD$BreedingYear <- as.factor(MY_TABLE_perDVD$BreedingYear)
}

head(MY_TABLE_perDVD)

{# remove DVD where one social parent unknown
length(unique(MY_TABLE_perDVD$BroodRef[is.na(MY_TABLE_perDVD$SocialMum) | is.na(MY_TABLE_perDVD$SocialDadID)])) # 38 broods - 63 files one parent unknown
MY_TABLE_perDVD <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$SocialMumID) & !is.na(MY_TABLE_perDVD$SocialDadID),] # where both parents known
}

nrow(MY_TABLE_perDVD) # 1599 files

{# add Adev + MedAsim !
median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}

MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$A - MY_TABLE_perDVD$MeanAsim 

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MedAsim = apply(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)


}

{# add MeanSsim and Sdev

MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$S - MY_TABLE_perDVD$MeanSsim

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MedSsim = apply(tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)
}


}

head(MY_TABLE_perDVD)

{# create a repeated table per DVD, with the column NbAlternation and NbS having observed values in first half, and sim values in second half
MY_TABLE_perDVD_Sim_long <- rbind(MY_TABLE_perDVD,MY_TABLE_perDVD)

MY_TABLE_perDVD_Sim_long$Type <- c(rep("z_Obsv", nrow(MY_TABLE_perDVD)),rep("a_Sim", nrow(MY_TABLE_perDVD)))

MY_TABLE_perDVD_Sim_long$A[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_Sim_long$MedAsim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim']
MY_TABLE_perDVD_Sim_long$S[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_Sim_long$MedSsim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim']

MY_TABLE_perDVD_Sim_long$rowID <- seq(1:nrow(MY_TABLE_perDVD_Sim_long))
}

head(MY_TABLE_perDVD_Sim_long)


{# analyse 

modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + (1|DVDRef), data=MY_TABLE_perDVD_Sim_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modS <- glmer(S~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + (1|DVDRef), data=MY_TABLE_perDVD_Sim_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
#modA_off <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + offset(log(MaxA)) + (1|DVDRef), data=MY_TABLE_perDVD_Sim_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa"))
#modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb), data=MY_TABLE_perDVD_Sim_long, family = 'binomial') 
#modTotalP <- glmer(TotalP ~ scale(A)*Type + scale(ChickNb) + (1|DVDRef), family = 'poisson', data=MY_TABLE_perDVD_Sim_long,control=glmerControl(optimizer = "bobyqa")) 
modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + scale(ChickNb) + (1|DVDRef) , family = 'poisson', data=MY_TABLE_perDVD_Sim_long,control=glmerControl(optimizer = "bobyqa")) 

}

{# results
results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*ChickNb', 'Type*A'), modA = rep(NA,4),modS = rep(NA,4),modA_off= rep(NA,4), modAoutofAmax = rep(NA,4),modP=rep(NA,4), modP_AMax= rep(NA,4))
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[6,4],4)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[7,4],4)
results$modA[results$Factor=='Type*ChickNb'] <- round(summary(modA)$coef[8,4],4)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[6,4],4)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[7,4],4)
results$modS[results$Factor=='Type*ChickNb'] <- round(summary(modS)$coef[8,4],4)
# results$modA_off[results$Factor=='Type*TotalP'] <- round(summary(modA_off)$coef[6,4],4)
# results$modA_off[results$Factor=='Type*DiffP'] <- round(summary(modA_off)$coef[7,4],4)
# results$modA_off[results$Factor=='Type*ChickNb'] <- round(summary(modA_off)$coef[8,4],4)
# results$modAoutofAmax[results$Factor=='Type*TotalP'] <- round(summary(modAoutofAmax)$coef[6,4],4)
# results$modAoutofAmax[results$Factor=='Type*DiffP'] <- round(summary(modAoutofAmax)$coef[7,4],4)
# results$modAoutofAmax[results$Factor=='Type*ChickNb'] <- round(summary(modAoutofAmax)$coef[8,4],4)
# results$modP[results$Factor=='Type*A'] <- round(summary(modTotalP)$coef[5,4],4)
results$modP_AMax[results$Factor=='Type*A'] <- round(summary(modTotalP_AMax)$coef[5,4],4)

}

return(list(results))

}



n <-100

all_results_realrandom <- pbreplicate(n,Randomize_real_data_re_randomize_them_and_analyse())
all_results_Sign_realrandom <- lapply(all_results_realrandom, function(x){x[,-1] <0.05})
all_results_Sign_compiled_realrandom <- Reduce('+',all_results_Sign_realrandom)/n*100
results_PercentageFactorSignificant_realrandom <- data.frame(Factor=all_results_realrandom[[1]]$Factor,all_results_Sign_compiled_realrandom)
results_PercentageFactorSignificant_realrandom

# n <- 100
        # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP    3        7            86   NA        NA
# 2   Type*DiffP    0        0            47   NA        NA
# 3 Type*ChickNb    0        0             1   NA        NA
# 4       Type*A   NA       NA            NA   39         0

# n <- 100
        # Factor modA modS modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP    0   98       NA            NA   NA        NA
# 2   Type*DiffP    0    0       NA            NA   NA        NA
# 3 Type*ChickNb    0   NA       NA            NA   NA        NA
# 4       Type*A   NA   NA       NA            NA   NA         0

# n <- 100
        # Factor modA modS modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP    1  100       NA            NA   NA        NA
# 2   Type*DiffP    0    1       NA            NA   NA        NA
# 3 Type*ChickNb    0    3       NA            NA   NA        NA
# 4       Type*A   NA   NA       NA            NA   NA         0

}

{## Randomized real dataset instead of generating data and run real test on A or Adev

{# Get real data

{### Get raw data from R_ExtractedData

{# output csv files

# source('Compilation_provisioning_DataExtraction.R')
# or :

ExtractedData_folder <- "R_ExtractedData"

MY_tblParentalCare <- read.csv(paste(ExtractedData_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblBroods <- read.csv(paste(ExtractedData_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
MY_tblDVDInfo <- read.csv(paste(ExtractedData_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(ExtractedData_folder,"R_MY_RawFeedingVisits.csv", sep="/")) # OF directly followed by IN are merged into one feeding visits ; will be used for simulation


}

{# input txt files  !!! needs updating if specific data change !!!

input_folder <- "R_input"

sys_LastSeenAlive <- read.table(file= paste(input_folder,"sys_LastSeenAlive_20160503.txt", sep="/"), sep='\t', header=T)	## !!! to update when new pedigree !!! (and other corrections potentially)
sys_LastSeenAlive$LastYearAlive <- substr(sys_LastSeenAlive$LastLiveRecord, 7,10)

pedigree <-  read.table(file= paste(input_folder,"Pedigree_20160309.txt", sep="/"), sep='\t', header=T)  ## !!! to update when new pedigree !!! 

FedBroods <-  read.table(file= paste(input_folder,"FedBroods.txt", sep="/"), sep='\t', header=T)  ## from Ian Cleasby 20160531

tblChicks <-  read.table(file= paste(input_folder,"R_tblChicks.txt", sep="/"), sep='\t', header=T)  ## to update if consider new year of data

}


}

{### select valid video files for studying behavioural compatibility in chick provisioning

list_non_valid_DVDRef <- 
c(
MY_tblParentalCare$DVDRef[!(MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 10 files with no visits at all + 2 files with no feeding visits at all
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)],# 6 - where 0 chicks
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$ChickAge >5 & MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef) ],# 171 - where still brooding (age <=5) and with chicks and with feeding visit
MY_tblParentalCare$DVDRef[(MY_tblParentalCare$MVisit1 ==0 | MY_tblParentalCare$FVisit1 ==0 )& MY_tblDVDInfo$DVDInfoChickNb > 0 & MY_tblDVDInfo$ChickAge >5  & (MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 153 - one sex did not visit for feeding despite having chicks above age 5
MY_tblDVDInfo$DVDRef[ !MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef],# 2 DVD where both parents unidentified
MY_tblDVDInfo$DVDRef[MY_tblDVDInfo$BroodRef %in% unlist(FedBroods)] # 106 extra files for 48 broods (the 49th: 980 already excluded as only female visited) fed by Ian 
)


length(unique(list_non_valid_DVDRef)) # 450 

MY_tblDVDInfo <- MY_tblDVDInfo[ ! MY_tblDVDInfo$DVDRef %in% list_non_valid_DVDRef,]
MY_tblParentalCare <- MY_tblParentalCare[ ! MY_tblParentalCare$DVDRef %in% list_non_valid_DVDRef,]
MY_RawFeedingVisits  <- MY_RawFeedingVisits[ ! MY_RawFeedingVisits$DVDRef %in% list_non_valid_DVDRef,]

MY_tblChicks <- tblChicks[tblChicks$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

MY_tblChicks_byRearingBrood <- as.data.frame(tblChicks %>% group_by(RearingBrood) %>% summarise(sd(AvgOfMass),sd(AvgOfTarsus), n(), sum(CrossFosteredYN)))
colnames(MY_tblChicks_byRearingBrood) <- c("RearingBrood","sdMass", "sdTarsus", "NbChicksMeasured", "NbChicksMeasuredCrossFostered")
MY_tblChicks_byRearingBrood$MixedBroodYN <- MY_tblChicks_byRearingBrood$NbChicksMeasured != MY_tblChicks_byRearingBrood$NbChicksMeasuredCrossFostered
head(MY_tblChicks_byRearingBrood)

MY_tblChicks_byRearingBrood <- MY_tblChicks_byRearingBrood[MY_tblChicks_byRearingBrood$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

MY_tblParentalCare <- dplyr::rename(MY_tblParentalCare,VisitRateDifference= DiffVisit1Rate)
MY_tblParentalCare <- dplyr::rename(MY_tblParentalCare, TotalProRate = MFVisit1RateH)

{# fill in manually the data where Julia deleted it 
# unfortunately this list is not exhaustive and migth even be arguable 
# they were deleted from the dataset by Julia because the genetic parents did not match the social parents
#  but the social parents most likely did raise that one chick ! and since I am looking at social fitness...

MY_tblBroods[MY_tblBroods$BroodRef == 1152,] 
MY_tblBroods$HatchingDate <- as.character(MY_tblBroods$HatchingDate)
MY_tblBroods$HatchingDate[MY_tblBroods$BroodRef == 1152] <- "2010-05-18" # couldn't add it because it is a new factor level...
MY_tblBroods$BreedingYear[MY_tblBroods$BroodRef == 1152] <- 2010
MY_tblBroods$HatchingDayAfter0401[MY_tblBroods$BroodRef == 1152] <- 47
MY_tblBroods$NbHatched[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$Nb3[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$NbRinged[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$DadAge[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$MumAge[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$ParentsAge[MY_tblBroods$BroodRef == 1152] <- 1
MY_tblBroods$PairIDYear <- as.character(MY_tblBroods$PairIDYear )
MY_tblBroods$PairIDYear[MY_tblBroods$BroodRef == 1152] <- "4573475420010" # couldn't add it because it is a new factor level...
MY_tblBroods$PairIDYear <- as.factor(MY_tblBroods$PairIDYear)
MY_tblBroods$AvgMass[MY_tblBroods$BroodRef == 1152] <- 23.3
MY_tblBroods$MinMass[MY_tblBroods$BroodRef == 1152] <- 23.3
MY_tblBroods$AvgTarsus[MY_tblBroods$BroodRef == 1152] <- 17.3

MY_tblBroods[MY_tblBroods$BroodRef == 457,] 
MY_tblBroods$NbRinged[MY_tblBroods$BroodRef == 457] <- 1

MY_tblBroods[MY_tblBroods$BroodRef==969,] # could have 2 hatchling and 1 ringed - not sure
MY_tblBroods$NbRinged[MY_tblBroods$BroodRef == 969] <- 1
MY_tblBroods$NbHatched[MY_tblBroods$BroodRef == 969] <- 2




}

}

{### Descriptive statistics

{### sample sizes
nrow(MY_tblParentalCare) # 1662 DVD files ; = length(unique(MY_RawFeedingVisits$DVDRef)) = nrow(MY_tblDVDInfo) 
length(unique(MY_tblDVDInfo$BroodRef)) # 910 broods videotaped at least once
range(table(MY_tblDVDInfo$BroodRef)) # range from 1 to 3
mean(table(MY_tblDVDInfo$BroodRef)) # on average 1.8 videos per brood watched


}

{# the typical sparrow

cor.test(MY_tblBroods$ParentsAge,MY_tblBroods$PairBroodNb) # cor = 0.54, p < 0.0001 
summary(MY_tblBroods$MBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])
summary(MY_tblBroods$FBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])
summary(MY_tblBroods$PairBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])

summary(MY_tblBroods$ParentsAge[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])


Mums <- MY_tblBroods %>% group_by(SocialMumID)%>% summarise(n_distinct(SocialDadID))
Dads <- MY_tblBroods %>% group_by(SocialDadID)%>% summarise(n_distinct(SocialMumID))
summary(Mums[!is.na(Mums$SocialMumID),2])
summary(Dads[!is.na(Dads$SocialDadID),2])
table(unlist(Mums[!is.na(Mums$SocialMumID),2]))
table(unlist(Dads[!is.na(Dads$SocialDadID),2]))


# female divorce more than male, in majority for the Exes ??? polyandrous females ?
summary(MY_tblBroods$FwillDivorce)
summary(MY_tblBroods$FwillDivorceforEx)
summary(MY_tblBroods$MwillDivorce)
summary(MY_tblBroods$MwillDivorceforEx)


}

{# first interval after setting up camera
outTsartMin <- do.call(rbind, by(MY_RawFeedingVisits, MY_RawFeedingVisits$DVDRef, function(x) x[which.min(x$TstartFeedVisit), c('DVDRef','TstartFeedVisit')] ))
summary(outTsartMin$TstartFeedVisit)

t.test(MY_RawFeedingVisits$Interval,outTsartMin$TstartFeedVisit)
}

}

head(MY_tblBroods) # even those where one parent unknown, needed divorce question
head(MY_tblDVDInfo) 
head(MY_tblParentalCare)
head(MY_RawFeedingVisits) # even those where one parent unknown, needed for simulation
head(MY_tblChicks)
head(MY_tblChicks_byRearingBrood)

RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		

}

head(RawInterfeeds,40)

Randomize_real_data_re_randomize_them_and_analyse_for_real <-function(AbsDev){

{### ONE Randomization Within nest watch, within individual: to get SimData as the new observed data

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData <- do.call(rbind,lapply(split(RawInterfeeds, RawInterfeeds$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

}

NreplicatesWithinFileRandomization <- 10

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData2 <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData2) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A2 <- do.call(rbind,lapply(X=split(SimData2,SimData2$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S2 <- do.call(rbind,lapply(X=split(SimData2,SimData2$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A2, SimData_S2)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S_fun(SimData),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

out_Asim_within_df$SimMean <- rowMeans(out_Asim_within_df[,2:(NreplicatesWithinFileRandomization)])
out_Ssim_within_df$SimMean <- rowMeans(out_Ssim_within_df[,2:(NreplicatesWithinFileRandomization)])

}

out <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), A=SimData_A,MeanAsim=out_Asim_within_df$SimMean,S=SimData_S,MeanSsim=out_Ssim_within_df$SimMean)
head(out)

{### create MY_TABLE_perDVD: select those where both parents known + add expected alternation from simulation
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

{# merge tables

{# add MaxA

MY_tblParentalCare$MaxA <- NA

for (i in 1:nrow(MY_tblParentalCare))
{
if((MY_tblParentalCare$FVisit1[i] - MY_tblParentalCare$MVisit1[i])==0)
{
MY_tblParentalCare$MaxA[i] <- min(MY_tblParentalCare$FVisit1[i],MY_tblParentalCare$MVisit1[i])*2-1}

else{
MY_tblParentalCare$MaxA[i] <- min(MY_tblParentalCare$FVisit1[i],MY_tblParentalCare$MVisit1[i])*2 
}

}
}

MY_tblParentalCare$TotalP <- MY_tblParentalCare$FVisit1+ MY_tblParentalCare$MVisit1
MY_TABLE_perDVD <- merge(MY_tblParentalCare[,c("DVDRef", "MaxA","TotalP","VisitRateDifference")],out, by="DVDRef")
MY_TABLE_perDVD <- merge(MY_TABLE_perDVD,
MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","DVDInfoAge", "ChickAgeCat","RelTimeHrs")], by="DVDRef")

names(MY_TABLE_perDVD)[names(MY_TABLE_perDVD) == 'VisitRateDifference'] <- 'DiffP'
names(MY_TABLE_perDVD)[names(MY_TABLE_perDVD) == 'DVDInfoChickNb'] <- 'ChickNb'

MY_TABLE_perDVD <- merge(MY_TABLE_perDVD, 
MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401",
"SocialMumID","SocialDadID","DadAge","MumAge","ParentsAge",
"MBroodNb","MPriorResidence","MnextNBsame", "MwillDivorce",
"FBroodNb","FPriorResidence","FnextNBsame","FwillDivorce",
"PairID","PairBroodNb","PairIDYear", "NbRinged","AvgMass", "MinMass", "AvgTarsus")], by= "BroodRef")

MY_TABLE_perDVD <- merge(MY_TABLE_perDVD, 
MY_tblChicks_byRearingBrood[,c("RearingBrood", "sdMass", "sdTarsus", "MixedBroodYN")], 
by.x="BroodRef", by.y="RearingBrood", all.x=TRUE)


MY_TABLE_perDVD$BroodRef <- as.factor(MY_TABLE_perDVD$BroodRef)
MY_TABLE_perDVD$SocialDadID <- as.factor(MY_TABLE_perDVD$SocialDadID)
MY_TABLE_perDVD$SocialMumID <- as.factor(MY_TABLE_perDVD$SocialMumID)
MY_TABLE_perDVD$PairID <- as.factor(MY_TABLE_perDVD$PairID)
MY_TABLE_perDVD$BreedingYear <- as.factor(MY_TABLE_perDVD$BreedingYear)
}

head(MY_TABLE_perDVD)

{# remove DVD where one social parent unknown
length(unique(MY_TABLE_perDVD$BroodRef[is.na(MY_TABLE_perDVD$SocialMum) | is.na(MY_TABLE_perDVD$SocialDadID)])) # 38 broods - 63 files one parent unknown
MY_TABLE_perDVD <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$SocialMumID) & !is.na(MY_TABLE_perDVD$SocialDadID),] # where both parents known
}

nrow(MY_TABLE_perDVD) # 1599 files

{# add Adev + MedAsim !
median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}

MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$A - MY_TABLE_perDVD$MeanAsim 

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MedAsim = apply(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)


}

{# add MeanSsim and Sdev

MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$S - MY_TABLE_perDVD$MeanSsim

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MedSsim = apply(tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)
}


}

head(MY_TABLE_perDVD)

{# create a repeated table per DVD, with the column NbAlternation and NbS having observed values in first half, and sim values in second half
MY_TABLE_perDVD_Sim_long <- rbind(MY_TABLE_perDVD,MY_TABLE_perDVD)

MY_TABLE_perDVD_Sim_long$Type <- c(rep("z_Obsv", nrow(MY_TABLE_perDVD)),rep("a_Sim", nrow(MY_TABLE_perDVD)))

MY_TABLE_perDVD_Sim_long$A[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_Sim_long$MedAsim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim']
MY_TABLE_perDVD_Sim_long$S[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_Sim_long$MedSsim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim']

MY_TABLE_perDVD_Sim_long$rowID <- seq(1:nrow(MY_TABLE_perDVD_Sim_long))
}

head(MY_TABLE_perDVD_Sim_long)

{# analyse & results

if (AbsDev == 'Abs'){

{modA <- glmer(A ~  
	
	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
    Type*FPriorResidence +
	
	Type*scale(ChickNb) + 
	Type*scale(TotalP) +
	Type*scale(DiffP)+
		
	(1|BroodRef) + 	
	(1|SocialMumID)+ (1|SocialDadID) + 
	(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	#(1|PairIDYear)
	+ (1|DVDRef) 
	#+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)
}
	
{modS <- glmer(S ~ 
	
	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
    Type*FPriorResidence +
	
	Type*scale(ChickNb) +
	Type*scale(TotalP) +
	Type*scale(DiffP)+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	#(1|PairIDYear)
	+ (1|DVDRef) 
	#+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)	
}

results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*ChickNb', 'Type*A'), modA = rep(NA,4),modS = rep(NA,4))
results$modA[results$Factor=='Type*ChickNb'] <- round(summary(modA)$coef[20,4],4)
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[21,4],4)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[22,4],4)
results$modS[results$Factor=='Type*ChickNb'] <- round(summary(modS)$coef[20,4],4)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[21,4],4)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[22,4],4)

}

if (AbsDev == 'Dev') {

modAdev <- lmer( Adev ~ scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
						scale(HatchingDayAfter0401) +
						scale(PairBroodNb) + 
						ChickAgeCat +
						scale(RelTimeHrs) + 
						MPriorResidence +
						FPriorResidence +
						
						scale(TotalP)+
						scale(DiffP)+
						scale(ChickNb) + 	
						
						(1|BroodRef) + 
						(1|SocialMumID)+ (1|SocialDadID) + 
						#(1|PairID) +  # explained 0% of the variance
						(1|BreedingYear)  
						# + (1|PairIDYear) # explained 0% of the variance
						, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])
	
modSdev <- lmer( Sdev ~ scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
						scale(HatchingDayAfter0401) +
						scale(PairBroodNb) + 
						ChickAgeCat +
						scale(RelTimeHrs) + 
						MPriorResidence +
						FPriorResidence +
												 
						scale(TotalP)+
						scale(DiffP)+
						scale(ChickNb) +
						
						(1|BroodRef) + 
						(1|SocialMumID)+ (1|SocialDadID) + 
						#(1|PairID) +  # explained 0% of the variance
						(1|BreedingYear)  
						# + (1|PairIDYear) # explained 0% of the variance
						, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])
						
results <- data.frame(Factor = c('TotalP', 'DiffP', 'ChickNb'), modAdev = rep(NA,3),modSdev = rep(NA,3))

results$modAdev[results$Factor=='TotalP'] <- summary(modAdev)$coef[9,3]
results$modAdev[results$Factor=='DiffP'] <- summary(modAdev)$coef[10,3]
results$modAdev[results$Factor=='ChickNb'] <- summary(modAdev)$coef[11,3]

results$modSdev[results$Factor=='TotalP'] <- summary(modSdev)$coef[9,3]
results$modSdev[results$Factor=='DiffP'] <- summary(modSdev)$coef[10,3]
results$modSdev[results$Factor=='ChickNb'] <- summary(modSdev)$coef[11,3]


}

}



return(list(results))

}



n <-20

all_results_realrandom_realtest <- pbreplicate(n,Randomize_real_data_re_randomize_them_and_analyse_for_real('Abs'))
all_results_Sign_realrandom_realtest <- lapply(all_results_realrandom_realtest, function(x){x[,-1] <0.05})
all_results_Sign_compiled_realrandom_realtest <- Reduce('+',all_results_Sign_realrandom_realtest)/n*100
results_PercentageFactorSignificant_realrandom_realtest <- data.frame(Factor=all_results_realrandom_realtest[[1]]$Factor,all_results_Sign_compiled_realrandom_realtest)
results_PercentageFactorSignificant_realrandom_realtest

# n <-20
        # Factor modA modS 
# 1  Type*TotalP    5  100  
# 2   Type*DiffP    0    5  
# 3 Type*ChickNb    0    5  
# 4       Type*A   NA   NA  


n <-100

all_results_realrandom_realtest_dev <- pbreplicate(n,Randomize_real_data_re_randomize_them_and_analyse_for_real('Dev'))
all_results_Sign_realrandom_realtest_dev <- lapply(all_results_realrandom_realtest_dev, function(x){abs(x[,-1]) >1.96})
all_results_Sign_compiled_realrandom_realtest_dev <- Reduce('+',all_results_Sign_realrandom_realtest_dev)/n*100
results_PercentageFactorSignificant_realrandom_realtest_dev <- data.frame(Factor=all_results_realrandom_realtest_dev[[1]]$Factor,all_results_Sign_compiled_realrandom_realtest_dev)
results_PercentageFactorSignificant_realrandom_realtest_dev


# n <-100
   # Factor modAdev modSdev
# 1  TotalP      17      16
# 2   DiffP      11      10
# 3 ChickNb       5       3


}

{## to test the link between Adev and TP


Generate_data_randomize_them_and_analyse_Adev <-function(){

{# generate data

avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))
VideoLength <- 90

# Joel suggested to pass on the log scale to be able to add Poisson distributed error, I assume (??)
meanlog <- log(avPR)
sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))

full_dat <- list()

for (i in 1:2000){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog ) # expected number of visits
FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
MaleP <- rpois(1, MalePexp) # realized number of visits including the poisson distributed error
FemaleP <- rpois(1, FemalePexp)
TotalP <- MaleP + FemaleP # total number of visits for that video
DiffP <- abs(MaleP - FemaleP)

MaleVisits <- sort(runif(MaleP,0,VideoLength))
FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
MaleIntervals <- c(diff(MaleVisits),90-MaleVisits[length(MaleVisits)])
FemaleIntervals <- c(diff(FemaleVisits),90-FemaleVisits[length(FemaleVisits)])

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- i
if (TotalP - DiffP >1) {full_dat[[i]] <- dat} # for my analyses I selected videos where both partners visited at least once

}

full_dat <- do.call(rbind,full_dat)

}

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

SimData <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(10,Randomize_Data_WithinFile_and_Calculate_A_S_fun(full_dat),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

NreplicatesWithinFileRandomization <-10
out_Asim_within_df$SimMean <- rowMeans(out_Asim_within_df[,2:(NreplicatesWithinFileRandomization)])
out_Ssim_within_df$SimMean <- rowMeans(out_Ssim_within_df[,2:(NreplicatesWithinFileRandomization)])
SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], MeanAsim = out_Asim_within_df$SimMean, MeanSsim= out_Ssim_within_df$SimMean)

}

{# create table

sumarize_one_DVD <- function(x){

MaleP <-nrow(x[x$Sex==1,])
FemaleP <-nrow(x[x$Sex==0,])
TotalP <- MaleP+FemaleP
DiffP <- abs(MaleP-FemaleP)

A <- sum(diff(x$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
S <- sum(diff(x$Sex)!=0 & diff(x$Visits) <= 0.5)

summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),TotalP,DiffP,MaxA,A,S))

summary_DVD

}

summary_full_dat <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))
summary_full_dat <- merge(summary_full_dat,SimOut, by='DVDRef' )

summary_full_dat$Adev <- summary_full_dat$A - summary_full_dat$MeanAsim 
summary_full_dat$Sdev <- summary_full_dat$S - summary_full_dat$MeanSsim 
head(summary_full_dat)

}

{# analyse 

modAdev <- lm(Adev~ scale(TotalP) + scale(DiffP) ,data=summary_full_dat) 
modSdev <- lm(Sdev~ scale(TotalP) + scale(DiffP) ,data=summary_full_dat) 

}

{# results
results <- data.frame(Factor = c('TotalP', 'DiffP'), modAdev = rep(NA,2),modSdev = rep(NA,2))
results$modAdev[results$Factor=='TotalP'] <- round(summary(modAdev)$coef[2,4],4)
results$modAdev[results$Factor=='DiffP'] <- round(summary(modAdev)$coef[3,4],4)
results$modSdev[results$Factor=='TotalP'] <- round(summary(modSdev)$coef[2,4],4)
results$modSdev[results$Factor=='DiffP'] <- round(summary(modSdev)$coef[3,4],4)

}

return(list(results))

}


n <- 100
all_results_Adev <- pbreplicate(n,Generate_data_randomize_them_and_analyse_Adev())
all_results_Adev_Sign <- lapply(all_results_Adev, function(x){x[,-1] <0.05})
all_results_Adev_Sign_compiled <- Reduce('+',all_results_Adev_Sign)/n*100
results_Adev_PercentageFactorSignificant <- data.frame(Factor=all_results_Adev[[1]]$Factor,all_results_Adev_Sign_compiled)
results_Adev_PercentageFactorSignificant

# n <- 100
  # Factor modAdev modSdev
# 1 TotalP      11     100
# 2  DiffP       9     100



}































