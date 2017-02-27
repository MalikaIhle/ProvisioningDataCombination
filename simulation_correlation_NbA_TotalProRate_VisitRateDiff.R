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
SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer))
			

sumarize_one_DVD <- function(x){

MaleP <-nrow(x[x$Sex==1,])
FemaleP <-nrow(x[x$Sex==0,])
TotalP <- MaleP+FemaleP
DiffP <- abs(MaleP-FemaleP)

A <- sum(diff(x$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
#S <- sum(diff(x$Sex)!=0 & diff(x$Visits) <= 0.5)

summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),TotalP,DiffP,MaxA,A))

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

modA <- glm(A~ Type*scale(TotalP) + Type*scale(DiffP), data=fulldat_long, family = 'poisson') 
modA_off <- glm(A~ Type*scale(TotalP) + Type*scale(DiffP) + offset(log(MaxA)), data=fulldat_long, family = 'poisson')
modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP), data=fulldat_long, family = 'binomial') 
modTotalP <- glmer(TotalP ~ scale(A)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 
modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 

}

{# results
results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*A'), modA = rep(NA,3),modA_off= rep(NA,3), modAoutofAmax = rep(NA,3),modP=rep(NA,3), modP_AMax= rep(NA,3))
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[5,4],3)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[6,4],3)
results$modA_off[results$Factor=='Type*TotalP'] <- round(summary(modA_off)$coef[5,4],3)
results$modA_off[results$Factor=='Type*DiffP'] <- round(summary(modA_off)$coef[6,4],3)
results$modAoutofAmax[results$Factor=='Type*TotalP'] <- round(summary(modAoutofAmax)$coef[5,4],3)
results$modAoutofAmax[results$Factor=='Type*DiffP'] <- round(summary(modAoutofAmax)$coef[6,4],3)
results$modP[results$Factor=='Type*A'] <- round(summary(modTotalP)$coef[4,4],3)
results$modP_AMax[results$Factor=='Type*A'] <- round(summary(modTotalP_AMax)$coef[4,4],3)
}

return(list(results))

}


n <- 20
all_results_A <- pbreplicate(n,Generate_data_randomize_them_and_analyse_A())
all_results_A_Sign <- lapply(all_results_A, function(x){x[,-1] <0.05})
all_results_A_Sign_compiled <- Reduce('+',all_results_A_Sign)/n*100
results_A_PercentageFactorSignificant <- data.frame(Factor=all_results_A[[1]]$Factor,all_results_A_Sign_compiled)
results_A_PercentageFactorSignificant

# modA <- glm(A~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'poisson') # [,1] 
# modA_off <- glm(A~ Type*TotalP + Type*DiffP + offset(log(MaxA)), data=fulldat_long, family = 'poisson')
# modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'binomial') # [,2] 
# modTotalP <- glmer(TotalP ~ scale(A)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) # [,4] 
# modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) # [,3] 

# n <- 100
       # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1 Type*TotalP   13       22            90   NA        NA
# 2  Type*DiffP    0       wrong         68   NA        NA
# 3      Type*A   NA       NA            NA   73         0

# n <- 20
       # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1 Type*TotalP    0       10            80   NA        NA
# 2  Type*DiffP    0        0            45   NA        NA
# 3      Type*A   NA       NA            NA   90         0



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
return(c(unique(x$DVDRef), A))
}

summary_A_obs <- do.call(rbind,lapply(split(raw_dat, raw_dat$DVDRef),sumarize_one_DVD))
colnames(summary_A_obs) <- c('DVDRef', 'A')

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
SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer))

SimOut <- merge (x=SimOut, y= full_dat[,c('DVDRef','TotalP', 'DiffP','ChickNb', 'MaleP', 'FemaleP', 'MaxA')], by='DVDRef', all.x=TRUE) # all meta data from full_dat but the data 'A'
SimOut$Type <- 'a_Sim'
full_dat$Type <- 'z_Obsv'

fulldat_long <- rbind(full_dat,SimOut) # first half: A is observed, second half, A is from randomization
fulldat_long$rowID <- seq(1:nrow(fulldat_long))

}

{# analyse 

modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modA_off <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb) + offset(log(MaxA)) + (1|DVDRef), data=fulldat_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa"))
modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP) + Type*scale(ChickNb), data=fulldat_long, family = 'binomial') 
modTotalP <- glmer(TotalP ~ scale(A)*Type + scale(ChickNb) + (1|DVDRef), family = 'poisson', data=fulldat_long,control=glmerControl(optimizer = "bobyqa")) 
modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + scale(ChickNb) + (1|DVDRef) , family = 'poisson', data=fulldat_long,control=glmerControl(optimizer = "bobyqa")) 

}

{# results
results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*ChickNb', 'Type*A'), modA = rep(NA,4),modA_off= rep(NA,4), modAoutofAmax = rep(NA,4),modP=rep(NA,4), modP_AMax= rep(NA,4))
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[6,4],4)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[7,4],4)
results$modA[results$Factor=='Type*ChickNb'] <- round(summary(modA)$coef[8,4],4)
results$modA_off[results$Factor=='Type*TotalP'] <- round(summary(modA_off)$coef[6,4],4)
results$modA_off[results$Factor=='Type*DiffP'] <- round(summary(modA_off)$coef[7,4],4)
results$modA_off[results$Factor=='Type*ChickNb'] <- round(summary(modA_off)$coef[8,4],4)
results$modAoutofAmax[results$Factor=='Type*TotalP'] <- round(summary(modAoutofAmax)$coef[6,4],4)
results$modAoutofAmax[results$Factor=='Type*DiffP'] <- round(summary(modAoutofAmax)$coef[7,4],4)
results$modAoutofAmax[results$Factor=='Type*ChickNb'] <- round(summary(modAoutofAmax)$coef[8,4],4)
results$modP[results$Factor=='Type*A'] <- round(summary(modTotalP)$coef[5,4],4)
results$modP_AMax[results$Factor=='Type*A'] <- round(summary(modTotalP_AMax)$coef[5,4],4)

}


return(list(results))
}


n <-20

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


#n <- 20
        # Factor modA modA_off modAoutofAmax modP modP_AMax
# 1  Type*TotalP   60       80           100   NA        NA
# 2   Type*DiffP    0        0           100   NA        NA
# 3 Type*ChickNb    0        0             0   NA        NA
# 4       Type*A   NA       NA            NA   50        10

}





{## Bebbington & Hatchwell 2015 analyses on A/(TP-1)

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

modA <- lm(Ascore~ scale(TotalP) + scale(DiffP) + scale(ChickNb), data=full_dat) # [,1] 
modTotalP <- lm(TotalP ~ scale(Adev) + scale(ChickNb), data=full_dat) # [,4] 
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


# modA <- lm(Ascore~ scale(TotalP) + scale(DiffP) + scale(ChickNb), data=full_dat) # [,1] 
# modTotalP <- lm(TotalP ~ scale(Adev) + scale(ChickNb), data=full_dat) # [,4] 
# modS <- glm(round(Sscore) ~ scale(TotalP)+ scale(Ascore) + scale(ChickNb), data=full_dat, family='poisson')


   # Factor modA modP modS
# 1  TotalP  100   NA  100
# 2   DiffP  100   NA   NA
# 3 ChickNb    3  100   17
# 4    Adev   NA    3   NA
# 5       A   NA   NA  100


}









