#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Joel PICK & Malika IHLE    joel.l.pick@gmail.com   malika_ihle@hotmail.fr
#	 Simulation to help decide which analyses to perfom on provisioning data sparrows
#	 Start : 02/02/2017
#	 last modif : 07/03/2017
#	 commit: simulations from generated, half generated, or randomized datasets
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

{### Get real data

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

head(MY_tblBroods) # even those where one parent unknown, needed divorce question
head(MY_tblDVDInfo) 
head(MY_tblParentalCare)
head(MY_RawFeedingVisits) # even those where one parent unknown, needed for simulation
head(MY_tblChicks)
head(MY_tblChicks_byRearingBrood)

RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		

}


{## Generate TP from poisson process


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
MaleP5 <- rpois(5, MalePexp) # potential realized number of visits including the poisson distributed error
MaleP <- sample(MaleP5[MaleP5>0],1) # picking one potential MaleP that is not zero (excluded in observed data)
FemaleP5 <- rpois(5, FemalePexp)
FemaleP <- sample(FemaleP5[FemaleP5>0],1)
TotalP <- MaleP + FemaleP # total number of visits for that video
DiffP <- abs(MaleP - FemaleP)

MaleVisits <- sort(runif(MaleP,0,VideoLength))
FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
MaleIntervals <- c(0,diff(MaleVisits))
FemaleIntervals <- c(0,diff(FemaleVisits))

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- i
if (TotalP - DiffP >1) {full_dat[[i]] <- dat} # for my analyses I selected videos where both partners visited at least once

}

full_dat <- do.call(rbind,full_dat)

}

{# Randomization Within nest watch, within individual

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
Asim <- sum(diff(x$Sex)!=0) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
Ssim <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= 2)
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
SimOutMed <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer), S= apply(out_Ssim_within_df[,-1],1,median_integer))
SimOutMean <- 	data.frame(DVDRef = out_Asim_within_df[,1], MeanAsim = apply(out_Asim_within_df[,-1],1,mean), MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))		

sumarize_one_DVD <- function(x){

MaleP <-nrow(x[x$Sex==1,])
FemaleP <-nrow(x[x$Sex==0,])
TotalP <- MaleP+FemaleP
DiffP <- abs(MaleP-FemaleP)

A <- sum(diff(x$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= 2)

summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),TotalP,DiffP,MaxA,A,S))

summary_DVD

}

summary_all_DVD <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))

# short table
summary_full_dat <- merge(summary_all_DVD,SimOutMean, by='DVDRef' )
summary_full_dat$Adev <- summary_full_dat$A - summary_full_dat$MeanAsim
summary_full_dat$Sdev <- summary_full_dat$S - summary_full_dat$MeanSsim 
summary_full_dat$Log_Aobs_Arand <- log((summary_full_dat$A+0.00001)/(summary_full_dat$MeanAsim+0.00001))
summary_full_dat$Log_Sobs_Srand <- log((summary_full_dat$S+0.00001)/(summary_full_dat$MeanSsim+0.00001))
head(summary_full_dat)

# long table
SimOutMed <- merge (x=SimOutMed, y= summary_all_DVD[,c('DVDRef', 'TotalP', 'DiffP', 'MaxA')], by='DVDRef', all.x=TRUE)
SimOutMed$Type <- 'a_Sim'
summary_all_DVD$Type <- 'z_Obsv'

head(summary_all_DVD)
head(SimOutMed)

fulldat_long <- rbind(summary_all_DVD,SimOutMed)
fulldat_long$rowID <- seq(1:nrow(fulldat_long))
head(fulldat_long)
}

{# analyse 

modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=fulldat_long, family = 'poisson') 
#modA_off <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + offset(log(MaxA))+ (1|DVDRef), data=fulldat_long, family = 'poisson')
#modAoutofAmax <- glmer(cbind(A, MaxA-A) ~ Type*scale(TotalP) + Type*scale(DiffP)+ (1|DVDRef), data=fulldat_long, family = 'binomial') 
modS <- glmer(S~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=fulldat_long, family = 'poisson') 
#modTotalP <- glmer(TotalP ~ scale(A)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 
#modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) 
modAdev <- lm(Adev~ scale(TotalP) + scale(DiffP) ,data=summary_full_dat) 
modSdev <- lm(Sdev~ scale(TotalP) + scale(DiffP) ,data=summary_full_dat) 
modLogAoAr <- lm(Log_Aobs_Arand~ scale(TotalP) + scale(DiffP) ,data=summary_full_dat) 
modLogSoSr <- lm(Log_Sobs_Srand~ scale(TotalP) + scale(DiffP) ,data=summary_full_dat) 


}

{# results
#results <- data.frame(Factor = c('Type*TotalP', 'Type*DiffP', 'Type*A'), modA = rep(NA,3),modA_off= rep(NA,3), modAoutofAmax = rep(NA,3),modS = rep(NA,3), modP=rep(NA,3), modP_AMax= rep(NA,3))
results <- data.frame(Factor = c('TotalP','DiffP','Type*TotalP', 'Type*DiffP'), modA = rep(NA,4),modS = rep(NA,4), modAdev=rep(NA,4), modSdev= rep(NA,4),modLogAoAr=rep(NA,4),modLogSoSr=rep(NA,4))

results$modA[results$Factor=='TotalP'] <- round(summary(modA)$coef[3,4],3)
results$modA[results$Factor=='DiffP'] <- round(summary(modA)$coef[4,4],3)
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[5,4],3)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[6,4],3)
#results$modA_off[results$Factor=='Type*TotalP'] <- round(summary(modA_off)$coef[5,4],3)
#results$modA_off[results$Factor=='Type*DiffP'] <- round(summary(modA_off)$coef[6,4],3)
#results$modAoutofAmax[results$Factor=='Type*TotalP'] <- round(summary(modAoutofAmax)$coef[5,4],3)
#results$modAoutofAmax[results$Factor=='Type*DiffP'] <- round(summary(modAoutofAmax)$coef[6,4],3)
results$modS[results$Factor=='TotalP'] <- round(summary(modS)$coef[3,4],3)
results$modS[results$Factor=='DiffP'] <- round(summary(modS)$coef[4,4],3)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[5,4],3)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[6,4],3)
#results$modP[results$Factor=='Type*A'] <- round(summary(modTotalP)$coef[4,4],3)
#results$modP_AMax[results$Factor=='Type*A'] <- round(summary(modTotalP_AMax)$coef[4,4],3)
results$modAdev[results$Factor=='TotalP'] <- round(summary(modAdev)$coef[2,4],4)
results$modAdev[results$Factor=='DiffP'] <- round(summary(modAdev)$coef[3,4],4)
results$modSdev[results$Factor=='TotalP'] <- round(summary(modSdev)$coef[2,4],4)
results$modSdev[results$Factor=='DiffP'] <- round(summary(modSdev)$coef[3,4],4)
results$modLogAoAr[results$Factor=='TotalP'] <- round(summary(modLogAoAr)$coef[2,4],4)
results$modLogAoAr[results$Factor=='DiffP'] <- round(summary(modLogAoAr)$coef[3,4],4)
results$modLogSoSr[results$Factor=='TotalP'] <- round(summary(modLogSoSr)$coef[2,4],4)
results$modLogSoSr[results$Factor=='DiffP'] <- round(summary(modLogSoSr)$coef[3,4],4)

}

return(list(results))

}


n <- 200
all_results_A <- pbreplicate(n,Generate_data_randomize_them_and_analyse_A())
all_results_A_Sign <- lapply(all_results_A, function(x){x[,-1] <0.05})
all_results_A_Sign_compiled <- Reduce('+',all_results_A_Sign)/n*100
results_A_PercentageFactorSignificant <- data.frame(Factor=all_results_A[[1]]$Factor,all_results_A_Sign_compiled)
results_A_PercentageFactorSignificant

# n <- 100
       # Factor modA modS modAdev modSdev modLogAoAr modLogSoSr
# 1      TotalP  100  100      18      15         11        100
# 2       DiffP  100  100       7      10          2        100
# 3 Type*TotalP   15  100      NA      NA         NA         NA
# 4  Type*DiffP    0   13      NA      NA         NA         NA

# n <- 200 with interval for synchorny set to 2 instead of 0.5
       # Factor modA  modS modAdev modSdev modLogAoAr modLogSoSr
# 1      TotalP  100 100.0      10    10.5        7.0      100.0
# 2       DiffP  100 100.0       6     7.0        3.5       77.5
# 3 Type*TotalP    7  78.5      NA      NA         NA         NA
# 4  Type*DiffP    0   0.0      NA      NA         NA         NA

}

{## Take observed CN, generate correlated TP

Generate_TP_ObservedCN_randomize_data_and_analyse <-function(){

{# generate TP correlated to existing CN

n <- nrow(MY_tblDVDInfo)
avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))

meanlog <- log(avPR)
sdlog <- sqrt(log(1 + sdPR^2/avPR^2))

CN <- MY_tblDVDInfo$DVDInfoChickNb     # fixed given data
r <- 0.6

b <- (r * sdlog)/ sd(CN)
a <- meanlog - mean(CN)*b

maleE <- rnorm(n,0, sqrt(sdlog^2*(1-r^2)))
femaleE <- rnorm(n,0, sqrt(sdlog^2*(1-r^2)))

MalePexp <- exp(a + b * CN + maleE)
FemalePexp <- exp(a + b * CN + femaleE)

MaleP <- NULL
FemaleP <- NULL
for (i in 1: length(MalePexp)){ # for my analyses I selected videos where both partners visited at least once
MaleP5 <- rpois(5, MalePexp[i])
MaleP[i] <- sample(MaleP5[MaleP5>0],1)

FemaleP5 <- rpois(5, FemalePexp[i])
FemaleP[i] <- sample(FemaleP5[FemaleP5>0],1)

}

MY_TABLE_per_DVD <- data.frame(DVDRef = seq(1:n), CN = CN, TotalP = MaleP+FemaleP, DiffP = abs(FemaleP-MaleP), FemaleP = FemaleP, MaleP = MaleP)

for (i in 1:nrow(MY_TABLE_per_DVD)){
if (MY_TABLE_per_DVD$MaleP[i] == MY_TABLE_per_DVD$FemaleP[i]) {MY_TABLE_per_DVD$MaxA[i] <- MY_TABLE_per_DVD$TotalP[i] - MY_TABLE_per_DVD$DiffP[i] -1} 
else {MY_TABLE_per_DVD$MaxA[i] <- MY_TABLE_per_DVD$TotalP[i] - MY_TABLE_per_DVD$DiffP[i]}
}

}

head(MY_TABLE_per_DVD)


{# create nest watches

full_dat <- list()

create_nest_watch <- function (x){

MaleVisits <- sort(runif(x$MaleP,0,90))
FemaleVisits <- sort(runif(x$FemaleP,0,90))
MaleIntervals <- c(0,diff(MaleVisits))
FemaleIntervals <- c(0,diff(FemaleVisits))

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- x$DVDRef
dat 

}

full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), create_nest_watch))

}

head(full_dat)

{# Randomization Within nest watch, within individual

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

{# create table short and long

median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
SimOutMed <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer), S= apply(out_Ssim_within_df[,-1],1,median_integer))
SimOutMean <- 	data.frame(DVDRef = out_Asim_within_df[,1], MeanAsim = apply(out_Asim_within_df[,-1],1,mean), MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))		

sumarize_one_DVD <- function(x){

A <- sum(diff(x$Sex)!=0)
S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= 0.5)
summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),A,S))

summary_DVD

}

summary_all_DVD <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_all_DVD, by='DVDRef')

# short table
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD,SimOutMean, by='DVDRef' )
MY_TABLE_per_DVD$Adev <- MY_TABLE_per_DVD$A - MY_TABLE_per_DVD$MeanAsim
MY_TABLE_per_DVD$Sdev <- MY_TABLE_per_DVD$S - MY_TABLE_per_DVD$MeanSsim 
head(MY_TABLE_per_DVD)

# long table
MY_TABLE_per_DVD_long <- rbind(MY_TABLE_per_DVD, MY_TABLE_per_DVD)
MY_TABLE_per_DVD_long$Type <- c(rep('a_Sim', nrow(MY_TABLE_per_DVD)), rep('z_Obsv', nrow(MY_TABLE_per_DVD)))

MY_TABLE_per_DVD_long$A[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- SimOutMed$A
MY_TABLE_per_DVD_long$S[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- SimOutMed$S

MY_TABLE_per_DVD_long$rowID <- seq(1:nrow(MY_TABLE_per_DVD_long))
head(MY_TABLE_per_DVD_long)
}

{# analyse 

modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modS <- glmer(S~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modAdev <- lm(Adev~ scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modSdev <- lm(Sdev~ scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 

}

{# results
results <- data.frame(Factor = c('TotalP','DiffP','Type*TotalP', 'Type*DiffP'), modA = rep(NA,4),modS = rep(NA,4), modAdev=rep(NA,4), modSdev= rep(NA,4))
results$modA[results$Factor=='TotalP'] <- round(summary(modA)$coef[3,4],3)
results$modA[results$Factor=='DiffP'] <- round(summary(modA)$coef[4,4],3)
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[5,4],3)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[6,4],3)
results$modS[results$Factor=='TotalP'] <- round(summary(modS)$coef[3,4],3)
results$modS[results$Factor=='DiffP'] <- round(summary(modS)$coef[4,4],3)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[5,4],3)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[6,4],3)
results$modAdev[results$Factor=='TotalP'] <- round(summary(modAdev)$coef[2,4],4)
results$modAdev[results$Factor=='DiffP'] <- round(summary(modAdev)$coef[3,4],4)
results$modSdev[results$Factor=='TotalP'] <- round(summary(modSdev)$coef[2,4],4)
results$modSdev[results$Factor=='DiffP'] <- round(summary(modSdev)$coef[3,4],4)

}

return(list(results))
}

n <-100

all_results_Generate_halfreal <- pbreplicate(n,Generate_TP_ObservedCN_randomize_data_and_analyse())
all_results_Generate_halfreal_sign <- lapply(all_results_Generate_halfreal, function(x){x[,-1] <0.05})
all_results_Generate_halfreal_sign_compiled <- Reduce('+',all_results_Generate_halfreal_sign)/n*100
results_PercentageFactorSignificant_Generate_halfreal <- data.frame(Factor=all_results_Generate_halfreal[[1]]$Factor,all_results_Generate_halfreal_sign_compiled)
results_PercentageFactorSignificant_Generate_halfreal

# n <- 100
       # Factor modA modS modAdev modSdev
# 1      TotalP  100  100      17      18
# 2       DiffP  100  100      10       8
# 3 Type*TotalP   11  100      NA      NA
# 4  Type*DiffP    0    6      NA      NA


}

{## Take observed CN and TP and generate visits

Generate_real_data_randomize_them_and_analyse <-function(){


{# create MY_TABLE_per_DVD

MY_TABLE_per_DVD <- merge(MY_tblParentalCare[,c('DVDRef', 'MVisit1','FVisit1', 'EffectiveTime')], MY_tblDVDInfo[,c("DVDRef","DVDInfoChickNb", "ChickAgeCat")], by = "DVDRef")
MY_TABLE_per_DVD$TotalP <- MY_TABLE_per_DVD$MVisit1 + MY_TABLE_per_DVD$FVisit1 # total number of visits for that video
MY_TABLE_per_DVD$DiffP <- abs(MY_TABLE_per_DVD$MVisit1 - MY_TABLE_per_DVD$FVisit1)
for (i in 1:nrow(MY_TABLE_per_DVD)){
if (MY_TABLE_per_DVD$MVisit1[i] == MY_TABLE_per_DVD$FVisit1[i]) {MY_TABLE_per_DVD$MaxA[i] <- MY_TABLE_per_DVD$MVisit1[i] + MY_TABLE_per_DVD$FVisit1[i] - (abs(MY_TABLE_per_DVD$MVisit1[i] - MY_TABLE_per_DVD$FVisit1[i])) -1}  else {MY_TABLE_per_DVD$MaxA[i]  <- MY_TABLE_per_DVD$MVisit1[i] + MY_TABLE_per_DVD$FVisit1[i] - (abs(MY_TABLE_per_DVD$MVisit1[i] - MY_TABLE_per_DVD$FVisit1[i]))}
}
}

head(MY_TABLE_per_DVD)

{# create nest watches

full_dat <- list()

create_nest_watch <- function (x){

MaleVisits <- sort(runif(x$MVisit1,0,x$EffectiveTime))
FemaleVisits <- sort(runif(x$FVisit1,0,x$EffectiveTime))
MaleIntervals <- c(0,diff(MaleVisits))
FemaleIntervals <- c(0,diff(FemaleVisits))

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- x$DVDRef
dat # for my analyses I selected videos where both partners visited at least once

}

full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), create_nest_watch))

}

head(full_dat)

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

{# create table short and long

median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
SimOutMed <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer), S= apply(out_Ssim_within_df[,-1],1,median_integer))
SimOutMean <- 	data.frame(DVDRef = out_Asim_within_df[,1], MeanAsim = apply(out_Asim_within_df[,-1],1,mean), MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))		

sumarize_one_DVD <- function(x){

A <- sum(diff(x$Sex)!=0)
S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= 0.5)
summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),A,S))

summary_DVD

}

summary_all_DVD <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_all_DVD, by='DVDRef')

# short table
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD,SimOutMean, by='DVDRef' )
MY_TABLE_per_DVD$Adev <- MY_TABLE_per_DVD$A - MY_TABLE_per_DVD$MeanAsim
MY_TABLE_per_DVD$Sdev <- MY_TABLE_per_DVD$S - MY_TABLE_per_DVD$MeanSsim 
MY_TABLE_per_DVD$Log_Aobs_Arand <- log((MY_TABLE_per_DVD$A+0.00001)/(MY_TABLE_per_DVD$MeanAsim+0.00001))
MY_TABLE_per_DVD$Log_Sobs_Srand <- log((MY_TABLE_per_DVD$S+0.00001)/(MY_TABLE_per_DVD$MeanSsim+0.00001))
head(MY_TABLE_per_DVD)

# long table
MY_TABLE_per_DVD_long <- rbind(MY_TABLE_per_DVD, MY_TABLE_per_DVD)
MY_TABLE_per_DVD_long$Type <- c(rep('a_Sim', nrow(MY_TABLE_per_DVD)), rep('z_Obsv', nrow(MY_TABLE_per_DVD)))

MY_TABLE_per_DVD_long$A[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- SimOutMed$A
MY_TABLE_per_DVD_long$S[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- SimOutMed$S

MY_TABLE_per_DVD_long$rowID <- seq(1:nrow(MY_TABLE_per_DVD_long))
head(MY_TABLE_per_DVD_long)
}

{# analyse 

modA <- glmer(A~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modS <- glmer(S~ Type*scale(TotalP) + Type*scale(DiffP) + (1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modAdev <- lm(Adev~ scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modSdev <- lm(Sdev~ scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modLogAoAr <- lm(Log_Aobs_Arand~ scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modLogSoSr <- lm(Log_Sobs_Srand~ scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 


}

{# results
results <- data.frame(Factor = c('TotalP','DiffP','Type*TotalP', 'Type*DiffP'), modA = rep(NA,4),modS = rep(NA,4), modAdev=rep(NA,4), modSdev= rep(NA,4),modLogAoAr=rep(NA,4), modLogSoSr= rep(NA,4))
results$modA[results$Factor=='TotalP'] <- round(summary(modA)$coef[3,4],3)
results$modA[results$Factor=='DiffP'] <- round(summary(modA)$coef[4,4],3)
results$modA[results$Factor=='Type*TotalP'] <- round(summary(modA)$coef[5,4],3)
results$modA[results$Factor=='Type*DiffP'] <- round(summary(modA)$coef[6,4],3)
results$modS[results$Factor=='TotalP'] <- round(summary(modS)$coef[3,4],3)
results$modS[results$Factor=='DiffP'] <- round(summary(modS)$coef[4,4],3)
results$modS[results$Factor=='Type*TotalP'] <- round(summary(modS)$coef[5,4],3)
results$modS[results$Factor=='Type*DiffP'] <- round(summary(modS)$coef[6,4],3)
results$modAdev[results$Factor=='TotalP'] <- round(summary(modAdev)$coef[2,4],4)
results$modAdev[results$Factor=='DiffP'] <- round(summary(modAdev)$coef[3,4],4)
results$modSdev[results$Factor=='TotalP'] <- round(summary(modSdev)$coef[2,4],4)
results$modSdev[results$Factor=='DiffP'] <- round(summary(modSdev)$coef[3,4],4)
results$modLogAoAr[results$Factor=='TotalP'] <- round(summary(modLogAoAr)$coef[2,4],4)
results$modLogAoAr[results$Factor=='DiffP'] <- round(summary(modLogAoAr)$coef[3,4],4)
results$modLogSoSr[results$Factor=='TotalP'] <- round(summary(modLogSoSr)$coef[2,4],4)
results$modLogSoSr[results$Factor=='DiffP'] <- round(summary(modLogSoSr)$coef[3,4],4)

}

return(list(results))

}

n <-100

all_results_Generate_real <- pbreplicate(n,Generate_real_data_randomize_them_and_analyse())
all_results_Generate_real_sign <- lapply(all_results_Generate_real, function(x){x[,-1] <0.05})
all_results_Generate_real_sign_compiled <- Reduce('+',all_results_Generate_real_sign)/n*100
results_PercentageFactorSignificant_Generate_real <- data.frame(Factor=all_results_Generate_real[[1]]$Factor,all_results_Generate_real_sign_compiled)
results_PercentageFactorSignificant_Generate_real

# n <- 100
       # Factor modA modS modAdev modSdev modLogAoAr modLogSoSr
# 1      TotalP  100  100      12      19          6        100
# 2       DiffP  100  100       7      13          5         91
# 3 Type*TotalP   22  100      NA      NA         NA         NA
# 4  Type*DiffP    0    1      NA      NA         NA         NA


}

{## Randomized real dataset instead of generating data

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


n <-200

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

# n <-200
   # Factor modAdev modSdev
# 1  TotalP    14.5    17.5
# 2   DiffP     5.0    11.5
# 3 ChickNb     5.0     3.0

}


{## Bebbington & Hatchwell 2015 analyses on A/(TP-1), while having generate TP correlated to existing CN

Generate_data_randomize_them_and_analyse_ben <-function(){

{# generate TP correlated to existing CN

n <- nrow(MY_tblDVDInfo)
avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))

meanlog <- log(avPR)
sdlog <- sqrt(log(1 + sdPR^2/avPR^2))

CN <- MY_tblDVDInfo$DVDInfoChickNb     # fixed given data
r <- 0.6

b <- (r * sdlog)/ sd(CN)
a <- meanlog - mean(CN)*b

maleE <- rnorm(n,0, sqrt(sdlog^2*(1-r^2)))
femaleE <- rnorm(n,0, sqrt(sdlog^2*(1-r^2)))

MalePexp <- exp(a + b * CN + maleE)
FemalePexp <- exp(a + b * CN + femaleE)

MaleP <- NULL
FemaleP <- NULL
for (i in 1: length(MalePexp)){ # for my analyses I selected videos where both partners visited at least once
MaleP5 <- rpois(5, MalePexp[i])
MaleP[i] <- sample(MaleP5[MaleP5>0],1)

FemaleP5 <- rpois(5, FemalePexp[i])
FemaleP[i] <- sample(FemaleP5[FemaleP5>0],1)

}

MY_TABLE_per_DVD <- data.frame(DVDRef = seq(1:n), CN = CN, TotalP = MaleP+FemaleP, DiffP = abs(FemaleP-MaleP), FemaleP = FemaleP, MaleP = MaleP)

for (i in 1:nrow(MY_TABLE_per_DVD)){
if (MY_TABLE_per_DVD$MaleP[i] == MY_TABLE_per_DVD$FemaleP[i]) {MY_TABLE_per_DVD$MaxA[i] <- MY_TABLE_per_DVD$TotalP[i] - MY_TABLE_per_DVD$DiffP[i] -1} 
else {MY_TABLE_per_DVD$MaxA[i] <- MY_TABLE_per_DVD$TotalP[i] - MY_TABLE_per_DVD$DiffP[i]}
}

}

head(MY_TABLE_per_DVD)

{# create nest watches

full_dat <- list()

create_nest_watch <- function (x){

MaleVisits <- sort(runif(x$MaleP,0,90))
FemaleVisits <- sort(runif(x$FemaleP,0,90))
MaleIntervals <- c(0,diff(MaleVisits))
FemaleIntervals <- c(0,diff(FemaleVisits))

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- x$DVDRef
dat 

}

full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), create_nest_watch))

}

head(full_dat)

{# Randomization Within nest watch, within individual

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

A_S_within_randomization <- do.call(cbind,replicate(10,Randomize_Data_WithinFile_and_Calculate_A_S_fun(full_dat),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

}

{# calculate deviation from randomization

#median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
#SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer))

SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], MeanAsim = rowMeans(out_Asim_within_df[,-1]), MeanSsim = rowMeans(out_Ssim_within_df[,-1]))

sumarize_one_DVD <- function(x){

Ascore <- sum(diff(x$Sex)!=0) / (nrow(x)-1)*100
Sscore <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= 0.5) / (nrow(x))*100
summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),Ascore,Sscore))

summary_DVD

}

summary_all_DVD <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_all_DVD, by='DVDRef')
MY_TABLE_per_DVD <- merge (x=MY_TABLE_per_DVD, y=SimOut , by='DVDRef', all.x=TRUE)
MY_TABLE_per_DVD$Adev <- MY_TABLE_per_DVD$Ascore - MY_TABLE_per_DVD$MeanAsim
MY_TABLE_per_DVD$Sdev <- MY_TABLE_per_DVD$Sscore - MY_TABLE_per_DVD$MeanSsim

}

{# analyse 

modA <- lm(Ascore~ scale(TotalP) + scale(DiffP) + scale(CN), data=MY_TABLE_per_DVD) 
modTotalP <- lm(TotalP ~ scale(Adev) + scale(CN), data=MY_TABLE_per_DVD) 
modS <- glm(round(Sscore) ~ scale(TotalP)+ scale(Ascore) + scale(CN), data=MY_TABLE_per_DVD, family='poisson')

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

# n <- 100
   # Factor modA modP modS
# 1  TotalP  100   NA  100
# 2   DiffP  100   NA   NA
# 3 ChickNb   11  100   91
# 4    Adev   NA    4   NA
# 5       A   NA   NA  100


}












