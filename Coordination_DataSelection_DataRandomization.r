#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 21/03/2017
#	 commit: massive restructuration code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# remarks
# LastSeenAlive information needs to be updated manually when DB updated
# MY_tblBrood$Nb3 is the number of post fledgling
# MY_tblBrood Mass and tarsus info: the last measurement, at d12, when ringed. nMass, nTarsus, NbRinged should in principle be equal: maybe should consider small difference of age, i.e. include all brood or a standardized subsets
# MY_TABLE_perDVD has one line per file
# MY_TABLE_perBrood has one line per brood, averaging the summary accross files
}

rm(list = ls(all = TRUE))

{# packages

library(tidyr)
library(dplyr) 
library(ggplot2)
library(pbapply)

}

{# functions

is.even <- function(x) x %% 2 == 0 

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
check_integer <- function(x) {x == round(x)}
median_integer <- function(x) {if (check_integer(median(x)) == TRUE) {return(median(x))} else {return(as.integer(median(x) +sample(c(0.5,-0.5),1)))}}
 
Calculate_AMax <- function(x){
if (x$MVisit1 == x$FVisit1) {x$AMax <- x$MVisit1 + x$FVisit1 - abs(x$MVisit1 - x$FVisit1) -1} else {x$AMax <- x$MVisit1 + x$FVisit1 - abs(x$MVisit1 - x$FVisit1)}
return(x)
}

Calculate_A_one_nest_watch <- function(x){
A <- sum(diff(x$Sex)!=0)
A
}

Calculate_S_one_nest_watch <- function(x){
S <- S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= syncint)
S
}

Calculate_A_S_one_nest_watch <- function(x){
A <- sum(diff(x$Sex)!=0)
S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= syncint)
data.frame(cbind(DVDRef=unique(x$DVDRef),A,S))
}


}

{# Parameter values for above functions

VideoLength <- 90

nPR <- 1599 # nb of selected DVD (see below)
avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))

syncint <- 2
}

{# Get raw data from R_ExtractedData

{## output csv files !!! needs updating if specific data change !!!

ExtractedData_folder <- "R_ExtractedData"

MY_tblParentalCare <- read.csv(paste(ExtractedData_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblDVDInfo <- read.csv(paste(ExtractedData_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(ExtractedData_folder,"R_MY_RawFeedingVisits.csv", sep="/")) # OF directly followed by IN are merged into one feeding visits ; will be used for simulation

## !!! to update when new pedigree !!! (and other corrections potentially)
MY_tblBroods <- read.csv(paste(ExtractedData_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded

}

{## input txt files  

input_folder <- "R_input"

FedBroods <-  read.table(file= paste(input_folder,"FedBroods.txt", sep="/"), sep='\t', header=T)  ## from Ian Cleasby 20160531
tblChicks <-  read.table(file= paste(input_folder,"R_tblChicks.txt", sep="/"), sep='\t', header=T)  ## to update if consider new year of data

}


}

{# select valid video files for studying behavioural compatibility in chick provisioning

{## exclusion of files

list_non_valid_DVDRef <- 
c(
MY_tblParentalCare$DVDRef[!(MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 10 files with no visits at all + 2 files with no feeding visits at all
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)],# 6 - where 0 chicks
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$ChickAge >5 & MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef) ],# 171 - where still brooding (age <=5) and with chicks and with feeding visit
MY_tblParentalCare$DVDRef[(MY_tblParentalCare$MVisit1 ==0 | MY_tblParentalCare$FVisit1 ==0 )& MY_tblDVDInfo$DVDInfoChickNb > 0 & MY_tblDVDInfo$ChickAge >5  & (MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 153 - one sex did not visit for feeding despite having chicks above age 5
MY_tblDVDInfo$DVDRef[ !MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef],# 2 DVD where both parents unidentified
MY_tblDVDInfo$DVDRef[MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef[is.na(MY_tblBroods$SocialDadID) | is.na(MY_tblBroods$SocialMumID)] ], # 63 files where one parent unidentified
MY_tblDVDInfo$DVDRef[MY_tblDVDInfo$BroodRef %in% unlist(FedBroods)] # 106 extra files for 48 broods (the 49th: 980 already excluded as only female visited) fed by Ian 
)


length(unique(list_non_valid_DVDRef)) # 450 

MY_tblDVDInfo <- MY_tblDVDInfo[ ! MY_tblDVDInfo$DVDRef %in% list_non_valid_DVDRef,]

MY_tblParentalCare <- MY_tblParentalCare[ ! MY_tblParentalCare$DVDRef %in% list_non_valid_DVDRef,]
MY_tblParentalCare <- dplyr::rename(MY_tblParentalCare,VisitRateDifference= DiffVisit1Rate)
MY_tblParentalCare <- dplyr::rename(MY_tblParentalCare, TotalProRate = MFVisit1RateH)

MY_RawFeedingVisits  <- MY_RawFeedingVisits[ ! MY_RawFeedingVisits$DVDRef %in% list_non_valid_DVDRef,]
RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		

MY_tblChicks <- tblChicks[tblChicks$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

MY_tblChicks_byRearingBrood <- as.data.frame(tblChicks %>% group_by(RearingBrood) %>% summarise(sd(AvgOfMass),sd(AvgOfTarsus), n(), sum(CrossFosteredYN)))
colnames(MY_tblChicks_byRearingBrood) <- c("RearingBrood","sdMass", "sdTarsus", "NbChicksMeasured", "NbChicksMeasuredCrossFostered")
MY_tblChicks_byRearingBrood$MixedBroodYN <- MY_tblChicks_byRearingBrood$NbChicksMeasured != MY_tblChicks_byRearingBrood$NbChicksMeasuredCrossFostered
head(MY_tblChicks_byRearingBrood)

MY_tblChicks_byRearingBrood <- MY_tblChicks_byRearingBrood[MY_tblChicks_byRearingBrood$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

}

{## fill in manually the data where Julia deleted it 
### unfortunately this list is not exhaustive and migth even be arguable 
### they were deleted from the dataset by Julia because the genetic parents did not match the social parents
### but the social parents most likely did raise that one chick ! and since I am looking at social fitness...

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

{# Create MY_TABLE_perDVD
### one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
### as broods were watched several time, the brood info appears in duplicate


MY_TABLE_perDVD <- merge(
MY_tblParentalCare[,c("DVDRef","FVisit1", "MVisit1",  "EffectiveTime","FVisit1RateH", "MVisit1RateH","VisitRateDifference","TotalProRate")],
MY_tblDVDInfo[,c("DVDRef","BroodRef","TapeLength","DVDInfoChickNb","DVDInfoAge", "ChickAgeCat","RelTimeHrs")], by="DVDRef")
MY_TABLE_perDVD$MFVisit1 <- MY_TABLE_perDVD$FVisit1+ MY_TABLE_perDVD$MVisit1


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


{### add meanAge and DeltaAge for testing within and between individual effect of age

MY_TABLE_perDVD <- MY_TABLE_perDVD %>%
  group_by(SocialDadID)%>%
  mutate(meanDadAge = mean(DadAge), DeltaDadAge = DadAge-mean(DadAge),FirstDadReproAge = min(DadAge), LastDadReproAge = max(DadAge))
  
MY_TABLE_perDVD <- MY_TABLE_perDVD %>%
  group_by(SocialMumID)%>%
  mutate(meanMumAge = mean(MumAge), DeltaMumAge = MumAge-mean(MumAge),FirstMumReproAge = min(MumAge), LastMumReproAge = max(MumAge))

}

MY_TABLE_perDVD <- as.data.frame(MY_TABLE_perDVD) 


### add A Max
MY_TABLE_perDVD <- do.call(rbind,lapply(split(MY_TABLE_perDVD,MY_TABLE_perDVD$DVDRef), Calculate_AMax))

### add A and S
MY_TABLE_perDVD <- merge(MY_TABLE_perDVD,
						do.call(rbind,lapply(split(RawInterfeeds,RawInterfeeds$DVDRef),Calculate_A_S_one_nest_watch)))
						
}

{# Descriptive statistics

{## sample sizes
nrow(MY_TABLE_perDVD) # 1599 DVD files 
length(unique(MY_TABLE_perDVD$BroodRef)) # 872 broods videotaped at least once
range(table(MY_TABLE_perDVD$BroodRef)) # range from 1 to 3
mean(table(MY_TABLE_perDVD$BroodRef)) # on average 1.8 videos per brood watched
median(table(MY_TABLE_perDVD$BroodRef))
summary(MY_TABLE_perDVD$TapeLength)
#hist(MY_TABLE_perDVD$TapeLength)
nrow(MY_TABLE_perDVD[MY_TABLE_perDVD$TapeLength <"90" | MY_TABLE_perDVD$TapeLength > "93.6",])
nrow(MY_TABLE_perDVD[MY_TABLE_perDVD$TapeLength >= "90" & MY_TABLE_perDVD$TapeLength <= "93.6",])
length(unique(MY_TABLE_perDVD$SocialMumID))#290
length(unique(MY_TABLE_perDVD$SocialDadID))#280
length(unique(MY_TABLE_perDVD$PairID))#443
summary(MY_TABLE_perDVD$MBroodNb[!is.na(MY_TABLE_perDVD$SocialDadID) & !is.na(MY_TABLE_perDVD$SocialMumID)]) # 4.8
summary(MY_TABLE_perDVD$FBroodNb[!is.na(MY_TABLE_perDVD$SocialDadID) & !is.na(MY_TABLE_perDVD$SocialMumID)]) # 4.6
}

{## the typical sparrow (judge from ALL broods)

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


### female divorce more than male, in majority for the Exes ??? polyandrous females ?
summary(MY_tblBroods$FwillDivorce)
summary(MY_tblBroods$FwillDivorceforEx)
summary(MY_tblBroods$MwillDivorce)
summary(MY_tblBroods$MwillDivorceforEx)


}

{## first interval after setting up camera
outTsartMin <- do.call(rbind, by(MY_RawFeedingVisits, MY_RawFeedingVisits$DVDRef, function(x) x[which.min(x$TstartFeedVisit), c('DVDRef','TstartFeedVisit')] ))
summary(outTsartMin$TstartFeedVisit)

t.test(MY_RawFeedingVisits$Interval,outTsartMin$TstartFeedVisit)
}

}



head(MY_tblBroods) # even those where one parent unknown, needed divorce question
head(RawInterfeeds) 
head(MY_TABLE_perDVD)


set.seed(10)

{# Randomizations to get A and S expected by chance

{## Generate Data

generate_fulldat <- function(nVideo,avPR,sdPR,VideoLength){

meanlog <- log(avPR)
sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))

create_DVD <- function(){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog )
MaleP <- rpois(1, MalePexp)
if (MaleP == 0){MaleP <- rpois(1, MalePexp)}

FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
FemaleP <- rpois(1, FemalePexp)
if (FemaleP == 0){FemaleP <- rpois(1, FemalePexp)}

TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP - FemaleP)

MaleVisits <- sort(runif(MaleP,0,VideoLength))
FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
DVD <- data.frame(rbind(cbind(Visits = MaleVisits, Sex = rep(1, length(MaleVisits))),cbind(Visits = FemaleVisits,Sex = rep(0, length(FemaleVisits)))))
DVD <- DVD[order(DVD$Visits),]

A <- sum(diff(DVD$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
S <- sum(diff(DVD$Sex)!=0 & diff(DVD$Visits) <= syncint)
dat <- data.frame(cbind(TotalP,DiffP,MaxA,A,S))

dat
}

fulldat <- data.frame(matrix(data=unlist(pbreplicate(nVideo, create_DVD())), nVideo,5, byrow = TRUE))
colnames(fulldat) <- c('TotalP','DiffP','MaxA','A','S')
fulldat$DVDRef <- seq(1:nrow(fulldat))
fulldat
}

one_generated_fulldat <- generate_fulldat(1599,avPR,sdPR,VideoLength) # see default parameter values
head(one_generated_fulldat)

}


NreplicatesAmongFileRandomization <- 100

{## Randomization among nest watch, within individual with same provisioning rate (this is only fare) and same sex

RawInterfeeds_with_ProRate <- merge(x=RawInterfeeds, y=MY_tblParentalCare[c('DVDRef','FVisit1RateH','MVisit1RateH')]) # this will only be used for shuffling within prorate

FRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 0,])
MRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 1,])

# save first Tstart of each file and each sex  (with interval = 0)
F_FirstTstart <- data.frame(FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1))
M_FirstTstart <- data.frame(MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1))

# remove the first line with interval (=0) from each file for each sex before shuffling interval
FData_to_Shuffle <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)
MData_to_Shuffle <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)


randomization_among_nest_watch_and_calculate_A_S_fun <- function(FData_to_Shuffle, MData_to_Shuffle){ # function that simulate all nest watches once with identical first Tstarts and shuffled interval among individual of same sex and same visit rate

## shuffled intervals among individuals of the same sex that have the same visit rate
FShuffled <- data.frame(FData_to_Shuffle %>% group_by(FVisit1RateH) %>% mutate(Interval=base::sample(Interval)))
MShuffled <- data.frame(MData_to_Shuffle %>% group_by(MVisit1RateH) %>% mutate(Interval=base::sample(Interval)))

# add first Tstart
SimFemale <- rbind(F_FirstTstart,FShuffled) # this set the order: the first Tstart from the original file is above the visits with shuffled intervals
SimFemale <- SimFemale[order(SimFemale$DVDRef),]

SimMale <- rbind(M_FirstTstart,MShuffled)
SimMale <- SimMale[order(SimMale$DVDRef),]

# recalculate new Tstarts (cumulative sum of sheffuled intervals)
SimFemale <- do.call(rbind,lapply(X=split(SimFemale,SimFemale$DVDRef), FUN=function(x){x$Tstart <- x$Tstart[1] + cumsum(x$Interval)
return(x)}))
rownames(SimFemale) <- NULL

SimMale <- do.call(rbind,lapply(X=split(SimMale,SimMale$DVDRef), FUN=function(x){x$Tstart <- x$Tstart[1] + cumsum(x$Interval)
return(x)}))
rownames(SimMale) <- NULL

# bind both sexes together
SimData <- data.frame(bind_rows(SimMale, SimFemale)) # different from rbind as it binds two df with different columns, adding NAs
SimData[is.na(SimData)] <- 0
SimData <- SimData[order(SimData$DVDRef,SimData$Tstart),]
rownames(SimData) <- NULL

## Calculate NbAlternation within each DVD

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= Calculate_A_one_nest_watch ))

## Calculate Synchrony value within each DVD

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= Calculate_S_one_nest_watch ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

Out_A_S_sim_Among <- do.call(cbind,pbreplicate(NreplicatesAmongFileRandomization,randomization_among_nest_watch_and_calculate_A_S_fun(FData_to_Shuffle, MData_to_Shuffle),simplify=FALSE ) )

# first half are A sim
out_Asim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))

# second Half are S sim
out_Ssim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))

}


NreplicatesWithinFileRandomization <- 100

{## Randomization Within nest watch, within individual

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

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= Calculate_A_one_nest_watch ))

## Calculate Synchrony within each DVD

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= Calculate_S_one_nest_watch ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,pbreplicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S_fun(RawInterfeeds),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

}


{## Shuffling consecutives intervals within one individual to keep some autocorrelation within nest watch

Switch_Consecutive_intervals_onesplit_fun <- function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x1sim <- x1 # only shuffle intervals for one sex

if (nrow(x1) > 1){

x1simInterval <- c(x1$Interval,x1$Interval[nrow(x1)]) # repeat the last one when uneven number of rows (see below)

for (i in 2:nrow(x1sim))
{ if (is.even(i)){x1sim$Interval[i] <- x1simInterval[i+1]}
else {x1sim$Interval[i] <- x1simInterval[i-1]}
}

x1sim$Tstart <- c(x1sim$Tstart[1] + cumsum(x1sim$Interval))

}


xsim <- rbind(x0,x1sim)
xsim <- xsim[order(xsim$Tstart),]

Asim <- sum(diff(xsim$Sex)!=0)

return(Asim)

}

Switch_Consecutive_intervals_out_A <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), Aswitch=
														do.call(rbind,lapply(split(RawInterfeeds,RawInterfeeds$DVDRef),Switch_Consecutive_intervals_onesplit_fun)))
}


{## Sort intervals to create full autocorrelation

Sort_intervals_onesplit_fun <- function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- sort(x0$Interval)
x1$Interval <- sort(x1$Interval) 

x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) # recalculate Tstart based on sorted intervals
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),]

Asim <- sum(diff(xsim$Sex)!=0)

return(Asim)

}

Sort_intervals_out_A <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), Asorted=
														do.call(rbind,lapply(split(RawInterfeeds,RawInterfeeds$DVDRef),Sort_intervals_onesplit_fun)))

}

}


{## plot A/AMax 

{### summarize for box plot A in percentage of AMax 

summary_Aobsv_outof_AMax <- data.frame(summarise (MY_TABLE_perDVD,
				Amean = mean(A/AMax*100),
				Alower = Amean - sd(A/AMax*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(A/AMax*100)/sqrt(n())*1.96,
				NbFiles = n()))

sumary_A_generated_outof_AMax <- summarise (one_generated_fulldat,
				Amean = mean(A/MaxA*100),
				Alower = Amean - sd(A/MaxA*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(A/MaxA*100)/sqrt(n())*1.96,
				NbFiles = n())	

Sort_intervals_out_A <- merge(x=Sort_intervals_out_A, y= MY_TABLE_perDVD[,c('DVDRef','AMax')], all.x=TRUE)
Summary_A_sorted_intervals_outof_AMax <- summarise (Sort_intervals_out_A,
				Amean = mean(Asorted/AMax*100),
				Alower = Amean - sd(Asorted/AMax*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(Asorted/AMax*100)/sqrt(n())*1.96,
				NbFiles = n())			
				

summarise_Asim_outof_AMax <- function(out_sim_df){

out_sim_df$SimMean <- rowMeans(out_sim_df[,2:(NreplicatesAmongFileRandomization)])
out_sim_df <- merge(x=out_sim_df, y= MY_TABLE_perDVD[,c('DVDRef','AMax')], all.x=TRUE)
out_sim_df$SimMean_outof_AMax <- out_sim_df$SimMean/out_sim_df$AMax*100

return(data.frame(summarise (out_sim_df,
				Amean = mean(SimMean_outof_AMax),
				Alower = Amean - sd(SimMean_outof_AMax)/sqrt(n())*1.96,
				Aupper = Amean + sd(SimMean_outof_AMax)/sqrt(n())*1.96,
				NbFiles = n())))
				
				
}				

summary_out_Asim_outof_AMax_among_df <- summarise_Asim_outof_AMax(out_Asim_among_df)
summary_out_Asim_outof_AMax_within_df <- summarise_Asim_outof_AMax(out_Asim_within_df)

Switch_Consecutive_intervals_out_A <- merge(x=Switch_Consecutive_intervals_out_A, y= MY_TABLE_perDVD[,c('DVDRef','AMax')], all.x=TRUE)
summary_Aswitch_outof_AMax <- data.frame(summarise (Switch_Consecutive_intervals_out_A,
				Amean = mean(Aswitch/AMax*100),
				Alower = Amean - sd(Aswitch/AMax*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(Aswitch/AMax*100)/sqrt(n())*1.96,
				NbFiles = n()))
		
Summary_A_sorted_intervals_outof_AMax$Type <- '1_Sorted'		
summary_Aobsv_outof_AMax$Type <- '2_Observed' 
summary_Aswitch_outof_AMax$Type <- '3_Switch' 
summary_out_Asim_outof_AMax_within_df$Type <-'4_Within'
summary_out_Asim_outof_AMax_among_df$Type <- '5_Among'
sumary_A_generated_outof_AMax$Type <- '6_Generated'

summary_A_outof_AMax <- do.call(rbind, 
list(summary_Aobsv_outof_AMax,
summary_out_Asim_outof_AMax_among_df,
summary_out_Asim_outof_AMax_within_df,
summary_Aswitch_outof_AMax,
sumary_A_generated_outof_AMax,
Summary_A_sorted_intervals_outof_AMax))

}

{my_labels <- c(
'Sorted
intervals',
'Observed
data', 
'Switched
intervals', 
'Within
random.', 
'Among
random.',
'Generated
data')}

{my_shapes <- c(
19, #circle
19, # square
19, 
15, 
15,
19)}

{my_colors <- c(
'grey','black', 'grey','dimgrey','dimgrey','dimgrey'
)}


Fig_A_AMax <- {ggplot(data=summary_A_outof_AMax, aes(x=Type, y=Amean,colour=Type, shape = Type))+
xlab(NULL)+
ylab("Number of alternations realized out of the maximum possible (%)\n")+

scale_y_continuous(breaks =seq(60,90, by = 5),limits = c(60,90)) +
scale_x_discrete(labels = my_labels)+

geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
geom_point(size = 3, aes(shape=Type, col=Type)) +
scale_colour_manual(values=my_colors, labels = my_labels)+
scale_shape_manual(values=my_shapes, labels=my_labels)+ 

theme_classic()+
theme(
legend.position="none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(size=14,face="bold", margin=margin(l=5)),
axis.text.x=element_text(size=14, face="bold",margin=margin(t=5)),
axis.title.x = NULL,
plot.margin = unit(c(0.2,0.2,0.3,0.3), "cm"))
}

}

dev.new()
Fig_A_AMax

{## plot S/SMax
### SMax = A observed in that specific nest watch (observed or randomized)

{# summarize the ratio Ssim/SMax made on each DVD and averaged accross the dataset

summary_Sobsv_outof_A <- data.frame(summarise (MY_TABLE_perDVD,
				Smean = mean(S/A*100),
				Slower = Smean - sd(S/A*100)/sqrt(n())*1.96,
				Supper = Smean + sd(S/A*100)/sqrt(n())*1.96,
				NbFiles = n()))
		

sumary_S_generated_outof_A <- summarise (one_generated_fulldat,
				Smean = mean(S/A*100),
				Slower = Smean - sd(S/A*100)/sqrt(n())*1.96,
				Supper = Smean + sd(S/A*100)/sqrt(n())*1.96,
				NbFiles = n())


summarise_S_sim_outof_A <- function(out_Ssim_df, out_Asim_df){ # Nreplicates needs to be identical in both randomization

out_sim_df <- out_Ssim_df[,2:(NreplicatesAmongFileRandomization)]/out_Asim_df[,2:(NreplicatesAmongFileRandomization)]*100
out_sim_df$SimMean <- rowMeans(out_sim_df)

return(data.frame(summarise (out_sim_df,
				Smean = mean(SimMean),
				Slower = Smean - sd(SimMean)/sqrt(n())*1.96,
				Supper = Smean + sd(SimMean)/sqrt(n())*1.96,
				NbFiles = n())))
	
}				

summary_out_Ssim_outof_A_among_df <- summarise_S_sim_outof_A(out_Ssim_among_df,out_Asim_among_df)
summary_out_Ssim_outof_A_within_df <- summarise_S_sim_outof_A(out_Ssim_within_df,out_Asim_within_df)

				
summary_Sobsv_outof_A$Type <- '2_Observed' 
summary_out_Ssim_outof_A_within_df$Type <-'4_Within'
summary_out_Ssim_outof_A_among_df$Type <- '5_Among'
sumary_S_generated_outof_A$Type <- '6_Generated'


summary_S_outof_A <- do.call(rbind, 
list(summary_Sobsv_outof_A,
summary_out_Ssim_outof_A_among_df,
summary_out_Ssim_outof_A_within_df,
sumary_S_generated_outof_A))

}

{my_labels_S <- c(
'Observed
data', 
'Within
random.', 
'Among
random.',
'Generated
data')}

{my_shapes_S <- c(
19, 
15, # square
15, 
19)}

{my_colors_S <- c(
'black','dimgrey','dimgrey','dimgrey'
)}

Fig_S_SMax <- {ggplot(data=summary_S_outof_A, aes(x=Type, y=Smean), colour=Type, shape=Type)+
xlab(NULL)+
ylab("Number of synchronized visits realized 
out of the maximum possible (%)\n")+

geom_errorbar(aes(ymin=Slower, ymax=Supper, col=Type),na.rm=TRUE)+
geom_point(size = 3, aes(col=Type, shape=Type)) +

scale_y_continuous(breaks =seq(45,55, by = 2),limits = c(45,55)) +
scale_x_discrete(labels = my_labels_S)+
scale_shape_manual(values=my_shapes_S, labels=my_labels_S)+ 
scale_colour_manual(values=my_colors_S, labels = my_labels_S)+


theme_classic()+
theme(
legend.position="none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(size=14,face="bold", margin=margin(l=5)),
axis.text.x=element_text(size=14, face="bold",margin=margin(t=5)),
axis.title.x = NULL,
plot.margin = unit(c(0.2,0.2,0.3,0.3), "cm"))
}

}

dev.new()
Fig_S_SMax


{## add output randomization to MY_TABLE_perDVD

MY_TABLE_perDVD <- cbind(MY_TABLE_perDVD,
							Asorted = Sort_intervals_out_A$Asorted,
							Aswitch= Switch_Consecutive_intervals_out_A$Aswitch,
							
							MeanAsimWithin = rowMeans(out_Asim_within_df[,-1]), 
							MeanAsimAmong = rowMeans(out_Asim_among_df[,-1]),
							
							MedAsimWithin = apply(out_Asim_within_df[,-1],1,median_integer),
							MedAsimAmong = apply(out_Asim_among_df[-1],1,median_integer),
							
							MeanSsimWithin = rowMeans(out_Ssim_within_df[,-1]), 
							MeanSsimAmong = rowMeans(out_Ssim_among_df[-1]),
							
							MedSsimWithin = apply(out_Ssim_within_df[,-1],1,median_integer),
							MedSsimAmong = apply(out_Ssim_among_df[-1],1,median_integer)
							)
	
MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$A - MY_TABLE_perDVD$MeanAsimWithin	
MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$S - MY_TABLE_perDVD$MeanSsimWithin

}

head(MY_TABLE_perDVD)


{# create MY_TABLE_perBrood

{## summarise DVD measures per brood, and merge tables

MY_TABLE_perBrood <- data.frame(summarise (group_by(MY_TABLE_perDVD, BroodRef),
							MeanTotalProRate = mean(TotalProRate), 
							MeanA = mean(A), 
							MeanAdev = mean(A)-mean(MeanAsimWithin),
							MeanAsim = mean(MeanAsimWithin),
							MeanVisitRateDifference = mean(VisitRateDifference), # to delete ?
							MeanS = mean(S),
							MeanMFVisit1 = mean(MFVisit1), # to delete ?
							MeanSdev = mean(S) - mean(MeanSsimWithin),
							MeanSsim = mean(MeanSsimWithin),
							MeanEffectiveTime = mean(EffectiveTime),
							MeanMVisit1RateH = mean(MVisit1RateH), 
							MeanFVisit1RateH = mean(FVisit1RateH),
							MeanDVDInfoChickNb = mean(DVDInfoChickNb)))

						
MY_TABLE_perBrood <- merge(MY_TABLE_perBrood, 
MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401",
"SocialMumID","SocialDadID","DadAge","MumAge","ParentsAge",
"MBroodNb","MPriorResidence","MnextNBsame", "MwillDivorce",
"FBroodNb","FPriorResidence","FnextNBsame","FwillDivorce",
"PairID","PairBroodNb","PairIDYear", "NbHatched","NbRinged","AvgMass", "MinMass", "AvgTarsus")], by= "BroodRef")

MY_TABLE_perBrood <- merge(MY_TABLE_perBrood, 
MY_tblChicks_byRearingBrood[,c("RearingBrood", "sdMass", "sdTarsus", "MixedBroodYN")], 
by.x="BroodRef", by.y="RearingBrood", all.x=TRUE)

}
	
nrow(MY_TABLE_perBrood) # 872
	
{## add residual mass on tarsus

ResMassTarsus <-  cbind( MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus),"BroodRef" ], 
						data.frame(residuals(lm(AvgMass~ AvgTarsus, 
						data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus), ]))))
colnames(ResMassTarsus) <- c("BroodRef" , "ResMassTarsus")

MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=ResMassTarsus, all.x=TRUE, by = "BroodRef")

}

}

head(MY_TABLE_perBrood)

{# create MY_TABLE_perChick
nrow(MY_tblChicks[is.na(MY_tblChicks$AvgOfMass),]) # 0
nrow(MY_tblChicks[is.na(MY_tblChicks$AvgOfTarsus),]) # 40

MY_TABLE_perChick <- merge(x= MY_tblChicks , 
y=MY_TABLE_perBrood[,c("BroodRef", "NbRinged","MeanA","MeanAdev","MeanAsim","MeanTotalProRate","MeanS","MeanSdev","MeanSsim",
"SocialMumID","SocialDadID","PairID","BreedingYear","HatchingDayAfter0401", "PairBroodNb")],
by.x="RearingBrood", by.y = "BroodRef", all.x=TRUE )

MY_TABLE_perChick <- MY_TABLE_perChick[!is.na(MY_TABLE_perChick$BreedingYear),]
MY_TABLE_perChick$GenPairID <- paste(MY_TABLE_perChick$sire, MY_TABLE_perChick$dam, sep="")

ResMassTarsus_perChick <-  cbind( MY_TABLE_perChick[!is.na(MY_TABLE_perChick$AvgOfTarsus),"ChickID" ], 
								data.frame(residuals(lm(AvgOfMass~ AvgOfTarsus, 
								data = MY_TABLE_perChick[!is.na(MY_TABLE_perChick$AvgOfTarsus), ]))))
colnames(ResMassTarsus_perChick) <- c("ChickID" , "ResMassTarsus_perChick")

MY_TABLE_perChick <- merge(x=MY_TABLE_perChick, y=ResMassTarsus_perChick, all.x=TRUE, by = "ChickID")

nrow(MY_tblChicks) # 2218 = length(unique(MY_tblChicks$ChickID)) 
nrow(MY_TABLE_perChick) # 2133(without those with no breding year) = length(unique(MY_TABLE_perChick$ChickID))
nrow(ResMassTarsus_perChick) # 2096 (without those with no tarsus measurement)


{## calculate sd residual mass on tarsus and add to MY_TABLE_perBrood

ResMassTarsus_perChick_perBrood <- as.data.frame(MY_TABLE_perChick %>% group_by(RearingBrood) %>% summarise(sd(ResMassTarsus_perChick)))
colnames(ResMassTarsus_perChick_perBrood) <- c('RearingBrood','sdResMassTarsus')
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood,y=ResMassTarsus_perChick_perBrood, by.x='BroodRef', by.y='RearingBrood', all.x=TRUE)

}

}

head(MY_TABLE_perChick)
head(MY_TABLE_perBrood)



# output_folder <- "R_Selected&RandomizedData"


# write.csv(MY_TABLE_perDVD, file = paste(output_folder,"R_MY_TABLE_perDVD.csv", sep="/"), row.names = FALSE) 
# 20161215
# 20161221
# 20170127 added AMax, NbAlternation, NbAMax
# 20170201 changed AlternationValue to NbAlternation and AMax to NbAMax and Adev to the difference between NbAlternation and NbAlternation from the simulation (id. for S)
# 20170208 added DVDInfoAge just for hist of variation in chick age (although Age cat used in model)
# 20170208 after rerunning data extraction (should be the same)
# 20170214 add MedAsim
# 20170321 add all output simulation into it, set seed
# 20170322 rerun
# 20170324 updated lastseen alive
# 20170327 added Asorted

# write.csv(MY_TABLE_perBrood, file = paste(output_folder,"R_MY_TABLE_perBrood.csv", sep="/"), row.names = FALSE) 
# 20161221
# 20170201 changed AlternationValue to NbAlternation and AMax to NbAMax and Adev to the difference between NbAlternation and NbAlternation from the simulation (id. for S)
# 20170203 replace ratioRingedHatched by Nb Hatched (to have cbind(Ringed,Hatched))
# 20170203 add MeanEffectiveTime
# 20170208 after rerunning data extraction (should be the same)
# 20170214 add MeanAsim 
# 20170321 set seed
# 20170322 rerun
# 20170324 updated lastseen alive
# 20170415 updated format last seen alive input to recover divorce YN



# write.csv(MY_TABLE_perChick, file = paste(output_folder,"R_MY_TABLE_perChick.csv", sep="/"), row.names = FALSE) 
# 20161221
# 20170208 after rerunning data extraction (should be the same)
# 20170214 add MeanAsim 
# 20170321 set seed
# 20170322 rerun
# 20170324 updated lastseen alive



# write.csv(RawInterfeeds, file = paste(output_folder,"R_RawInterfeeds.csv", sep="/"), row.names = FALSE) 
# 20170321 the raw data of the DVDs where both parents are known
# 20170322 rerun






