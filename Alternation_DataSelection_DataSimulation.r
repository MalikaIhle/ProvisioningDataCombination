#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 01/02/2017
#	 commit: change Alternation value to NbAlternation for output simulation and Table DVD and Brood
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### remarks
# LastSeenAlive information needs to be updated manually when DB updated
# MY_tblBrood$Nb3 is the number of post fledgling
# MY_tblBrood Mass and tarsus info: the last measurement, at d12, when ringed. nMass, nTarsus, NbRinged should in principle be equal: maybe should consider small difference of age, i.e. include all brood or a standardized subsets
# MY_TABLE_perDVD has one line per file
# MY_TABLE_perBrood has one line per brood, averaging the summary accross files
}

rm(list = ls(all = TRUE))

{### packages

library(tidyr)
library(dplyr) 
library(ggplot2)
library(pbapply)

}

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{#### Simulations to get NbAlternation and Nb of synchronous visits

{## the raw Data, observed and maximum scores

{# raw data

RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		
head(RawInterfeeds,40)

RawInterfeeds_with_ProRate <- merge(x=RawInterfeeds, y=MY_tblParentalCare[c('DVDRef','FVisit1RateH','MVisit1RateH')]) # this will only be used for shuffling within prorate

FRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 0,])
MRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 1,])

# save first Tstart of each file and each sex  (with interval = 0)
F_FirstTstart <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1)
M_FirstTstart <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1)

# remove the first line with interval (=0) from each file for each sex before shuffling interval
FData_to_Shuffle <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)
MData_to_Shuffle <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)

# add NbAMax

MY_tblParentalCare$NbAMax <- NA

for (i in 1:nrow(MY_tblParentalCare))
{
if((MY_tblParentalCare$FVisit1[i] - MY_tblParentalCare$MVisit1[i])==0)
{
MY_tblParentalCare$NbAMax[i] <- min(MY_tblParentalCare$FVisit1[i],MY_tblParentalCare$MVisit1[i])*2-1}

else{
MY_tblParentalCare$NbAMax[i] <- min(MY_tblParentalCare$FVisit1[i],MY_tblParentalCare$MVisit1[i])*2 
}

}

}

{# observation

Summarise_A_S <- function(DataSummary, AS) {

if (AS == 'A') {
return(data.frame(summarise (DataSummary,
				Amean = mean(NbAlternation),
				Alower = Amean - sd(NbAlternation)/sqrt(n())*1.96,
				Aupper = Amean + sd(NbAlternation)/sqrt(n())*1.96,
				NbFiles = n())))
				}
				
if (AS == 'S') {
return(data.frame(summarise (DataSummary,
				Smean = mean(NbSynchro_ChickFeedingEquanim),
				Slower = Smean - sd(NbSynchro_ChickFeedingEquanim)/sqrt(n())*1.96,
				Supper = Smean + sd(NbSynchro_ChickFeedingEquanim)/sqrt(n())*1.96,
				NbFiles = n())))				
				}
}

summary_Observed_A <- Summarise_A_S (MY_tblParentalCare, 'A')
summary_Observed_S <- Summarise_A_S (MY_tblParentalCare, 'S')
}

{# A max 


summary_Amax <- data.frame(summarise (MY_tblParentalCare,
				Amean = mean(NbAMax),
				Alower = Amean - sd(NbAMax)/sqrt(n())*1.96,
				Aupper = Amean + sd(NbAMax)/sqrt(n())*1.96,
				NbFiles = n()))

}

}

head(RawInterfeeds_with_ProRate)


NreplicatesAmongFileRandomization <- 100

{### Randomization among nest watch, within individual with same provisioning rate (this is only fare) and same sex

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

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony value within each DVD

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

Out_A_S_sim_Among <- do.call(cbind,replicate(NreplicatesAmongFileRandomization,randomization_among_nest_watch_and_calculate_A_S_fun(FData_to_Shuffle, MData_to_Shuffle),simplify=FALSE ) )

# first half are A sim
out_Asim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))

# second Half are S sim
out_Ssim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))



}


NreplicatesWithinFileRandomization <- 100

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

A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S_fun(RawInterfeeds),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))

}


{## summarize Asim and Ssim 

summarise_sim <- function(out_sim_df, AS){

out_sim_df$SimMean <- rowMeans(out_sim_df[,2:(NreplicatesAmongFileRandomization)])

if (AS == 'A'){
return(data.frame(summarise (out_sim_df,
				Amean = mean(SimMean),
				Alower = Amean - sd(SimMean)/sqrt(n())*1.96,
				Aupper = Amean + sd(SimMean)/sqrt(n())*1.96,
				NbFiles = n())))
				}
				
if (AS == 'S') {
return(data.frame(summarise (out_sim_df,
				Smean = mean(SimMean),
				Slower = Smean - sd(SimMean)/sqrt(n())*1.96,
				Supper = Smean + sd(SimMean)/sqrt(n())*1.96,
				NbFiles = n())))				
				}

}				

summary_out_Asim_among_df <- summarise_sim(out_Asim_among_df, 'A')
summary_out_Ssim_among_df <- summarise_sim(out_Ssim_among_df, 'S')

summary_out_Asim_within_df <- summarise_sim(out_Asim_within_df, 'A')
summary_out_Ssim_within_df <- summarise_sim(out_Ssim_within_df, 'S')

}


{### Shuffling consecutives intervals within one individual to keep some autocorrelation within nest watch

is.even <- function(x) x %% 2 == 0 

x <- split(RawInterfeeds,RawInterfeeds$DVDRef)[[3]]


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

# summarise

summary_Aswitch <- data.frame(summarise (Switch_Consecutive_intervals_out_A,
				Amean = mean(Aswitch),
				Alower = Amean - sd(Aswitch)/sqrt(n())*1.96,
				Aupper = Amean + sd(Aswitch)/sqrt(n())*1.96,
				NbFiles = n()))

}


{## save the output

SimulationOutput <- cbind(MY_tblParentalCare[,c('DVDRef','NbAlternation')],
							Aswitch = Switch_Consecutive_intervals_out_A$Aswitch,
							MeanAsimWithin = rowMeans(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef)))), 
							MeanAsimAmong = rowMeans(head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef)))))
							

SimulationOutput_long <- rbind(cbind(MY_tblParentalCare[,c('DVDRef','NbAlternation')], Type = '1_Observed'),
			data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbAlternation = Switch_Consecutive_intervals_out_A$Aswitch,Type = '2_Switch')),
			data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbAlternation = rowMeans(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef)))), Type = '3_Within')), 
			data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbAlternation = rowMeans(head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef)))), Type = '4_Among')))


# some median are .5 : randomly round up or down.
check.integer <- function(x) {x == round(x)}
median_integer <- function(x) {if (check.integer(median(x)) == TRUE) {return(median(x))} else {return(as.integer(median(x) +sample(c(0.5,-0.5),1)))}}
		
			
SimulationOutput_long_median <- rbind(cbind(MY_tblParentalCare[,c('DVDRef','NbAlternation')], Type = '1_Observed'),
	data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbAlternation = Switch_Consecutive_intervals_out_A$Aswitch,Type = '2_Switch')),
	data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbAlternation = apply(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer), Type = '3_Within')), 
	data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbAlternation = apply(head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))),1,median_integer), Type = '4_Among')))

SimulationOutput_long_median <- merge(x=SimulationOutput_long_median, y = MY_tblParentalCare[,c('DVDRef','NbAMax')], by = 'DVDRef', all.x=TRUE)
SimulationOutput_long_median$LineID <- as.character(1:nrow(SimulationOutput_long_median))



SimulationOutput_S_long_median <- rbind(cbind(MY_tblParentalCare[,c('DVDRef','NbSynchro_ChickFeedingEquanim')], Type = '1_Observed'),
	data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbSynchro_ChickFeedingEquanim = apply(tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer), Type = '3_Within')), 
	data.frame(cbind(DVDRef = MY_tblParentalCare$DVDRef, NbSynchro_ChickFeedingEquanim = apply(tail(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))),1,median_integer), Type = '4_Among')))

colnames(SimulationOutput_S_long_median) <- c("DVDRef","NbS", "Type")
SimulationOutput_S_long_median$NbS <- as.numeric(as.character(SimulationOutput_S_long_median$NbS))
SimulationOutput_S_long_median <- merge(x=SimulationOutput_S_long_median, y = MY_tblParentalCare[,c('DVDRef','NbAMax')], by = 'DVDRef', all.x=TRUE)
SimulationOutput_S_long_median$LineID <- as.character(1:nrow(SimulationOutput_S_long_median))


}


{## plot Nb Alternation and Nb Synchrony

{# Alternation

summary_Amax$Type <- '1_Maximum' 
summary_Observed_A$Type <- '2_Observed' 
summary_Aswitch$Type <- '3_Switch' 
summary_out_Asim_within_df$Type <-'4_Within'
summary_out_Asim_among_df$Type <- '5_Among'

summary_A <- do.call(rbind, 
list(summary_Observed_A,
summary_Amax, 
summary_out_Asim_among_df,
summary_out_Asim_within_df,
summary_Aswitch))


{my_labels <- c(
"maximum possible",
"observed" , 
"after exchanging consecutive intervals once",
"after 100 randomizations within nest watches", 
"after 100 randomizations among nest watches")}

{my_colors <- c(
'grey',
'black',
'#009E73', # turquoise
'#56B4E9', # sky blue
'#0072B2')} # king blue

{my_shapes <- c(
21, #circle
21,
24, # triangle up
22, # square
25)} # triangle down


Fig_A <- {ggplot(data=summary_A, aes(x=Type, y=Amean, group=Type, colour=Type, shape = Type, fill = Type))+
xlab("Type")+
ylab("Mean alternation")+

geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+

scale_colour_manual(values=my_colors,labels= my_labels)+ 

geom_point(size = 1.5, aes(fill = NULL)) +
geom_point(aes(shape=Type), size=1.5) +
scale_shape_manual(values=my_shapes,labels=my_labels)+  

scale_fill_manual(values = my_colors,labels=my_labels)+ 

scale_y_continuous(breaks = pretty(summary_A$Amean, n = 9)) +

theme_classic()+
theme(
legend.justification= c(1,1),
legend.position = c(1,1), 
legend.title = element_blank(),
legend.background = element_rect(colour = "black"),
panel.border = element_rect(colour = "black", fill=NA),
axis.title=element_text(size=14,face="bold"))

}

}
 
{# Synchrony

summary_Smax <- summary_Amax
colnames(summary_Smax) <- c('Smean', 'Slower', 'Supper', 'NbFiles', 'Type')
summary_Smax$Type <- '1_Maximum' 
summary_Observed_S$Type <- '2_Observed' 
summary_out_Ssim_within_df$Type <-'4_Within'
summary_out_Ssim_among_df$Type <- '5_Among'


summary_S <- do.call(rbind, 
list(
summary_Smax,
summary_Observed_S,
summary_out_Ssim_among_df,
summary_out_Ssim_within_df
))


{my_labelsS <- c(
"maximum possible",
"observed" , 
"after 100 randomizations within nest watches", 
"after 100 randomizations among nest watches")}

{my_colorsS <- c(
'grey',
'black',
'#56B4E9', # sky blue
'#0072B2')} # king blue

{my_shapesS <- c(
21, #circle
21,
22, # square
25)} # triangle down

Fig_S <- {ggplot(data=summary_S, aes(x=Type, y=Smean, group=Type, colour=Type, shape = Type, fill = Type))+
xlab("Type")+
ylab("Mean synchrony")+

geom_line()+
geom_errorbar(aes(ymin=Slower, ymax=Supper),na.rm=TRUE)+

scale_colour_manual(values=my_colorsS,labels= my_labelsS)+ 

geom_point(size = 1.5, aes(fill = NULL)) +
geom_point(aes(shape=Type), size=1.5) +
scale_shape_manual(values=my_shapesS,labels=my_labelsS)+  

scale_fill_manual(values = my_colorsS,labels=my_labelsS)+ 

scale_y_continuous(breaks = pretty(summary_S$Smean, n = 10)) +

theme_classic()+
theme(
legend.justification= c(1,1),
legend.position = c(1,1), 
legend.title = element_blank(),
legend.background = element_rect(colour = "black"),
panel.border = element_rect(colour = "black", fill=NA),
axis.title=element_text(size=14,face="bold"))

}

}

}


{## Generate Data

create_1662_fulldat <- function(avPR,sdPR,VideoLength){

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
S <- sum(diff(DVD$Sex)!=0 & diff(DVD$Visits) <= 0.5)
dat <- data.frame(cbind(TotalP,DiffP,MaxA,A,S))

dat
}

fulldat <- data.frame(matrix(data=unlist(pbreplicate(1662, create_DVD())), 1662,5, byrow = TRUE)) # 1662 DVDs for simulation
colnames(fulldat) <- c('TotalP','DiffP','MaxA','A','S')
fulldat$DVDRef <- seq(1:nrow(fulldat))
fulldat
}


one_generated_fulldat <- create_1662_fulldat(15,8,90)
head(one_generated_fulldat)


sumary_A_generated <- summarise (one_generated_fulldat,
				Amean = mean(A/MaxA*100),
				Alower = Amean - sd(A/MaxA*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(A/MaxA*100)/sqrt(n())*1.96,
				NbFiles = n())

sumary_A_generated$Type <- '6_Generated'

sumary_S_generated <- summarise (one_generated_fulldat,
				Smean = mean(S/MaxA*100),
				Slower = Smean - sd(S/MaxA*100)/sqrt(n())*1.96,
				Supper = Smean + sd(S/MaxA*100)/sqrt(n())*1.96,
				NbFiles = n())

sumary_S_generated$Type <- '6_Generated'

}

{## plot Asim/AMax 

{# summarize the ratio Asim/AMax made on each DVD and averaged accross the dataset

summary_Aobsv_outof_AMax <- data.frame(summarise (MY_tblParentalCare,
				Amean = mean(NbAlternation/NbAMax*100),
				Alower = Amean - sd(NbAlternation/NbAMax*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(NbAlternation/NbAMax*100)/sqrt(n())*1.96,
				NbFiles = n()))


summarise_sim_outof_AMax <- function(out_sim_df, AS){

out_sim_df$SimMean <- rowMeans(out_sim_df[,2:(NreplicatesAmongFileRandomization)])
out_sim_df <- merge(x=out_sim_df, y= MY_tblParentalCare[,c('DVDRef','NbAMax')], all.x=TRUE)
out_sim_df$SimMean_outof_AMax <- out_sim_df$SimMean/out_sim_df$NbAMax*100

if (AS == 'A'){
return(data.frame(summarise (out_sim_df,
				Amean = mean(SimMean_outof_AMax),
				Alower = Amean - sd(SimMean_outof_AMax)/sqrt(n())*1.96,
				Aupper = Amean + sd(SimMean_outof_AMax)/sqrt(n())*1.96,
				NbFiles = n())))
				}
				
}				

summary_out_Asim_outof_AMax_among_df <- summarise_sim_outof_AMax(out_Asim_among_df, 'A')
summary_out_Asim_outof_AMax_within_df <- summarise_sim_outof_AMax(out_Asim_within_df, 'A')

Switch_Consecutive_intervals_out_A <- merge(x=Switch_Consecutive_intervals_out_A, y= MY_tblParentalCare[,c('DVDRef','NbAMax')], all.x=TRUE)
summary_Aswitch_outof_AMax <- data.frame(summarise (Switch_Consecutive_intervals_out_A,
				Amean = mean(Aswitch/NbAMax*100),
				Alower = Amean - sd(Aswitch/NbAMax*100)/sqrt(n())*1.96,
				Aupper = Amean + sd(Aswitch/NbAMax*100)/sqrt(n())*1.96,
				NbFiles = n()))
				
summary_Aobsv_outof_AMax$Type <- '2_Observed' 
summary_Aswitch_outof_AMax$Type <- '3_Switch' 
summary_out_Asim_outof_AMax_within_df$Type <-'4_Within'
summary_out_Asim_outof_AMax_among_df$Type <- '5_Among'

summary_A_outof_AMax <- do.call(rbind, 
list(summary_Aobsv_outof_AMax,
summary_out_Asim_outof_AMax_among_df,
summary_out_Asim_outof_AMax_within_df,
summary_Aswitch_outof_AMax,
sumary_A_generated)) # not run

# summary_A_outof_AMax <- rbind(summary_A_outof_AMax, sumary_A_generated)

}

{# plot Asim/AMax

Fig_A_AMax <- {ggplot(data=summary_A_outof_AMax, aes(x=Type, y=Amean))+
xlab(NULL)+
ylab("Number of alternations realized out of the maximum possible (%)\n")+

geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
geom_point(size = 3) +

scale_y_continuous(breaks =seq(60,80, by = 5),limits = c(60,80)) +
scale_x_discrete(labels = 
c('Observed
data', 
'Switched
intervals', 'Within
random.', 
'Among
random.',
'Generated
data'))+

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

}

{## plot Ssim/SMax 

{# summarize the ratio Ssim/AMax made on each DVD and averaged accross the dataset

summary_Sobsv_outof_AMax <- data.frame(summarise (MY_tblParentalCare,
				Smean = mean(NbSynchro_ChickFeedingEquanim/NbAMax*100),
				Slower = Smean - sd(NbSynchro_ChickFeedingEquanim/NbAMax*100)/sqrt(n())*1.96,
				Supper = Smean + sd(NbSynchro_ChickFeedingEquanim/NbAMax*100)/sqrt(n())*1.96,
				NbFiles = n()))


summarise_sim_outof_AMax <- function(out_sim_df, AS){ # Nreplicates needs to be identical in both randomization

out_sim_df$SimMean <- rowMeans(out_sim_df[,2:(NreplicatesAmongFileRandomization)])
out_sim_df <- merge(x=out_sim_df, y= MY_tblParentalCare[,c('DVDRef','NbAMax')], all.x=TRUE)
out_sim_df$SimMean_outof_AMax <- out_sim_df$SimMean/out_sim_df$NbAMax*100

if (AS == 'S'){
return(data.frame(summarise (out_sim_df,
				Smean = mean(SimMean_outof_AMax),
				Slower = Smean - sd(SimMean_outof_AMax)/sqrt(n())*1.96,
				Supper = Smean + sd(SimMean_outof_AMax)/sqrt(n())*1.96,
				NbFiles = n())))
				}
				
}				

summary_out_Ssim_outof_AMax_among_df <- summarise_sim_outof_AMax(out_Ssim_among_df, 'S')
summary_out_Ssim_outof_AMax_within_df <- summarise_sim_outof_AMax(out_Ssim_within_df, 'S')

				
summary_Sobsv_outof_AMax$Type <- '2_Observed' 
summary_out_Ssim_outof_AMax_within_df$Type <-'4_Within'
summary_out_Ssim_outof_AMax_among_df$Type <- '5_Among'


summary_S_outof_AMax <- do.call(rbind, 
list(summary_Sobsv_outof_AMax,
summary_out_Ssim_outof_AMax_among_df,
summary_out_Ssim_outof_AMax_within_df,
sumary_S_generated))

}

{# plot Ssim/AMax

Fig_S_AMax <- {ggplot(data=summary_S_outof_AMax, aes(x=Type, y=Smean))+
xlab(NULL)+
ylab("Number of synchronized visits realized 
out of the maximum possible (%)\n")+

geom_errorbar(aes(ymin=Slower, ymax=Supper),na.rm=TRUE)+
geom_point(size = 3) +

scale_y_continuous(breaks =seq(5,15, by = 1),limits = c(5,15)) +
scale_x_discrete(labels = 
c('Observed
data', 
'Within
random.', 
'Among
random.',
'Generated
data'))+

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

}


}

# or 
# summary_A_outof_AMax <- read.csv(paste("R_Selected&SimulatedData","R_summary_A_outof_AMax.csv", sep="/"))
# summary_S_outof_AMax <- read.csv(paste("R_Selected&SimulatedData","R_summary_S_outof_AMax.csv", sep="/"))

dev.new()
Fig_A_AMax
dev.new()
Fig_S_AMax

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


{### create MY_TABLE_perDVD: select those where both parents known + add expected alternation from simulation
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

{# merge tables
MY_TABLE_perDVD <- merge(
MY_tblParentalCare[,c("DVDRef","FVisit1", "MVisit1", "NbAlternation", "NbAMax","EffectiveTime","FVisit1RateH", "MVisit1RateH","VisitRateDifference","TotalProRate",
#"AlternationValue", "SynchronyFeedValue", "AMax",
"NbSynchro_ChickFeedingEquanim", "PropSynchroFemaleStart")], 
MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","DVDInfoAge", "ChickAgeCat","RelTimeHrs")], by="DVDRef")
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

}

head(MY_TABLE_perDVD)

{# remove DVD where one social parent unknown
length(unique(MY_TABLE_perDVD$BroodRef[is.na(MY_TABLE_perDVD$SocialMum) | is.na(MY_TABLE_perDVD$SocialDadID)])) # 38 broods - 63 files one parent unknown
MY_TABLE_perDVD <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$SocialMumID) & !is.na(MY_TABLE_perDVD$SocialDadID),] # where both parents known
}

nrow(MY_TABLE_perDVD) # 1599 files

{# add MeanAsim and Adev + MedAsim !

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MeanAsim = rowMeans(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

#MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$AlternationValue - MY_TABLE_perDVD$MeanAsim # reversed 22/06/2016
MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$NbAlternation - MY_TABLE_perDVD$MeanAsim # reversed 22/06/2016 # changed to NbAlternation 01/02/2017

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MedAsim = apply(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)


}

{# add MeanSsim and Sdev

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MeanSsim = rowMeans(tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

#MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$SynchronyFeedValue - MY_TABLE_perDVD$MeanSsim 
MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim - MY_TABLE_perDVD$MeanSsim # changed to NbSynchro_ChickFeedingEquanim on 01/02/2017

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MedSsim = apply(tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))),1,median_integer)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)
}

{# add meanAge and DeltaAge for testing within and between individual effect of age

MY_TABLE_perDVD <- MY_TABLE_perDVD %>%
  group_by(SocialDadID)%>%
  mutate(meanDadAge = mean(DadAge), DeltaDadAge = DadAge-mean(DadAge),FirstDadReproAge = min(DadAge), LastDadReproAge = max(DadAge))
  
MY_TABLE_perDVD <- MY_TABLE_perDVD %>%
  group_by(SocialMumID)%>%
  mutate(meanMumAge = mean(MumAge), DeltaMumAge = MumAge-mean(MumAge),FirstMumReproAge = min(MumAge), LastMumReproAge = max(MumAge))

}

MY_TABLE_perDVD <- as.data.frame(MY_TABLE_perDVD) 

}

head(MY_TABLE_perDVD)


{### create MY_TABLE_perBrood

{# summarise DVD measures per brood, and merge tables

MY_TABLE_perBrood <- data.frame(summarise (group_by(MY_TABLE_perDVD, BroodRef),
							MeanTotalProRate = mean(TotalProRate), 
							#MeanA = mean(AlternationValue), 
							#MeanAdev = mean(AlternationValue)-mean(MeanAsim),  # reversed 22/06/2016
							MeanA = mean(NbAlternation), 
							MeanAdev = mean(NbAlternation)-mean(MeanAsim),
							MeanAsim = mean(MeanAsim),
							MeanVisitRateDifference = mean(VisitRateDifference), # to delete ?
							#MeanS = mean(SynchronyFeedValue),
							MeanS = mean(NbSynchro_ChickFeedingEquanim),
							MeanMFVisit1 = mean(MFVisit1), # to delete ?
							#MeanSdev = mean(SynchronyFeedValue) - mean(MeanSsim), 
							MeanSdev = mean(NbSynchro_ChickFeedingEquanim) - mean(MeanSsim),
							MeanSsim = mean(MeanSsim),
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
	
{# add residual mass on tarsus

ResMassTarsus <-  cbind( MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus),"BroodRef" ], 
						data.frame(residuals(lm(AvgMass~ AvgTarsus, 
						data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus), ]))))
colnames(ResMassTarsus) <- c("BroodRef" , "ResMassTarsus")

MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=ResMassTarsus, all.x=TRUE, by = "BroodRef")

}

}

head(MY_TABLE_perBrood)

{### create MY_TABLE_perChick
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


{# calculate sd residual mass on tarsus and add to MY_TABLE_perBrood

ResMassTarsus_perChick_perBrood <- as.data.frame(MY_TABLE_perChick %>% group_by(RearingBrood) %>% summarise(sd(ResMassTarsus_perChick)))
colnames(ResMassTarsus_perChick_perBrood) <- c('RearingBrood','sdResMassTarsus')
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood,y=ResMassTarsus_perChick_perBrood, by.x='BroodRef', by.y='RearingBrood', all.x=TRUE)

}

}

head(MY_TABLE_perChick)
head(MY_TABLE_perBrood)



# output_folder <- "R_Selected&SimulatedData"


# write.csv(MY_TABLE_perDVD, file = paste(output_folder,"R_MY_TABLE_perDVD.csv", sep="/"), row.names = FALSE) 
# 20161215
# 20161221
# 20170127 added AMax, NbAlternation, NbAMax
# 20170201 changed AlternationValue to NbAlternation and AMax to NbAMax and Adev to the difference between NbAlternation and NbAlternation from the simulation (id. for S)
# 20170208 added DVDInfoAge just for hist of variation in chick age (although Age cat used in model)
# 20170208 after rerunning data extraction (should be the same)
# 20170214 add MedAsim

# write.csv(MY_TABLE_perBrood, file = paste(output_folder,"R_MY_TABLE_perBrood.csv", sep="/"), row.names = FALSE) 
# 20161221
# 20170201 changed AlternationValue to NbAlternation and AMax to NbAMax and Adev to the difference between NbAlternation and NbAlternation from the simulation (id. for S)
# 20170203 replace ratioRingedHatched by Nb Hatched (to have cbind(Ringed,Hatched))
# 20170203 add MeanEffectiveTime
# 20170208 after rerunning data extraction (should be the same)
# 20170214 add MeanAsim 

# write.csv(MY_TABLE_perChick, file = paste(output_folder,"R_MY_TABLE_perChick.csv", sep="/"), row.names = FALSE) 
# 20161221
# 20170208 after rerunning data extraction (should be the same)
# 20170214 add MeanAsim 

# write.csv(SimulationOutput, file = paste(output_folder,"R_SimulationOutput.csv", sep="/"), row.names = FALSE) 
# 20170206
# 20170208 after rerunning data extraction (should be the same)

# write.csv(SimulationOutput_long, file = paste(output_folder,"R_SimulationOutput_long.csv", sep="/"), row.names = FALSE) 
# 20170206
# 20170208 after rerunning data extraction (should be the same)

# write.csv(SimulationOutput_long_median, file = paste(output_folder,"R_SimulationOutput_long_median.csv", sep="/"), row.names = FALSE) 
# 20170207
# 20170208 with AMax and lineID
# 20170208 after rerunning data extraction (should be the same)

# write.csv(summary_A_outof_AMax, file = paste(output_folder,"R_summary_A_outof_AMax.csv", sep="/"), row.names = FALSE) 
#  20180208 to save data for plot without need to rerun simulation for checking it.

# write.csv(SimulationOutput_S_long_median, file = paste(output_folder,"R_SimulationOutput_S_long_median.csv", sep="/"), row.names = FALSE) 
# 20170209

# write.csv(summary_S_outof_AMax, file = paste(output_folder,"R_summary_S_outof_AMax.csv", sep="/"), row.names = FALSE) 
#  20180209 to save data for plot without need to rerun simulation for checking it.










