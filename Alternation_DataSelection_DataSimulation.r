#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 19/12/2016
#	 commit: clean up simulation script
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

library(dplyr) 
library(ggplot2)

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

{#### Simulations

{## the raw Data

RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		
head(RawInterfeeds)

RawInterfeeds_with_ProRate <- merge(x=RawInterfeeds, y=MY_tblParentalCare[c('DVDRef','FVisit1RateH','MVisit1RateH','VisitRateDifference')])

FRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 0,])
MRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 1,])

# save first Tstart of each file and each sex  (with interval = 0)
F_FirstTstart <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1)
M_FirstTstart <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1)

# remove the first line with interval (=0) from each file for each sex before shuffling interval
FData_to_Shuffle <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)
MData_to_Shuffle <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)
}

head(RawInterfeeds)

{## the observed and maximum scores

# observed scores

Summarise_A_S_perVisitRateDifference <- function(DataSummary, AS) {

if (AS == 'A'){
return(data.frame(summarise (group_by(DataSummary, VisitRateDifference),
				Amean = mean(AlternationValue),
				Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
				Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
				NbFiles = n())))
				}
				
if (AS == 'S') {
return(data.frame(summarise (group_by(DataSummary, TotalProRate),
				Smean = mean(SynchronyFeedValue),
				Slower = Smean - sd(SynchronyFeedValue)/sqrt(n())*1.96,
				Supper = Smean + sd(SynchronyFeedValue)/sqrt(n())*1.96,
				NbFiles = n())))				
				}
}

summary_Observed_A <- Summarise_A_S_perVisitRateDifference (MY_tblParentalCare, 'A')
summary_Observed_S <- Summarise_A_S_perVisitRateDifference (MY_tblParentalCare, 'S')

# A max and S max

MY_tblParentalCare$AMax <- NA

for (i in 1:nrow(MY_tblParentalCare))
{
if((MY_tblParentalCare$FVisit1RateH[i] - MY_tblParentalCare$MVisit1RateH[i])==0)
{
MY_tblParentalCare$AMax[i] <- 
round((((min(MY_tblParentalCare$FVisit1RateH[i],MY_tblParentalCare$MVisit1RateH[i]))*2-1) / (MY_tblParentalCare$FVisit1RateH[i] + MY_tblParentalCare$MVisit1RateH[i] -1))*100,2) }

else{
MY_tblParentalCare$AMax[i] <- 
round((((min(MY_tblParentalCare$FVisit1RateH[i],MY_tblParentalCare$MVisit1RateH[i]))*2) / (MY_tblParentalCare$FVisit1RateH[i] + MY_tblParentalCare$MVisit1RateH[i] -1))*100,2) 
}
}

summary_Amax <- data.frame(summarise (group_by(MY_tblParentalCare, VisitRateDifference),
				Amean = mean(AMax),
				Alower = Amean - sd(AMax)/sqrt(n())*1.96,
				Aupper = Amean + sd(AMax)/sqrt(n())*1.96,
				NbFiles = n()))
				
summary_Smax <- data.frame(summarise (group_by(MY_tblParentalCare, TotalProRate),
				Smean = mean(AMax),
				Slower = Smean - sd(AMax)/sqrt(n())*1.96,
				Supper = Smean + sd(AMax)/sqrt(n())*1.96,
				NbFiles = n()))
				
}


NreplicatesAmongFileRandomization <- 100

{### Randomization among nest watch, within individual with same provisioning rate and same sex

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

## Calculate Alternation value within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)])/(nrow(x) -1) *100 # AlternationValue
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony value within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- (length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)]) / (nrow(x) -1) ) *100
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first hald of the rows, Ssim of each DVD, second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

Out_A_S_sim_Among <- do.call(cbind,replicate(NreplicatesAmongFileRandomization,randomization_among_nest_watch_and_calculate_A_S_fun(FData_to_Shuffle, MData_to_Shuffle),simplify=FALSE ) )

# first half are A sim
out_Asim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))
out_Asim_among_df <- merge(x=out_Asim_among_df, y= MY_tblParentalCare[,c('DVDRef','VisitRateDifference')], by='DVDRef', all.x =TRUE)

# second Half are S sim
out_Ssim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))
out_Ssim_among_df <- merge(x=out_Ssim_among_df, y= MY_tblParentalCare[,c('DVDRef','TotalProRate')], by='DVDRef', all.x =TRUE)

# Summarise A ans S sim (NreplicatesAmongFileRandomization per file) per visit rate difference (A) or total pro rate (S)

summarise_Sim_Among <- function(out_sim_among_df, AS){

out_A_or_S_among_sim_df_per_VisitRateDifference_fun <- function(x) {

x <- x[,-1]
x <- x[,-ncol(x)]
v <- unlist(list(x))

return(c(
mean(v), # Amean OR Smean
mean(v) - sd(v)*1.96, # Alower OR Slower
mean(v) + sd(v)*1.96, # Aupper OR Supper
nrow(x) # NbFiles
))
}

if (AS == "A")
{
out1_among_df <- lapply(split(out_sim_among_df,out_sim_among_df$VisitRateDifference),out_A_or_S_among_sim_df_per_VisitRateDifference_fun)
out2_among_df <- data.frame(rownames(do.call(rbind,out1_among_df)),do.call(rbind, out1_among_df))
rownames(out2_among_df) <- NULL
colnames(out2_among_df) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')
}

if (AS == "S")
{
out1_among_df <- lapply(split(out_sim_among_df,out_sim_among_df$TotalProRate),out_A_or_S_among_sim_df_per_VisitRateDifference_fun)
out2_among_df <- data.frame(rownames(do.call(rbind,out1_among_df)),do.call(rbind, out1_among_df))
rownames(out2_among_df) <- NULL
colnames(out2_among_df) <- c('TotalProRate','Smean','Slower','Supper','NbFiles')
}


return(out2_among_df)

}

summary_out_Asim_among_df <- summarise_Sim_Among(out_Asim_among_df,'A')
summary_out_Ssim_among_df <- summarise_Sim_Among(out_Ssim_among_df,'S')

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

## Calculate Alternation value within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)])/(nrow(x) -1) *100 # AlternationValue
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony value within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- (length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)]) / (nrow(x) -1) ) *100
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first hald of the rows, Ssim of each DVD, second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S_fun(RawInterfeeds),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))
out_Asim_within_df <- merge(x=out_Asim_within_df, y= MY_tblParentalCare[,c('DVDRef','VisitRateDifference')], by='DVDRef', all.x =TRUE)

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))
out_Ssim_within_df <- merge(x=out_Ssim_within_df, y= MY_tblParentalCare[,c('DVDRef','TotalProRate')], by='DVDRef', all.x =TRUE)

# Summarise A ans S sim (NreplicateswithinFileRandomization per file) per visit rate difference

summarise_Sim_within <- function(out_sim_within_df, AS){

out_A_or_S_within_sim_df_fun <- function(x) {

x <- x[,-1]
x <- x[,-ncol(x)]
v <- unlist(list(x))

return(c(
mean(v), # Amean OR Smean
mean(v) - sd(v)/sqrt(length(v)/NreplicatesWithinFileRandomization)*1.96, # Alower OR Slower
mean(v) + sd(v)/sqrt(length(v)/NreplicatesWithinFileRandomization)*1.96, # Aupper OR Supper
nrow(x) # NbFiles
))
}

if (AS == "A")
{
out1_within_df <- lapply(split(out_sim_within_df,out_sim_within_df$VisitRateDifference),out_A_or_S_within_sim_df_fun)
out2_within_df <- data.frame(rownames(do.call(rbind,out1_within_df)),do.call(rbind, out1_within_df))
rownames(out2_within_df) <- NULL
colnames(out2_within_df) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')
}

if (AS == "S")
{
out1_within_df <- lapply(split(out_sim_within_df,out_sim_within_df$TotalProRate),out_A_or_S_within_sim_df_fun)
out2_within_df <- data.frame(rownames(do.call(rbind,out1_within_df)),do.call(rbind, out1_within_df))
rownames(out2_within_df) <- NULL
colnames(out2_within_df) <- c('TotalProRate','Smean','Slower','Supper','NbFiles')
}


return(out2_within_df)

}

summary_out_Asim_within_df <- summarise_Sim_within(out_Asim_within_df,'A')
summary_out_Ssim_within_df <- summarise_Sim_within(out_Ssim_within_df,'S')


}


{### Shuffling consecutives intervals within one individual to keep some autocorrelation within nest watch

is.even <- function(x) x %% 2 == 0 

Switch_Consecutive_intervals_onesplit_fun <- function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x1sim <- x1 # only shuffle intervals for one sex

if (nrow(x1) > 1){

x1simInterval <- c(x1$Interval,x1$Interval[nrow(x1)])

for (i in 2:nrow(x1sim))
{ if (is.even(i)){x1sim$Interval[i] <- x1simInterval[i+1]}
else {x1sim$Interval[i] <- x1simInterval[i-1]}
}

x1sim$Tstart <- c(x1sim$Tstart[1] + cumsum(x1sim$Interval))

}


xsim <- rbind(x0,x1sim)
xsim <- xsim[order(xsim$Tstart),]

Asim <- ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100

return(Asim)

}

Switch_Consecutive_intervals_onesplit <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), Aswitch=
														do.call(rbind,lapply(split(RawInterfeeds,RawInterfeeds$DVDRef),Switch_Consecutive_intervals_onesplit_fun)))

# summarise per visit rate difference

Switch_Consecutive_intervals_onesplit <- merge(x=Switch_Consecutive_intervals_onesplit, y=MY_tblParentalCare[c('DVDRef','VisitRateDifference')])

summary_Aswitch <- data.frame(summarise (group_by(Switch_Consecutive_intervals_onesplit, VisitRateDifference),
				Amean = mean(Aswitch),
				Alower = Amean - sd(Aswitch)/sqrt(n())*1.96,
				Aupper = Amean + sd(Aswitch)/sqrt(n())*1.96,
				NbFiles = n()))

}


{## plot the output

{# Alternation

summary_Amax$Type <- '1_Maximum' 
summary_Observed_A$Type <- '2_Observed' 
summary_Aswitch$Type <- '3_Switch' 
summary_out_Asim_within_df$Type <-'4_Within'
summary_out_Asim_among_df$Type <- '5_Among'

maxline <- 21

summary_A <- do.call(rbind, 
list(summary_Observed_A[1:maxline,],
summary_Amax[1:maxline,], 
summary_out_Asim_among_df[1:maxline,],
summary_out_Asim_within_df[1:maxline,],
summary_Aswitch[1:maxline,]))

summary_A$VisitRateDifference <- as.numeric(summary_A$VisitRateDifference)

{my_labels <- c(
"maximum possible",
"observed" , 
"after exchanging consecutive intervals to keep some autocorrelation",
"after 100 randomizations among nest watches", 
"after 100 randomizations within nest watches")}

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


Fig_A <- {ggplot(data=summary_A, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type, shape = Type, fill = Type))+
xlab("Visit rate difference between partners")+
ylab("Mean alternation")+

geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+

scale_colour_manual(values=my_colors,labels= my_labels)+ 

geom_point(size = 1.5, aes(fill = NULL)) +
geom_point(aes(shape=Type), size=1.5) +
scale_shape_manual(values=my_shapes,labels=my_labels)+  

scale_fill_manual(values = my_colors,labels=my_labels)+ 

scale_x_continuous(breaks = pretty(summary_A$VisitRateDifference, n = 12)) +
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

summary_Smax$Type <- '1_Maximum' 
summary_Observed_S$Type <- '2_Observed' 
summary_out_Ssim_within_df$Type <-'4_Within'
summary_out_Ssim_among_df$Type <- '5_Among'


summary_S <- do.call(rbind, 
list(
#summary_Smax[4:39,],
summary_Observed_S[4:39,],
summary_out_Ssim_among_df[4:39,],
summary_out_Ssim_within_df[4:39,]
))

summary_S$TotalProRate <- as.numeric(summary_S$TotalProRate)

{my_labelsS <- c(
#"maximum possible",
"observed" , 
"after 100 randomizations among nest watches", 
"after 100 randomizations within nest watches")}

{my_colorsS <- c(
#'grey',
'black',
'#56B4E9', # sky blue
'#0072B2')} # king blue

{my_shapesS <- c(
#21, #circle
21,
22, # square
25)} # triangle down

Fig_S <- {ggplot(data=summary_S, aes(x=TotalProRate, y=Smean, group=Type, colour=Type, shape = Type, fill = Type))+
xlab("Total provisioning rate from both partners")+
ylab("Mean synchrony")+

geom_line()+
geom_errorbar(aes(ymin=Slower, ymax=Supper),na.rm=TRUE)+

scale_colour_manual(values=my_colorsS,labels= my_labelsS)+ 

geom_point(size = 1.5, aes(fill = NULL)) +
geom_point(aes(shape=Type), size=1.5) +
scale_shape_manual(values=my_shapesS,labels=my_labelsS)+  

scale_fill_manual(values = my_colorsS,labels=my_labelsS)+ 

scale_x_continuous(breaks = pretty(summary_S$TotalProRate, n = 8)) +
scale_y_continuous(breaks = pretty(summary_S$Smean, n = 10)) +

theme_classic()+
theme(
legend.justification= c(0,1),
legend.position = c(0,1), 
legend.title = element_blank(),
legend.background = element_rect(colour = "black"),
panel.border = element_rect(colour = "black", fill=NA),
axis.title=element_text(size=14,face="bold"))

}

}

}

}

Fig_A
dev.new()
Fig_S


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### create MY_TABLE_perDVD: where both parents known + add expected alternation from simulation
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

MY_TABLE_perDVD <- MY_tblParentalCare[,c("DVDRef","MVisit1","FVisit1","FVisit1RateH","MVisit1RateH","VisitRateDifference","TotalProRate","NbAlternation","AlternationValue", "NbSynchro_ChickFeedingEquanim", "NbSynchro_LessConspicuous", "SynchronyFeedValue","SynchronyMvtValue","NbSynchroFemaleStart", "PropSynchroFemaleStart","MmeanDuration","FmeanDuration")]
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","ChickAge","ChickAgeCat","DVDdate","RelTimeHrs")], by='DVDRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, 
y=MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401","SocialMumID","SocialDadID","NbRinged","DadAge","MumAge","ParentsAge",
"MBroodNb","MPriorResidence","MnextNBsame", "MwillDivorce","MwillDivorceforEx",
"FBroodNb","FPriorResidence","FnextNBsame","FwillDivorce","FwillDivorceforEx","PairID","PairBroodNb","PairIDYear", "AvgMass", "MinMass", "AvgTarsus")], by='BroodRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblChicks_byRearingBrood[,c("RearingBrood", "sdMass", "sdTarsus", "MixedBroodYN")], by.x="BroodRef", by.y="RearingBrood", all.x=TRUE)

length(unique(MY_TABLE_perDVD$BroodRef[is.na(MY_TABLE_perDVD$SocialMum) | is.na(MY_TABLE_perDVD$SocialDadID)])) # 38 broods - 63 files one parent unknown

# MY_TABLE_perDVD2 <- do.call(merge, list(c(MY_tblParentalCare,MY_tblDVDInfo,MY_TABLE_perDVD)))
# MY_TABLE_perDVD2 <-Reduce(function(x,y) merge(x,y, all=TRUE), list(MY_tblParentalCare, MY_tblDVDInfo, MY_TABLE_perDVD))
# ncol(MY_TABLE_perDVD2)
# head(MY_TABLE_perDVD2)
# head(MY_TABLE_perDVD)

MY_TABLE_perDVD <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$SocialMumID) & !is.na(MY_TABLE_perDVD$SocialDadID),] # where both parents known
nrow(MY_TABLE_perDVD) # 1599 files
length(unique(MY_TABLE_perDVD$BroodRef)) # 872 broods
MY_TABLE_perDVD$MFVisit1 <- MY_TABLE_perDVD$FVisit1+ MY_TABLE_perDVD$MVisit1
MY_TABLE_perDVD$MFmeanDuration <- (MY_TABLE_perDVD$FmeanDuration+MY_TABLE_perDVD$MmeanDuration)/2
MY_TABLE_perDVD$NbSynchroMaleStart <- MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim - MY_TABLE_perDVD$NbSynchroFemaleStart

# scatter.smooth(MY_TABLE_perDVD$FVisit1, MY_TABLE_perDVD$FmeanDuration)
# scatter.smooth(MY_TABLE_perDVD$MVisit1, MY_TABLE_perDVD$MmeanDuration)
# scatter.smooth(MY_TABLE_perDVD$MFVisit1, MY_TABLE_perDVD$MFmeanDuration)
# hist( MY_TABLE_perDVD$FmeanDuration, breaks=40)
# hist( MY_TABLE_perDVD$MmeanDuration, breaks=40)


{# add MeanAsim and Adev

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MeanAsim = rowMeans(head(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$AlternationValue - MY_TABLE_perDVD$MeanAsim # reversed 22/06/2016

}

{# add MeanSsim and Sdev

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawInterfeeds$DVDRef),MeanSsim = rowMeans(tail(A_S_within_randomization,length(unique(RawInterfeeds$DVDRef))))), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$SynchronyFeedValue - MY_TABLE_perDVD$MeanSsim 

#hist(MY_TABLE_perDVD$Sdev, breaks=50)

}



{# add meanAge and DeltaAge for testing within and between individual effect of age

MY_TABLE_perDVD <- MY_TABLE_perDVD %>%
  group_by(SocialDadID)%>%
  mutate(meanDadAge = mean(DadAge), DeltaDadAge = DadAge-mean(DadAge),FirstDadReproAge = min(DadAge), LastDadReproAge = max(DadAge))
  
MY_TABLE_perDVD <- MY_TABLE_perDVD %>%
  group_by(SocialMumID)%>%
  mutate(meanMumAge = mean(MumAge), DeltaMumAge = MumAge-mean(MumAge),FirstMumReproAge = min(MumAge), LastMumReproAge = max(MumAge))

  
MY_TABLE_perDVD <- as.data.frame(MY_TABLE_perDVD) 
}

MY_TABLE_perDVD$TotalProRatePerChick <- round(MY_TABLE_perDVD$TotalProRate /MY_TABLE_perDVD$DVDInfoChickNb,2)


MY_TABLE_perDVD$BroodRef <- as.factor(MY_TABLE_perDVD$BroodRef)
MY_TABLE_perDVD$SocialDadID <- as.factor(MY_TABLE_perDVD$SocialDadID)
MY_TABLE_perDVD$SocialMumID <- as.factor(MY_TABLE_perDVD$SocialMumID)
MY_TABLE_perDVD$PairID <- as.factor(MY_TABLE_perDVD$PairID)
MY_TABLE_perDVD$BreedingYear <- as.factor(MY_TABLE_perDVD$BreedingYear)

}

head(MY_TABLE_perDVD)


{### create MY_TABLE_perBrood
MY_TABLE_perDVD[is.na(MY_TABLE_perDVD$MFVisit1RateH),]
summary(MY_TABLE_perDVD$MFVisit1RateH)

MY_TABLE_perBrood <- split(MY_TABLE_perDVD,MY_TABLE_perDVD$BroodRef)
	# MY_TABLE_perBrood[[1]]

MY_TABLE_perBrood_fun = function(x)  {

return(c(
mean(x$TotalProRate), # TotalProRate
mean(x$AlternationValue), #MeanA
mean(x$AlternationValue)-mean(x$MeanAsim), # MeanAdev  # reversed 22/06/2016
mean(x$VisitRateDifference), # MeanDiffVisit1Rate
mean(x$SynchronyFeedValue), # MeanSynchroFeed
mean(x$NbSynchro_ChickFeedingEquanim), # MeanSynchroFeed_nb
mean(x$MFVisit1), # MeanMFVisit1
mean(x$SynchronyFeedValue) - mean(x$MeanSsim), # MeanSdev
mean(x$MVisit1RateH), #MeanMVisit1RateH
mean(x$FVisit1RateH), #MeanFVisit1RateH
mean(x$DVDInfoChickNb) # MeanDVDInfoChickNb

))
}

MY_TABLE_perBrood_out1 <- lapply(MY_TABLE_perBrood, FUN=MY_TABLE_perBrood_fun)
MY_TABLE_perBrood_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perBrood_out1)),do.call(rbind, MY_TABLE_perBrood_out1))

nrow(MY_TABLE_perBrood_out2)	# 872
rownames(MY_TABLE_perBrood_out2) <- NULL
colnames(MY_TABLE_perBrood_out2) <- c('BroodRef','TotalProRate','MeanA', 'MeanAdev','MeanDiffVisit1Rate','MeanSynchroFeed','MeanSynchroFeed_nb','MeanMFVisit1', 'MeanSdev','MeanMVisit1RateH','MeanFVisit1RateH','MeanDVDInfoChickNb')

MY_TABLE_perBrood <- merge(y=unique(MY_TABLE_perDVD[,-which(names(MY_TABLE_perDVD) %in% c("DVDRef","FVisit1","FVisit1RateH","MVisit1","MVisit1RateH","DiffVisit1Rate","MFVisit1RateH","MFVisit1",
																							"NbAlternation","AlternationValue","MeanAsim", "Adev","AMax","PropSynchroFemaleStart","MmeanDuration","FmeanDuration","MFmeanDuration","NbSynchroFemaleStart", "NbSynchroMaleStart",
																							"NbSynchro_ChickFeedingEquanim","NbSynchro_LessConspicuous","SynchronyFeedValue","SynchronyMvtValue",
																							"DVDInfoChickNb","ChickAge","ChickAgeCat","DVDdate","RelTimeHrs", "TotalProRatePerChick","RatioObsvMax", "MeanSsim","Sdev"))]),
							x=MY_TABLE_perBrood_out2,all.x=TRUE, by='BroodRef')

			
{# calculate residual mass on tarsus

ResMassTarsus <-  cbind( MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus),"BroodRef" ], 
											data.frame(residuals(lm(AvgMass~ AvgTarsus, data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus), ]))))
colnames(ResMassTarsus) <- c("BroodRef" , "ResMassTarsus")
head(ResMassTarsus)

MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=ResMassTarsus, all.x=TRUE, by = "BroodRef")

}

nrow(MY_TABLE_perBrood) # 872

{# add ratioRingedHatched
BroodPercSurvived <- MY_tblBroods %>% group_by(BroodRef) %>% summarize(round(NbRinged/NbHatched*100,2))
colnames(BroodPercSurvived) <- c("BroodRef", "RatioNbRingedNbHatched")
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood,y=BroodPercSurvived, all.x=TRUE)
MY_TABLE_perBrood$RatioNbRingedNbHatched <- as.numeric(as.character(MY_TABLE_perBrood$RatioNbRingedNbHatched))
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$RatioNbRingedNbHatched),]
MY_tblBroods[MY_tblBroods$BroodRef==969,] # could have 2 hatchling  - not sure

hist(MY_TABLE_perBrood$RatioNbRingedNbHatched)
}

{#summary nb of broods
Mums_brood <- MY_TABLE_perBrood %>% group_by(SocialMumID)%>% summarise(n_distinct(BroodRef))
Dads_brood <- MY_TABLE_perBrood %>% group_by(SocialDadID)%>% summarise(n_distinct(BroodRef))
summary(Mums_brood[!is.na(Mums_brood$SocialMumID),2])
summary(Dads_brood[!is.na(Dads_brood$SocialDadID),2])
}

# difference in visit rate decline with pairbrood nb ?

scatter.smooth(MY_TABLE_perBrood$PairBroodNb,MY_TABLE_perBrood$MeanDiffVisit1Rate)


}

head(MY_TABLE_perBrood)

{### create MY_TABLE_perChick
nrow(MY_tblChicks[is.na(MY_tblChicks$AvgOfMass),]) # 0
nrow(MY_tblChicks[is.na(MY_tblChicks$AvgOfTarsus),]) # 79

MY_TABLE_perChick <- merge(x= MY_tblChicks , y=MY_TABLE_perBrood[,c("BroodRef", "NbRinged","MeanA","MeanAdev","TotalProRate","MeanSdev", "SocialMumID","SocialDadID","PairID","BreedingYear","HatchingDayAfter0401", "PairBroodNb")]
							, by.x="RearingBrood", by.y = "BroodRef", all.x=TRUE )
MY_TABLE_perChick <- MY_TABLE_perChick[!is.na(MY_TABLE_perChick$BreedingYear),]
MY_TABLE_perChick$GenPairID <- paste(MY_TABLE_perChick$sire, MY_TABLE_perChick$dam, sep="")

ResMassTarsus_perChick <-  cbind( MY_TABLE_perChick[!is.na(MY_TABLE_perChick$AvgOfTarsus),"ChickID" ], 
											data.frame(residuals(lm(AvgOfMass~ AvgOfTarsus, data = MY_TABLE_perChick[!is.na(MY_TABLE_perChick$AvgOfTarsus), ]))))
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

{### create MY_TABLE_perBirdYear

{# get both sex piled up
MY_TABLE_Survival <- merge(x=MY_TABLE_perDVD[c("BroodRef","AlternationValue","SocialMumID","SocialDadID","DadAge","MumAge","PairID","BreedingYear","FVisit1RateH","MVisit1RateH")],
						  y=sys_LastSeenAlive[,c("BirdID","LastYearAlive")],
						  by.x="SocialMumID", by.y="BirdID",
						  all.x=TRUE)
MY_TABLE_Survival <- merge(x=MY_TABLE_Survival, 
						  y=sys_LastSeenAlive[,c("BirdID","LastYearAlive")],
						  by.x="SocialDadID", by.y="BirdID",
						  all.x=TRUE)
						  
MY_TABLE_Survival$BreedingYear <- as.numeric(as.character(MY_TABLE_Survival$BreedingYear))
						  
MY_TABLE_Survival$FAliveNextYear <- as.numeric(MY_TABLE_Survival$LastYearAlive.x) > MY_TABLE_Survival$BreedingYear
MY_TABLE_Survival$MAliveNextYear <- as.numeric(MY_TABLE_Survival$LastYearAlive.y) > MY_TABLE_Survival$BreedingYear


MY_TABLE_Female_Survival <- MY_TABLE_Survival[,c("BroodRef","AlternationValue","SocialMumID","MumAge","PairID","BreedingYear","FAliveNextYear","FVisit1RateH")]
MY_TABLE_Male_Survival <- MY_TABLE_Survival[,c("BroodRef","AlternationValue","SocialDadID","DadAge","PairID","BreedingYear","MAliveNextYear","MVisit1RateH")]
MY_TABLE_Female_Survival$Sex <- 0
MY_TABLE_Male_Survival$Sex <- 1	
				
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "MumAge")] <- "Age"						
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "DadAge")] <- "Age"	
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "SocialMumID")] <- "BirdID"		
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "SocialDadID")] <- "BirdID"		
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "FAliveNextYear")] <- "AliveNextYear"		
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "MAliveNextYear")] <- "AliveNextYear"	
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "FVisit1RateH")] <- "Visit1RateH"		
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "MVisit1RateH")] <- "Visit1RateH"	


head(MY_TABLE_Female_Survival)
head(MY_TABLE_Male_Survival)

MY_TABLE_Survival_perBird <- rbind(MY_TABLE_Female_Survival,MY_TABLE_Male_Survival)	
MY_TABLE_Survival_perBird$BirdIDYear <- paste(MY_TABLE_Survival_perBird$BirdID, MY_TABLE_Survival_perBird$BreedingYear, sep="")
}

head(MY_TABLE_Survival_perBird)

{# get mean Alternation and pro rate per year per BirdID

MY_TABLE_perBirdYear <- split(MY_TABLE_Survival_perBird,MY_TABLE_Survival_perBird$BirdIDYear)
MY_TABLE_perBirdYear[[1]]

MY_TABLE_perBirdYear_fun = function(x)  {
return(c(mean(x$AlternationValue), #MeanAYear
mean(x$Visit1RateH))) #MeanVisit1RateHYear

}

MY_TABLE_perBirdYear_out1 <- lapply(MY_TABLE_perBirdYear, FUN=MY_TABLE_perBirdYear_fun)
MY_TABLE_perBirdYear_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perBirdYear_out1)),do.call(rbind, MY_TABLE_perBirdYear_out1))

nrow(MY_TABLE_perBirdYear_out2)	# 999
rownames(MY_TABLE_perBirdYear_out2) <- NULL
colnames(MY_TABLE_perBirdYear_out2) <- c('BirdIDYear','MeanAYear','MeanVisit1RateHYear')


MY_TABLE_perBirdYear <- merge(x=unique(MY_TABLE_Survival_perBird[,c("BirdID", "Age","PairID", "BreedingYear","AliveNextYear","Sex","BirdIDYear" )]),
							y=MY_TABLE_perBirdYear_out2,all.x=TRUE, by='BirdIDYear')
							
							
tspag = ggplot(MY_TABLE_perBirdYear, aes(x=Age, y=MeanAYear)) + 
  geom_line() + guides(colour=FALSE) + xlab("Bird's Age") +
  ylab("Mean Altnernation Value") + scale_x_continuous(breaks=1:9)
spag = tspag + aes(colour = factor(BirdID))
spag
spag + facet_wrap(~ Sex)
sspag = spag + stat_summary(fun.y=mean, colour="black", geom="line", size = 2)
sspag + facet_wrap(~ Sex)
}

{# descriptive stats on survival per year
FemaleSurvival <- list()
MaleSurvival <- list()
survivalperyear <- as.data.frame(table(MY_TABLE_perBirdYear$AliveNextYear, MY_TABLE_perBirdYear$BreedingYear, MY_TABLE_perBirdYear$Sex))

for (i in 2004:2015){
FemaleSurvival[i] <-  survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'TRUE']/  (survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'TRUE']+survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'FALSE'] ) 
MaleSurvival[i] <-  survivalperyear$Freq[survivalperyear$Var3 == 1 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'TRUE']/  (survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'TRUE']+survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'FALSE'] ) }

Survival <- as.data.frame(cbind(2004:2015,do.call(rbind,FemaleSurvival),do.call(rbind,MaleSurvival)))
colnames(Survival) <- c("Year", "FSurvival","MSurvival")

Survival$AvgSurvival <- round((Survival$FSurvival+Survival$MSurvival)*100/2,2)
mean(Survival$AvgSurvival) # 57.05417 # 58.11417 on 20160919

ggplot(Survival, aes(x=Year, y=AvgSurvival))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept=mean(Survival$AvgSurvival), size= 1, linetype= "dashed", colour="indianred")+
  ylim(0,100)+
  theme_classic()
}


# because we don't know for all birds if they survived until 2016:
MY_TABLE_perBirdYear <- MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear != 2015,]


}

head(MY_TABLE_perBirdYear)





## output_folder <- "R_Selected&SimulatedData"


## write.csv(MY_TABLE_perDVD, file = paste(output_folder,"R_MY_TABLE_perDVD.csv", sep="/"), row.names = FALSE) 
# 20161215













