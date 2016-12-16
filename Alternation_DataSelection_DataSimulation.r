#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 07/12/2016
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

library(dplyr) # for some part of the extraction of the data written by Andrew for the simulation 
library(ggplot2)
library(boot) # for Kat's simulation

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
colnames(tblChicks_byRearingBrood) <- c("RearingBrood","sdMass", "sdTarsus", "NbChicksMeasured", "NbChicksMeasuredCrossFostered")
tblChicks_byRearingBrood$MixedBroodYN <- tblChicks_byRearingBrood$NbChicksMeasured != tblChicks_byRearingBrood$NbChicksMeasuredCrossFostered
head(tblChicks_byRearingBrood)

MY_tblChicks_byRearingBrood <- tblChicks_byRearingBrood[tblChicks_byRearingBrood$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

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


{#### Simulation random alternation vs observed alternation



{### simulation alternation and synchrony: shuffling intervals within files where both sex visit at least once

## I think it could be still interesting to remove extreme values of provisioning rate (not normal to have just one visit, or 50...)
## I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

RawFeedingVisitsBothSexes <- MY_RawFeedingVisits[,c('DVDRef','TstartFeedVisit','Sex','Interval')]
RawFeedingVisitsBothSexes$Sex <- as.numeric(RawFeedingVisitsBothSexes$Sex )


{# creation of i simulated dataset (and calculation of i Asim) for each j file

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
  
out_Asim_j = list()
out_Asim_i = list()
out_Ssim_j = list()
out_Ssim_i = list()
out_SsimFemale_j = list()
out_SsimFemale_i = list()

for (j in 1:length(unique(RawFeedingVisitsBothSexes$DVDRef))){

x <- split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[j]]

		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[2]] # a normal file
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[1]] # only one male visit
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[13]] # only 2 female visits > screw up the function 'sample'
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[['935']] # no female visits > now removed
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[15]] # only one male and one female visit

x <- x[order(x$TstartFeedVisit),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]


for (i in 1:100) # to increase up to 1000
{

x0sim <- x0
x1sim <- x1

x0sim$Interval <- c(0, sample_vector(x0sim$Interval[-1]))
#x0sim$TstartFeedVisit <- c(x0sim$TstartFeedVisit[1],x0sim$TstartFeedVisit[-nrow(x0sim)]+x0sim$Interval[-1])
x0sim$TstartFeedVisit <- c(x0sim$TstartFeedVisit[1] + cumsum(x0sim$Interval)) # corrected 20161024 

x1sim$Interval <- c(0, sample_vector(x1sim$Interval[-1]))
#x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])
x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1] + cumsum(x1sim$Interval)) # corrected 20161024 


xsim <- rbind(x0sim,x1sim)
xsim <- xsim[order(xsim$TstartFeedVisit),]
xsim$NextSexSame <- c(xsim$Sex[-1],NA) == xsim$Sex
xsim$NextTstartafterhalfminTstart <-  c(xsim$TstartFeedVisit[-1],NA) <= xsim$TstartFeedVisit +0.5 &  c(xsim$TstartFeedVisit[-1],NA) >= xsim$TstartFeedVisit # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?


Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)
Ssim <- round( (length(xsim$NextSexSame[xsim$NextSexSame == FALSE & !is.na(xsim$NextSexSame) 
		& xsim$NextTstartafterhalfminTstart == TRUE & !is.na(xsim$NextTstartafterhalfminTstart)]) / (nrow(xsim) -1) ) *100   ,2)
SsimFemale <- length(xsim$NextSexSame[xsim$NextSexSame == FALSE & !is.na(xsim$NextSexSame) 
		& xsim$NextTstartafterhalfminTstart == TRUE & !is.na(xsim$NextTstartafterhalfminTstart) & xsim$Sex == 0])	

out_Asim_i[i] <- Asim
out_Asim_j[j] <- list(unlist(out_Asim_i))

out_Ssim_i[i] <- Ssim
out_Ssim_j[j] <- list(unlist(out_Ssim_i))

out_SsimFemale_i[i] <- SsimFemale
out_SsimFemale_j[j] <- list(unlist(out_SsimFemale_i))


		# clean up
		x0sim <- NULL
		x1sim <- NULL
		Asim <- NULL
		Ssim <- NULL
		SsimFemale <- NULL
}

		# clean up
		x <- NULL
		x0 <- NULL
		x1 <- NULL

}

out_Asim <- do.call(rbind, out_Asim_j)
out_Ssim <- do.call(rbind, out_Ssim_j)
out_SsimFemale <- do.call(rbind, out_SsimFemale_j)

}

head(out_Asim)
head(out_Ssim)
head(out_SsimFemale)

{# out A sim summary

out_Asim_df <- data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef), out_Asim)
out_Asim_df <- merge(x=out_Asim_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

out_Asim_df_perDiffVisit1Rate <- split(out_Asim_df,out_Asim_df$DiffVisit1Rate)

 # x <-out_Asim_df_perDiffVisit1Rate[[31]]
 # x <-out_Asim_df_perDiffVisit1Rate[[30]] # just one file


out_Asim_df_perDiffVisit1Rate_fun <- function(x) {

x <- x[,-1]
x <- x[,-ncol(x)]
v <- unlist(list(x))

return(c(
mean(v), # Amean
mean(v) - sd(v)/sqrt(length(v))*1.96, # Alower
mean(v) + sd(v)/sqrt(length(v))*1.96, # Aupper
nrow(x) # NbFiles
))
}

out_Asim_df_perDiffVisit1Rate_out1 <- lapply(out_Asim_df_perDiffVisit1Rate,out_Asim_df_perDiffVisit1Rate_fun)
out_Asim_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_Asim_df_perDiffVisit1Rate_out1)),do.call(rbind, out_Asim_df_perDiffVisit1Rate_out1))

nrow(out_Asim_df_perDiffVisit1Rate_out2)	# 33
rownames(out_Asim_df_perDiffVisit1Rate_out2) <- NULL
colnames(out_Asim_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

head(out_Asim_df_perDiffVisit1Rate_out2)


{# out S sim summary

out_Ssim_df <- data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef), out_Ssim)
out_Ssim_df <- merge(x=out_Ssim_df, y= MY_tblParentalCare[,c('DVDRef','MFVisit1RateH')], by='DVDRef', all.x =TRUE)

out_Ssim_df_per_MFVisit1RateH <- split(out_Ssim_df,out_Ssim_df$MFVisit1RateH)

 # x <-out_Ssim_df_per_MFVisit1RateH[[31]]
 # x <-out_Ssim_df_per_MFVisit1RateH[[30]] # just one file


out_Ssim_df_per_MFVisit1RateH_fun <- function(x) {

x <- x[,-1]
x <- x[,-ncol(x)]
v <- unlist(list(x))

return(c(
mean(v), # Smean
mean(v) - sd(v)/sqrt(length(v))*1.96, # Slower
mean(v) + sd(v)/sqrt(length(v))*1.96, # Supper
nrow(x) # NbFiles
))
}

out_Ssim_df_per_MFVisit1RateH_out1 <- lapply(out_Ssim_df_per_MFVisit1RateH,out_Ssim_df_per_MFVisit1RateH_fun)
out_Ssim_df_per_MFVisit1RateH_out2 <- data.frame(rownames(do.call(rbind,out_Ssim_df_per_MFVisit1RateH_out1)),do.call(rbind, out_Ssim_df_per_MFVisit1RateH_out1))

nrow(out_Ssim_df_per_MFVisit1RateH_out2)	# 59
rownames(out_Ssim_df_per_MFVisit1RateH_out2) <- NULL
colnames(out_Ssim_df_per_MFVisit1RateH_out2) <- c('TotalProRate','Smean','Slower','Supper','NbFiles')

}

head(out_Ssim_df_per_MFVisit1RateH_out2)



{# summary Aobserved when both sexes visit

MY_tblParentalCare_perVisitRateDiff_bothSexes <- group_by(MY_tblParentalCare, DiffVisit1Rate)

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes <- summarise (MY_tblParentalCare_perVisitRateDiff_bothSexes,
					Amean = mean(AlternationValue),
					Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
					NbFiles = n())
					
Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes <- dplyr::rename(Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes,VisitRateDifference= DiffVisit1Rate)

}

as.data.frame(Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes)

{# summary Sobserved when both sexes visit

MY_tblParentalCare_perTotalProRate_bothSexes <- group_by(MY_tblParentalCare, MFVisit1RateH)

Summary_MY_tblParentalCare_perTotalProRate_bothSexes <- summarise (MY_tblParentalCare_perTotalProRate_bothSexes,
					Smean = mean(SynchronyFeedValue),
					Slower = Smean - sd(SynchronyFeedValue)/sqrt(n())*1.96,
					Supper = Smean + sd(SynchronyFeedValue)/sqrt(n())*1.96,
					NbFiles = n())
					
Summary_MY_tblParentalCare_perTotalProRate_bothSexes <- dplyr::rename(Summary_MY_tblParentalCare_perTotalProRate_bothSexes,TotalProRate= MFVisit1RateH)

}

as.data.frame(Summary_MY_tblParentalCare_perTotalProRate_bothSexes)



{# A: for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes$Type <- "Observed"
out_Asim_df_perDiffVisit1Rate_out2$Type <- "Expected"


VisitRateDiff_Amean_bis <- as.data.frame(rbind( Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes[1:21,],out_Asim_df_perDiffVisit1Rate_out2[1:21,] ))
VisitRateDiff_Amean_bis$VisitRateDifference <- as.numeric(VisitRateDiff_Amean_bis$VisitRateDifference)



Fig1bis <- ggplot(data=VisitRateDiff_Amean_bis, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_bis$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_bis$Amean, n = 9)) +  
  theme_classic()
  
}

{# S: for the moment cut before 4 and after 40 total pro rate in both randomized and observed, and plot

Summary_MY_tblParentalCare_perTotalProRate_bothSexes$Type <- "Observed"
out_Ssim_df_per_MFVisit1RateH_out2$Type <- "Expected"


TotalProRate_Smean_bis <- as.data.frame(rbind( Summary_MY_tblParentalCare_perTotalProRate_bothSexes[4:39,],out_Ssim_df_per_MFVisit1RateH_out2[4:39,] ))
TotalProRate_Smean_bis$TotalProRate <- as.numeric(TotalProRate_Smean_bis$TotalProRate)



FigS <- ggplot(data=TotalProRate_Smean_bis, aes(x=TotalProRate, y=Smean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Slower, ymax=Supper))+
  xlab("Total provisioning rate")+
  ylab("Mean synchrony")+
  scale_colour_manual(values=c("orange", "grey"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(TotalProRate_Smean_bis$TotalProRate, n = 20)) +
  scale_y_continuous(breaks = pretty(TotalProRate_Smean_bis$Smean, n = 20)) +  
  theme_classic()
  
 
FigSppt_observed <- ggplot(data=TotalProRate_Smean_bis[TotalProRate_Smean_bis$Type == "Observed",], aes(x=TotalProRate, y=Smean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Slower, ymax=Supper))+
  xlab("Total provisioning rate")+
  ylab("Mean synchrony")+
  scale_colour_manual(values=c("black"), labels=c("95% Observed"))+
  scale_x_continuous(breaks = pretty(TotalProRate_Smean_bis$TotalProRate, n = 9)) +
  scale_y_continuous(breaks = pretty(TotalProRate_Smean_bis$Smean, n = 10)) +  
  theme_classic() + theme(legend.position="none")
  
  
FigS_pptexpected <- ggplot(data=TotalProRate_Smean_bis, aes(x=TotalProRate, y=Smean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Slower, ymax=Supper))+
  xlab("Total provisioning rate")+
  ylab("Mean synchrony")+
  scale_colour_manual(values=c("#56B4E9", "white"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(TotalProRate_Smean_bis$TotalProRate, n = 9)) +
  scale_y_continuous(breaks = pretty(TotalProRate_Smean_bis$Smean, n = 10)) +  
  theme_classic()+ theme(legend.position="none")
  
  
FigS_pptcomplete <- ggplot(data=TotalProRate_Smean_bis, aes(x=TotalProRate, y=Smean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Slower, ymax=Supper))+
  xlab("Total provisioning rate")+
  ylab("Mean synchrony")+
  scale_colour_manual(values=c("#56B4E9", "black"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(TotalProRate_Smean_bis$TotalProRate, n = 9)) +
  scale_y_continuous(breaks = pretty(TotalProRate_Smean_bis$Smean, n = 10)) +  
  theme_classic()+ theme(legend.position="none")
  
}



}

Fig1bis


{### comparison both method of randomization
  
VisitRateDiff_Amean$TypeSim <- c(rep('ExpectedKat',20),rep('ObservedKat',20))
VisitRateDiff_Amean_bis$TypeSim <- c(rep('ObservedMalika',21),rep('ExpectedMalika',21))
VisitRateDiff_Amean_for_comparison <- rbind(VisitRateDiff_Amean[VisitRateDiff_Amean$TypeSim != 'ObservedKat',],VisitRateDiff_Amean_bis[,-5])


Fig1comparison <- ggplot(data=VisitRateDiff_Amean_for_comparison, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','grey'), labels=c("95% Expected Kat", "95% Expected Malika","95% Observed"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$Amean, n = 9)) +  
theme_classic()
}  
  
Fig1comparison 


{### simulation alternation keeping some temporal autocorrelation: shuffling consecutives intervals within one individual 

head(RawFeedingVisitsBothSexes)

{# creation of i simulated dataset (and calculation of i Asim) for each j file

is.even <- function(x) x %% 2 == 0 

out_Ashift_j = list()

for (j in 1:length(unique(RawFeedingVisitsBothSexes$DVDRef))){

x <- split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[j]]

		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[2]] # a normal file
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[1]] # only one male visit
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[13]] # only 2 female visits > screw up the function 'sample'
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[['935']] # no female visits > now removed
		# split(RawFeedingVisitsBothSexes,RawFeedingVisitsBothSexes$DVDRef)[[15]] # only one male and one female visit

x <- x[order(x$TstartFeedVisit),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]



x1sim <- x1

if (nrow(x1) > 1){

x1simInterval <- c(x1$Interval,x1$Interval[nrow(x1)])

for (i in 2:nrow(x1sim))
{ if (is.even(i)){x1sim$Interval[i] <- x1simInterval[i+1]}
else {x1sim$Interval[i] <- x1simInterval[i-1]}
}

x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])

}


xsim <- rbind(x0,x1sim)
xsim <- xsim[order(xsim$TstartFeedVisit),]

Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)

out_Ashift_j[j] <- Asim

		# clean up
		x1sim <- NULL
		Asim <- NULL
		x <- NULL
		x0 <- NULL
		x1 <- NULL

}

out_Ashift <- do.call(rbind, out_Ashift_j)

}

head(out_Ashift)

{# out A sim summary

out_Ashift_df <- data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef), out_Ashift)
out_Ashift_df <- merge(x=out_Ashift_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

out_Ashift_df_perDiffVisit1Rate <- split(out_Ashift_df,out_Ashift_df$DiffVisit1Rate)
x <- out_Ashift_df_perDiffVisit1Rate[[1]]

out_Ashift_df_perDiffVisit1Rate <- split(out_Ashift_df,out_Ashift_df$DiffVisit1Rate)
x <- out_Ashift_df_perDiffVisit1Rate[[1]]

out_Ashift_df_perDiffVisit1Rate_fun <- function(x) {

x <- x[,-1]
v <- x[,-ncol(x)]


return(c(
mean(v), # Amean
mean(v) - sd(v)/sqrt(length(v))*1.96, # Alower
mean(v) + sd(v)/sqrt(length(v))*1.96, # Aupper
nrow(x) # NbFiles
))
}

out_Ashift_df_perDiffVisit1Rate_out1 <- lapply(out_Ashift_df_perDiffVisit1Rate,out_Ashift_df_perDiffVisit1Rate_fun)
out_Ashift_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_Ashift_df_perDiffVisit1Rate_out1)),do.call(rbind, out_Ashift_df_perDiffVisit1Rate_out1))

nrow(out_Ashift_df_perDiffVisit1Rate_out2)	# 33
rownames(out_Ashift_df_perDiffVisit1Rate_out2) <- NULL
colnames(out_Ashift_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

head(out_Ashift_df_perDiffVisit1Rate_out2)

{### comparison both method of randomization

out_Ashift_df_perDiffVisit1Rate_out2_forcomparison <- out_Ashift_df_perDiffVisit1Rate_out2[1:21,-5]
out_Ashift_df_perDiffVisit1Rate_out2_forcomparison$Type <- 'Expected'
out_Ashift_df_perDiffVisit1Rate_out2_forcomparison$TypeSim <- 'ExpectedTempoAuto'
VisitRateDiff_Amean_for_comparison_ter <- rbind(VisitRateDiff_Amean_for_comparison,out_Ashift_df_perDiffVisit1Rate_out2_forcomparison)
VisitRateDiff_Amean_for_comparison_ter$VisitRateDifference <- as.numeric(as.character(VisitRateDiff_Amean_for_comparison_ter$VisitRateDifference))

Fig1comparisonbis <- ggplot(data=VisitRateDiff_Amean_for_comparison_ter, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','green','grey'), labels=c("95% Expected Kat (100 random.)", "95% Expected Malika (100 random.)", "95% Expected Autocor (1 random.)","95% Observed"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$Amean, n = 9)) +  
theme_classic()
}  
  


}

 

}
















#### Simulation 

## the Functions used several times

Summarise_A_perVisitRateDifference <- function(DataSummary) {

return(data.frame(summarise (group_by(DataSummary, VisitRateDifference),
				Amean = mean(AlternationValue),
				Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
				Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
				NbFiles = n())))
}


{## the Data

RawInterfeeds <- MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')]
colnames(RawInterfeeds)[which(names(RawInterfeeds) == "TstartFeedVisit")] <- "Tstart"		
head(RawInterfeeds)

RawInterfeeds_with_ProRate <- merge(x=RawInterfeeds, y=MY_tblParentalCare[c('DVDRef','FVisit1RateH','MVisit1RateH','DiffVisit1Rate')])

FRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 0,])
MRawInterfeeds_with_ProRate <- subset(RawInterfeeds_with_ProRate[RawInterfeeds_with_ProRate$Sex == 1,])

# save first Tstart of each file and each sex  (with interval = 0)
F_FirstTstart <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1)
M_FirstTstart <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(1)

# remove the first line with interval (=0) from each file for each sex before shuffling interval
FData_to_Shuffle <- FRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)
MData_to_Shuffle <- MRawInterfeeds_with_ProRate %>% group_by(DVDRef) %>% slice(-1)
}


### among nest watch, within individual with same provisioning rate and same sex

NreplicatesAmongFileRandomization = 100

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
Asim <- round(length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)])/(nrow(x) -1) *100,1) # AlternationValue
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony value within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- round( (length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)]) / (nrow(x) -1) ) *100   ,2)
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first hald of the rows, Ssim of each DVD, second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

Out_A_S_sim_Among <- do.call(cbind,replicate(NreplicatesAmongFileRandomization,randomization_among_nest_watch_and_calculate_A_S_fun(FData_to_Shuffle, MData_to_Shuffle),simplify=FALSE ) )

# first half are A sim
out_Asim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), head(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))
out_Asim_among_df <- merge(x=out_Asim_among_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

# second Half are S sim
out_Ssim_among_df <- data.frame(DVDRef = unique(RawInterfeeds$DVDRef), tail(Out_A_S_sim_Among,length(unique(RawInterfeeds$DVDRef))))
out_Ssim_among_df <- merge(x=out_Asim_among_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

# Summarise A ans S sim per visit rate difference

summarise_Sim_Among <- function(out_sim_among_df, AS){

out_A_or_S_among_sim_df_perDiffVisit1Rate_fun <- function(x) {

x <- x[,-1]
x <- x[,-ncol(x)]
v <- unlist(list(x))

return(c(
mean(v), # Amean OR Smean
mean(v) - sd(v)/sqrt(length(v))*1.96, # Alower OR Slower
mean(v) + sd(v)/sqrt(length(v))*1.96, # Aupper OR Supper
nrow(x) # NbFiles
))
}

out_among_df_perDiffVisit1Rate_out1 <- lapply(split(out_sim_among_df,out_sim_among_df$DiffVisit1Rate),out_A_or_S_among_sim_df_perDiffVisit1Rate_fun)
out_among_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_among_df_perDiffVisit1Rate_out1)),do.call(rbind, out_among_df_perDiffVisit1Rate_out1))

nrow(out_among_df_perDiffVisit1Rate_out2)	# 33
rownames(out_among_df_perDiffVisit1Rate_out2) <- NULL

if (AS == "A")
{
colnames(out_among_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')
}

if (AS == "S")
{
colnames(out_among_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Smean','Slower','Supper','NbFiles')
}


return(out_among_df_perDiffVisit1Rate_out2)

}


summarise_Aim_Among(out_Asim_among_df,'A')
summarise_Sim_Among(out_Ssim_among_df,'S')














{# out A and S sim Among summary
out_Asim_among_df_perDiffVisit1Rate_out1 <- lapply(split(out_Asim_among_df,out_Asim_among_df$DiffVisit1Rate),out_A_or_S_among_sim_df_perDiffVisit1Rate_fun)
out_Asim_among_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_Asim_among_df_perDiffVisit1Rate_out1)),do.call(rbind, out_Asim_among_df_perDiffVisit1Rate_out1))

nrow(out_Asim_among_df_perDiffVisit1Rate_out2)	# 33
rownames(out_Asim_among_df_perDiffVisit1Rate_out2) <- NULL
colnames(out_Asim_among_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

summary_out_Asim_Among <- out_Asim_among_df_perDiffVisit1Rate_out2

out_Asim_among_df_perDiffVisit1Rate_out1 <- lapply(split(out_Asim_among_df,out_Asim_among_df$DiffVisit1Rate),out_A_or_S_among_sim_df_perDiffVisit1Rate_fun)
out_Asim_among_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_Asim_among_df_perDiffVisit1Rate_out1)),do.call(rbind, out_Asim_among_df_perDiffVisit1Rate_out1))

nrow(out_Asim_among_df_perDiffVisit1Rate_out2)	# 33
rownames(out_Asim_among_df_perDiffVisit1Rate_out2) <- NULL
colnames(out_Asim_among_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

summary_out_Asim_Among <- out_Asim_among_df_perDiffVisit1Rate_out2

}



{# A observed summary

MY_tblParentalCare_perVisitRateDiff_bothSexes <- group_by(MY_tblParentalCare, DiffVisit1Rate)

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes <- summarise (MY_tblParentalCare_perVisitRateDiff_bothSexes,
					Amean = mean(AlternationValue),
					Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
					NbFiles = n())
					
Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes <- dplyr::rename(Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes,VisitRateDifference= DiffVisit1Rate)

}


{# A: for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes$Type <- "Observed"
out_Asim_among_df_perDiffVisit1Rate_out2$Type <- "Expected"


VisitRateDiff_Amean_ter <- as.data.frame(rbind( Summary_MY_tblParentalCare_perVisitRateDiff_bothSexes[1:21,],out_Asim_among_df_perDiffVisit1Rate_out2[1:21,] ))
VisitRateDiff_Amean_ter$VisitRateDifference <- as.numeric(VisitRateDiff_Amean_ter$VisitRateDifference)



Fig1bis <- ggplot(data=VisitRateDiff_Amean_ter, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_ter$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_ter$Amean, n = 9)) +  
  theme_classic()
  
}

{### comparison both method of randomization
  
VisitRateDiff_Amean_bis$TypeSim <- c(rep('ObservedWithin',21),rep('ExpectedWithin',21))
VisitRateDiff_Amean_ter$TypeSim <- c(rep('Observed',21),rep('ExpectedAmong',21))
VisitRateDiff_Amean_for_comparison <- rbind(VisitRateDiff_Amean_bis[VisitRateDiff_Amean_bis$TypeSim != 'ObservedWithin',],VisitRateDiff_Amean_ter)


Fig1comparison <- ggplot(data=VisitRateDiff_Amean_for_comparison, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','grey'), labels=c("95% Expected Among", "95% Expected Within","95% Observed"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$Amean, n = 9)) +  
theme_classic()
}  
  
Fig1comparison 




Randomize_Data_WithinFile_and_Calculate_AlternationValue <- function(Data, Type) {

RandomizeData_oneSplit <-  function(x){

if (Type == 'Scaled'){
x <- x[,c('DVDRef','Sex','splitID','ScaledInterval','ScaledTstart')]
names(x)[names(x) == 'ScaledTstart'] <- 'Tstart'
names(x)[names(x) == 'ScaledInterval'] <- 'Interval'
}

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
xsim$NextSexSame <- c(xsim$Sex[-1],NA) == xsim$Sex

return(round(length(xsim$NextSexSame[xsim$NextSexSame == FALSE & !is.na(xsim$NextSexSame)])/(nrow(xsim) -1) *100,1)) # AlternationValue

}


Asim <- data.frame(splitID = unique(Data$splitID), 
do.call(cbind, replicate(NreplicatesWithinFileRandomization,do.call(rbind,lapply(split(Data,Data$splitID),RandomizeData_oneSplit)),simplify=FALSE)))

return(Asim)

}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


{### create MY_TABLE_perDVD: where both parents known + add expected alternation from simulation
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

MY_TABLE_perDVD <- MY_tblParentalCare[,c("DVDRef","MVisit1","FVisit1","FVisit1RateH","MVisit1RateH","DiffVisit1Rate","MFVisit1RateH","NbAlternation","AlternationValue", "NbSynchro_ChickFeedingEquanim", "NbSynchro_LessConspicuous", "SynchronyFeedValue","SynchronyMvtValue","NbSynchroFemaleStart", "PropSynchroFemaleStart","MmeanDuration","FmeanDuration")]
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","ChickAge","ChickAgeCat","DVDdate","RelTimeHrs")], by='DVDRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, 
y=MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401","SocialMumID","SocialDadID","NbRinged","DadAge","MumAge","ParentsAge",
"MBroodNb","MPriorResidence","MnextNBsame", "MwillDivorce","MwillDivorceforEx",
"FBroodNb","FPriorResidence","FnextNBsame","FwillDivorce","FwillDivorceforEx","PairID","PairBroodNb","PairIDYear", "AvgMass", "MinMass", "AvgTarsus")], by='BroodRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblChicks_byRearingBrood[,c("RearingBrood", "sdMass", "sdTarsus", "MixedBroodYN")], by.x="BroodRef", by.y="RearingBrood", all.x=TRUE)

length(unique(MY_TABLE_perDVD$BroodRef[is.na(MY_TABLE_perDVD$SocialMum) | is.na(MY_TABLE_perDVD$SocialDadID)])) # 38 broods - 63 files one parent unknown


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

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef),MeanAsim = rowMeans(out_Asim)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

MY_TABLE_perDVD$Adev <-  MY_TABLE_perDVD$AlternationValue - MY_TABLE_perDVD$MeanAsim # reversed 22/06/2016

}

{# add MeanSsim and Sdev

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef),MeanSsim = rowMeans(out_Ssim)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

MY_TABLE_perDVD$Sdev <-  MY_TABLE_perDVD$SynchronyFeedValue - MY_TABLE_perDVD$MeanSsim 

#hist(MY_TABLE_perDVD$Sdev, breaks=50)

}


{# add Max A possible considering both birds provisioning rate - create figures with it
MY_TABLE_perDVD$AMax <- NA

for (i in 1:nrow(MY_TABLE_perDVD))
{
if((MY_TABLE_perDVD$FVisit1RateH[i] - MY_TABLE_perDVD$MVisit1RateH[i])==0)
{
MY_TABLE_perDVD$AMax[i] <- 
round((((min(MY_TABLE_perDVD$FVisit1RateH[i],MY_TABLE_perDVD$MVisit1RateH[i]))*2-1) / (MY_TABLE_perDVD$FVisit1RateH[i] + MY_TABLE_perDVD$MVisit1RateH[i] -1))*100,2) }

else{
MY_TABLE_perDVD$AMax[i] <- 
round((((min(MY_TABLE_perDVD$FVisit1RateH[i],MY_TABLE_perDVD$MVisit1RateH[i]))*2) / (MY_TABLE_perDVD$FVisit1RateH[i] + MY_TABLE_perDVD$MVisit1RateH[i] -1))*100,2) 
}
}

MY_TABLE_perDVD$RatioObsvMax <- round(MY_TABLE_perDVD$AlternationValue / MY_TABLE_perDVD$AMax *100)


{# add AMax to Figure 1

MY_TABLE_perDVD_perVisitRateDiff <- group_by(MY_TABLE_perDVD, DiffVisit1Rate)
Summary_MY_TABLE_perDVD_perVisitRateDiff <- summarise (MY_TABLE_perDVD_perVisitRateDiff, AMaxmean = mean(AMax))

Summary_MY_TABLE_perDVD_perVisitRateDiff <- cbind(as.data.frame(Summary_MY_TABLE_perDVD_perVisitRateDiff), rep("ZMaximum Alternation", nrow(Summary_MY_TABLE_perDVD_perVisitRateDiff)))
colnames(Summary_MY_TABLE_perDVD_perVisitRateDiff) <- c("VisitRateDifference","Amean","TypeSim")

VisitRateDiff_Amean_for_comparison_withAMax <- bind_rows(VisitRateDiff_Amean_for_comparison,as.data.frame(Summary_MY_TABLE_perDVD_perVisitRateDiff[1:21,]) )
as.data.frame(VisitRateDiff_Amean_for_comparison_withAMax)

Fig1comparison_withMax <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','grey', "black"), labels=c("95% Expected Kat", "95% Expected Malika","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax$Amean, n = 9)) +  
theme_classic()


}

Fig1comparison_withMax

{# add AMax to Figure 1 comparison bis

VisitRateDiff_Amean_for_comparison_withAMax_bis <- bind_rows(VisitRateDiff_Amean_for_comparison_ter,as.data.frame(Summary_MY_TABLE_perDVD_perVisitRateDiff[1:21,]) )
as.data.frame(VisitRateDiff_Amean_for_comparison_withAMax_bis)

Fig1comparison_withMax_bis <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','green','grey', "black"), labels=c("95% sim among watch (100 random.)", "95% sim within watch(100 random.)","95% sim within watch with autocor (1 random.)","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="bottom",legend.direction="vertical")


}

Fig1comparison_withMax_bis

{# create figures for ppt

# observed

Fig1comparison_withMax_bis_0 <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis[VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ObservedMalika" |VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ZMaximum Alternation" | VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ExpectedKat" ,],
 aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c( "white",'black', "white"), labels=c("bla", "95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


# observed and maximum

Fig1comparison_withMax_bis_a <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis[VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ObservedMalika" |VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ZMaximum Alternation" | VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ExpectedKat"  ,],
 aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c('white','black', "grey"), labels=c("bla","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


# among files > does take into account the interdependency of the intervals of a nest watch

Fig1comparison_withMax_bis_b <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis[VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ObservedMalika" |VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ZMaximum Alternation" | VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim == "ExpectedKat" ,],
aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("#0072B2", "black","grey"), labels=c("95% sim among watch (100 random.)", "95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


# within files > does not take into account the autocorrelation to environment variables

Fig1comparison_withMax_bis_c <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis[VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim != "ExpectedTempoAuto",], 
aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("#0072B2", '#56B4E9',"black","grey"), labels=c("95% sim among watch (100 random.)", "95% sim within watch(100 random.)","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


# switching two by two intervals, to keep some autocorrelation and not having to assume a relevant time window 
# > not a randomisation per see, just one created nest watch to compare with

Fig1comparison_withMax_bis_d <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("#0072B2", '#56B4E9','#009E73','black', "grey"), labels=c("95% sim among watch (100 random.)", "95% sim within watch(100 random.)","95% sim within watch with autocor (1 random.)","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")

# what will be used

Fig1comparison_withMax_bis_e <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis[VisitRateDiff_Amean_for_comparison_withAMax_bis$TypeSim != "ExpectedTempoAuto",], 
aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("white", '#56B4E9',"black","white"), labels=c("95% sim among watch (100 random.)", "95% sim within watch(100 random.)","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


# what will be used + autocorrelation

Fig1comparison_withMax_bis_f <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("white", '#56B4E9','#009E73','black', "white"), labels=c("95% sim among watch (100 random.)", "95% sim within watch(100 random.)","95% sim within watch with autocor (1 random.)","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


# among + within + autocorrelation + observed

Fig1comparison_withMax_bis_g <- ggplot(data=VisitRateDiff_Amean_for_comparison_withAMax_bis, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper),na.rm=TRUE)+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("#0072B2", '#56B4E9','#009E73','black', "white"), labels=c("95% sim among watch (100 random.)", "95% sim within watch(100 random.)","95% sim within watch with autocor (1 random.)","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()+
theme(legend.position="none")


}



}

{# add Smax to FigS

MY_TABLE_perDVD_perTotalProRate <- group_by(MY_TABLE_perDVD, MFVisit1RateH)
Summary_MY_TABLE_perDVD_perTotalProRate <- summarise (MY_TABLE_perDVD_perTotalProRate, SMaxmean = mean(AlternationValue))

Summary_MY_TABLE_perDVD_perTotalProRate <- cbind(as.data.frame(Summary_MY_TABLE_perDVD_perTotalProRate), rep("ZMaximum Synchrony", nrow(Summary_MY_TABLE_perDVD_perTotalProRate)))
colnames(Summary_MY_TABLE_perDVD_perTotalProRate) <- c("TotalProRate","Smean","Type")

TotalProRate_Smean_for_comparison_withSMax <- bind_rows(TotalProRate_Smean_bis,as.data.frame(Summary_MY_TABLE_perDVD_perTotalProRate[4:39,]) )
as.data.frame(TotalProRate_Smean_for_comparison_withSMax)

FigScomparison_withMax <- ggplot(data=TotalProRate_Smean_for_comparison_withSMax, aes(x=TotalProRate, y=Smean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Slower, ymax=Supper))+
  xlab("Total provisioning rate")+
  ylab("Mean synchrony")+
  scale_colour_manual(values=c("orange", "grey", "black"), labels=c("95% Expected", "95% Observed", "Maximum"))+
  scale_x_continuous(breaks = pretty(TotalProRate_Smean_for_comparison_withSMax$TotalProRate, n = 20)) +
  scale_y_continuous(breaks = pretty(TotalProRate_Smean_for_comparison_withSMax$Smean, n = 20)) +  
  theme_classic()
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

MY_TABLE_perDVD$TotalProRatePerChick <- round(MY_TABLE_perDVD$MFVisit1RateH /MY_TABLE_perDVD$DVDInfoChickNb,2)


MY_TABLE_perDVD$BroodRef <- as.factor(MY_TABLE_perDVD$BroodRef)
MY_TABLE_perDVD$SocialDadID <- as.factor(MY_TABLE_perDVD$SocialDadID)
MY_TABLE_perDVD$SocialMumID <- as.factor(MY_TABLE_perDVD$SocialMumID)
MY_TABLE_perDVD$PairID <- as.factor(MY_TABLE_perDVD$PairID)
MY_TABLE_perDVD$BreedingYear <- as.factor(MY_TABLE_perDVD$BreedingYear)

}

head(MY_TABLE_perDVD)
Fig1comparison_withMax_bis_d
FigScomparison_withMax

{### create MY_TABLE_perBrood
MY_TABLE_perDVD[is.na(MY_TABLE_perDVD$MFVisit1RateH),]
summary(MY_TABLE_perDVD$MFVisit1RateH)

MY_TABLE_perBrood <- split(MY_TABLE_perDVD,MY_TABLE_perDVD$BroodRef)
	# MY_TABLE_perBrood[[1]]

MY_TABLE_perBrood_fun = function(x)  {

return(c(
mean(x$MFVisit1RateH), # TotalProRate
mean(x$AlternationValue), #MeanA
mean(x$AlternationValue)-mean(x$MeanAsim), # MeanAdev  # reversed 22/06/2016
mean(x$DiffVisit1Rate), # MeanDiffVisit1Rate
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













