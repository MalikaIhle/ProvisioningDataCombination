#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 15/04/2015
#	 last modif : 05/05/2016  
#	 commit: modProRate + modAvgMass 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### remarks
# LastSeenAlive information needs to be updated manually when DB updated
# MY_tblBrood$Nb3 is the number of post fledgling
# MY_tblBrood Mass and tarsus info: the last measurement, at d12, when ringed. nMass, nTarsus, NbRinged should in principle be equal: maybe should consider small difference of age, i.e. include all brood or a standardized subsets
# 2 broods corresponding to 2 DVDs have both parents NA > removed
# 39 brood with one parent NA, corresponding to 66 files
# if tblDVDinfois updated > the date associated to DVDtime and to Sunrise will change
}

rm(list = ls(all = TRUE))

{### packages
library(dplyr)
library(ggplot2)
library(boot)
library(lme4)
library(rptR)
}


{### Get raw data (from source() or R_output folder)

# source('COMPILATION_PROVISIONING.R')
# or :

output_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_output"

MY_tblParentalCare <- read.csv(paste(output_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblBroods <- read.csv(paste(output_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
MY_tblDVDInfo <- read.csv(paste(output_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(output_folder,"R_MY_RawFeedingVisits.xlsx", sep="/")) # OF directly followed by IN are merged feeding visits ; will be used for simulation

}

{### select valid video files for studying behavioural compatibility in chick provisioning

list_non_valid_DVDRef <- 
c(MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$DVDInfoChickNb > 0],# 6 - where 0 chicks
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$ChickAge >5],# 906 - where still brooding (age <=5)
MY_tblParentalCare$DVDRef[(MY_tblParentalCare$MVisit1 ==0 | MY_tblParentalCare$FVisit1 ==0 )& !is.na(MY_tblParentalCare$DVDRef)], # 171 - one sex did not visit
MY_tblDVDInfo$DVDRef[ !MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef],# 2 - both parents unidentified
MY_tblParentalCare$DVDRef[is.na(MY_tblParentalCare$EffectiveTime)]) # 9 files with no visits at all

list_non_valid_DVDRef <- list_non_valid_DVDRef[!is.na(list_non_valid_DVDRef)]

MY_tblDVDInfo <- MY_tblDVDInfo[ ! MY_tblDVDInfo$DVDRef %in% list_non_valid_DVDRef,]
MY_tblParentalCare <- MY_tblParentalCare[ ! MY_tblParentalCare$DVDRef %in% list_non_valid_DVDRef,]
MY_RawFeedingVisits  <- MY_RawFeedingVisits[ ! MY_RawFeedingVisits$DVDRef %in% list_non_valid_DVDRef,]

{# fill in manually the data where Julia deleted it
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


}

{### sample sizes

nrow(MY_tblParentalCare) # 1768 DVD files ; = length(unique(MY_RawFeedingVisits$DVDRef)) = nrow(MY_tblDVDInfo) 
length(unique(MY_tblDVDInfo$BroodRef)) # 958 broods videotaped at least once
range(table(MY_tblDVDInfo$BroodRef)) # range from 1 to 3
mean(table(MY_tblDVDInfo$BroodRef)) # on average 1.8 videos per brood watched

}

}

head(MY_tblBroods) 
head(MY_tblDVDInfo) 
head(MY_tblParentalCare)
head(MY_RawFeedingVisits)

  
{### create MY_TABLE
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

MY_TABLE <- MY_tblParentalCare[,c("DVDRef","FVisit1RateH","MVisit1RateH","DiffVisit1Rate","MFVisit1RateH","AlternationValue")]
MY_TABLE <- merge(x=MY_TABLE, y=MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","ChickAgeCat","DVDdate","RelTimeHrs")], by='DVDRef')
MY_TABLE <- merge(x=MY_TABLE, 
y=MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401","SocialMumID","SocialDadID","NbRinged","DadAge","MumAge","ParentsAge",
"MPrevNbRinged","MBroodNb","MPriorResidence","MDivorce","MDivorceforEx",
"FPrevNbRinged","FBroodNb","FPriorResidence","FDivorce","FDivorceforEx","PairID","PairBroodNb","PairIDYear", "AvgMass", "MinMass", "AvgTarsus")], by='BroodRef')

MY_TABLE <- MY_TABLE[!is.na(MY_TABLE$SocialMumID) & !is.na(MY_TABLE$SocialDadID),] # where both parents known
nrow(MY_TABLE) # 1702 files
length(unique(MY_TABLE$BroodRef)) # 919 broods

}

head(MY_TABLE)





############################################ 
# replication Bebbington & Hatchwell study #
############################################

{#### Simulation random alternation vs observed alternation

{## ? keep the middle 90% of feeding rates for each sex ?

summary(MY_tblParentalCare$FVisit1RateH)
summary(MY_tblParentalCare$MVisit1RateH)
dev.new()
par(mfrow=c(2,1)) 
hist(MY_tblParentalCare$FVisit1RateH, xlim=c(0,50), ylim = c(0,1000))
hist(MY_tblParentalCare$MVisit1RateH, xlim=c(0,50), ylim = c(0,1000))

quantile(MY_tblParentalCare$FVisit1RateH[!is.na(MY_tblParentalCare$FVisit1RateH)], c(0.05,0.95))
quantile(MY_tblParentalCare$MVisit1RateH[!is.na(MY_tblParentalCare$MVisit1RateH)], c(0.05,0.95))

# MY_tblParentalCare <- MY_tblParentalCare[MY_tblParentalCare$FVisit1RateH >=3 
#                                          & MY_tblParentalCare$FVisit1RateH <= 22 &
#                                            MY_tblParentalCare$MVisit1RateH >=3 
#                                          & MY_tblParentalCare$MVisit1RateH <= 22,]
# 
# MY_RawFeedingVisits <- MY_RawFeedingVisits[ MY_RawFeedingVisits$DVDRef %in% unique(MY_tblParentalCare$DVDRef) ,]
# MY_tblDVDInfo <- MY_tblDVDInfo[ MY_tblDVDInfo$DVDRef %in% unique(MY_tblParentalCare$DVDRef),]                                     

nrow(MY_tblParentalCare) # 1499
}


{### simulation alternation with Kat's method

{## Description of Kat’ simulation described in the paper page 3 + supp figure and my point of view on it
 
# Alternation score fore observed nest watches:
# A = F/ (t-1)
# with F the number of alternation and t the number of feeding visits
# for a female provisioning rate x = 7 and a male provisioning rate y = 10, the number of feeding visits is 17 (if one hour was watched).

# Simulation steps:
# 1) select the provisioning rates for individuals of either sexes that are not too infrequent (remove the extreme low and high values of x or y)
# 2) extract all interfeed intervals for all individuals of a same provisioning rate and of a same sex and randomize them
# 3) (see supp fig) create all combinations of female and male with provisioning rate x and y by sampling without replacement x-1 female interfeed intervals and y-1 male interfeed intervals. Several simulated nest watches are possible per each provisioning rate combination, depending on the number of individuals with these provisioning rates that were observed . Simulated nest watches are created until one of the pool of interfeed intervals per each provisioning rate per sex is empty. Each pool of interfeed intervals, for instance the one for female with provisioning rate x, are reuse for each combination involving x.
# 4) (see supp fig) the cumulative sum of interfeed interval are calculated for each sex separately, and then rows are merged and sorted 

# 5) Alternation score for those simulated nest watch is then calculated with the formula:
# A = F/ (t-1)
# with F the number of alternation and t the number of feeding interval
# for a female provisioning rate x = 7 and a male provisioning rate y = 10, the number of feeding intervals is 15.

# 6) within each combination of provisioning rate combination, 10000 bootstraps of alternation scores were ran (this is what the paper says, but in fact the code Kat sent shows that the bootstrapping was made at a latter stage, see below)
# 7) All simulated alternation scores were pooled into groups of visit rate differences (absolute value of x minus y) [and the bootstrapping happened here in Kat’s code – but maybe it does not matter]
# 8) All observed alternation scores were also pooled into groups of visit rate differences and compared to the simulated one leading to Fig. 1.

# My point on view on this:
# 1) the selection seems arbitrary, if anything, I suggest we remove the extreme 5% quantile ?
# 2) what about non-independence of some nest watches because of same MID or FID or PairID ? Is it ok because they will then be more represented in both observed and simulated nest watches ? But their individuality is anyway disrupted, for instance for a same provisioning rate, a bird can be consistently very regular or can be very irregular. I think this argues for randomizing within nest watch.
# 3) what if we randomly select several large intervals and spill over the hour ? Also, in our case, we also calculated visit rate per hour although we typically record for 90 min, so we maybe have more variance in interfeed intervals to select from when picking ‘visit rate-1’ interfeed intervals.
# 4) (see supp fig) intervals length are considered like the starting time of feeding visits, and like if the first visit of the male and the female were both at time zero. I think maybe it is better again to shuffle intervals within a file, keeping the firs time start for each sex in this file.
# 5) the formula to calculate A is different for observed and simulated nest watches I believe, and the maximum A that can be obtained for a same combination of x and y is systematically lower for the simulated ones as soon as the provisioning rate difference (absolute value of x minus y) is larger than 1 (see excel file attached).
# 8) and 1) 
# for simulation : selection of individuals that have a provisioning rate not extreme, then pooled into visit rate differences, only with pairs whose individuals have non extreme provisioning rate.
# in observed values: we take every individuals, every combinations, so for a same visit rate difference, individuals can have very extreme provisioning rates. I think observed combinations where one of the sex has an extreme value for provisioning rate should be remove for comparison with simulated data.

# I don’t know how this impact Kat’s results, but I will now try to run simulations within files because I can better argue for it, and maybe compare both outputs to let her know.

}




{## Get all simulated combinations of individuals with specific provisioning rates, and calculate their alternation

{# Create RawInterfeeds and split per sex and select provisioning rates from 3 to 22
RawInterfeeds <- merge(x= MY_RawFeedingVisits[,c('DVDRef','Sex','Interval')], 
                       y=MY_tblParentalCare[,c('DVDRef','MVisit1RateH', 'FVisit1RateH','DiffVisit1Rate','AlternationValue')] , 
                       by='DVDRef', all.x=TRUE)

MRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','Interval','MVisit1RateH')], RawInterfeeds$Sex == 1)
MRawInterfeeds322 <- MRawInterfeeds[MRawInterfeeds$MVisit1RateH >=3 & MRawInterfeeds$MVisit1RateH <=22,]
FRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','Interval','FVisit1RateH')], RawInterfeeds$Sex == 0)
FRawInterfeeds322 <- FRawInterfeeds[FRawInterfeeds$FVisit1RateH >=3 & FRawInterfeeds$FVisit1RateH <=22,]
}

{# Randomise the interfeed intervals within individuals of the same sex that have the same visit rate
FShuffledInterfeeds322 <- FRawInterfeeds322[-1] %>% group_by(FVisit1RateH) %>% mutate(Interval=sample(Interval))
MShuffledInterfeeds322 <- MRawInterfeeds322[-1] %>% group_by(MVisit1RateH) %>% mutate(Interval=sample(Interval))
}

{# create one simulated df per sex per visit rate, with shuffled intervals associated to a SimID of length 'visit rate - 1'

SimFemale <- list ()

for (i in 3:22)
{
# one group of visit rate at a time
SimFemale[[i]] <- filter(FShuffledInterfeeds322, FVisit1RateH == i)
# add SimID to (visit rate - 1) visits
SimFemale[[i]] <- mutate(SimFemale[[i]], SimID = rep(1:((nrow(SimFemale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimFemale[[i]])))
# Shuffle the SimID
SimFemale[[i]]<-mutate(SimFemale[[i]], SimID = sample(SimID)) # sample without replacement
# sort (not needed but easier to look at output)
SimFemale[[i]]<-arrange(SimFemale[[i]],SimID)
# Calculate cumulative sum for each SimID
SimFemale[[i]]<-SimFemale[[i]]%>%
  group_by(SimID)%>%
  mutate(CumInt = cumsum(Interval))

}


SimMale <- list ()

for (i in 3:22)
{
# one group of visit rate at a time
SimMale[[i]] <- filter(MShuffledInterfeeds322, MVisit1RateH == i)
# add SimID to (visit rate - 1) visits
SimMale[[i]] <- mutate(SimMale[[i]], SimID = rep(1:((nrow(SimMale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimMale[[i]])))
# Shuffle the SimID
SimMale[[i]]<-mutate(SimMale[[i]], SimID = sample(SimID)) # sample without replacement
# sort
SimMale[[i]]<-arrange(SimMale[[i]],SimID)
# Calculate cumulative sum for each SimID
SimMale[[i]]<-SimMale[[i]]%>%
  group_by(SimID)%>%
  mutate(CumInt = cumsum(Interval))

}


SimMale <- do.call(rbind,SimMale)
SimFemale <- do.call(rbind,SimFemale)
SimData <- bind_rows(SimMale, SimFemale) # different from rbind as it binds two df with different columns, adding NAs
SimData[is.na(SimData)] <- 0
}

head(SimData)

{# create MiFj: 400 dataframe of combine male visit * female visit rate
# all individuals of one sex of one visit rate are reused for each combination involving this visit rate

MiFj <- list()
i = rep(3:22, each = 20) # male visit rate
j = rep((3:22), 20) # female visit rate
  
for (k in 1:400) # 400 combination
{ 
MiFj[[k]]<-SimData%>%
filter(MVisit1RateH==i[k] | FVisit1RateH==j[k])%>%
arrange(SimID, CumInt) 
}

AllMiFj <- do.call(rbind, MiFj)
nrow(AllMiFj) # 1075820
}

{# add running OverallSimID and select combinations with both sex, with the full number of intervals for a given provisioning rates
AllMiFj$OverallSimID <- cumsum(AllMiFj$SimID != c(0,head(AllMiFj$SimID,-1))) # shift all SimID from 1, get a running number changing at each mismatch between the original vector of SimID and the shifted one
AllMiFj$Sex <- as.numeric(as.character(AllMiFj$Sex))

AllMiFj_splitperOverallSimID <- split(AllMiFj, AllMiFj$OverallSimID)

AllMiFj_splitperOverallSimID_fun <- function(x){
return(c(
length(x$Sex[x$Sex==0]), 
length(x$Sex[x$Sex==1])
))
}

AllMiFj_splitperOverallSimID_out1 <- lapply(AllMiFj_splitperOverallSimID,FUN= AllMiFj_splitperOverallSimID_fun )
AllMiFj_splitperOverallSimID_out2 <- data.frame(rownames(do.call(rbind,AllMiFj_splitperOverallSimID_out1)),do.call(rbind, AllMiFj_splitperOverallSimID_out1))
 
rownames(AllMiFj_splitperOverallSimID_out2 ) <- NULL
colnames(AllMiFj_splitperOverallSimID_out2 ) <- c('OverallSimID','NbF', 'NbM')
 
# remove all OverSimID where one sex not present
AllMiFj <- AllMiFj[ ! AllMiFj$OverallSimID %in% AllMiFj_splitperOverallSimID_out2$OverallSimID[AllMiFj_splitperOverallSimID_out2$NbF == 0 | AllMiFj_splitperOverallSimID_out2$NbM == 0] ,]
nrow(AllMiFj) # 778147
 
# rename OverallSimID to have it continuous
AllMiFj$OverallSimID <- cumsum(AllMiFj$SimID != c(0,head(AllMiFj$SimID,-1)))

# write.table(AllMiFj, file = "AllMiFj.xls", col.names=TRUE, sep='\t') # 20160412
}

head(AllMiFj)

{# calculate alternation for each combination of individuals with specific provisioning rates

FinalMiFj <- group_by(AllMiFj,OverallSimID)

SimulatedSummaryKat <- summarise(FinalMiFj,
                            tt = n(), # what we have here are interfeeds > if we want numbers of feeds add 2 ?							
                            F = sum(diff(Sex)!=0),
                            A = round((F/(tt-1))*100,2),# what we have are interfeeds > ?
                            MVisitRate = max(MVisit1RateH),## added this for bootstrapping per category - this allows removing lines with 0 ?
                            FVisitRate = max(FVisit1RateH),## added this for bootstrapping per category
                            MFVisitRate = paste(max(MVisit1RateH),max(FVisit1RateH), sep="-"), ## added this for bootstrapping per category
                            VisitRateDifference= abs(max(MVisit1RateH)-max(FVisit1RateH)))

							
tail(as.data.frame(SimulatedSummaryKat),60)				
freqCombination <- arrange(count(SimulatedSummaryKat, MFVisitRate), n)
nrow(freqCombination) # 400 combinations

}

}

head(SimulatedSummaryKat)

{## bootstrap A from SimulatedSummaryKat within each visit rate difference

samplemean <- function(x, d) {return(mean(x[d]))}

Aboot <- data.frame(matrix(,data=NA, nrow=20, ncol=4))
colnames(Aboot) <- c('VisitRateDifference','Amean','Alower','Aupper')
Aboot$VisitRateDifference <- c(0:19)

for (i in 1:20)
{
Aboot$Amean[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$t0
Aboot$Alower[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$normal[2] 
Aboot$Aupper[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$normal[3] 
}
}

Aboot

{# summary Aobserved
# per visit rate difference like in the paper
MY_tblParentalCare_perVisitRateDiff <- group_by(MY_tblParentalCare, DiffVisit1Rate)

Summary_MY_tblParentalCare_perVisitRateDiff <- summarise (MY_tblParentalCare_perVisitRateDiff,
					Amean = mean(AlternationValue),
					Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96)
					
Summary_MY_tblParentalCare_perVisitRateDiff20 <- Summary_MY_tblParentalCare_perVisitRateDiff[1:20,]
Summary_MY_tblParentalCare_perVisitRateDiff20 <- dplyr::rename(Summary_MY_tblParentalCare_perVisitRateDiff20,VisitRateDifference= DiffVisit1Rate)

}

Summary_MY_tblParentalCare_perVisitRateDiff20

{# combine observed and expected and plot

# per visit rate difference like in the paper
Summary_MY_tblParentalCare_perVisitRateDiff20$Type <- "Observed"
Aboot$Type <- "Expected"

VisitRateDiff_Amean <- rbind(Aboot, Summary_MY_tblParentalCare_perVisitRateDiff20)

Fig1 <- ggplot(data=VisitRateDiff_Amean, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean$Amean, n = 9)) +  
  theme_classic()
  
}

}

Fig1


{### simulation alternation: shuffling intervals within files where both sex visit at least once

## I think it could be still interesting to remove extreme values of provisioning rate (not normal to have just one visit, or 50...)
## I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

RawFeedingVisitsBothSexes <- MY_RawFeedingVisits[,c('DVDRef','TstartFeedVisit','Sex','Interval')]
RawFeedingVisitsBothSexes$Sex <- as.numeric(RawFeedingVisitsBothSexes$Sex )


{# creation of i simulated dataset (and calculation of i Asim) for each j file

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
  
out_Asim_j = list()
out_Asim_i = list()

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


for (i in 1:10) # to increase up to 1000
{

x0sim <- x0
x1sim <- x1

x0sim$Interval <- c(0, sample_vector(x0sim$Interval[-1]))
x0sim$TstartFeedVisit <- c(x0sim$TstartFeedVisit[1],x0sim$TstartFeedVisit[-nrow(x0sim)]+x0sim$Interval[-1])

x1sim$Interval <- c(0, sample_vector(x1sim$Interval[-1]))
x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])

xsim <- rbind(x0sim,x1sim)
xsim <- xsim[order(xsim$TstartFeedVisit),]

Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)

out_Asim_i[i] <- Asim
out_Asim_j[j] <- list(unlist(out_Asim_i))

		# clean up
		x0sim <- NULL
		x1sim <- NULL
		Asim <- NULL
}

		# clean up
		x <- NULL
		x0 <- NULL
		x1 <- NULL

}

out_Asim <- do.call(rbind, out_Asim_j)

}

head(out_Asim)

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


{# for the moment cut at 20 visit rate difference in both randomized and observed, and plot

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

}

Fig1comparison

{# add MeanAsim and Adev for each DVD file to MY_TABLE

MY_TABLE <- merge(y=data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef),MeanAsim = rowMeans(out_Asim)), 
				  x= MY_TABLE, by='DVDRef', all.x =TRUE)

MY_TABLE$Adev <- MY_TABLE$MeanAsim - MY_TABLE$AlternationValue

}

head(MY_TABLE)



{#### Predictors of alternation

{# check dependent and explanatory variables

is.numeric(MY_TABLE$BreedingYear)
is.numeric(MY_TABLE$HatchingDayAfter0401)
is.numeric(MY_TABLE$DVDInfoChickNb)
is.numeric(MY_TABLE$ChickAge) 
is.numeric(MY_TABLE$DiffVisit1Rate)

is.numeric(MY_TABLE$ParentsAge)
is.numeric(MY_TABLE$PairBroodNb)
cor.test(MY_TABLE$ParentsAge,MY_TABLE$PairBroodNb) # cor = 0.63, p < 0.0001 ! > take one or the other variable

is.numeric(MY_TABLE$RelTimeHrs) # number of hours after sunrise for that day
summary(MY_TABLE$RelTimeHrs) # 6 NA's > if this covariate is use, reduce MY_Table from those RelTimeHrs NAs
scatter.smooth(MY_TABLE$AlternationValue,MY_TABLE$RelTimeHrs)# linear ? >linear enough to keep it as it is ?

shapiro.test(MY_TABLE$AlternationValue) # normal ok

}

{# modA_ParentAge

modA_ParentAge <- lmer(AlternationValue ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	#scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
	# + (1|PairIDYear) # explain 0% of the variance
	, data = MY_TABLE)

summary(modA_ParentAge) # Number of obs: 1696, groups:  BroodRef, 916; PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

{# model assumptions checking
mean(unlist(ranef(modA_ParentAge)$BroodRef))
mean(unlist(ranef(modA_ParentAge)$SocialMumID))
mean(unlist(ranef(modA_ParentAge)$SocialDadID))
mean(unlist(ranef(modA_ParentAge)$PairID))
mean(unlist(ranef(modA_ParentAge)$BreedingYear))

# qqplots residuals and ranef
qqnorm(resid(modA_ParentAge))
qqline(resid(modA_ParentAge))
qqnorm(unlist(ranef(modA_ParentAge))) # not quite normal ?
qqline(unlist(ranef(modA_ParentAge)))

# residuals vs fitted
scatter.smooth(fitted(modA_ParentAge), resid(modA_ParentAge))	
abline(h=0, lty=2)

# residuals vs predictors
scatter.smooth(MY_TABLE$ParentsAge[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE$HatchingDayAfter0401[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE$DVDInfoChickNb[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)	
plot(MY_TABLE$ChickAgeCat[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE$DiffVisit1Rate[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_ParentAge)) # one influencal data point
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE$RelTimeHrs[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)		


# data vs. fitted ?
d <- MY_TABLE[!is.na(MY_TABLE$RelTimeHrs),]
d$fitted <- fitted(modA_ParentAge)
scatter.smooth(d$fitted, jitter(d$AlternationValue, 0.05),ylim=c(0, 100))
abline(0,1)	

# data and fitted against all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ChickAgeCat")
scatter.smooth(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")

}

}

{# modA_PairBroodNb

modA_PairBroodNb <- lmer(AlternationValue ~  # scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
											scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
											scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
											scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
											ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
											DiffVisit1Rate +  
											scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
											(1|BroodRef) + 
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
											# + (1|PairIDYear) # explain 0% of the variance
											, data = MY_TABLE)
								

summary(modA_PairBroodNb)# Number of obs: 1696, groups:  BroodRef, 916; PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

{# model assumptions checking
mean(unlist(ranef(modA_PairBroodNb)$BroodRef))
mean(unlist(ranef(modA_PairBroodNb)$SocialMumID))
mean(unlist(ranef(modA_PairBroodNb)$SocialDadID))
mean(unlist(ranef(modA_PairBroodNb)$PairID))
mean(unlist(ranef(modA_PairBroodNb)$BreedingYear))

# qqplots residuals and ranef
qqnorm(resid(modA_PairBroodNb))
qqline(resid(modA_PairBroodNb))
qqnorm(unlist(ranef(modA_PairBroodNb))) # not quite normal ?
qqline(unlist(ranef(modA_PairBroodNb)))

# residuals vs fitted
scatter.smooth(fitted(modA_PairBroodNb), resid(modA_PairBroodNb))	
abline(h=0, lty=2)

# residuals vs predictors
scatter.smooth(MY_TABLE$PairBroodNb[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE$HatchingDayAfter0401[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE$DVDInfoChickNb[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)	
plot(MY_TABLE$ChickAgeCat[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE$DiffVisit1Rate[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_PairBroodNb)) # one influencal data point
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE$RelTimeHrs[!is.na(MY_TABLE$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)		


# data vs. fitted ?
d <- MY_TABLE[!is.na(MY_TABLE$RelTimeHrs),]
d$fitted <- fitted(modA_PairBroodNb)
scatter.smooth(d$fitted, jitter(d$AlternationValue, 0.05),ylim=c(0, 100))
abline(0,1)	

# data and fitted against all predictors
scatter.smooth(d$PairBroodNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="PairBroodNb")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ChickAgeCat")
scatter.smooth(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")

}

}

}

summary(modA_ParentAge)$coefficients
summary(modA_PairBroodNb)$coefficients
print(VarCorr(modA_ParentAge),comp=c("Variance","Std.Dev."))
print(VarCorr(modA_PairBroodNb),comp=c("Variance","Std.Dev."))



### repeatability of alternation




{# create MY_TABLE_perBrood
MY_TABLE[is.na(MY_TABLE$MFVisit1RateH),]
summary(MY_TABLE$MFVisit1RateH)

MY_TABLE_perBrood <- split(MY_TABLE,MY_TABLE$BroodRef)
MY_TABLE_perBrood[[1]]

MY_TABLE_perBrood_fun = function(x)  {

return(c(
mean(x$MFVisit1RateH), # TotalProRate
mean(x$AlternationValue), #MeanA
mean(x$MeanAsim) - mean(x$AlternationValue) # Adev
))
}

MY_TABLE_perBrood_out1 <- lapply(MY_TABLE_perBrood, FUN=MY_TABLE_perBrood_fun)
MY_TABLE_perBrood_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perBrood_out1)),do.call(rbind, MY_TABLE_perBrood_out1))

nrow(MY_TABLE_perBrood_out2)	# 919
rownames(MY_TABLE_perBrood_out2) <- NULL
colnames(MY_TABLE_perBrood_out2) <- c('BroodRef','TotalProRate','MeanA', 'Adev')

MY_TABLE_perBrood <- merge(x=unique(MY_TABLE[,c("NbRinged","AvgMass","AvgTarsus","BroodRef","SocialMumID", "SocialDadID","PairID", "BreedingYear","PairIDYear" )]),
							y=MY_TABLE_perBrood_out2,all.x=TRUE, by='BroodRef')
}

head(MY_TABLE_perBrood)



{#### fitness correlate of alternation

{## total provisioning rate
# like in Kat&Ben's paper: the mean per nest for total provisioning rate and for Adev, and have Broodsize at day 11

modFitnessAsProRate <- lmer(TotalProRate ~  NbRinged +
											Adev +
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
											# + (1|PairIDYear) # explain 0% of the variance
											, data = MY_TABLE_perBrood)
											
summary(modFitnessAsProRate) # Number of obs: 919, groups:  PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

{# model assumptions checking
mean(unlist(ranef(modFitnessAsProRate)$SocialMumID))
mean(unlist(ranef(modFitnessAsProRate)$SocialDadID))
mean(unlist(ranef(modFitnessAsProRate)$PairID))
mean(unlist(ranef(modFitnessAsProRate)$BreedingYear))

# qqplots residuals and ranef
qqnorm(resid(modFitnessAsProRate))
qqline(resid(modFitnessAsProRate))
qqnorm(unlist(ranef(modFitnessAsProRate))) # not quite normal ?
qqline(unlist(ranef(modFitnessAsProRate)))

# residuals vs fitted
scatter.smooth(fitted(modFitnessAsProRate), resid(modFitnessAsProRate))	
abline(h=0, lty=2)

# residuals vs predictors
scatter.smooth(MY_TABLE_perBrood$NbRinged, resid(modFitnessAsProRate))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perBrood$Adev, resid(modFitnessAsProRate))
abline(h=0, lty=2)

# data vs. fitted ?
d <- MY_TABLE_perBrood
d$fitted <- fitted(modFitnessAsProRate)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))
abline(0,1)	

# data and fitted against all predictors
scatter.smooth(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="PairBroodNb")
scatter.smooth(d$Adev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")

}


}

{## mean chick mass

{# check dependent and explanatory variables
nrow(MY_TABLE_perBrood[ MY_TABLE_perBrood$NbRinged == 0 ,]) # 45 broods with no ringed chicks
nrow(MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,]) # 20 broods where ringed chicks but no mass nor tarsus: for some reasons were ringed not at the rigth age for comparable measurements)
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,] # 2 broods with ringed with mass but not tarsus
}

modFitnessAsChickMass <- lmer(AvgMass ~ NbRinged +
										MeanA + # Kat&Ben's paper: I assume they used again the average of alternation per nest 
										AvgTarsus +
										(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) ,
										data = MY_TABLE_perBrood)
										
summary(modFitnessAsChickMass) # Number of obs: 852, groups:  PairID, 436; SocialMumID, 287; SocialDadID, 276; BreedingYear, 12

{# model assumptions checking
mean(unlist(ranef(modFitnessAsChickMass)$SocialMumID))
mean(unlist(ranef(modFitnessAsChickMass)$SocialDadID))
mean(unlist(ranef(modFitnessAsChickMass)$PairID))
mean(unlist(ranef(modFitnessAsChickMass)$BreedingYear))

# qqplots residuals and ranef
qqnorm(resid(modFitnessAsChickMass))
qqline(resid(modFitnessAsChickMass))
qqnorm(unlist(ranef(modFitnessAsChickMass))) # not quite normal ?
qqline(unlist(ranef(modFitnessAsChickMass)))

# residuals vs fitted
scatter.smooth(fitted(modFitnessAsChickMass), resid(modFitnessAsChickMass))	
abline(h=0, lty=2)

# residuals vs predictors
d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MeanA) & !is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass),]
scatter.smooth(d$NbRinged, resid(modFitnessAsChickMass))
abline(h=0, lty=2)
scatter.smooth(d$MeanA, resid(modFitnessAsChickMass))
abline(h=0, lty=2)
scatter.smooth(d$AvgTarsus, resid(modFitnessAsChickMass))
abline(h=0, lty=2)

# data vs. fitted ?
d$fitted <- fitted(modFitnessAsChickMass)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))
abline(0,1)	

# data and fitted against all predictors
scatter.smooth(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="PairBroodNb")
scatter.smooth(d$Adev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")

}

}

# Parent survival


}

summary(modFitnessAsProRate)
summary(modFitnessAsChickMass)





#########################################
# replication Nagagawa et al 2007 study #
#########################################

{# get provisioning rate for both sex piled up
FemaleProRate <- MY_TABLE[,c("FVisit1RateH","DVDInfoChickNb","ChickAgeCat","HatchingDayAfter0401","RelTimeHrs", 
							"DVDRef","BroodRef","SocialMumID", "SocialDadID","PairID", "BreedingYear")]
MaleProRate <- MY_TABLE[,c("MVisit1RateH","DVDInfoChickNb","ChickAgeCat","HatchingDayAfter0401","RelTimeHrs", 
							"DVDRef","BroodRef","SocialMumID", "SocialDadID","PairID", "BreedingYear")]

FemaleProRate$Sex <- 0
MaleProRate$Sex <- 1					
colnames(FemaleProRate)[which(names(FemaleProRate) == "FVisit1RateH")] <- "Visit1RateH"						
colnames(MaleProRate)[which(names(MaleProRate) == "MVisit1RateH")] <- "Visit1RateH"	
colnames(FemaleProRate)[which(names(FemaleProRate) == "SocialMumID")] <- "BirdID"		
colnames(MaleProRate)[which(names(MaleProRate) == "SocialDadID")] <- "BirdID"		
colnames(FemaleProRate)[which(names(FemaleProRate) == "SocialDadID")] <- "SocialPartnerID"		
colnames(MaleProRate)[which(names(MaleProRate) == "SocialMumID")] <- "SocialPartnerID"	

head(FemaleProRate)
head(MaleProRate)

BirdProRate <- rbind(FemaleProRate,MaleProRate)	
}

head(BirdProRate)

{### repeatbility of provisioning rate
# Shinichi does repeatability of provisioning rate on visit/chick/hour



modProRateRpt <- lmer(Visit1RateH ~ scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ (1|SocialPartnerID) + (1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate)
									
summary(modProRateRpt)


modProRateRptwithoutBirdID <- lmer(Visit1RateH ~ scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									#(1|BirdID)+ 
									(1|SocialPartnerID) + (1|BreedingYear) 
									# + (1|PairID)
									, data = BirdProRate)

summary(modProRateRptwithoutBirdID)
anova(modProRateRpt,modProRateRptwithoutBirdID) # ***




modProRateRptwithoutSocialPartnerID <- lmer(Visit1RateH ~ scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									#(1|SocialPartnerID) + 
									(1|BreedingYear) 
									# + (1|PairID)
									, data = BirdProRate)

summary(modProRateRptwithoutSocialPartnerID)
anova(modProRateRpt,modProRateRptwithoutSocialPartnerID) # ***


modProRateRptwithoutPairID <- lmer(Visit1RateH ~ scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + 
									(1|BreedingYear) 
									#+ (1|PairID)
									, data = BirdProRate)

summary(modProRateRptwithoutPairID)
anova(modProRateRpt,modProRateRptwithoutPairID) # NS
}









{#############################  TO DO + ISSUES
  
## repeatability of provisioning rate:
# bootstrap instead of ML (though clear answer)
# get rpt package to work (under construction)
# do analyses on provisioning rate per chick like shinichi
# boxcox transfo to approach normality ?


## repeatability alternation 
# considering more than two measures and use rptR package to fit mixed effect model (not working)
# or randomise order of measurements with 2 measures


## take a decision for time of the day
# check if the effect is linear to keep a continuous variable


## predictor of alternation
# solve the issue of correlation between parent age and PairBroodNb


## fitness model
# are models on average values good ? 
# should it be weigthed ?
# or should the error been kept forward and how ?
# get table ready for parental survival

## sealed bid by male
# how can female adjust ? if purely alternate but for a low male provisioning > low fitness !


}#############################

{#############################  MESS

par(mfrow=c(3,1)) 
hist(as.POSIXct(MY_tblDVDInfo$DVDtime), 10)
hist((MY_tblDVDInfo$RelTimeMins), 20)
hist((MY_tblDVDInfo$LogRelTimeMins), 10)
hist((MY_TABLE$RelTimeHrs), 20)
max(as.POSIXct(MY_tblDVDInfo$DVDtime), na.rm=T)

#MY_TABLE$DVDtime2 <- substr(MY_TABLE$DVDtime, 12, 16)
#MY_TABLE$NumTime <- as.numeric(MY_TABLE$DVDtime)
#MY_TABLE[,c('DVDtime','NumTime')][with(MY_TABLE[,c('DVDtime','NumTime')],order(MY_TABLE$NumTime)),]
#MY_TABLE$NumTimeposixct <- as.numeric(as.POSIXct(MY_TABLE$DVDtime))
#MY_TABLE[,c('DVDtime','NumTime','NumTimeposixct')][with(MY_TABLE[,c('DVDtime','NumTime')],order(MY_TABLE$NumTimeposixct)),]

}#############################