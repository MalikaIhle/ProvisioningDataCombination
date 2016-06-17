#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 15/04/2015
#	 last modif : 16/06/2016  
#	 commit: analyses on synchrony 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### remarks
# LastSeenAlive information needs to be updated manually when DB updated
# MY_tblBrood$Nb3 is the number of post fledgling
# MY_tblBrood Mass and tarsus info: the last measurement, at d12, when ringed. nMass, nTarsus, NbRinged should in principle be equal: maybe should consider small difference of age, i.e. include all brood or a standardized subsets
# 2 broods corresponding to 2 DVDs have both parents NA > removed
# 39 brood with one parent NA, corresponding to 66 files
# if tblDVDinfos updated > the date associated to DVDtime and to Sunrise will change
# MY_TABLE_perDVD has one line per file
# MY_TABLE_perBrood has one line per brood, averaging the summary accross files
}

rm(list = ls(all = TRUE))

{### packages

# install.packages("beepr")
# install.packages("RODBC")
# install.packages("MASS")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("boot")
# install.packages("lme4")
# install.packages("arm")
# install.packages("RLRsim")
# install.packages("MCMCglmm")
# install.packages("R2admb")
# install.packages("glmmADMB", 
    # repos=c("http://glmmadmb.r-forge.r-project.org/repos",
            # getOption("repos")),
    # type="source")



library(beepr) # so that markdown beeps when error generated
library(RODBC)
library(MASS) # for boxcox
library(dplyr) # for some part of the extraction of the data written by Andrew for the simulation 
library(ggplot2)
library(boot) # for simulation
library(lme4)
# library(rptR) under construction
library(RLRsim) # for testing significance randome effect in repeatability part
library(MCMCglmm)
library(glmmADMB)

# options(warn=2)	# when loop generate a error at one iteration, the loop stop, so one can call the filename and check what's wrong with it
options(warn=-1) # for Rmarkdown not to print the warnings
}

{### Get raw data (from source() or R_output folder)

{# output csv files

# source('COMPILATION_PROVISIONING.R')
# or :

output_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_output"

MY_tblParentalCare <- read.csv(paste(output_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblBroods <- read.csv(paste(output_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
MY_tblDVDInfo <- read.csv(paste(output_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(output_folder,"R_MY_RawFeedingVisits.csv", sep="/")) # OF directly followed by IN are merged feeding visits ; will be used for simulation
}

{# input txt files

input_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_input"

sys_LastSeenAlive <- read.table(file= paste(input_folder,"sys_LastSeenAlive_20160503.txt", sep="/"), sep='\t', header=T)	## !!! to update when new pedigree !!! (and other corrections potentially)
sys_LastSeenAlive$LastYearAlive <- substr(sys_LastSeenAlive$LastLiveRecord, 7,10)

pedigree <-  read.table(file= paste(input_folder,"Pedigree_20160309.txt", sep="/"), sep='\t', header=T)  ## !!! to update when new pedigree !!! 

FedBroods <-  read.table(file= paste(input_folder,"FedBroods.txt", sep="/"), sep='\t', header=T)  ## from Ian Cleasby 20160531

}

{# query DB

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")

# MassTarsusRearinBrood_allChicks (see annotated sql query 'LastMassTarsusChick')
# considering only chicks alive and measured between 11 and 14 days

{MassTarsusRearinBrood_allChicks <-  sqlQuery(conDB, "
SELECT tblCaptures.BirdID AS ChickID, 
usys_qRearingBrood.NatalBrood,
usys_qRearingBrood.RearingBrood,
usys_qRearingBrood.CrossFosteredYN,
 Avg(tblMeasurements.Mass) AS AvgOfMass, 
 Avg(tblMeasurements.Tarsus) AS AvgOfTarsus, 
 Avg(usys_qRelativeChickMassClassesForCaptures.Age) AS AvgOfAge, 
 Count(usys_qRearingBrood.BirdID) AS nMeasures
 
FROM tblBirdID INNER JOIN 
((
	(SELECT tblBirdID.BirdID, IIf([FosterBrood] Is Null,[BroodRef],[FosterBrood]) AS RearingBrood,  
			tblBirdID.BroodRef AS NatalBrood, IIf([FosterBrood] Is Null,0,1) AS CrossFosteredYN
	FROM tblBirdID LEFT JOIN tblFosterBroods ON tblBirdID.BirdID = tblFosterBroods.BirdID
	WHERE (((tblBirdID.BroodRef) Is Not Null))) 
	AS usys_qRearingBrood 

INNER JOIN (
	(SELECT First(tblCaptures.CaptureRef) AS CaptureRef, 
	14 AS MassClass, 
	First(tblCaptures.CaptureDate-[HatchDate])+1 AS Age

	FROM (tblBirdID INNER JOIN tblCaptures ON tblBirdID.BirdID = tblCaptures.BirdID)
	
	INNER JOIN 
	 
		(
			SELECT tblBirdID.BirdID, 
			usys_qBroodEggDate.LayDate AS EggDate, 
			usys_qBroodHatchDate.HatchDate, 
			usys_qBroodEggDate.DateEstimated AS EggDateEst, 
			IIf(usys_qBroodHatchDate.BroodRef Is Not Null,usys_qBroodHatchDate.DateEstimated,0) AS HatchDateEst

			FROM ((tblBroods 
			LEFT JOIN 

					(SELECT tblBroods.BroodRef, 
					IIf(usys_qBroodTrueEggDate.LayDate,
					usys_qBroodTrueEggDate.LayDate,
					usys_qBroodEggDateFromFirstSeen.LayDate) AS LayDate, 
					IIf(usys_qBroodTrueEggDate.BroodRef,
					usys_qBroodTrueEggDate.DateEstimated,True) AS DateEstimated
					
					FROM (
						(SELECT tblBroodEvents.BroodRef, 
						tblBroodEvents.EventDate AS LayDate, 
						tblBroodEvents.DateEstimated
						FROM tblBroodEvents
						WHERE (((tblBroodEvents.EventDate) Is Not Null) 
						AND ((tblBroodEvents.EventNumber)=0))
						) 
						AS usys_qBroodTrueEggDate 
						
					RIGHT JOIN tblBroods ON usys_qBroodTrueEggDate.BroodRef = tblBroods.BroodRef) 
					LEFT JOIN 
						(SELECT tblBroodEvents.BroodRef, 
						IIf([usys_qBroodHatchDatesFromTable].[Hatchdate] Is Null,
						[EventDate]-[EggCount],
						[Hatchdate]-14) AS LayDate, 
						IIf([usys_qBroodHatchDatesFromTable].[Hatchdate] Is Null,
						'EggCount','HatchDate') AS EstimateSource
						
						FROM tblBroodEvents 
						LEFT JOIN 
							(SELECT tblBroodEvents.BroodRef, 
							tblBroodEvents.EventDate AS HatchDate, 
							tblBroodEvents.DateEstimated
							FROM tblBroodEvents
							WHERE (((tblBroodEvents.EventDate) Is Not Null) 
							AND ((tblBroodEvents.EventNumber)=1))
							) 
							AS usys_qBroodHatchDatesFromTable 
						
						ON tblBroodEvents.BroodRef = usys_qBroodHatchDatesFromTable.BroodRef
						
						WHERE (((tblBroodEvents.EventDate) Is Not Null) 
						AND ((tblBroodEvents.EventNumber)=4) 
						AND ((usys_qBroodHatchDatesFromTable.HatchDate)>=[EventDate])) 
						OR (((tblBroodEvents.EventNumber)=4) 
						AND ((tblBroodEvents.EggCount) Is Not Null))
						
						) 
						AS usys_qBroodEggDateFromFirstSeen 
						ON tblBroods.BroodRef = usys_qBroodEggDateFromFirstSeen.BroodRef
						
					WHERE (((IIf([usys_qBroodTrueEggDate].[LayDate],[usys_qBroodTrueEggDate].[LayDate],[usys_qBroodEggDateFromFirstSeen].[LayDate])) Is Not Null))
					
					) 
					AS usys_qBroodEggDate
			 
			ON tblBroods.BroodRef = usys_qBroodEggDate.BroodRef) 
			
			LEFT JOIN 
				(
				SELECT usys_qBroodsWithHatchlings.BroodRef, 
				IIf(usys_qBroodHatchDatesFromTable.HatchDate Is Not Null,
				usys_qBroodHatchDatesFromTable.HatchDate,
				usys_qBroodEggDate.LayDate+14) AS HatchDate, 
				usys_qBroodHatchDatesFromTable.HatchDate Is Null Or usys_qBroodHatchDatesFromTable.DateEstimated AS DateEstimated
				FROM (
					(SELECT DISTINCT tblBirdID.BroodRef, 
					Count(*) AS NoHatchlings
					FROM tblBirdID
					WHERE (((tblBirdID.LastStage)>1) AND ((tblBirdID.BroodRef) Is Not Null))
					GROUP BY tblBirdID.BroodRef
					) 
					AS usys_qBroodsWithHatchlings 
					
				LEFT JOIN 
					(SELECT tblBroodEvents.BroodRef, 
					tblBroodEvents.EventDate AS HatchDate, 
					tblBroodEvents.DateEstimated
					FROM tblBroodEvents
					WHERE (((tblBroodEvents.EventDate) Is Not Null) AND ((tblBroodEvents.EventNumber)=1))
					) 
					AS usys_qBroodHatchDatesFromTable 
					ON usys_qBroodsWithHatchlings.BroodRef = usys_qBroodHatchDatesFromTable.BroodRef) 
					
				LEFT JOIN 
					(SELECT tblBroods.BroodRef, 
					IIf(usys_qBroodTrueEggDate.LayDate,
					usys_qBroodTrueEggDate.LayDate,
					usys_qBroodEggDateFromFirstSeen.LayDate) AS LayDate, 
					IIf(usys_qBroodTrueEggDate.BroodRef,
					usys_qBroodTrueEggDate.DateEstimated,True) AS DateEstimated
					
					FROM (
						(SELECT tblBroodEvents.BroodRef, 
						tblBroodEvents.EventDate AS LayDate, 
						tblBroodEvents.DateEstimated
						FROM tblBroodEvents
						WHERE (((tblBroodEvents.EventDate) Is Not Null) 
						AND ((tblBroodEvents.EventNumber)=0))
						) 
						AS usys_qBroodTrueEggDate 
						
					RIGHT JOIN tblBroods ON usys_qBroodTrueEggDate.BroodRef = tblBroods.BroodRef) 
					LEFT JOIN 
						(SELECT tblBroodEvents.BroodRef, 
						IIf([usys_qBroodHatchDatesFromTable].[Hatchdate] Is Null,
						[EventDate]-[EggCount],
						[Hatchdate]-14) AS LayDate, 
						IIf([usys_qBroodHatchDatesFromTable].[Hatchdate] Is Null,
						'EggCount','HatchDate') AS EstimateSource
						
						FROM tblBroodEvents 
						LEFT JOIN 
							(SELECT tblBroodEvents.BroodRef, 
							tblBroodEvents.EventDate AS HatchDate, 
							tblBroodEvents.DateEstimated
							FROM tblBroodEvents
							WHERE (((tblBroodEvents.EventDate) Is Not Null) 
							AND ((tblBroodEvents.EventNumber)=1))
							) 
							AS usys_qBroodHatchDatesFromTable 
						
						ON tblBroodEvents.BroodRef = usys_qBroodHatchDatesFromTable.BroodRef
						
						WHERE (((tblBroodEvents.EventDate) Is Not Null) 
						AND ((tblBroodEvents.EventNumber)=4) 
						AND ((usys_qBroodHatchDatesFromTable.HatchDate)>=[EventDate])) 
						OR (((tblBroodEvents.EventNumber)=4) 
						AND ((tblBroodEvents.EggCount) Is Not Null))
						
						) 
						AS usys_qBroodEggDateFromFirstSeen 
						ON tblBroods.BroodRef = usys_qBroodEggDateFromFirstSeen.BroodRef
						
					WHERE (((IIf([usys_qBroodTrueEggDate].[LayDate],[usys_qBroodTrueEggDate].[LayDate],[usys_qBroodEggDateFromFirstSeen].[LayDate])) Is Not Null))
					
					) AS usys_qBroodEggDate 
					ON usys_qBroodsWithHatchlings.BroodRef = usys_qBroodEggDate.BroodRef
					
				) 
				AS usys_qBroodHatchDate 

			ON tblBroods.BroodRef = usys_qBroodHatchDate.BroodRef) 
			INNER JOIN tblBirdID ON tblBroods.BroodRef = tblBirdID.BroodRef
			WHERE (((tblBirdID.BroodRef) Is Not Null))

			)
		AS usys_qBirdEggHatchDates ON tblCaptures.BirdID = usys_qBirdEggHatchDates.BirdID

		WHERE (((([tblCaptures].[CaptureDate]-[HatchDate])+1)<=14) 
		AND ((tblCaptures.Stage)<3)
		AND (((tblBirdID.DeathDate) Is Null Or (tblBirdID.DeathDate)<>[Capturedate])))
		GROUP BY tblCaptures.CaptureRef
		HAVING (((First(tblCaptures.CaptureDate-[HatchDate])+1)=11 Or 
				 (First(tblCaptures.CaptureDate-[HatchDate])+1)=12 Or 
				 (First(tblCaptures.CaptureDate-[HatchDate])+1)=13 Or 
				 (First(tblCaptures.CaptureDate-[HatchDate])+1)=14))
	) AS usys_qRelativeChickMassClassesForCaptures 

INNER JOIN tblCaptures ON usys_qRelativeChickMassClassesForCaptures.CaptureRef = tblCaptures.CaptureRef) ON usys_qRearingBrood.BirdID = tblCaptures.BirdID) 


INNER JOIN tblMeasurements ON tblCaptures.CaptureRef = tblMeasurements.CaptureRef) ON tblBirdID.BirdID = tblCaptures.BirdID

WHERE (((usys_qRelativeChickMassClassesForCaptures.MassClass)=14) AND ((usys_qRearingBrood.RearingBrood) Is Not Null) AND ((tblMeasurements.Mass)>0) AND ((tblBirdID.DeathDate) Is Null Or (tblBirdID.DeathDate)<>[CaptureDate]))
GROUP BY tblCaptures.BirdID, usys_qRearingBrood.RearingBrood, usys_qRearingBrood.NatalBrood,usys_qRearingBrood.CrossFosteredYN, tblCaptures.CaptureDate;

")
}

# taking the first measurement for chicks measured twice in the right range of age 
# (even though likely measured when tarsus forgotten the first time but here put priority on getting chicks with the same age...)
MassTarsusRearinBrood_allChicks[!is.na(MassTarsusRearinBrood_allChicks$ChickID) & MassTarsusRearinBrood_allChicks$ChickID == '4764',]
MassTarsusRearinBrood_allChicks <- MassTarsusRearinBrood_allChicks[order(MassTarsusRearinBrood_allChicks$AvgOfAge), ] 
MassTarsusRearinBrood_allChicks <- MassTarsusRearinBrood_allChicks[!duplicated(MassTarsusRearinBrood_allChicks$ChickID),]

tblChicks <- merge(x=MassTarsusRearinBrood_allChicks, y= pedigree[,c("id","dam","sire")], all.x=TRUE, by.x="ChickID", by.y = "id")

close(conDB)

summary(tblChicks)

tblChicks_byRearingBrood <- as.data.frame(tblChicks %>% group_by(RearingBrood) %>% summarise(sd(AvgOfMass),sd(AvgOfTarsus), n(), sum(CrossFosteredYN)))
colnames(tblChicks_byRearingBrood) <- c("RearingBrood","sdMass", "sdTarsus", "NbChicksMeasured", "NbChicksMeasuredCrossFostered")
tblChicks_byRearingBrood$MixedBroodYN <- tblChicks_byRearingBrood$NbChicksMeasured != tblChicks_byRearingBrood$NbChicksMeasuredCrossFostered

nrow(tblChicks) # 2352 = length(unique(tblChicks$ChickID))

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
MY_tblChicks_byRearingBrood <- tblChicks_byRearingBrood[tblChicks_byRearingBrood$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 

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
nrow(MY_tblParentalCare) # 1662 DVD files ; = length(unique(MY_RawFeedingVisits$DVDRef)) = nrow(MY_tblDVDInfo) 
length(unique(MY_tblDVDInfo$BroodRef)) # 910 broods videotaped at least once
range(table(MY_tblDVDInfo$BroodRef)) # range from 1 to 3
mean(table(MY_tblDVDInfo$BroodRef)) # on average 1.8 videos per brood watched


}

{# the typical sparrow

cor.test(MY_tblBroods$ParentsAge,MY_tblBroods$PairBroodNb) # cor = 0.54, p < 0.0001 
scatter.smooth(jitter(MY_tblBroods$ParentsAge,3), jitter(MY_tblBroods$PairBroodNb,3))# 
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
summary(MY_tblBroods$FDivorce)
summary(MY_tblBroods$FDivorceforEx)
summary(MY_tblBroods$MDivorce)
summary(MY_tblBroods$MDivorceforEx)


}

}

head(MY_tblBroods) 
head(MY_tblDVDInfo) 
head(MY_tblParentalCare)
head(MY_RawFeedingVisits)
head(MY_tblChicks)
head(MY_tblChicks_byRearingBrood)


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

nrow(MY_tblParentalCare) # 1499 if remove quantiles
}

{## look at raw data
hist(MY_RawFeedingVisits$Interval,breaks=200)
summary(MY_RawFeedingVisits$Interval)
MY_RawFeedingVisits$Duration <- MY_RawFeedingVisits$TendFeedVisit - MY_RawFeedingVisits$TstartFeedVisit
hist(MY_RawFeedingVisits$Duration,breaks=200)
summary(MY_RawFeedingVisits$Duration)


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
x0sim$TstartFeedVisit <- c(x0sim$TstartFeedVisit[1],x0sim$TstartFeedVisit[-nrow(x0sim)]+x0sim$Interval[-1])

x1sim$Interval <- c(0, sample_vector(x1sim$Interval[-1]))
x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])

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
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(TotalProRate_Smean_bis$TotalProRate, n = 20)) +
  scale_y_continuous(breaks = pretty(TotalProRate_Smean_bis$Smean, n = 20)) +  
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


{### simulation alternation keeping some temporal autocorrelation: shuffling consecutives intervals within one individual 

head(RawFeedingVisitsBothSexes)

{# creation of i simulated dataset (and calculation of i Asim) for each j file

sample_vector_prob <- function(x,...){if(length(x)<=1) x else sample(x,replace=F,prob=(seq(0.9,0.1, along.with=x) ))} 

out_Asimter_j = list()
out_Asimter_i = list()


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

x1sim <- x1

x1sim$Interval <- c(0, sample_vector_prob(x1sim$Interval[-1]))
x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])

xsim <- rbind(x0,x1sim)
xsim <- xsim[order(xsim$TstartFeedVisit),]

Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)

out_Asimter_i[i] <- Asim
out_Asimter_j[j] <- list(unlist(out_Asimter_i))

		# clean up
		x1sim <- NULL
		Asim <- NULL
}

		# clean up
		x <- NULL
		x0 <- NULL
		x1 <- NULL

}

out_Asimter <- do.call(rbind, out_Asimter_j)

}

head(out_Asimter)

{# out A sim summary

out_Asimter_df <- data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef), out_Asimter)
out_Asimter_df <- merge(x=out_Asimter_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

out_Asimter_df_perDiffVisit1Rate <- split(out_Asimter_df,out_Asimter_df$DiffVisit1Rate)

 # x <-out_Asimter_df_perDiffVisit1Rate[[31]]
 # x <-out_Asimter_df_perDiffVisit1Rate[[30]] # just one file


out_Asimter_df_perDiffVisit1Rate_fun <- function(x) {

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

out_Asimter_df_perDiffVisit1Rate_out1 <- lapply(out_Asimter_df_perDiffVisit1Rate,out_Asimter_df_perDiffVisit1Rate_fun)
out_Asimter_df_perDiffVisit1Rate_out2 <- data.frame(rownames(do.call(rbind,out_Asimter_df_perDiffVisit1Rate_out1)),do.call(rbind, out_Asimter_df_perDiffVisit1Rate_out1))

nrow(out_Asimter_df_perDiffVisit1Rate_out2)	# 33
rownames(out_Asimter_df_perDiffVisit1Rate_out2) <- NULL
colnames(out_Asimter_df_perDiffVisit1Rate_out2) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

head(out_Asimter_df_perDiffVisit1Rate_out2)

{### comparison both method of randomization

out_Asimter_df_perDiffVisit1Rate_out2_forcomparison <- out_Asimter_df_perDiffVisit1Rate_out2[1:21,-5]
out_Asimter_df_perDiffVisit1Rate_out2_forcomparison$Type <- 'Expected'
out_Asimter_df_perDiffVisit1Rate_out2_forcomparison$TypeSim <- 'ExpectedTempoAuto'
VisitRateDiff_Amean_for_comparison_ter <- rbind(VisitRateDiff_Amean_for_comparison,out_Asimter_df_perDiffVisit1Rate_out2_forcomparison)
VisitRateDiff_Amean_for_comparison_ter$VisitRateDifference <- as.numeric(as.character(VisitRateDiff_Amean_for_comparison_ter$VisitRateDifference))

Fig1comparisonbis <- ggplot(data=VisitRateDiff_Amean_for_comparison_ter, aes(x=VisitRateDifference, y=Amean, group=TypeSim, colour=TypeSim))+
geom_point()+
geom_line()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','green','grey'), labels=c("95% Expected Kat", "95% Expected Malika", "95% Expected Autocor","95% Observed"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison$Amean, n = 9)) +  
theme_classic()
}  
  


}



}

Fig1comparisonbis
FigS

{### create MY_TABLE_perDVD: where both parents known + add expected alternation from simulation
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

MY_TABLE_perDVD <- MY_tblParentalCare[,c("DVDRef","MVisit1","FVisit1","FVisit1RateH","MVisit1RateH","DiffVisit1Rate","MFVisit1RateH","NbAlternation","AlternationValue", "NbSynchro_ChickFeedingEquanim", "NbSynchro_LessConspicuous", "SynchronyFeedValue","SynchronyMvtValue","NbSynchroFemaleStart", "PropSynchroFemaleStart","MmeanDuration","FmeanDuration")]
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","ChickAge","ChickAgeCat","DVDdate","RelTimeHrs")], by='DVDRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, 
y=MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401","SocialMumID","SocialDadID","NbRinged","DadAge","MumAge","ParentsAge",
"MPrevNbRinged","MBroodNb","MPriorResidence","MDivorce","MDivorceforEx",
"FPrevNbRinged","FBroodNb","FPriorResidence","FDivorce","FDivorceforEx","PairID","PairBroodNb","PairIDYear", "AvgMass", "MinMass", "AvgTarsus")], by='BroodRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblChicks_byRearingBrood[,c("RearingBrood", "sdMass", "sdTarsus", "MixedBroodYN")], by.x="BroodRef", by.y="RearingBrood", all.x=TRUE)

length(unique(MY_TABLE_perDVD$BroodRef[is.na(MY_TABLE_perDVD$SocialMum) | is.na(MY_TABLE_perDVD$SocialDadID)])) # 38 broods - 63 files one parent unknown


MY_TABLE_perDVD <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$SocialMumID) & !is.na(MY_TABLE_perDVD$SocialDadID),] # where both parents known
nrow(MY_TABLE_perDVD) # 1599 files
length(unique(MY_TABLE_perDVD$BroodRef)) # 872 broods
MY_TABLE_perDVD$MFVisit1 <- MY_TABLE_perDVD$FVisit1+ MY_TABLE_perDVD$MVisit1
MY_TABLE_perDVD$MFmeanDuration <- (MY_TABLE_perDVD$FmeanDuration+MY_TABLE_perDVD$MmeanDuration)/2
MY_TABLE_perDVD$NbSynchroMaleStart <- MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim - MY_TABLE_perDVD$NbSynchroFemaleStart

scatter.smooth(MY_TABLE_perDVD$FVisit1, MY_TABLE_perDVD$FmeanDuration)
scatter.smooth(MY_TABLE_perDVD$MVisit1, MY_TABLE_perDVD$MmeanDuration)
scatter.smooth(MY_TABLE_perDVD$MFVisit1, MY_TABLE_perDVD$MFmeanDuration)
hist( MY_TABLE_perDVD$FmeanDuration, breaks=40)
hist( MY_TABLE_perDVD$MmeanDuration, breaks=40)


{# add MeanAsim and Adev

MY_TABLE_perDVD <- merge(y=data.frame(DVDRef = unique(RawFeedingVisitsBothSexes$DVDRef),MeanAsim = rowMeans(out_Asim)), 
				  x= MY_TABLE_perDVD, by='DVDRef', all.x =TRUE)

MY_TABLE_perDVD$Adev <- MY_TABLE_perDVD$MeanAsim - MY_TABLE_perDVD$AlternationValue

}


{# add Max A possible considering both birds provisioning rate
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
geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
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
geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
xlab("Visit rate difference")+
ylab("Mean alternation")+
scale_colour_manual(values=c("red", 'orange','green','grey', "black"), labels=c("95% sim among watch", "95% sim within watch","95% sim within watch with autocor","95% Observed" ,"Maximum Alternation possible"))+
scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$VisitRateDifference, n = 12)) +
scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_for_comparison_withAMax_bis$Amean, n = 9)) +  
theme_classic()


}

Fig1comparison_withMax_bis


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

}

head(MY_TABLE_perDVD)
Fig1comparison_withMax

{### create MY_TABLE_perBrood
MY_TABLE_perDVD[is.na(MY_TABLE_perDVD$MFVisit1RateH),]
summary(MY_TABLE_perDVD$MFVisit1RateH)

MY_TABLE_perBrood <- split(MY_TABLE_perDVD,MY_TABLE_perDVD$BroodRef)
MY_TABLE_perBrood[[1]]

MY_TABLE_perBrood_fun = function(x)  {

return(c(
mean(x$MFVisit1RateH), # TotalProRate
mean(x$AlternationValue), #MeanA
mean(x$MeanAsim) - mean(x$AlternationValue), # MeanAdev
mean(x$DiffVisit1Rate), # MeanDiffVisit1Rate
mean(x$SynchronyFeedValue), # MeanSynchroFeed
mean(x$NbSynchro_ChickFeedingEquanim), # MeanSynchroFeed_nb
mean(x$MFVisit1) # MeanMFVisit1
))
}

MY_TABLE_perBrood_out1 <- lapply(MY_TABLE_perBrood, FUN=MY_TABLE_perBrood_fun)
MY_TABLE_perBrood_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perBrood_out1)),do.call(rbind, MY_TABLE_perBrood_out1))

nrow(MY_TABLE_perBrood_out2)	# 872
rownames(MY_TABLE_perBrood_out2) <- NULL
colnames(MY_TABLE_perBrood_out2) <- c('BroodRef','TotalProRate','MeanA', 'MeanAdev','MeanDiffVisit1Rate','MeanSynchroFeed','MeanSynchroFeed_nb','MeanMFVisit1')

MY_TABLE_perBrood <- merge(y=unique(MY_TABLE_perDVD[,-which(names(MY_TABLE_perDVD) %in% c("DVDRef","FVisit1","FVisit1RateH","MVisit1","MVisit1RateH","DiffVisit1Rate","MFVisit1RateH","MFVisit1",
																							"NbAlternation","AlternationValue","MeanAsim", "Adev","AMax","PropSynchroFemaleStart","MmeanDuration","FmeanDuration","MFmeanDuration","NbSynchroFemaleStart", "NbSynchroMaleStart",
																							"NbSynchro_ChickFeedingEquanim","NbSynchro_LessConspicuous","SynchronyFeedValue","SynchronyMvtValue",
																							"DVDInfoChickNb","ChickAge","ChickAgeCat","DVDdate","RelTimeHrs"))]),
							x=MY_TABLE_perBrood_out2,all.x=TRUE, by='BroodRef')
							
			
{# calculate residual mass on tarsus

ResMassTarsus <-  cbind( MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus),"BroodRef" ], 
											data.frame(residuals(lm(AvgMass~ AvgTarsus, data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus), ]))))
colnames(ResMassTarsus) <- c("BroodRef" , "ResMassTarsus")
head(ResMassTarsus)

MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=ResMassTarsus, all.x=TRUE, by = "BroodRef")

}

nrow(MY_TABLE_perBrood) # 872
}

head(MY_TABLE_perBrood)

{### create MY_TABLE_perChick
nrow(MY_tblChicks[is.na(MY_tblChicks$AvgOfMass),]) # 0
nrow(MY_tblChicks[is.na(MY_tblChicks$AvgOfTarsus),]) # 79

MY_TABLE_perChick <- merge(x= MY_tblChicks , y=MY_TABLE_perBrood[,c("BroodRef", "NbRinged","MeanA", "SocialMumID","SocialDadID","PairID","BreedingYear")]
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

}

head(MY_TABLE_perChick)

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
mean(Survival$AvgSurvival) # 57.05417

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


###############
# ALTERNATION #
###############

{#### Predictors of alternation

{# check dependent and explanatory variables

is.numeric(MY_TABLE_perDVD$BreedingYear)
is.numeric(MY_TABLE_perDVD$HatchingDayAfter0401)
is.numeric(MY_TABLE_perDVD$DiffVisit1Rate)

is.numeric(MY_TABLE_perDVD$DVDInfoChickNb)
is.numeric(MY_TABLE_perDVD$ChickAge) 
cor.test(MY_TABLE_perDVD$ChickAge,MY_TABLE_perDVD$DVDInfoChickNb) # cor = -0.08, p<0.001 
cor.test(MY_TABLE_perDVD$ChickAge,MY_TABLE_perDVD$NbRinged) # cor = 0.06, p=0.01 




is.numeric(MY_TABLE_perDVD$ParentsAge)
is.numeric(MY_TABLE_perDVD$PairBroodNb)
cor.test(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$PairBroodNb) # cor = 0.63, p < 0.0001 ! > take one or the other variable
scatter.smooth(jitter(MY_TABLE_perDVD$ParentsAge,3), jitter(MY_TABLE_perDVD$PairBroodNb,3))# 


is.numeric(MY_TABLE_perDVD$RelTimeHrs) # number of hours after sunrise for that day
summary(MY_TABLE_perDVD$RelTimeHrs) # 6 NA's > if this covariate is use, reduce MY_TABLE_perDVD from those RelTimeHrs NAs
scatter.smooth(MY_TABLE_perDVD$AlternationValue,MY_TABLE_perDVD$RelTimeHrs)# linear ? >linear enough to keep it as it is ?
scatter.smooth(MY_TABLE_perDVD$RelTimeHrs,MY_TABLE_perDVD$AlternationValue)# linear ? >linear enough to keep it as it is ?

hist(MY_TABLE_perDVD$AlternationValue)


boxcox(lm(AlternationValue ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE), data = MY_TABLE_perDVD))
	
hist(MY_TABLE_perDVD$AlternationValue^1.35)
shapiro.test(MY_TABLE_perDVD$AlternationValue^1.6) # not normal 	


}

{# modA

modA <- lmer(AlternationValue^1.6~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
	# + (1|PairIDYear) # explain 0% of the variance
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])

summary(modA) # Number of obs: 1593, groups:  BroodRef, 869; PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12


{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modA), resid(modA))	# box cox 1.6 makes it worse
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modA))
qqline(resid(modA))
qqnorm(unlist(ranef(modA))) 
qqline(unlist(ranef(modA)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modA))),fitted(modA)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modA)$BroodRef))
mean(unlist(ranef(modA)$SocialMumID))
mean(unlist(ranef(modA)$SocialDadID))
mean(unlist(ranef(modA)$PairID))
mean(unlist(ranef(modA)$BreedingYear))

# residuals vs predictors
scatter.smooth(MY_TABLE_perDVD$ParentsAge[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD$HatchingDayAfter0401[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD$DVDInfoChickNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD$ChickAgeCat[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$DiffVisit1Rate[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA)) # one influential data point
abline(h=0, lty=2)	

	# MY_TABLE_perDVD[MY_TABLE_perDVD$DiffVisit1Rate > 40,] # DVDRef == 2337
	# scatter.smooth(MY_TABLE_perDVD$DiffVisit1Rate[!is.na(MY_TABLE_perDVD$RelTimeHrs) & MY_TABLE_perDVD$DVDRef != 2337], resid(modA)) # when modA made witghout this datapoint
	# abline(h=0, lty=2)	

scatter.smooth(MY_TABLE_perDVD$RelTimeHrs[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)		

# dependent variable vs fitted
d <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
d$fitted <- fitted(modA)
scatter.smooth(d$fitted, jitter(d$AlternationValue^1.6, 0.05),ylim=c(0, 100^1.6))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ChickAgeCat")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")

}

}

summary(modA)$coefficients
print(VarCorr(modA),comp=c("Variance","Std.Dev."))

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
	, data = MY_TABLE_perDVD)

summary(modA_ParentAge) # Number of obs: 1696, groups:  BroodRef, 916; PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modA_ParentAge), resid(modA_ParentAge))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modA_ParentAge))
qqline(resid(modA_ParentAge))
qqnorm(unlist(ranef(modA_ParentAge))) 
qqline(unlist(ranef(modA_ParentAge)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modA_ParentAge))),fitted(modA_ParentAge)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modA_ParentAge)$BroodRef))
mean(unlist(ranef(modA_ParentAge)$SocialMumID))
mean(unlist(ranef(modA_ParentAge)$SocialDadID))
mean(unlist(ranef(modA_ParentAge)$PairID))
mean(unlist(ranef(modA_ParentAge)$BreedingYear))

# residuals vs fitted
scatter.smooth(fitted(modA_ParentAge), resid(modA_ParentAge))	
abline(h=0, lty=2)

# residuals vs predictors
scatter.smooth(MY_TABLE_perDVD$ParentsAge[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD$HatchingDayAfter0401[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD$DVDInfoChickNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD$ChickAgeCat[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$DiffVisit1Rate[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_ParentAge)) # one influencal data point
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$RelTimeHrs[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_ParentAge))
abline(h=0, lty=2)		

# dependent variable vs fitted
d <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
d$fitted <- fitted(modA_ParentAge)
scatter.smooth(d$fitted, jitter(d$AlternationValue, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ChickAgeCat")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
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
											, data = MY_TABLE_perDVD)
								

summary(modA_PairBroodNb)# Number of obs: 1696, groups:  BroodRef, 916; PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modA_PairBroodNb), resid(modA_PairBroodNb))	
abline(h=0, lty=2)

# qqplots residuals and ranef
qqnorm(resid(modA_PairBroodNb))
qqline(resid(modA_PairBroodNb))
qqnorm(unlist(ranef(modA_PairBroodNb))) # not quite normal ?
qqline(unlist(ranef(modA_PairBroodNb)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modA_PairBroodNb))),fitted(modA_PairBroodNb)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modA_PairBroodNb)$BroodRef))
mean(unlist(ranef(modA_PairBroodNb)$SocialMumID))
mean(unlist(ranef(modA_PairBroodNb)$SocialDadID))
mean(unlist(ranef(modA_PairBroodNb)$PairID))
mean(unlist(ranef(modA_PairBroodNb)$BreedingYear))

# residuals vs predictors
plot(MY_TABLE_perDVD$PairBroodNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD$HatchingDayAfter0401[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD$DVDInfoChickNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD$ChickAgeCat[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$DiffVisit1Rate[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_PairBroodNb)) # one influencal data point
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$RelTimeHrs[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA_PairBroodNb))
abline(h=0, lty=2)		

# dependent variable vs fitted
d <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
d$fitted <- fitted(modA_PairBroodNb)
scatter.smooth(d$fitted, jitter(d$AlternationValue, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
plot(d$PairBroodNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="PairBroodNb")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ChickAgeCat")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")

}

}

summary(modA_ParentAge)$coefficients
summary(modA_PairBroodNb)$coefficients
print(VarCorr(modA_ParentAge),comp=c("Variance","Std.Dev."))
print(VarCorr(modA_PairBroodNb),comp=c("Variance","Std.Dev."))


{# modA_NbRinged

modA_NbRinged <- lmer(AlternationValue ~ scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
											scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
											scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
											scale(NbRinged, scale=FALSE) + # like in Kat&Ben's paper: use brood size d11
											ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
											DiffVisit1Rate +  
											scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
											(1|BroodRef) + 
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
											# + (1|PairIDYear) # explain 0% of the variance
											, data = MY_TABLE_perDVD)
								

summary(modA_NbRinged)# Number of obs: 1696, groups:  BroodRef, 916; PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12
}


{# modA_Age6

{# take the youngest age of duplicates 6 and 6+ and check dependent variable
dat6 <- MY_TABLE_perDVD[MY_TABLE_perDVD$ChickAgeCat == "Age06",]
dat6 <- dat6[ order(dat6$ChickAge), ] 
dat6 <- dat6[!duplicated(dat6[,c('BroodRef')]),]
nrow(dat6)
length(dat6$BroodRef)

boxcox(lm(AlternationValue ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE), data = dat6))
	
hist(dat6$AlternationValue^1.6)
shapiro.test(dat6$AlternationValue^1.6) # normal
}

modA_Age6 <- lmer(AlternationValue^1.6 ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + 
	scale(PairBroodNb, scale=FALSE) + 
	scale(DVDInfoChickNb, scale=FALSE) + 
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
	, data = dat6[!is.na(dat6$RelTimeHrs),])

summary(modA_Age6) # Number of obs: 830, groups:  PairID, 432; SocialMumID, 285; SocialDadID, 278; BreedingYear, 12


{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modA_Age6), resid(modA_Age6))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modA_Age6))
qqline(resid(modA_Age6))
qqnorm(unlist(ranef(modA_Age6))) 
qqline(unlist(ranef(modA_Age6)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modA_Age6))),fitted(modA_Age6)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modA_Age6)$SocialMumID))
mean(unlist(ranef(modA_Age6)$SocialDadID))
mean(unlist(ranef(modA_Age6)$PairID))
mean(unlist(ranef(modA_Age6)$BreedingYear))

# residuals vs predictors
scatter.smooth(dat6$ParentsAge[!is.na(dat6$RelTimeHrs)], resid(modA_Age6))
abline(h=0, lty=2)
scatter.smooth(dat6$HatchingDayAfter0401[!is.na(dat6$RelTimeHrs)], resid(modA_Age6))
abline(h=0, lty=2)
plot(dat6$DVDInfoChickNb[!is.na(dat6$RelTimeHrs)], resid(modA_Age6))
abline(h=0, lty=2)	
scatter.smooth(dat6$DiffVisit1Rate[!is.na(dat6$RelTimeHrs)], resid(modA_Age6)) # one influencal data point
abline(h=0, lty=2)	
scatter.smooth(dat6$RelTimeHrs[!is.na(dat6$RelTimeHrs)], resid(modA_Age6))
abline(h=0, lty=2)		

# dependent variable vs fitted
d <- dat6[!is.na(dat6$RelTimeHrs),]
d$fitted <- fitted(modA_Age6)
scatter.smooth(d$fitted, jitter(d$AlternationValue^1.6, 0.05),ylim=c(0, 100^1.6))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")

}

}

{# modA_Age10

{# take the youngest age of duplicates 10 and 10+ and check dependent variable
dat10 <- MY_TABLE_perDVD[MY_TABLE_perDVD$ChickAgeCat == "Age10",]
dat10 <- dat10[ order(dat10$ChickAge), ] 
dat10 <- dat10[!duplicated(dat10[,c('BroodRef')]),]
dat10 <- dat10[!is.na(dat10$RelTimeHrs),]
nrow(dat10)

boxcox(lm(AlternationValue ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE), data = dat10))
	
hist(dat10$AlternationValue^1.3)
shapiro.test(dat10$AlternationValue^1.3) # normal

}

modA_Age10 <- lmer(AlternationValue^1.3 ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + 
	scale(PairBroodNb, scale=FALSE) + 
	scale(DVDInfoChickNb, scale=FALSE) + 
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + 
	(1|SocialMumID)+ (1|SocialDadID) 
	+ (1|PairID) #explained 0% of variance
	+ (1|BreedingYear) #explained 0% of variance
	, data = dat10[!is.na(dat10$RelTimeHrs),])

summary(modA_Age10) #Number of obs: 719, groups:  PairID, 391; SocialMumID, 274; SocialDadID, 262; BreedingYear, 12

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modA_Age10), resid(modA_Age10))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modA_Age10))
qqline(resid(modA_Age10))
qqnorm(unlist(ranef(modA_Age10))) 
qqline(unlist(ranef(modA_Age10)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modA_Age10))),fitted(modA_Age10)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modA_Age10)$SocialMumID))
mean(unlist(ranef(modA_Age10)$SocialDadID))
mean(unlist(ranef(modA_Age10)$PairID))
mean(unlist(ranef(modA_Age10)$BreedingYear))

# residuals vs predictors
scatter.smooth(dat10$ParentsAge[!is.na(dat10$RelTimeHrs)], resid(modA_Age10))
abline(h=0, lty=2)
scatter.smooth(dat10$HatchingDayAfter0401[!is.na(dat10$RelTimeHrs)], resid(modA_Age10))
abline(h=0, lty=2)
plot(dat10$DVDInfoChickNb[!is.na(dat10$RelTimeHrs)], resid(modA_Age10))
abline(h=0, lty=2)	
scatter.smooth(dat10$DiffVisit1Rate[!is.na(dat10$RelTimeHrs)], resid(modA_Age10)) # one influencal data point
abline(h=0, lty=2)	
scatter.smooth(dat10$RelTimeHrs[!is.na(dat10$RelTimeHrs)], resid(modA_Age10))
abline(h=0, lty=2)		

# dependent variable vs fitted
d <- dat10[!is.na(dat10$RelTimeHrs),]
d$fitted <- fitted(modA_Age10)
scatter.smooth(d$fitted, jitter(d$AlternationValue, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")

}

}

summary(modA_Age6)$coefficients
summary(modA_Age10)$coefficients
print(VarCorr(modA_Age6),comp=c("Variance","Std.Dev."))
print(VarCorr(modA_Age10),comp=c("Variance","Std.Dev."))

{# modA_withinIndAgeEffect

modA_withinIndAgeEffect <- lmer(AlternationValue^1.6~  

	scale(meanMumAge, scale=FALSE) + 
	scale(DeltaMumAge, scale=FALSE) +
	scale(meanDadAge, scale=FALSE) + 
	scale(DeltaDadAge, scale=FALSE) +
	#MumAge+
	#DadAge+
	#scale(LastMumReproAge, scale=FALSE) +
	#scale(LastDadReproAge, scale=FALSE) +
	#scale(FirstMumReproAge, scale=FALSE) +
	#scale(FirstDadReproAge, scale=FALSE) +

	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
	# + (1|PairIDYear) # explain 0% of the variance
	, data = MY_TABLE_perDVD)
	
summary(modA_withinIndAgeEffect)
# removing all Age covariate > hatching date become NS

# see graph in create in paragraph 'create MY_TABLE_perBirdYear', 'get mean Alternation per year per BirdID'

}

}

summary(modA)

{#### repeatability of Alternation 

VarianceRandomEffectsAlternation <- as.data.frame(VarCorr(modA),comp=c("Variance","Std.Dev."))[,c(1,4,5)]

VarianceRandomEffectsAlternation$vcov[VarianceRandomEffectsAlternation$grp=='SocialDadID'] / sum(VarianceRandomEffectsAlternation$vcov) *100 # % variance explained by MID
VarianceRandomEffectsAlternation$vcov[VarianceRandomEffectsAlternation$grp=='SocialMumID'] / sum(VarianceRandomEffectsAlternation$vcov) *100 # % variance explained by FID
VarianceRandomEffectsAlternation$vcov[VarianceRandomEffectsAlternation$grp=='PairID'] / sum(VarianceRandomEffectsAlternation$vcov) *100 # % variance explained by PairID
VarianceRandomEffectsAlternation$vcov[VarianceRandomEffectsAlternation$grp=='BroodRef'] / sum(VarianceRandomEffectsAlternation$vcov) *100 # % variance explained by BroodRef

{# correlation of provisioning rate accross two randomly picked nestwatches (among those that have 2 or 3 nest watches) within a brood - like Kat & Ben
	# this does not take into account the pseudoreplication of pairs having several broods together
	# nor that individual have several broods with different partner
	# nor that this happen in different years
	
MY_TABLE_perDVD_perBroodRef <-  split(MY_TABLE_perDVD,MY_TABLE_perDVD$BroodRef)

# x <- MY_TABLE_perDVD_perBroodRef[['1398']]
# x <- MY_TABLE_perDVD_perBroodRef[['5']]

MY_TABLE_perDVD_perBroodRef_fun <- function(x,A){

if(nrow(x)>1)
{
A <-sample(x$AlternationValue,2)
}

if(nrow(x)==1)
{
A <-NA
}

return(A)

}

MY_TABLE_perDVD_perBroodRef_out1 <- lapply(MY_TABLE_perDVD_perBroodRef, FUN=MY_TABLE_perDVD_perBroodRef_fun)
MY_TABLE_perDVD_perBroodRef_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perDVD_perBroodRef_out1)),do.call(rbind, MY_TABLE_perDVD_perBroodRef_out1))

nrow(MY_TABLE_perDVD_perBroodRef_out2)	# 872
rownames(MY_TABLE_perDVD_perBroodRef_out2) <- NULL
colnames(MY_TABLE_perDVD_perBroodRef_out2) <- c('BroodRef','Ay','Ax')

scatter.smooth(MY_TABLE_perDVD_perBroodRef_out2$Ay~ MY_TABLE_perDVD_perBroodRef_out2$Ax)
abline(0,1)
cor.test(MY_TABLE_perDVD_perBroodRef_out2$Ay, MY_TABLE_perDVD_perBroodRef_out2$Ax)
}

{# repeatability using MCMCglmm

MY_TABLE_perDVD_wihoutNA <-MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]

# http://www.wildanimalmodels.org/tiki-index.php?page=repeated%20measures
# here we are just using weak priors where the 
# phenotypic variation is split among the various
# factors:
p.var<-var(MY_TABLE_perDVD_wihoutNA$AlternationValue,na.rm=TRUE)

prior_modA_MCMCglmm<-list(G=list(
					  G1=list(V=matrix(p.var/6),n=1),
                      G2=list(V=matrix(p.var/6),n=1),
                      G3=list(V=matrix(p.var/6),n=1),
                      G4=list(V=matrix(p.var/6),n=1),
					  G5=list(V=matrix(p.var/6),n=1)),
                      R=list(V=matrix(p.var/6),n=1))

modA_MCMCglmm <- MCMCglmm(AlternationValue~1+ParentsAge+HatchingDayAfter0401+PairBroodNb+DVDInfoChickNb+ChickAgeCat+DiffVisit1Rate+RelTimeHrs,
												random = ~BroodRef+SocialMumID+SocialDadID+PairID+BreedingYear,
												data=MY_TABLE_perDVD_wihoutNA,
												prior = prior_modA_MCMCglmm)

summary(modA_MCMCglmm)
posterior.mode(modA_MCMCglmm$VCV)											
												
VP_Alternation <-  modA_MCMCglmm$VCV[,"BroodRef"]+ modA_MCMCglmm$VCV[,"SocialMumID"]+ modA_MCMCglmm$VCV[,"SocialDadID"]+ modA_MCMCglmm$VCV[,"PairID"]+ modA_MCMCglmm$VCV[,"BreedingYear"]+modA_MCMCglmm$VCV[,"units"]

R_Alternation_BroodRef <- modA_MCMCglmm$VCV[,"BroodRef"]/VP_Alternation
posterior.mode(R_Alternation_BroodRef)
HPDinterval(R_Alternation_BroodRef)

R_Alternation_SocialMumID <- modA_MCMCglmm$VCV[,"SocialMumID"]/VP_Alternation
posterior.mode(R_Alternation_SocialMumID)
HPDinterval(R_Alternation_SocialMumID)

R_Alternation_SocialDadID <- modA_MCMCglmm$VCV[,"SocialDadID"]/VP_Alternation
posterior.mode(R_Alternation_SocialDadID)
HPDinterval(R_Alternation_SocialDadID)

R_Alternation_PairID <- modA_MCMCglmm$VCV[,"PairID"]/VP_Alternation
posterior.mode(R_Alternation_PairID)
HPDinterval(R_Alternation_PairID)

R_Alternation_BreedingYear <- modA_MCMCglmm$VCV[,"BreedingYear"]/VP_Alternation
posterior.mode(R_Alternation_BreedingYear)
HPDinterval(R_Alternation_BreedingYear)

}

}

{#### fitness correlate of alternation

{## total provisioning rate

{# check dependent and explanatory variables

hist(MY_TABLE_perBrood$TotalProRate)
summary(MY_TABLE_perBrood$TotalProRate)
shapiro.test(MY_TABLE_perBrood$TotalProRate) 

boxcox(lm(TotalProRate ~  NbRinged + poly(MeanAdev,1), data = MY_TABLE_perBrood))
hist(MY_TABLE_perBrood$TotalProRate^0.45)
shapiro.test(MY_TABLE_perBrood$TotalProRate^0.45) 

summary(MY_TABLE_perBrood$NbRinged)

}

modFitnessAsProRate <- lmer(TotalProRate^0.45 ~  NbRinged + 
											poly(MeanAdev,1) 
											+(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
											# + (1|PairIDYear) # explain 0% of the variance
											, data = MY_TABLE_perBrood)
											
summary(modFitnessAsProRate) # Number of obs: 919, groups:  PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12


{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsProRate), resid(modFitnessAsProRate))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modFitnessAsProRate))
qqline(resid(modFitnessAsProRate))
qqnorm(unlist(ranef(modFitnessAsProRate)))
qqline(unlist(ranef(modFitnessAsProRate)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modFitnessAsProRate))),fitted(modFitnessAsProRate)) # quite not ! > much nicer if exp 0.45
	# tried when removing the 5% quantile extreme of provisioning rate, model estimates quite similar, random effect all much much lower

# Mean of ranefs: should be zero
mean(unlist(ranef(modFitnessAsProRate)$SocialMumID))
mean(unlist(ranef(modFitnessAsProRate)$SocialDadID))
mean(unlist(ranef(modFitnessAsProRate)$PairID))
mean(unlist(ranef(modFitnessAsProRate)$BreedingYear))

# residuals vs predictors
plot(MY_TABLE_perBrood$NbRinged, resid(modFitnessAsProRate))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perBrood$MeanAdev, resid(modFitnessAsProRate))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perBrood
d$fitted <- fitted(modFitnessAsProRate)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$MeanAdev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="MeanAdev") # polynomial ?

}


modFitnessAsProRate_poly <- lmer(TotalProRate^0.45 ~  NbRinged +
											poly(MeanAdev,2) +
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
											# + (1|PairIDYear) # explain 0% of the variance
											, data = MY_TABLE_perBrood)
											
summary(modFitnessAsProRate_poly) # Number of obs: 919, groups:  PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsProRate_poly), resid(modFitnessAsProRate_poly))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modFitnessAsProRate_poly))	# improved with exp 0.45
qqline(resid(modFitnessAsProRate_poly))
qqnorm(unlist(ranef(modFitnessAsProRate_poly)))
qqline(unlist(ranef(modFitnessAsProRate_poly)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modFitnessAsProRate_poly))),fitted(modFitnessAsProRate_poly)) # quite not ! > much nicer if exp 0.45
	# tried when removing the 5% quantile extreme of provisioning rate, model estimates quite similar, random effect all much much lower

# Mean of ranefs: should be zero
mean(unlist(ranef(modFitnessAsProRate_poly)$SocialMumID))
mean(unlist(ranef(modFitnessAsProRate_poly)$SocialDadID))
mean(unlist(ranef(modFitnessAsProRate_poly)$PairID))
mean(unlist(ranef(modFitnessAsProRate_poly)$BreedingYear))

# residuals vs predictors
plot(MY_TABLE_perBrood$NbRinged, resid(modFitnessAsProRate_poly))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perBrood$MeanAdev, resid(modFitnessAsProRate_poly))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perBrood
d$fitted <- fitted(modFitnessAsProRate_poly)
scatter.smooth(d$fitted, jitter(d$TotalProRate^0.45, 0.05),ylim=c(0, 100^0.45))
abline(0,1)	

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$MeanAdev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="MeanAdev") # polynomial ?

}



}

{## mean chick mass

{# check dependent and explanatory variables
nrow(MY_TABLE_perBrood[ MY_TABLE_perBrood$NbRinged == 0 ,]) # 45 broods with no ringed chicks
nrow(MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,]) # 20 broods where ringed chicks but no mass nor tarsus: for some reasons were ringed not at the rigth age for comparable measurements)
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,] # 2 broods with ringed with mass but not tarsus

scatter.smooth(MY_TABLE_perBrood$AvgMass~ MY_TABLE_perBrood$AvgTarsus)

}


modFitnessAsChickMass <- lmer(AvgMass ~ NbRinged + 
										MeanA + # Kat&Ben's paper: I assume they used again the average of alternation per nest 
										AvgTarsus +
										(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) ,
										data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$AvgMass) &!is.na(MY_TABLE_perBrood$AvgTarsus), ] )
										
summary(modFitnessAsChickMass) # Number of obs: 805, groups:  PairID, 426; SocialMumID, 282; SocialDadID, 273; BreedingYear, 12

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsChickMass), resid(modFitnessAsChickMass))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modFitnessAsChickMass))
qqline(resid(modFitnessAsChickMass))
qqnorm(unlist(ranef(modFitnessAsChickMass))) # not quite normal ?
qqline(unlist(ranef(modFitnessAsChickMass)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modFitnessAsChickMass))),fitted(modFitnessAsChickMass)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modFitnessAsChickMass)$SocialMumID))
mean(unlist(ranef(modFitnessAsChickMass)$SocialDadID))
mean(unlist(ranef(modFitnessAsChickMass)$PairID))
mean(unlist(ranef(modFitnessAsChickMass)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MeanA) & !is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass),]
plot(d$NbRinged, resid(modFitnessAsChickMass))
abline(h=0, lty=2)
scatter.smooth(d$MeanA, resid(modFitnessAsChickMass))
abline(h=0, lty=2)
scatter.smooth(d$AvgTarsus, resid(modFitnessAsChickMass))
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(modFitnessAsChickMass)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AvgMass", xlab="NbRinged")
scatter.smooth(d$MeanA,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AvgMass", xlab="MeanA")

}


modFitnessAsResChickMass <- lmer(ResMassTarsus ~ NbRinged + 
												 MeanA + 
												 (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) ,
												 data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$ResMassTarsus),])
										
summary(modFitnessAsResChickMass) # Number of obs: 805, groups:  PairID, 426; SocialMumID, 282; SocialDadID, 273; BreedingYear, 12
# identical results as model above

{# model on MY_TABLE_perChick
	# here 'AvgOf' are not averages but simply the Mass and Tarsus of the chick as the minimum age (between 11 and 14) he was measured

modFitnessAsChickMasslikeabove <- lmer(AvgOfMass ~ NbRinged + 
										MeanA + 
										AvgOfTarsus +
										(1|RearingBrood)+
										(1|SocialMumID)+ 
										(1|SocialDadID) +
										(1|PairID) + (1|BreedingYear) 
										#+ (1|dam) + (1|sire) + (1|GenPairID)
										,data = MY_TABLE_perChick[!is.na(MY_TABLE_perChick$AvgOfTarsus),])

summary(modFitnessAsChickMasslikeabove)	# Number of obs: 2096, groups:  RearingBrood, 793; PairID, 424; SocialMumID, 282; SocialDadID, 271; BreedingYear, 12


modFitnessAsChickMasswithGenParents <- lmer(AvgOfMass ~ NbRinged + 
										MeanA + 
										AvgOfTarsus +
										(1|RearingBrood)+
										(1|SocialMumID)+ (1|SocialDadID) + 
										(1|PairID) + (1|BreedingYear) 
										+ (1|dam) + (1|sire) + (1|GenPairID)
										, data = MY_TABLE_perChick[!is.na(MY_TABLE_perChick$AvgOfTarsus),])

summary(modFitnessAsChickMasswithGenParents) # Number of obs: 2061, groups:  RearingBrood, 790; GenPairID, 683; PairID, 424; sire, 308; dam, 294; SocialMumID, 282; SocialDadID, 271; BreedingYear, 12

modFitnessAsChickMassRedidualswithGenParents <- lmer(ResMassTarsus_perChick ~ NbRinged + 
												MeanA + 
												(1|RearingBrood)+
												(1|SocialMumID)+ (1|SocialDadID) + 
												(1|PairID) + (1|BreedingYear) 
												+ (1|dam) + (1|sire) + (1|GenPairID)
												, data = MY_TABLE_perChick[!is.na(MY_TABLE_perChick$ResMassTarsus_perChick),])

summary(modFitnessAsChickMassRedidualswithGenParents) # 2061, groups:  RearingBrood, 790; GenPairID, 683; PairID, 424; sire, 308; dam, 294; SocialMumID, 282; SocialDadID, 271; BreedingYear, 12


print(VarCorr(modFitnessAsChickMass),comp=c("Variance","Std.Dev."))
print(VarCorr(modFitnessAsChickMasswithGenParents),comp=c("Variance","Std.Dev."))
summary(modFitnessAsChickMass)$coefficients
summary(modFitnessAsChickMasswithGenParents)$coefficients
}

}

{## number of chicks ringed

hist(MY_TABLE_perBrood$NbRinged)

# modFitnessAsNbRinged <- glmer(NbRinged ~ #scale(MeanA, scale=FALSE) + 
										# poly(MeanA,2)+
										# # (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										# (1|BreedingYear) , data = MY_TABLE_perBrood, family="poisson")

modFitnessAsNbRinged <- lmer(NbRinged ~ #scale(MeanA, scale=FALSE) + 
										poly(MeanA,2)+
										# (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(modFitnessAsNbRinged) # Model is nearly unidentifiable

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsNbRinged), resid(modFitnessAsNbRinged))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modFitnessAsNbRinged))
qqline(resid(modFitnessAsNbRinged))
qqnorm(unlist(ranef(modFitnessAsNbRinged))) 
qqline(unlist(ranef(modFitnessAsNbRinged)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(modFitnessAsNbRinged)$SocialMumID))
# mean(unlist(ranef(modFitnessAsNbRinged)$SocialDadID))
# mean(unlist(ranef(modFitnessAsNbRinged)$PairID))
mean(unlist(ranef(modFitnessAsNbRinged)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood
scatter.smooth(d$MeanA, resid(modFitnessAsNbRinged)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)

summary(MY_TABLE_perBrood$MeanMFVisit1[MY_TABLE_perBrood$MeanA >80])
summary(MY_TABLE_perBrood$MeanMFVisit1[MY_TABLE_perBrood$MeanA <=80])

# dependent variable vs fitted
d$fitted <- fitted(modFitnessAsNbRinged)
scatter.smooth(d$fitted, jitter(d$NbRinged, 0.05),ylim=c(0, 10))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$MeanA,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="MeanA")

}

}

{## Parent survival

{# check dependent and explanatory variables
summary(MY_TABLE_perBirdYear$AliveNextYear)
scatter.smooth(MY_TABLE_perBirdYear$MeanAYear, MY_TABLE_perBirdYear$Age)
scatter.smooth(MY_TABLE_perBirdYear$MeanAYear[MY_TABLE_perBirdYear$Sex == 1], MY_TABLE_perBirdYear$Age[MY_TABLE_perBirdYear$Sex == 1])
scatter.smooth(MY_TABLE_perBirdYear$MeanAYear[MY_TABLE_perBirdYear$Sex == 1], MY_TABLE_perBirdYear$Age[MY_TABLE_perBirdYear$Sex == 0])
table( MY_TABLE_perBirdYear$AliveNextYear[MY_TABLE_perBirdYear$Sex == 0])
table( MY_TABLE_perBirdYear$AliveNextYear[MY_TABLE_perBirdYear$Sex == 1])


}

modSurvival <- glmer(AliveNextYear ~ MeanAYear + Sex + Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear, family = "binomial" )
									
summary(modSurvival) # Number of obs: 1006, groups:  BirdID, 543; BreedingYear, 11


modSurvivalMale <- glmer(AliveNextYear ~ scale(MeanAYear, scale=FALSE) + Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$Sex == 1,], family = "binomial" )
									
summary(modSurvivalMale) # Number of obs: 503, groups:  BirdID, 268; BreedingYear, 11
# birdID explains a lot of variance 

modSurvivalFemale <- glmer(AliveNextYear ~ MeanAYear + Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$Sex == 0,], family = "binomial" )
									
summary(modSurvivalFemale) # Number of obs: 503, groups:  BirdID, 275; BreedingYear, 11
# birdID do not explains a lot of variance


modSurvival_SexAgeInteraction <- glmer(AliveNextYear ~ MeanAYear + Sex*Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear, family = "binomial" )
									
summary(modSurvival_SexAgeInteraction) #Number of obs: 1006, groups:  BirdID, 543; BreedingYear, 11





{# model assumptions checking >> residuals not normal !!!!!!

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modSurvival), resid(modSurvival))	# awful !
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modSurvival))# not quite normal !
qqline(resid(modSurvival))

{# get our qqplot within others:
N <- length(resid(modSurvival))
sigma <- summary(modSurvival)$sigma # Extract the estimated standard deviation of the errors
par(mfrow=c(3,3))  
rnum<-sample(1:9, 1)
for(i in 1:(rnum-1)){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
  }
qqnorm(resid(modSurvival), main=rnum)
qqline(resid(modSurvival))
for(i in (rnum+1):9){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
  }
  }
# can we see our plot ? solution is:
rnum

qqnorm(unlist(ranef(modSurvival))) 
qqline(unlist(ranef(modSurvival)))


# check for overdispersion
modSurvival_withOverdispersionAccounted <- glmer(AliveNextYear ~ MeanAYear + Sex + Age +
									(1|BirdID) +
									(1|BreedingYear)+
									(1|BirdIDYear) # overdispersion parameter
									, data = MY_TABLE_perBirdYear, family = "binomial" )
summary(modSurvival_withOverdispersionAccounted)
anova(modSurvival, modSurvival_withOverdispersionAccounted) # p = 0.6037


# Mean of ranefs: should be zero
mean(unlist(ranef(modSurvival)$BirdID))
mean(unlist(ranef(modSurvival)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBirdYear
plot(d$MeanAYear, resid(modSurvival))
abline(h=0, lty=2)
plot(d$Sex, resid(modSurvival))
abline(h=0, lty=2)
plot(d$Age, resid(modSurvival))
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(modSurvival)
plot(d$fitted, d$AliveNextYear,ylim=c(0, 1))
abline(0,1)	

# fitted vs all predictors
plot(d$MeanAYear,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AliveNextYear", xlab="MeanAYear")
plot(d$Sex,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AliveNextYear", xlab="Sex")
plot(d$Age,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AliveNextYear", xlab="Age")

}


{# survival analysis per year >> residuals not normal !!!!!!

modSurvival2004 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2004,], family = "binomial" )
summary(modSurvival2004)

modSurvival2005 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2005,], family = "binomial" )
summary(modSurvival2005)

modSurvival2006 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2006,], family = "binomial" )
summary(modSurvival2006)

modSurvival2007 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2007,], family = "binomial" )
summary(modSurvival2007)

# modSurvival2008 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									# , data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2008,], family = "binomial" )
# summary(modSurvival2008)

modSurvival2009 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2009,], family = "binomial" )
summary(modSurvival2009)

modSurvival2010 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2010,], family = "binomial" )
summary(modSurvival2010)

modSurvival2011 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2011,], family = "binomial" )
summary(modSurvival2011)

modSurvival2012 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2012,], family = "binomial" )
summary(modSurvival2012)

modSurvival2013 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2013,], family = "binomial" )
summary(modSurvival2013)

modSurvival2014 <- glm(AliveNextYear ~ MeanAYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2014,], family = "binomial" )
summary(modSurvival2014)


{# model assumptions checking >> residuals aweful !

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modSurvival2004), resid(modSurvival2004))	# aweful
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modSurvival2004))# terrible !
qqline(resid(modSurvival2004))

# residuals vs predictors
d <- MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2004,]
scatter.smooth(d$MeanAYear, resid(modSurvival2004))
abline(h=0, lty=2)
plot(d$Sex, resid(modSurvival2004))
abline(h=0, lty=2)
plot(d$Age, resid(modSurvival2004))
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(modSurvival2004)
plot(d$fitted, d$AliveNextYear,ylim=c(0, 1))

# fitted vs all predictors
plot(d$Sex,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="Survival", xlab="Sex")
plot(d$MeanAYear,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="Survival", xlab="MeanAYear")
plot(d$Age,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="Survival", xlab="Age")

}



}

{# using the survival package and the cox proportional hazard > NOT DONE !!!!
# library(survival)

# MY_TABLE_perBirdYear$DeadNextYearYN[MY_TABLE_perBirdYear$AliveNextYear == TRUE] <- 0
# MY_TABLE_perBirdYear$DeadNextYearYN[MY_TABLE_perBirdYear$AliveNextYear == FALSE] <- 1
# head(MY_TABLE_perBirdYear)


# modSurvival <- glmer(AliveNextYear ~ MeanAYear + Sex + Age +
									# (1|BirdID) +
									# #(1|PairID) + 
									# (1|BreedingYear)
									# , data = MY_TABLE_perBirdYear, family = "binomial" )
									
# coxph(Surv(BreedingYear, DeadNextYearYN) ~ MeanAYear + strata(Sex) + Age, MY_TABLE_perBirdYear) 

	
# test1 <- list(time=c(4,3,1,1,2,2,3), 
# status=c(1,1,1,0,1,1,0), 
# x=c(0,2,1,1,1,0,0), 
# sex=c(0,0,0,0,1,1,1)) 
# # Fit a stratified model 
# coxph(Surv(time, status) ~ x + strata(sex), test1) 
# # Create a simple data set for a time-dependent model 
# test2 <- list(start=c(1,2,5,2,1,7,3,4,8,8), 
              # stop=c(2,3,6,7,8,9,9,9,14,17), 
              # event=c(1,1,1,1,1,1,1,0,0,0), 
              # x=c(1,0,0,1,0,1,1,1,0,0)) 
# summary(coxph(Surv(start, stop, event) ~ x, test2)) \
# bladder1 <- bladder[bladder$enum < 5, ] 
# coxph(Surv(stop, event) ~ (rx + size + number) * strata(enum) + 
      # cluster(id), bladder1)
}

}

}

summary(modFitnessAsProRate)
summary(modFitnessAsChickMass)
summary(modSurvival)




#####################
# PROVISIONING RATE #
#####################

{### get provisioning rate for both sex piled up
FemaleProRate <- MY_TABLE_perDVD[,c("FVisit1RateH","DVDInfoChickNb","ChickAgeCat","HatchingDayAfter0401","RelTimeHrs", 
							"DVDRef","BroodRef","SocialMumID", "SocialDadID","PairID", "BreedingYear")]
MaleProRate <- MY_TABLE_perDVD[,c("MVisit1RateH","DVDInfoChickNb","ChickAgeCat","HatchingDayAfter0401","RelTimeHrs", 
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
BirdProRate <- BirdProRate[!is.na(BirdProRate$RelTimeHrs),]

}

head(BirdProRate)


{# correlation between female and male number of visits
head(MY_TABLE_perDVD)

scatter.smooth(MY_TABLE_perDVD$MVisit1 , jitter(MY_TABLE_perDVD$FVisit1,3))

mod_AbsPro_male <- lmer(MVisit1~FVisit1 + (1|BroodRef) + (1|SocialDadID)+ (1|SocialMumID) + (1|BreedingYear) + (1|PairID)
					, data=MY_TABLE_perDVD)
summary(mod_AbsPro_male)

mod_AbsPro_female <- lmer(FVisit1~MVisit1 + (1|BroodRef) + (1|SocialDadID)+ (1|SocialMumID) + (1|BreedingYear) + (1|PairID)
					, data=MY_TABLE_perDVD)
summary(mod_AbsPro_female)


mod_ProRate_male <- lmer(MVisit1RateH~FVisit1RateH + (1|BroodRef) + (1|SocialDadID)+ (1|SocialMumID) + (1|BreedingYear) + (1|PairID)
					, data=MY_TABLE_perDVD)
summary(mod_ProRate_male)

mod_ProRate_female <- lmer(FVisit1RateH~MVisit1RateH + (1|BroodRef) + (1|SocialDadID)+ (1|SocialMumID) + (1|BreedingYear) + (1|PairID)
					, data=MY_TABLE_perDVD)
summary(mod_ProRate_female)

}


{# repeatability of provisioning rate

{## with Franzi's code

modProRateRpt <- lmer(Visit1RateH ~ Sex +
									scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) +
									(1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE)
									
summary(modProRateRpt)


{modProRateRptwithoutBirdID <- lmer(Visit1RateH ~ Sex + 
												scale(HatchingDayAfter0401, scale=FALSE) + 
												scale(DVDInfoChickNb, scale=FALSE) + 
												ChickAgeCat + 
												scale(RelTimeHrs, scale=FALSE) + 
												(1|BroodRef) + 
												#(1|BirdID)+ 
												(1|SocialPartnerID) + (1|BreedingYear) 
												 + (1|PairID)
												, data = BirdProRate, REML=FALSE)

summary(modProRateRptwithoutBirdID)
#anova(modProRateRpt,modProRateRptwithoutBirdID) # ***

# use parametric bootstrap to simulate the distribution of the likelihood ratio test statistics given the null hypothesis
lrt.obs <- anova(modProRateRpt, modProRateRptwithoutBirdID)$Chisq[2] # save the observed likelihood ratio test statistic
n.sim <- 10  # use 1000 for a real data analysis
lrt.sim <- numeric(n.sim)
for(i in 1:n.sim){
  BirdProRate$ysim <- unlist(simulate(modProRateRptwithoutBirdID)) # simulate new observations from the null-model
  
  modnull <- lmer(ysim ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									#(1|BirdID)+ 
									(1|SocialPartnerID) + (1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE) # fit the null-model
									
  modalt <- lmer(ysim ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + (1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE)  # fit the alternative model
  
  
  lrt.sim[i] <- anova(modnull, modalt)$Chisq[2] # save the likelihood ratio test statistic
  #print(head(BirdProRate)) 
  BirdProRate$ysim <- NULL

  }
  
# compare to a Chisquare distribution with df=1
xx <- seq(0, 40, by=0.02)
xy <- dchisq(xx, df=1)
hist(lrt.sim, xlim=c(0, max(c(lrt.sim, lrt.obs))),breaks= 20,col="blue", xlab="likelihood ratio test statistic", ylab="density", cex.lab=1.5, cex.axis=1.2, freq=FALSE)
abline(v=lrt.obs, col="orange", lwd=3)
box()
lines(xx, xy, lwd=2, col="violet")

# obtain the p-value (proportion of lrt.sim that are higher than lrt.obs)
(sum(lrt.sim>=lrt.obs)+1)/(n.sim+1)  # the observed likelihood ratio has to be considered as part of the distribution
# sim 1000 > p-value  = 0.000999001
# sim 50 >  p-value = 0.01960784 ??
# sim 10 > p-value = 0.09090909

}

{modProRateRptwithoutSocialPartnerID <- lmer(Visit1RateH ~ Sex + 
									scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									#(1|SocialPartnerID) + 
									(1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE)

summary(modProRateRptwithoutSocialPartnerID)
#anova(modProRateRpt,modProRateRptwithoutSocialPartnerID) # ***

# use parametric bootstrap to simulate the distribution of the likelihood ratio test statistics given the null hypothesis
lrt.obs2 <- anova(modProRateRpt, modProRateRptwithoutSocialPartnerID)$Chisq[2] # save the observed likelihood ratio test statistic
n.sim2 <- 10  # use 1000 for a real data analysis
lrt.sim2 <- numeric(n.sim2)
for(i in 1:n.sim2){
  BirdProRate$ysim2 <- unlist(simulate(modProRateRptwithoutSocialPartnerID)) # simulate new observations from the null-model
  
  modnull2 <- lmer(ysim2 ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									#(1|SocialPartnerID) + 
									(1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE) # fit the null-model
									
  modalt2 <- lmer(ysim2 ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + (1|BreedingYear) 
									+ (1|PairID)
									, data = BirdProRate, REML=FALSE)  # fit the alternative model
  
  
  lrt.sim2[i] <- anova(modnull2, modalt2)$Chisq[2] # save the likelihood ratio test statistic
  BirdProRate$ysim2 <- NULL
 
 }
  
# compare to a Chisquare distribution with df=1
xx <- seq(0, 40, by=0.02)
xy <- dchisq(xx, df=1)
hist(lrt.sim2, xlim=c(0, max(c(lrt.sim2, lrt.obs2))),breaks= 20,col="blue", xlab="likelihood ratio test statistic", ylab="density", cex.lab=1.5, cex.axis=1.2, freq=FALSE)
abline(v=lrt.obs2, col="orange", lwd=3)
box()
lines(xx, xy, lwd=2, col="violet")

# obtain the p-value (proportion of lrt.sim that are higher than lrt.obs)
(sum(lrt.sim2>=lrt.obs2)+1)/(n.sim2+1)  # the observed likelihood ratio has to be considered as part of the distribution
# sim 50 >  p-value = 0.01960784 ?? same number as above ??
# sim 10 > p-value = 0.09090909 ?? same number as above ??
}

{modProRateRptwithoutPairID <- lmer(Visit1RateH ~ Sex + 
									scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + 
									(1|BreedingYear) 
									#+ (1|PairID)
									, data = BirdProRate, REML=FALSE)

summary(modProRateRptwithoutPairID)
#anova(modProRateRpt,modProRateRptwithoutPairID) # NS

# use parametric bootstrap to simulate the distribution of the likelihood ratio test statistics given the null hypothesis
lrt.obs3 <- anova(modProRateRpt, modProRateRptwithoutPairID)$Chisq[2] # save the observed likelihood ratio test statistic
n.sim3 <- 10  # use 1000 for a real data analysis
lrt.sim3 <- numeric(n.sim3)
for(i in 1:n.sim3){
  BirdProRate$ysim3 <- unlist(simulate(modProRateRptwithoutPairID, seed =1)) # simulate new observations from the null-model
  
  modnull3 <- lmer(ysim3 ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + 
									(1|BreedingYear) 
									# + (1|PairID)
									, data = BirdProRate, REML=FALSE) # fit the null-model
									
  modalt3 <- lmer(ysim3 ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + (1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE)  # fit the alternative model
  
  
  lrt.sim3[i] <- anova(modnull3, modalt3)$Chisq[2] # save the likelihood ratio test statistic
  BirdProRate$ysim3 <- NULL
 
 }
  
# compare to a Chisquare distribution with df=1
xx <- seq(0, 40, by=0.02)
xy <- dchisq(xx, df=1)
hist(lrt.sim3, xlim=c(0, max(c(lrt.sim3, lrt.obs3))),breaks= 20,col="blue", xlab="likelihood ratio test statistic", ylab="density", cex.lab=1.5, cex.axis=1.2, freq=FALSE)
abline(v=lrt.obs3, col="orange", lwd=3)
box()
lines(xx, xy, lwd=2, col="violet")

# obtain the p-value (proportion of lrt.sim that are higher than lrt.obs)
(sum(lrt.sim3>=lrt.obs3)+1)/(n.sim3+1)  # the observed likelihood ratio has to be considered as part of the distribution
# sim 10 > p value = 1
# sim 50 > p value = 1

}

{modProRateRptwithoutBreedingYear <- lmer(Visit1RateH ~ Sex + 
									scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + 
									#(1|BreedingYear) 
									(1|PairID)
									, data = BirdProRate, REML=FALSE)

summary(modProRateRptwithoutBreedingYear)
#anova(modProRateRpt,modProRateRptwithoutBreedingYear) # ***

# use parametric bootstrap to simulate the distribution of the likelihood ratio test statistics given the null hypothesis
lrt.obs4 <- anova(modProRateRpt, modProRateRptwithoutBreedingYear)$Chisq[2] # save the observed likelihood ratio test statistic
n.sim4 <- 50  # use 1000 for a real data analysis
lrt.sim4 <- numeric(n.sim4)
for(i in 1:n.sim4){
  BirdProRate$ysim4 <- unlist(simulate(modProRateRptwithoutPairID)) # simulate new observations from the null-model
  
  modnull4 <- lmer(ysim4 ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + 
									#(1|BreedingYear) 
									 (1|PairID)
									, data = BirdProRate, REML=FALSE) # fit the null-model
									
  modalt4 <- lmer(ysim4 ~ Sex + scale(HatchingDayAfter0401, scale=FALSE) + 
									scale(DVDInfoChickNb, scale=FALSE) + 
									ChickAgeCat + 
									scale(RelTimeHrs, scale=FALSE) + 
									(1|BroodRef) + 
									(1|BirdID)+ 
									(1|SocialPartnerID) + (1|BreedingYear) 
									 + (1|PairID)
									, data = BirdProRate, REML=FALSE)  # fit the alternative model
  
  
  lrt.sim4[i] <- anova(modnull4, modalt4)$Chisq[2] # save the likelihood ratio test statistic
    BirdProRate$ysim4 <- NULL

  }
  
# compare to a Chisquare distribution with df=1
xx <- seq(0, 40, by=0.02)
xy <- dchisq(xx, df=1)
hist(lrt.sim4, xlim=c(0, max(c(lrt.sim4, lrt.obs4))),breaks= 20,col="blue", xlab="likelihood ratio test statistic", ylab="density", cex.lab=1.5, cex.axis=1.2, freq=FALSE)
abline(v=lrt.obs4, col="orange", lwd=3)
box()
lines(xx, xy, lwd=2, col="violet")

# obtain the p-value (proportion of lrt.sim that are higher than lrt.obs)
(sum(lrt.sim4>=lrt.obs4)+1)/(n.sim4+1)  # the observed likelihood ratio has to be considered as part of the distribution
# sim 10 > p value = 0.5454545
# sim 50 > p value = 0.4117647
# sim 100 > p value = 0.4158416
# sim 1000 > p value = 0.4185814
}

}

{## with RLRT package

{# BirdID ***

m <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BirdID)
						, data = BirdProRate)

mA <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BroodRef) + 
						(1|BirdID)+ (1|SocialPartnerID) + (1|BreedingYear) 
						 + (1|PairID)
						, data = BirdProRate)					
						
m0 <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BroodRef) + 
						#(1|BirdID)+ 
						(1|SocialPartnerID) + (1|BreedingYear) 
						 + (1|PairID)
						, data = BirdProRate)	 	

exactRLRT(m, mA , m0, nsim = 5000)		# RLRT = 41.839, p-value < 2.2e-16				
exactRLRT(m, mA , m0, nsim = 10)		# RLRT = 41.839, p-value < 2.2e-16	

}

{# PairID NS
m_PairID <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|PairID)
						, data = BirdProRate)
					
						
m0_PairID <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BroodRef) + 
						(1|BirdID)+ 
						(1|SocialPartnerID) + (1|BreedingYear) 
						 #+ (1|PairID)
						, data = BirdProRate)	 	

exactRLRT(m_PairID, mA , m0_PairID, nsim = 5000)		#RLRT = 0, p-value = 1
	
}	

{# SocialPartnerID ***
m_SocialPartnerID <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|SocialPartnerID)
						, data = BirdProRate)
					
						
m0_SocialPartnerID <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BroodRef) + 
						(1|BirdID)+ 
						#(1|SocialPartnerID) + 
						(1|BreedingYear) 
						 + (1|PairID)
						, data = BirdProRate)	 	

exactRLRT(m_SocialPartnerID, mA , m0_SocialPartnerID, nsim = 5000)		#RLRT = 27.802, p-value < 2.2e-16

	
}	

{# BreedingYear ***
m_BreedingYear <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BreedingYear)
						, data = BirdProRate)
					
						
m0_BreedingYear <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BroodRef) + 
						(1|BirdID)+ 
						(1|SocialPartnerID) 
						#(1|BreedingYear) 
						 + (1|PairID)
						, data = BirdProRate)	 	

exactRLRT(m_BreedingYear, mA , m0_BreedingYear, nsim = 5000)		#RLRT = 15.707, p-value < 2.2e-16

	
}	

{# BroodRef ***
m_BroodRef <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						(1|BroodRef)
						, data = BirdProRate)
					
						
m0_BroodRef <- lmer(Visit1RateH ~ Sex +
						scale(HatchingDayAfter0401, scale=FALSE) + 
						scale(DVDInfoChickNb, scale=FALSE) + 
						ChickAgeCat + 
						scale(RelTimeHrs, scale=FALSE) + 
						#(1|BroodRef) + 
						(1|BirdID)+ 
						(1|SocialPartnerID) +
						(1|BreedingYear) 
						 + (1|PairID)
						, data = BirdProRate)	 	

exactRLRT(m_BroodRef, mA , m0_BroodRef, nsim = 5000)		#RLRT = 28.183, p-value < 2.2e-16

	
}	

}

}

{# repeatability of provisioning rate per sex
modProRateRpt_Male <- lmer(Visit1RateH ~ scale(HatchingDayAfter0401, scale=FALSE) + 
										scale(DVDInfoChickNb, scale=FALSE) + 
										ChickAgeCat + 
										scale(RelTimeHrs, scale=FALSE) + 
										(1|BroodRef) + 
										(1|BirdID)+ 
										(1|SocialPartnerID) +
										(1|BreedingYear) 
										 + (1|PairID)
										, data = BirdProRate[BirdProRate$Sex == 1,], REML=FALSE)
										
summary(modProRateRpt_Male)

VarianceRandomEffectsMale <- as.data.frame(VarCorr(modProRateRpt_Male),comp=c("Variance","Std.Dev."))[,c(1,4,5)]
VarianceRandomEffectsMale$vcov[VarianceRandomEffectsMale$grp=='BirdID'] / sum(VarianceRandomEffectsMale$vcov) *100 # variance explained by MID



modProRateRpt_Female <- lmer(Visit1RateH ~ scale(HatchingDayAfter0401, scale=FALSE) + 
										scale(DVDInfoChickNb, scale=FALSE) + 
										ChickAgeCat + 
										scale(RelTimeHrs, scale=FALSE) + 
										(1|BroodRef) + 
										(1|BirdID)+ 
										(1|SocialPartnerID) +
										(1|BreedingYear) 
										 + (1|PairID)
										, data = BirdProRate[BirdProRate$Sex == 0,], REML=FALSE)
										
summary(modProRateRpt_Female)
VarianceRandomEffectsFemale <- as.data.frame(VarCorr(modProRateRpt_Female),comp=c("Variance","Std.Dev."))[,c(1,4,5)]
VarianceRandomEffectsFemale$vcov[VarianceRandomEffectsFemale$grp=='BirdID'] / sum(VarianceRandomEffectsFemale$vcov) *100 # variance explained by FID
}

{# repeatability of provisioning rate using MCMCglmm

{# both sexes

p.var_modProRateRpt<-var(BirdProRate$Visit1RateH,na.rm=TRUE)

prior_modProRateRpt_MCMCglmm<-list(G=list(
					  G1=list(V=matrix(p.var_modProRateRpt/6),n=1),
                      G2=list(V=matrix(p.var_modProRateRpt/6),n=1),
                      G3=list(V=matrix(p.var_modProRateRpt/6),n=1),
                      G4=list(V=matrix(p.var_modProRateRpt/6),n=1),
					  G5=list(V=matrix(p.var_modProRateRpt/6),n=1)),
                      R=list(V=matrix(p.var_modProRateRpt/6),n=1))
					  
modProRateRpt_MCMCglmm <- MCMCglmm(Visit1RateH ~
										HatchingDayAfter0401 + 
										DVDInfoChickNb + 
										ChickAgeCat + 
										RelTimeHrs ,
									random= ~
										BroodRef + 
										BirdID+ 
										SocialPartnerID +
										BreedingYear +
										PairID
										, data = BirdProRate
										, prior = prior_modProRateRpt_MCMCglmm)
									
summary(modProRateRpt_MCMCglmm)
posterior.mode(modProRateRpt_MCMCglmm$VCV)											
												
											
VP_ProRate <-  modProRateRpt_MCMCglmm$VCV[,"BroodRef"]+ modProRateRpt_MCMCglmm$VCV[,"BirdID"]+ modProRateRpt_MCMCglmm$VCV[,"SocialPartnerID"]+ modProRateRpt_MCMCglmm$VCV[,"PairID"]+ modProRateRpt_MCMCglmm$VCV[,"BreedingYear"]+modProRateRpt_MCMCglmm$VCV[,"units"]

R_Alternation_BroodRef <- modProRateRpt_MCMCglmm$VCV[,"BroodRef"]/VP_ProRate
posterior.mode(R_Alternation_BroodRef)
HPDinterval(R_Alternation_BroodRef)

R_Alternation_BirdID <- modProRateRpt_MCMCglmm$VCV[,"BirdID"]/VP_ProRate
posterior.mode(R_Alternation_BirdID)
HPDinterval(R_Alternation_BirdID)

R_Alternation_SocialPartnerID <- modProRateRpt_MCMCglmm$VCV[,"SocialPartnerID"]/VP_ProRate
posterior.mode(R_Alternation_SocialPartnerID)
HPDinterval(R_Alternation_SocialPartnerID)

R_Alternation_PairID <- modProRateRpt_MCMCglmm$VCV[,"PairID"]/VP_ProRate
posterior.mode(R_Alternation_PairID)
HPDinterval(R_Alternation_PairID)

R_Alternation_BreedingYear <- modProRateRpt_MCMCglmm$VCV[,"BreedingYear"]/VP_ProRate
posterior.mode(R_Alternation_BreedingYear)
HPDinterval(R_Alternation_BreedingYear)

}

{# separating the sexes

{# Male
modProRateRpt_MCMCglmm_Male <- MCMCglmm(Visit1RateH ~
										HatchingDayAfter0401 + 
										DVDInfoChickNb + 
										ChickAgeCat + 
										RelTimeHrs ,
									random= ~
										BroodRef + 
										BirdID+ 
										SocialPartnerID +
										BreedingYear +
										PairID
										, data = BirdProRate[BirdProRate$Sex == 1,]
										, prior = prior_modProRateRpt_MCMCglmm)
									
summary(modProRateRpt_MCMCglmm_Male)
posterior.mode(modProRateRpt_MCMCglmm_Male$VCV)		
VP_ProRate_Male <-  modProRateRpt_MCMCglmm_Male$VCV[,"BroodRef"]+ modProRateRpt_MCMCglmm_Male$VCV[,"BirdID"]+ modProRateRpt_MCMCglmm_Male$VCV[,"SocialPartnerID"]+ modProRateRpt_MCMCglmm_Male$VCV[,"PairID"]+ modProRateRpt_MCMCglmm_Male$VCV[,"BreedingYear"]+modProRateRpt_MCMCglmm_Male$VCV[,"units"]

R_Alternation_BroodRef_Male <- modProRateRpt_MCMCglmm_Male$VCV[,"BroodRef"]/VP_ProRate_Male
posterior.mode(R_Alternation_BroodRef_Male)
HPDinterval(R_Alternation_BroodRef_Male)

R_Alternation_BirdID_Male <- modProRateRpt_MCMCglmm_Male$VCV[,"BirdID"]/VP_ProRate_Male
posterior.mode(R_Alternation_BirdID_Male)
HPDinterval(R_Alternation_BirdID_Male)

R_Alternation_SocialPartnerID_Male <- modProRateRpt_MCMCglmm_Male$VCV[,"SocialPartnerID"]/VP_ProRate_Male
posterior.mode(R_Alternation_SocialPartnerID_Male)
HPDinterval(R_Alternation_SocialPartnerID_Male)

R_Alternation_PairID_Male <- modProRateRpt_MCMCglmm_Male$VCV[,"PairID"]/VP_ProRate_Male
posterior.mode(R_Alternation_PairID_Male)
HPDinterval(R_Alternation_PairID_Male)

R_Alternation_BreedingYear_Male <- modProRateRpt_MCMCglmm_Male$VCV[,"BreedingYear"]/VP_ProRate_Male
posterior.mode(R_Alternation_BreedingYear_Male)
HPDinterval(R_Alternation_BreedingYear_Male)									
}

{# Female
modProRateRpt_MCMCglmm_Female <- MCMCglmm(Visit1RateH ~
										HatchingDayAfter0401 + 
										DVDInfoChickNb + 
										ChickAgeCat + 
										RelTimeHrs ,
									random= ~
										BroodRef + 
										BirdID+ 
										SocialPartnerID +
										BreedingYear +
										PairID
										, data = BirdProRate[BirdProRate$Sex == 0,]
										, prior = prior_modProRateRpt_MCMCglmm)
									
summary(modProRateRpt_MCMCglmm_Female)
posterior.mode(modProRateRpt_MCMCglmm_Female$VCV)		
VP_ProRate_Female <-  modProRateRpt_MCMCglmm_Female$VCV[,"BroodRef"]+ modProRateRpt_MCMCglmm_Female$VCV[,"BirdID"]+ modProRateRpt_MCMCglmm_Female$VCV[,"SocialPartnerID"]+ modProRateRpt_MCMCglmm_Female$VCV[,"PairID"]+ modProRateRpt_MCMCglmm_Female$VCV[,"BreedingYear"]+modProRateRpt_MCMCglmm_Female$VCV[,"units"]

R_Alternation_BroodRef_Female <- modProRateRpt_MCMCglmm_Female$VCV[,"BroodRef"]/VP_ProRate_Female
posterior.mode(R_Alternation_BroodRef_Female)
HPDinterval(R_Alternation_BroodRef_Female)

R_Alternation_BirdID_Female <- modProRateRpt_MCMCglmm_Female$VCV[,"BirdID"]/VP_ProRate_Female
posterior.mode(R_Alternation_BirdID_Female)
HPDinterval(R_Alternation_BirdID_Female)

R_Alternation_SocialPartnerID_Female <- modProRateRpt_MCMCglmm_Female$VCV[,"SocialPartnerID"]/VP_ProRate_Female
posterior.mode(R_Alternation_SocialPartnerID_Female)
HPDinterval(R_Alternation_SocialPartnerID_Female)

R_Alternation_PairID_Female <- modProRateRpt_MCMCglmm_Female$VCV[,"PairID"]/VP_ProRate_Female
posterior.mode(R_Alternation_PairID_Female)
HPDinterval(R_Alternation_PairID_Female)

R_Alternation_BreedingYear_Female <- modProRateRpt_MCMCglmm_Female$VCV[,"BreedingYear"]/VP_ProRate_Female
posterior.mode(R_Alternation_BreedingYear_Female)
HPDinterval(R_Alternation_BreedingYear_Female)
}

}

}


{# cost of provisioning rate in terms of survival ?
head(MY_TABLE_perBirdYear)

modPorRateSurvival2004 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2004,], family = "binomial" )
summary(modPorRateSurvival2004)

modPorRateSurvival2005 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2005,], family = "binomial" )
summary(modPorRateSurvival2005)

modPorRateSurvival2006 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2006,], family = "binomial" )
summary(modPorRateSurvival2006)

modPorRateSurvival2007 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2007,], family = "binomial" )
summary(modPorRateSurvival2007)

modPorRateSurvival2009 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2009,], family = "binomial" )
summary(modPorRateSurvival2009)

modPorRateSurvival2010 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2010,], family = "binomial" )
summary(modPorRateSurvival2010) # positive trend

modPorRateSurvival2011 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2011,], family = "binomial" )
summary(modPorRateSurvival2011)

modPorRateSurvival2012 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2012,], family = "binomial" )
summary(modPorRateSurvival2012)

modPorRateSurvival2013 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2013,], family = "binomial" )
summary(modPorRateSurvival2013)

modPorRateSurvival2014 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2014,], family = "binomial" )
summary(modPorRateSurvival2014)

# modPorRateSurvival2015 <- glm(AliveNextYear ~ MeanVisit1RateHYear + Sex + Age
									# , data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$BreedingYear == 2015,], family = "binomial" )
# summary(modPorRateSurvival2015)

}

{# Fitness benefits of provisioning rate ?
MY_TABLE_perBrood


# mod_Prorate_FitnessAsNbRinged <- glmer(NbRinged ~ scale(TotalProRate, scale=FALSE) + 
										# #(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										# (1|BreedingYear) , data = MY_TABLE_perBrood, family = "poisson")

mod_Prorate_FitnessAsNbRinged <- lmer(NbRinged ~ scale(TotalProRate, scale=FALSE) + 
										#(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(mod_Prorate_FitnessAsNbRinged)

{# model assumptions checking > weird residuals ?

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_Prorate_FitnessAsNbRinged), resid(mod_Prorate_FitnessAsNbRinged))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_Prorate_FitnessAsNbRinged))
qqline(resid(mod_Prorate_FitnessAsNbRinged))
qqnorm(unlist(ranef(mod_Prorate_FitnessAsNbRinged))) 
qqline(unlist(ranef(mod_Prorate_FitnessAsNbRinged)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(mod_Prorate_FitnessAsNbRinged)$SocialMumID))
# mean(unlist(ranef(mod_Prorate_FitnessAsNbRinged)$SocialDadID))
# mean(unlist(ranef(mod_Prorate_FitnessAsNbRinged)$PairID))
mean(unlist(ranef(mod_Prorate_FitnessAsNbRinged)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood
scatter.smooth(d$TotalProRate, resid(mod_Prorate_FitnessAsNbRinged)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(mod_Prorate_FitnessAsNbRinged)
scatter.smooth(d$fitted, jitter(d$NbRinged, 0.05),ylim=c(0, 10))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$TotalProRate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="TotalProRate")

}


}




#############
# SYNCHRONY #
#############

head(MY_TABLE_perDVD)
head(MY_TABLE_perBrood)

{# compare to simulation correlation A-S

ggplot(data=MY_TABLE_perDVD, aes(y=NbSynchro_ChickFeedingEquanim,x=NbAlternation) ) + 
							geom_point() + 
							geom_smooth(method = "lm") +
							geom_abline(intercept=0,slope=0.5)+
							geom_abline(intercept=0,slope=1)
							
							
ggplot(data=MY_TABLE_perDVD, aes(y=NbSynchro_LessConspicuous,x=NbAlternation) ) + 
							geom_point() + 
							geom_smooth(method = "lm") +
							geom_abline(intercept=0,slope=0.5)+
							geom_abline(intercept=0,slope=1)	

ggplot(data=MY_TABLE_perDVD, aes(y=SynchronyFeedValue,x=AlternationValue) ) + 
							geom_point() + 
							geom_smooth(method = "lm") +
							geom_abline(intercept=0,slope=0.5)+
							geom_abline(intercept=0,slope=1)
							
							
ggplot(data=MY_TABLE_perDVD, aes(y=SynchronyMvtValue,x=AlternationValue) ) + 
							geom_point() + 
							geom_smooth(method = "lm") +
							geom_abline(intercept=0,slope=0.5)+
							geom_abline(intercept=0,slope=1)							
							

hist(MY_TABLE_perDVD$AlternationValue)
}

{#### predictors of synchrony

{# check dependent and explanatory variables 
hist(MY_TABLE_perDVD$SynchronyFeedValue, breaks =length(unique(MY_TABLE_perDVD$SynchronyFeedValue)))
table(MY_TABLE_perDVD$SynchronyFeedValue)

scatter.smooth(MY_TABLE_perDVD$MFVisit1,MY_TABLE_perDVD$DiffVisit1Rate )
cor.test(MY_TABLE_perDVD$MFVisit1,MY_TABLE_perDVD$DiffVisit1Rate)

scatter.smooth(MY_TABLE_perDVD$SynchronyFeedValue~MY_TABLE_perDVD$MFVisit1 )

# pairs that have twice zero for synchrony
MeanSynchronyFeedValue_perPair <- as.data.frame(MY_TABLE_perDVD %>% group_by(PairID) %>% summarise(mean(SynchronyFeedValue)))
colnames(MeanSynchronyFeedValue_perPair) <- c("PairID", "MeanSynchronyFeedValue")
MeanSynchronyFeedValue_perPair[MeanSynchronyFeedValue_perPair$MeanSynchronyFeedValue == 0,]

}

{# synchrony score > assumptions model weird

modS <- lmer(SynchronyFeedValue~  
	# scale(MFVisit1, scale=FALSE) + # this is strongly correlated to DiffVisit1Rate and with chickNb and this is mathematically linked to Sync score
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	#scale(HatchingDayAfter0401, scale=FALSE) + 
	#scale(PairBroodNb, scale=FALSE) + 
	scale(DVDInfoChickNb, scale=FALSE) + 
	ChickAgeCat + 
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + 
	#(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) 
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),])

summary(modS) # Nr of obs: 1593, groups:  BroodRef, 869; PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12

{# model assumptions checking > not quite !

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modS), resid(modS))	#
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modS))
qqline(resid(modS))
qqnorm(unlist(ranef(modS))) 
qqline(unlist(ranef(modS)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modS))),fitted(modS)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(modS)$BroodRef))
mean(unlist(ranef(modS)$SocialMumID))
mean(unlist(ranef(modS)$SocialDadID))
mean(unlist(ranef(modS)$PairID))
mean(unlist(ranef(modS)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),]

scatter.smooth(d$ParentsAge, resid(modS))
abline(h=0, lty=2)
scatter.smooth(d$HatchingDayAfter0401, resid(modS))
abline(h=0, lty=2)
plot(d$DVDInfoChickNb, resid(modS))
abline(h=0, lty=2)	
plot(d$ChickAgeCat, resid(modS))
abline(h=0, lty=2)	
scatter.smooth(d$DiffVisit1Rate, resid(modS))
abline(h=0, lty=2)	
scatter.smooth(d$RelTimeHrs, resid(modS))
abline(h=0, lty=2)		

# dependent variable vs fitted
d$fitted <- fitted(modS)
scatter.smooth(d$fitted, jitter(d$SynchronyFeedValue, 0.05),ylim=c(0, 40))

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="SynchronyFeedValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="SynchronyFeedValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="SynchronyFeedValue", xlab="ChickAgeCat")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="SynchronyFeedValue", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="SynchronyFeedValue", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="SynchronyFeedValue", xlab="RelTimeHrs")

}

}

{# synchrony Feed Nb > assumption model awful

modS_nb <- lmer(NbSynchro_ChickFeedingEquanim~  
	scale(MFVisit1, scale=FALSE) + # this is strongly correlated to DiffVisit1Rate and with chickNb
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	#scale(HatchingDayAfter0401, scale=FALSE) + 
	#scale(PairBroodNb, scale=FALSE) + 
	scale(DVDInfoChickNb, scale=FALSE) + 
	ChickAgeCat + 
	DiffVisit1Rate +  
	# scale(RelTimeHrs, scale=FALSE) + 
	#(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) 
	 +(1|PairID) 
	+ (1|BreedingYear) 
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),])

summary(modS_nb) # Nr of obs: 1593, groups:  BroodRef, 869; PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12

{# model assumptions checking > awful !!!

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modS_nb), resid(modS_nb))	# curved !
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modS_nb))
qqline(resid(modS_nb))
qqnorm(unlist(ranef(modS_nb))) 
qqline(unlist(ranef(modS_nb)))

# homogeneity of variance	# awful !!
scatter.smooth(sqrt(abs(resid(modS_nb))),fitted(modS_nb)) 

# Mean of ranefs: should be zero
#mean(unlist(ranef(modS_nb)$BroodRef))
mean(unlist(ranef(modS_nb)$SocialMumID))
mean(unlist(ranef(modS_nb)$SocialDadID))
mean(unlist(ranef(modS_nb)$PairID)) # 0 !
mean(unlist(ranef(modS_nb)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),]

scatter.smooth(d$ParentsAge, resid(modS_nb))
abline(h=0, lty=2)
scatter.smooth(d$HatchingDayAfter0401, resid(modS_nb))
abline(h=0, lty=2)
plot(d$DVDInfoChickNb, resid(modS_nb))
abline(h=0, lty=2)	
plot(d$ChickAgeCat, resid(modS_nb))
abline(h=0, lty=2)	
scatter.smooth(d$DiffVisit1Rate, resid(modS_nb))
abline(h=0, lty=2)	
scatter.smooth(d$RelTimeHrs, resid(modS_nb))
abline(h=0, lty=2)		

# dependent variable vs fitted
d$fitted <- fitted(modS_nb)
scatter.smooth(d$fitted, jitter(d$NbSynchro_ChickFeedingEquanim, 0.05),ylim=c(0, 40))

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbSynchro_ChickFeedingEquanim", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbSynchro_ChickFeedingEquanim", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbSynchro_ChickFeedingEquanim", xlab="ChickAgeCat")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbSynchro_ChickFeedingEquanim", xlab="DVDInfoChickNb")
scatter.smooth(d$DiffVisit1Rate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbSynchro_ChickFeedingEquanim", xlab="DiffVisit1Rate") # strongly correlated
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbSynchro_ChickFeedingEquanim", xlab="RelTimeHrs")

}

}

{# with glmmADMB : hurdle model with random effect > predicted variables needs to be a count > use synchrony Feed Nb
# first analyse the factors that produce zeros (vs.non-zeros) by a logistic regression
# then use a truncated Poisson-distribution (at y=1) for the non-zero counts
# http://glmmadmb.r-forge.r-project.org/glmmADMB.pdf


MY_TABLE_perDVD$MFVisit1 <- MY_TABLE_perDVD$FVisit1+ MY_TABLE_perDVD$MVisit1
MY_TABLE_perDVD$BroodRef <- as.factor(MY_TABLE_perDVD$BroodRef)
MY_TABLE_perDVD$SocialDadID <- as.factor(MY_TABLE_perDVD$SocialDadID)
MY_TABLE_perDVD$SocialMumID <- as.factor(MY_TABLE_perDVD$SocialMumID)
MY_TABLE_perDVD$PairID <- as.factor(MY_TABLE_perDVD$PairID)
MY_TABLE_perDVD$BreedingYear <- as.factor(MY_TABLE_perDVD$BreedingYear)


modS_nb_glmmadmb <- glmmadmb(NbSynchro_ChickFeedingEquanim~scale(MFVisit1, scale=FALSE) +# this is strongly correlated to DiffVisit1Rate and with chickNb
														scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
														# scale(HatchingDayAfter0401, scale=FALSE) + 
														# scale(PairBroodNb, scale=FALSE) + 
														scale(DVDInfoChickNb, scale=FALSE) + 
														ChickAgeCat + 
														DiffVisit1Rate +  
														# scale(RelTimeHrs, scale=FALSE) + 
														#(1|BroodRef) + 
														(1|SocialMumID)+ (1|SocialDadID) 
														 + (1|PairID) 
														+ (1|BreedingYear) 
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),], zeroInflation=TRUE, family="poisson")

summary(modS_nb_glmmadmb) # might have an issue because doesnt give the variance of the random effects in the output thought ranefs available

plot(unlist(ranef(modS_nb_glmmadmb)$SocialMumID),unlist(ranef(modS_nb)$SocialMumID))
plot(unlist(ranef(modS_nb_glmmadmb)$SocialDadID),unlist(ranef(modS_nb)$SocialDadID))
plot(unlist(ranef(modS_nb_glmmadmb)$PairID),unlist(ranef(modS_nb)$PairID))
plot(unlist(ranef(modS_nb_glmmadmb)$BreedingYear),unlist(ranef(modS_nb)$BreedingYear))

ranefs_modS_nb_glmmadmb_PairID <- as.data.frame(cbind(rownames(ranef(modS_nb_glmmadmb)$PairID), unlist(ranef(modS_nb_glmmadmb)$PairID)))
colnames(ranefs_modS_nb_glmmadmb_PairID) <- c("PairID", "ranefs")
rownames(ranefs_modS_nb_glmmadmb_PairID) <- NULL
ranefs_modS_nb_glmmadmb_PairID$ranefs <- as.numeric(ranefs_modS_nb_glmmadmb_PairID$ranefs)
ranefs_modS_nb_glmmadmb_PairID$PairswithMeanS0 <- ifelse(ranefs_modS_nb_glmmadmb_PairID$PairID %in%  MeanSynchronyFeedValue_perPair$PairID[MeanSynchronyFeedValue_perPair$MeanSynchronyFeedValue == 0], 0, 1)
ggplot(ranefs_modS_nb_glmmadmb_PairID, aes(PairID,ranefs, colour = as.factor(PairswithMeanS0))) + geom_point()


}

{## Gamma hurdle model with continuous data : NOT WORKING
# http://seananderson.ca/2014/05/18/gamma-hurdle.html

MY_TABLE_perDVD$SynchroFeed_non_zero <- ifelse(MY_TABLE_perDVD$SynchronyFeedValue > 0, 1, 0)
ggplot(MY_TABLE_perDVD, aes(DVDRef, SynchronyFeedValue, colour = as.factor(SynchroFeed_non_zero))) + geom_point()

modS1_Logistic <- glmer(SynchroFeed_non_zero ~ MFVisit1 +# this is strongly correlated to DiffVisit1Rate
												ParentsAge + # this is strongly correlated to PairBroodNb
												#HatchingDayAfter0401 + 
												#PairBroodNb + 
												DVDInfoChickNb + 
												ChickAgeCat + 
												DiffVisit1Rate +  
												#RelTimeHrs + 
												#(1|BroodRef) + 
												#(1|SocialMumID)+ 
												(1|SocialDadID) + 
												(1|PairID)
												# +(1|BreedingYear) 
												, data = MY_TABLE_perDVD, family = binomial(link = logit))

summary(modS1_Logistic)

modS2_Gamma <- glm(SynchronyFeedValue ~ # MFVisit1 +# this is strongly correlated to DiffVisit1Rate and this is mathematically linked to Sync score
										ParentsAge + # this is strongly correlated to PairBroodNb
										#HatchingDayAfter0401 + 
										#PairBroodNb + 
										DVDInfoChickNb + 
										ChickAgeCat + 
										DiffVisit1Rate 
										#scale(RelTimeHrs, scale=FALSE) + 
										#(1|BroodRef) + 
										#(1|SocialMumID)+ (1|SocialDadID) 
										# +(1|PairID) 
										#+ (1|BreedingYear) 
										, data = subset(MY_TABLE_perDVD, MY_TABLE_perDVD$SynchroFeed_non_zero == 1), family = Gamma(link = log))

summary(modS2_Gamma)	# can't make the glmer to converge
}
	
}

summary(modS_nb_glmmadmb)

{#### fitness benefits of synchrony

{## provisioning rate > do not make sense ?
# mathematical negative correlation between number of synchronous provisioning/ total nb of provisioning and total nb of provisioning / time
# conceptual positive correlation between number of synchronous provisioning and pro rate, as synchrony becomes more likely if interfeed interval are shorter.

{# MeanSynchroFeed
mod_Sync_FitnessAsProRate <- lmer(TotalProRate^0.45 ~  NbRinged + # strongly correlated with Synchrony
														HatchingDayAfter0401 + 
														scale(MeanSynchroFeed, scale=FALSE)
														+(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
														 , data = MY_TABLE_perBrood)

summary(mod_Sync_FitnessAsProRate) # Number of obs: 872, groups:  PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12

{# model assumptions checking > good

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_Sync_FitnessAsProRate), resid(mod_Sync_FitnessAsProRate))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_Sync_FitnessAsProRate))
qqline(resid(mod_Sync_FitnessAsProRate))
qqnorm(unlist(ranef(mod_Sync_FitnessAsProRate)))
qqline(unlist(ranef(mod_Sync_FitnessAsProRate)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(mod_Sync_FitnessAsProRate))),fitted(mod_Sync_FitnessAsProRate)) # quite not ! > much nicer if exp 0.45
	# tried when removing the 5% quantile extreme of provisioning rate, model estimates quite similar, random effect all much much lower

# Mean of ranefs: should be zero
mean(unlist(ranef(mod_Sync_FitnessAsProRate)$SocialMumID))
mean(unlist(ranef(mod_Sync_FitnessAsProRate)$SocialDadID))
mean(unlist(ranef(mod_Sync_FitnessAsProRate)$PairID))
mean(unlist(ranef(mod_Sync_FitnessAsProRate)$BreedingYear))

# residuals vs predictors
plot(MY_TABLE_perBrood$NbRinged, resid(mod_Sync_FitnessAsProRate))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perBrood$MeanSynchroFeed, resid(mod_Sync_FitnessAsProRate))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perBrood
d$fitted <- fitted(mod_Sync_FitnessAsProRate)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="MeanSynchroFeed")

}

}

{# MeanSynchroFeed_nb
mod_Sync_nb_FitnessAsProRate <- lmer(TotalProRate^0.45 ~  NbRinged + # strongly correlated with Synchrony
														HatchingDayAfter0401 + 
														scale(MeanSynchroFeed_nb, scale=FALSE)
														+(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
														 , data = MY_TABLE_perBrood)

summary(mod_Sync_nb_FitnessAsProRate) # Number of obs: 872, groups:  PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12

{# model assumptions checking > not quite good

# residuals vs fitted: mean should constantly be zero	> not quite !
scatter.smooth(fitted(mod_Sync_nb_FitnessAsProRate), resid(mod_Sync_nb_FitnessAsProRate))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_Sync_nb_FitnessAsProRate))
qqline(resid(mod_Sync_nb_FitnessAsProRate))
qqnorm(unlist(ranef(mod_Sync_nb_FitnessAsProRate)))
qqline(unlist(ranef(mod_Sync_nb_FitnessAsProRate)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(mod_Sync_nb_FitnessAsProRate))),fitted(mod_Sync_nb_FitnessAsProRate)) # quite not ! > much nicer if exp 0.45
	# tried when removing the 5% quantile extreme of provisioning rate, model estimates quite similar, random effect all much much lower

# Mean of ranefs: should be zero
mean(unlist(ranef(mod_Sync_nb_FitnessAsProRate)$SocialMumID))
mean(unlist(ranef(mod_Sync_nb_FitnessAsProRate)$SocialDadID))
mean(unlist(ranef(mod_Sync_nb_FitnessAsProRate)$PairID))
mean(unlist(ranef(mod_Sync_nb_FitnessAsProRate)$BreedingYear))

# residuals vs predictors
plot(MY_TABLE_perBrood$NbRinged, resid(mod_Sync_nb_FitnessAsProRate))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perBrood$MeanSynchroFeed_nb, resid(mod_Sync_nb_FitnessAsProRate))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perBrood
d$fitted <- fitted(mod_Sync_nb_FitnessAsProRate)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$MeanSynchroFeed_nb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="MeanSynchroFeed_nb")

}

}

}

{# Nb ringed

{# MeanSynchroFeed (highly correlated to TotalProRate)
cor.test(MY_TABLE_perBrood$MeanSynchroFeed,MY_TABLE_perBrood$TotalProRate) # 0.66 !


mod_Sync_FitnessAsNbRinged <- glmer(NbRinged ~ scale(MeanSynchroFeed, scale=FALSE) + 
												scale(TotalProRate, scale=FALSE) +
												#(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
												(1|BreedingYear) , data = MY_TABLE_perBrood, family = "poisson")
										
summary(mod_Sync_FitnessAsNbRinged) # Number of obs: 872, groups:  PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12


{# model assumptions checking > very weird residuals !

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_Sync_FitnessAsNbRinged), resid(mod_Sync_FitnessAsNbRinged))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_Sync_FitnessAsNbRinged))
qqline(resid(mod_Sync_FitnessAsNbRinged))
qqnorm(unlist(ranef(mod_Sync_FitnessAsNbRinged))) 
qqline(unlist(ranef(mod_Sync_FitnessAsNbRinged)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(mod_Sync_FitnessAsNbRinged)$SocialMumID))
# mean(unlist(ranef(mod_Sync_FitnessAsNbRinged)$SocialDadID))
# mean(unlist(ranef(mod_Sync_FitnessAsNbRinged)$PairID))
mean(unlist(ranef(mod_Sync_FitnessAsNbRinged)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood
scatter.smooth(d$TotalProRate, resid(mod_Sync_FitnessAsNbRinged)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)
scatter.smooth(d$MeanSynchroFeed, resid(mod_Sync_FitnessAsNbRinged)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(mod_Sync_FitnessAsNbRinged)
scatter.smooth(d$fitted, jitter(d$NbRinged, 0.05),ylim=c(0, 10))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$TotalProRate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="TotalProRate")
scatter.smooth(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="MeanSynchroFeed")

}

}

{# MeanSynchroFeed_nb
mod_Sync_nb_FitnessAsNbRinged <- lmer(NbRinged ~ scale(MeanSynchroFeed_nb, scale=FALSE) + scale(TotalProRate, scale=FALSE) +
										 (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(mod_Sync_nb_FitnessAsNbRinged) # Number of obs: 872, groups:  PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12

{# model assumptions checking >  weird residuals but better than above ?

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_Sync_nb_FitnessAsNbRinged), resid(mod_Sync_nb_FitnessAsNbRinged))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_Sync_nb_FitnessAsNbRinged))
qqline(resid(mod_Sync_nb_FitnessAsNbRinged))
qqnorm(unlist(ranef(mod_Sync_nb_FitnessAsNbRinged))) 
qqline(unlist(ranef(mod_Sync_nb_FitnessAsNbRinged)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(mod_Sync_nb_FitnessAsNbRinged)$SocialMumID))
# mean(unlist(ranef(mod_Sync_nb_FitnessAsNbRinged)$SocialDadID))
# mean(unlist(ranef(mod_Sync_nb_FitnessAsNbRinged)$PairID))
mean(unlist(ranef(mod_Sync_nb_FitnessAsNbRinged)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood
scatter.smooth(d$TotalProRate, resid(mod_Sync_nb_FitnessAsNbRinged)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)
scatter.smooth(d$MeanSynchroFeed_nb, resid(mod_Sync_nb_FitnessAsNbRinged)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(mod_Sync_nb_FitnessAsNbRinged)
scatter.smooth(d$fitted, jitter(d$NbRinged, 0.05),ylim=c(0, 10))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$TotalProRate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="TotalProRate")
scatter.smooth(d$MeanSynchroFeed_nb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="MeanSynchroFeed_nb")
}

}

}

}

summary(mod_Sync_FitnessAsNbRinged)

{#### consequence of synchrony in term of divorce

{# check dependent and explanatory variables 
head(MY_TABLE_perBrood)
nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MDivorce) & !is.na(MY_TABLE_perBrood$MPrevNbRinged),])
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$MDivorce) & !is.na(MY_TABLE_perBrood$MPrevNbRinged),] # when Social female was NA
MY_TABLE_perBrood[MY_TABLE_perBrood$SocialDadID == 4060,]
MY_tblBroods[!is.na(MY_tblBroods$SocialDadID) & MY_tblBroods$SocialDadID == 4060,]

}


mod_MaleDivorce <- glmer(MDivorce~MeanSynchroFeed + 
									MeanA	+
									#scale(DadAge, scale=FALSE) + 
									scale(PairBroodNb, scale=FALSE) +
									# MeanDiffVisit1Rate +  
									 MPriorResidence + 
									 MPrevNbRinged +
									(1|SocialDadID) + (1|BreedingYear) 
									, data = MY_TABLE_perBrood, family="binomial")

summary(mod_MaleDivorce) # Number of obs: 688, groups:  SocialDadID, 231; BreedingYear, 12


{# model assumptions checking >> residuals not normal !!!!!!

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_MaleDivorce), resid(mod_MaleDivorce))	# awful !
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_MaleDivorce))# not quite normal !
qqline(resid(mod_MaleDivorce))

{# get our qqplot within others:
N <- length(resid(mod_MaleDivorce))
sigma <- summary(mod_MaleDivorce)$sigma # Extract the estimated standard deviation of the errors
par(mfrow=c(3,3))  
rnum<-sample(1:9, 1)
for(i in 1:(rnum-1)){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
  }
qqnorm(resid(mod_MaleDivorce), main=rnum)
qqline(resid(mod_MaleDivorce))
for(i in (rnum+1):9){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
  }
  }
# can we see our plot ? solution is:
rnum

qqnorm(unlist(ranef(mod_MaleDivorce))) 
qqline(unlist(ranef(mod_MaleDivorce)))


# check for overdispersion
mod_MaleDivorce_withOverdispersionAccounted <- glmer(MDivorce~MeanSynchroFeed + 
									# MeanA	+
									#scale(DadAge, scale=FALSE) + 
									scale(PairBroodNb, scale=FALSE) +
									# MeanDiffVisit1Rate +  
									MPriorResidence + MPrevNbRinged +
									 (1|SocialDadID) + (1|BreedingYear) +(1|BroodRef)
									, data = MY_TABLE_perBrood, family="binomial")
summary(mod_MaleDivorce_withOverdispersionAccounted)
anova(mod_MaleDivorce, mod_MaleDivorce_withOverdispersionAccounted)


# Mean of ranefs: should be zero
mean(unlist(ranef(mod_MaleDivorce)$SocialDadID))
mean(unlist(ranef(mod_MaleDivorce)$BreedingYear))

# residuals vs predictors

d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MDivorce) & !is.na(MY_TABLE_perBrood$MPrevNbRinged),]
plot(d$MeanSynchroFeed, resid(mod_MaleDivorce))
abline(h=0, lty=2)
plot(d$PairBroodNb, resid(mod_MaleDivorce))
abline(h=0, lty=2)
plot(d$MPriorResidence, resid(mod_MaleDivorce))
abline(h=0, lty=2)
plot(d$MPrevNbRinged, resid(mod_MaleDivorce))
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(mod_MaleDivorce)
plot(d$fitted, d$MDivorce,ylim=c(0, 1))
abline(0,1)	

# fitted vs all predictors
plot(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="MeanSynchroFeed")
plot(d$PairBroodNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="PairBroodNb")
plot(d$MPriorResidence,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="MPriorResidence")
plot(d$MPrevNbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="MPrevNbRinged")
}



mod_FemaleDivorce <- glmer(FDivorce~MeanSynchroFeed + 
									 MeanA	+
									#scale(MumAge, scale=FALSE) + 
									scale(PairBroodNb, scale=FALSE) +
									# MeanDiffVisit1Rate +  
									# FPriorResidence + 
									FPrevNbRinged +
									 (1|SocialMumID) + (1|BreedingYear) 
									, data = MY_TABLE_perBrood, family="binomial")

summary(mod_FemaleDivorce)

{# model assumptions checking >> residuals not normal !!!!!!

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_FemaleDivorce), resid(mod_FemaleDivorce))	# awful !
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_FemaleDivorce))# not quite normal !
qqline(resid(mod_FemaleDivorce))

{# get our qqplot within others:
N <- length(resid(mod_FemaleDivorce))
sigma <- summary(mod_FemaleDivorce)$sigma # Extract the estimated standard deviation of the errors
par(mfrow=c(3,3))  
rnum<-sample(1:9, 1)
for(i in 1:(rnum-1)){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
  }
qqnorm(resid(mod_FemaleDivorce), main=rnum)
qqline(resid(mod_FemaleDivorce))
for(i in (rnum+1):9){
  x<-rnorm(N, 0, sigma)
  qqnorm(x, main=i)
  qqline(x)
  }
  }
# can we see our plot ? solution is:
rnum

qqnorm(unlist(ranef(mod_FemaleDivorce))) 
qqline(unlist(ranef(mod_FemaleDivorce)))


# check for overdispersion
mod_FemaleDivorce_withOverdispersionAccounted <- glmer(FDivorce~MeanSynchroFeed + 
									# MeanA	+
									#scale(DadAge, scale=FALSE) + 
									scale(PairBroodNb, scale=FALSE) +
									# MeanDiffVisit1Rate +  
									FPriorResidence + FPrevNbRinged +
									 (1|SocialMumID) + (1|BreedingYear) +(1|BroodRef)
									, data = MY_TABLE_perBrood, family="binomial")
summary(mod_FemaleDivorce_withOverdispersionAccounted)
anova(mod_FemaleDivorce, mod_FemaleDivorce_withOverdispersionAccounted) 


# Mean of ranefs: should be zero
mean(unlist(ranef(mod_FemaleDivorce)$SocialMumID))
mean(unlist(ranef(mod_FemaleDivorce)$BreedingYear))

# residuals vs predictors

d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$FDivorce) & !is.na(MY_TABLE_perBrood$FPrevNbRinged),]
plot(d$MeanSynchroFeed, resid(mod_FemaleDivorce))
abline(h=0, lty=2)
plot(d$PairBroodNb, resid(mod_FemaleDivorce))
abline(h=0, lty=2)
plot(d$FPriorResidence, resid(mod_FemaleDivorce))
abline(h=0, lty=2)
plot(d$FPrevNbRinged, resid(mod_FemaleDivorce))
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(mod_FemaleDivorce)
plot(d$fitted, d$FDivorce,ylim=c(0, 1))
abline(0,1)	

# fitted vs all predictors
plot(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="MeanSynchroFeed")
plot(d$PairBroodNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="PairBroodNb")
plot(d$FPriorResidence,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="FPriorResidence")
plot(d$FPrevNbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="FPrevNbRinged")
}

}	

summary(mod_MaleDivorce)	
summary(mod_FemaleDivorce)		

{#### proportion of synchronous visits where female enters first > repeatability within pair could induce alternation

mod_proportionSexStartSynchro <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ MFmeanDuration+MFVisit1RateH + 
													(1|BroodRef) +
													(1|PairID)
													 #+(1|DVDRef) 
													, data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")

summary(mod_proportionSexStartSynchro)

{# model assumptions checking

# check for overdispersion
mod_proportionSexStartSynchro_overdisp <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ MFmeanDuration+MFVisit1RateH + 
												(1|BroodRef) +
												(1|PairID)
												 +(1|DVDRef) 
												, data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >0,], family ="binomial")
summary(mod_proportionSexStartSynchro_overdisp)
anova(mod_proportionSexStartSynchro_overdisp,mod_proportionSexStartSynchro)

# qqplots residuals and ranef
qqnorm(resid(mod_proportionSexStartSynchro))
qqline(resid(mod_proportionSexStartSynchro))
qqnorm(unlist(ranef(mod_proportionSexStartSynchro)))	
qqline(unlist(ranef(mod_proportionSexStartSynchro)))

# residuals vs fitted					?
scatter.smooth(fitted(mod_proportionSexStartSynchro), resid(mod_proportionSexStartSynchro))
abline(h=0, lty=2)

# residuals vs predictors		
scatter.smooth(MY_TABLE_perDVD$MFmeanDuration[MY_TABLE_perDVD$SynchronyFeedValue >0], resid(mod_proportionSexStartSynchro))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD$MFVisit1RateH[MY_TABLE_perDVD$SynchronyFeedValue >0], resid(mod_proportionSexStartSynchro))
abline(h=0, lty=2)

# data vs. fitted ?							
d <- MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >0,]
d$fitted <- fitted(mod_proportionSexStartSynchro)
scatter.smooth(d$fitted, jitter(d$NbSynchroFemaleStart/(d$NbSynchroFemaleStart+d$NbSynchroMaleStart), 0.05),ylim=c(0, 1))
abline(0,1)	

# data and fitted against all predictors
scatter.smooth(d$MFmeanDuration,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="proportion of synchronous visits where female enters first", xlab="MFmeanDuration")	
scatter.smooth(d$MFVisit1RateH,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="proportion of synchronous visits where female enters first", xlab="MFVisit1RateH")	

}

}

summary(mod_proportionSexStartSynchro)

{#### variance in chick mass ~ synchrony

head(MY_TABLE_perChick)
head(MY_TABLE_perBrood)

ResMassTarsus_perChick_perBrood <- as.data.frame(MY_TABLE_perChick %>% group_by(RearingBrood) %>% summarise(sd(ResMassTarsus_perChick)))
colnames(ResMassTarsus_perChick_perBrood) <- c('RearingBrood','sdResMassTarsus')
head(ResMassTarsus_perChick_perBrood)
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood,y=ResMassTarsus_perChick_perBrood, by.x='BroodRef', by.y='RearingBrood', all.x=TRUE)


hist(MY_TABLE_perBrood$sdMass)
hist(MY_TABLE_perBrood$sdResMassTarsus)

mod_Sync_sdResMassTarsus <- lmer(sdResMassTarsus ~ MixedBroodYN +
											NbRinged + 
											MeanSynchroFeed + 
											#(1|SocialMumID)+ (1|SocialDadID) + 
											(1|PairID) + (1|BreedingYear) ,data=MY_TABLE_perBrood)
summary(mod_Sync_sdResMassTarsus) # Number of obs: 680, groups:  PairID, 378; SocialMumID, 263; SocialDadID, 253; BreedingYear, 12

{# model assumptions checking  > not quite but alright ??

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_Sync_sdResMassTarsus), resid(mod_Sync_sdResMassTarsus))	#
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_Sync_sdResMassTarsus))
qqline(resid(mod_Sync_sdResMassTarsus))
qqnorm(unlist(ranef(mod_Sync_sdResMassTarsus)$BreedingYear)) 
qqline(unlist(ranef(mod_Sync_sdResMassTarsus)$BreedingYear))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(mod_Sync_sdResMassTarsus))),fitted(mod_Sync_sdResMassTarsus)) 

# Mean of ranefs: should be zero
mean(unlist(ranef(mod_Sync_sdResMassTarsus)$SocialMumID))
mean(unlist(ranef(mod_Sync_sdResMassTarsus)$SocialDadID))
mean(unlist(ranef(mod_Sync_sdResMassTarsus)$PairID))
mean(unlist(ranef(mod_Sync_sdResMassTarsus)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$sdResMassTarsus),]

boxplot(d$MixedBroodYN, resid(mod_Sync_sdResMassTarsus))
abline(h=0, lty=2)
scatter.smooth(d$NbRinged, resid(mod_Sync_sdResMassTarsus))
abline(h=0, lty=2)
scatter.smooth(d$MeanSynchroFeed, resid(mod_Sync_sdResMassTarsus))
abline(h=0, lty=2)	
	

# dependent variable vs fitted
d$fitted <- fitted(mod_Sync_sdResMassTarsus)
scatter.smooth(d$fitted, jitter(d$sdResMassTarsus, 0.05),ylim=c(0, 5))

# fitted vs all predictors
boxplot(d$MixedBroodYN,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="sdResMassTarsus", xlab="MixedBroodYN")
scatter.smooth(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="sdResMassTarsus", xlab="NbRinged")
scatter.smooth(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="sdResMassTarsus", xlab="MeanSynchroFeed")


}


}

summary(mod_Sync_sdResMassTarsus)


###############
# FAMILIARITY #
###############

{## same partner > higher fitness ?
# need to control for same residency

## same partner > higher synchrony ?
# well... PairID did not explain vairaince in synchrony...

## same partner > alternation ?
# well... PairID did not explain vairaince in synchrony...
}








{#############################  TO DO + ISSUES
  
## repeatability of provisioning rate:
# bootstrap instead of LRT - DONE >> correct ?
# get rpt package to work (under construction)
# do analyses on provisioning rate per chick like shinichi ?
# boxcox transfo to approach normality ?
# do analysis per sex separately or add sex to the model with both sexes piled up ?


## repeatability alternation 
# considering more than two measures and use rptR package to fit mixed effect model (not working)
# or randomise order of measurements with 2 measures
# or analyse the random effects in model alternation <<<<<<

## take a decision for time of the day
# check if the effect is linear to keep a continuous variable - DONE

## predictor of alternation
# solve the issue of correlation between parent age and PairBroodNb 
# solve the issue of correlation between ChickNb and ChickAge - DONE

## fitness model
# are models on average values good ? 
# should it be weigthed ?
# or should the error been kept forward by just adding another random effect ?

## total provisioning rate
# transform dependent variable to have normal residual ?
# box cox transformation like Shinichi ? but how with mixed effect models ??

## chick mass model
# add genetic parents ? - DONE
# test both body condition (dev mass on tarsus or have tarsus in model) and body size per se ??
# shouldn't we check whether provisioning rate increase body condition ?
# what if regularity (low variance in interfeed interval) increase body condition ?


## survival model
# do model for sexes separately ?
# include sex in a model with data of both sexes piled up ?
# average Alternation value per year or have one line per file and birdID ect as random factor ?
# survival analysis !! if dead one year, cannot be alive next year ! 
# temporal autocorrelation to take into account !
# have Adev instead of Mean A ?
# include polynomial term ? (high alternation > runaway investement > cost. select for an optimal alternation ?)

## sealed bid by male
# how can female adjust ? if purely alternate but for a low male provisioning > low fitness !


# integrate Schlicht at al 2016 analyses
# calculate p and check in our data like Johnstone did in his reply
# check for temporal autocorrelation simply with the previous interfeed interval? 
# randomization within 30 minutes windows ? 
# overall correlation ? 
# ask Emmi if she is conviced by Johnstone reply



}#############################
