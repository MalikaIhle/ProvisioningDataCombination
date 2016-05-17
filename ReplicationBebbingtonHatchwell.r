#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 15/04/2015
#	 last modif : 16/05/2016  
#	 commit: annotations 
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
library(RODBC)
library(MASS) # for boxcox
library(dplyr) # for some part of the extraction of the data written by Andrew for the simulation 
library(ggplot2)
library(boot) # for simulation
library(lme4)
# library(rptR) under construction
library(RLRsim) # for testing significance randome effect in repeatability part

options(warn=2)	# when loop generate a error at one iteration, the loop stop, so one can call the filename and check what's wrong with it
# options(warn=-1) # for Rmarkdown not to print the warnings
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

}

{# query DB

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")

# MassTarsusRearinBrood_allChicks (see annotated sql query 'LastMassTarsusChick')

{MassTarsusRearinBrood_allChicks <-  sqlQuery(conDB, "
SELECT tblCaptures.BirdID AS ChickID, 
usys_qRearingBrood.RearingBrood,
 Avg(tblMeasurements.Mass) AS AvgOfMass, 
 Avg(tblMeasurements.Tarsus) AS AvgOfTarsus, 
 Avg(usys_qRelativeChickMassClassesForCaptures.Age) AS AvgOfAge, 
 Count(usys_qRearingBrood.BirdID) AS nMeasures
 
FROM tblBirdID INNER JOIN 
((
	(SELECT tblBirdID.BirdID, IIf([FosterBrood] Is Null,[BroodRef],[FosterBrood]) AS RearingBrood
	FROM tblBirdID LEFT JOIN tblFosterBroods ON tblBirdID.BirdID = tblFosterBroods.BirdID
	WHERE (((tblBirdID.BroodRef) Is Not Null))) 
	AS usys_qRearingBrood 

INNER JOIN (
	(SELECT tblCaptures.CaptureRef, 
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
GROUP BY tblCaptures.BirdID, usys_qRearingBrood.RearingBrood, tblCaptures.CaptureDate;

")
}

tblChicks <- merge(x=MassTarsusRearinBrood_allChicks, y= pedigree[,c("id","dam","sire")], all.x=TRUE, by.x="ChickID", by.y = "id")

close(conDB)


}

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

MY_tblChicks <- tblChicks[tblChicks$RearingBrood %in% MY_tblDVDInfo$BroodRef,] 


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

{# the typical sparrow

cor.test(MY_tblBroods$ParentsAge,MY_tblBroods$PairBroodNb) # cor = 0.54, p < 0.0001 
scatter.smooth(jitter(MY_tblBroods$ParentsAge,3), jitter(MY_tblBroods$PairBroodNb,3))# 
summary(MY_tblBroods$MBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])
summary(MY_tblBroods$FBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])
summary(MY_tblBroods$PairBroodNb[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])

summary(MY_tblBroods$ParentsAge[!is.na(MY_tblBroods$SocialDadID) & !is.na(MY_tblBroods$SocialMumID)])

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

nrow(MY_tblParentalCare) # 1499 if remove quantiles
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





{### create MY_TABLE_perDVD: where both parents known
# one line is a valid DVDRef, with the summary of the DVD, its metadata, and the brood characteristics.
# as broods were watched several time, the brood info appears in duplicate

MY_TABLE_perDVD <- MY_tblParentalCare[,c("DVDRef","FVisit1RateH","MVisit1RateH","DiffVisit1Rate","MFVisit1RateH","AlternationValue")]
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, y=MY_tblDVDInfo[,c("DVDRef","BroodRef","DVDInfoChickNb","ChickAge","ChickAgeCat","DVDdate","RelTimeHrs")], by='DVDRef')
MY_TABLE_perDVD <- merge(x=MY_TABLE_perDVD, 
y=MY_tblBroods[,c("BroodRef","BreedingYear","HatchingDayAfter0401","SocialMumID","SocialDadID","NbRinged","DadAge","MumAge","ParentsAge",
"MPrevNbRinged","MBroodNb","MPriorResidence","MDivorce","MDivorceforEx",
"FPrevNbRinged","FBroodNb","FPriorResidence","FDivorce","FDivorceforEx","PairID","PairBroodNb","PairIDYear", "AvgMass", "MinMass", "AvgTarsus")], by='BroodRef')


MY_TABLE_perDVD <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$SocialMumID) & !is.na(MY_TABLE_perDVD$SocialDadID),] # where both parents known
nrow(MY_TABLE_perDVD) # 1702 files
length(unique(MY_TABLE_perDVD$BroodRef)) # 919 broods


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



}

}

head(MY_TABLE_perDVD)


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
	, data = MY_TABLE_perDVD)

summary(modA) # Number of obs: 1696, groups:  BroodRef, 916; PairID, 453; SocialMumID, 295; SocialDadID, 283; BreedingYear, 12

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
	, data = dat6)

summary(modA_Age6) # Number of obs: 874, groups:  PairID, 443; SocialMumID, 292; SocialDadID, 281; BreedingYear, 12


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
	, data = dat10)

summary(modA_Age10) #Number of obs: 758, groups:  PairID, 398; SocialMumID, 278; SocialDadID, 264; BreedingYear, 12

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

}

summary(modA)

summary(modA)$coefficients
summary(modA_Age6)$coefficients
summary(modA_Age10)$coefficients
print(VarCorr(modA),comp=c("Variance","Std.Dev."))
print(VarCorr(modA_Age6),comp=c("Variance","Std.Dev."))
print(VarCorr(modA_Age10),comp=c("Variance","Std.Dev."))

summary(modA_ParentAge)$coefficients
summary(modA_PairBroodNb)$coefficients
print(VarCorr(modA_ParentAge),comp=c("Variance","Std.Dev."))
print(VarCorr(modA_PairBroodNb),comp=c("Variance","Std.Dev."))




{### create MY_TABLE_perBrood
MY_TABLE_perDVD[is.na(MY_TABLE_perDVD$MFVisit1RateH),]
summary(MY_TABLE_perDVD$MFVisit1RateH)

MY_TABLE_perBrood <- split(MY_TABLE_perDVD,MY_TABLE_perDVD$BroodRef)
MY_TABLE_perBrood[[1]]

MY_TABLE_perBrood_fun = function(x)  {

return(c(
mean(x$MFVisit1RateH), # TotalProRate
mean(x$AlternationValue), #MeanA
mean(x$MeanAsim) - mean(x$AlternationValue), # Adev
mean(x$DiffVisit1Rate) # MeanDiffVisit1Rate
))
}

MY_TABLE_perBrood_out1 <- lapply(MY_TABLE_perBrood, FUN=MY_TABLE_perBrood_fun)
MY_TABLE_perBrood_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perBrood_out1)),do.call(rbind, MY_TABLE_perBrood_out1))

nrow(MY_TABLE_perBrood_out2)	# 919
rownames(MY_TABLE_perBrood_out2) <- NULL
colnames(MY_TABLE_perBrood_out2) <- c('BroodRef','TotalProRate','MeanA', 'Adev','MeanDiffVisit1Rate')

MY_TABLE_perBrood <- merge(x=unique(MY_TABLE_perDVD[,c("NbRinged","AvgMass","AvgTarsus","BroodRef","SocialMumID", "SocialDadID","PairID", "BreedingYear","PairIDYear" )]),
							y=MY_TABLE_perBrood_out2,all.x=TRUE, by='BroodRef')
							
							
{# calculate residual mass on tarsus

ResMassTarsus <-  cbind(as.numeric(rownames(data.frame(residuals(lm(MY_TABLE_perBrood$AvgMass~ MY_TABLE_perBrood$AvgTarsus))))), 
											data.frame(residuals(lm(MY_TABLE_perBrood$AvgMass~ MY_TABLE_perBrood$AvgTarsus))))
colnames(ResMassTarsus) <- c("rownb" , "ResMassTarsus")
head(ResMassTarsus)

rownameBrood <- data.frame(as.numeric(rownames(MY_TABLE_perBrood)))
colnames(rownameBrood) <- "rownb"


MY_TABLE_perBrood <- cbind(MY_TABLE_perBrood,rownameBrood)
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=ResMassTarsus, all.x=TRUE, by = "rownb")
MY_TABLE_perBrood$rownb <- as.numeric(as.character(MY_TABLE_perBrood$rownb))
MY_TABLE_perBrood <- MY_TABLE_perBrood[order(MY_TABLE_perBrood$rownb),]


}

}

head(MY_TABLE_perBrood)


{### create MY_TABLE_perChick

MY_TABLE_perChick <- merge(x= MY_tblChicks , y=MY_TABLE_perBrood, by.x="RearingBrood", by.y = "BroodRef", all.x=TRUE )

MY_TABLE_perChick$GenPairID <- paste(MY_TABLE_perChick$sire, MY_TABLE_perChick$dam, sep="")

}

head(MY_TABLE_perChick)


{### create MY_TABLE_perBirdYear

{# get both sex piled up
MY_TABLE_Survival <- merge(x=MY_TABLE_perDVD[c("BroodRef","AlternationValue","SocialMumID","SocialDadID","DadAge","MumAge","PairID","BreedingYear")],
						  y=sys_LastSeenAlive[,c("BirdID","LastYearAlive")],
						  by.x="SocialMumID", by.y="BirdID",
						  all.x=TRUE)
MY_TABLE_Survival <- merge(x=MY_TABLE_Survival, 
						  y=sys_LastSeenAlive[,c("BirdID","LastYearAlive")],
						  by.x="SocialDadID", by.y="BirdID",
						  all.x=TRUE)

MY_TABLE_Survival$FAliveNextYear <- as.numeric(MY_TABLE_Survival$LastYearAlive.x) > MY_TABLE_Survival$BreedingYear
MY_TABLE_Survival$MAliveNextYear <- as.numeric(MY_TABLE_Survival$LastYearAlive.y) > MY_TABLE_Survival$BreedingYear


MY_TABLE_Female_Survival <- MY_TABLE_Survival[,c("BroodRef","AlternationValue","SocialMumID","MumAge","PairID","BreedingYear","FAliveNextYear")]
MY_TABLE_Male_Survival <- MY_TABLE_Survival[,c("BroodRef","AlternationValue","SocialDadID","DadAge","PairID","BreedingYear","MAliveNextYear")]
MY_TABLE_Female_Survival$Sex <- 0
MY_TABLE_Male_Survival$Sex <- 1	
				
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "MumAge")] <- "Age"						
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "DadAge")] <- "Age"	
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "SocialMumID")] <- "BirdID"		
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "SocialDadID")] <- "BirdID"		
colnames(MY_TABLE_Female_Survival)[which(names(MY_TABLE_Female_Survival) == "FAliveNextYear")] <- "AliveNextYear"		
colnames(MY_TABLE_Male_Survival)[which(names(MY_TABLE_Male_Survival) == "MAliveNextYear")] <- "AliveNextYear"	

head(MY_TABLE_Female_Survival)
head(MY_TABLE_Male_Survival)

MY_TABLE_Survival_perBird <- rbind(MY_TABLE_Female_Survival,MY_TABLE_Male_Survival)	
MY_TABLE_Survival_perBird$BirdIDYear <- paste(MY_TABLE_Survival_perBird$BirdID, MY_TABLE_Survival_perBird$BreedingYear, sep="")
}

head(MY_TABLE_Survival_perBird)

{# get mean Alternation per year per BirdID

MY_TABLE_perBirdYear <- split(MY_TABLE_Survival_perBird,MY_TABLE_Survival_perBird$BirdIDYear)
MY_TABLE_perBirdYear[[1]]

MY_TABLE_perBirdYear_fun = function(x)  {
return(mean(x$AlternationValue) #MeanAYear
)
}

MY_TABLE_perBirdYear_out1 <- lapply(MY_TABLE_perBirdYear, FUN=MY_TABLE_perBirdYear_fun)
MY_TABLE_perBirdYear_out2 <- data.frame(rownames(do.call(rbind,MY_TABLE_perBirdYear_out1)),do.call(rbind, MY_TABLE_perBirdYear_out1))

nrow(MY_TABLE_perBirdYear_out2)	# 999
rownames(MY_TABLE_perBirdYear_out2) <- NULL
colnames(MY_TABLE_perBirdYear_out2) <- c('BirdIDYear','MeanAYear')


MY_TABLE_perBirdYear <- merge(x=unique(MY_TABLE_Survival_perBird[,c("BirdID", "Age","PairID", "BreedingYear","AliveNextYear","Sex","BirdIDYear" )]),
							y=MY_TABLE_perBirdYear_out2,all.x=TRUE, by='BirdIDYear')
}



FemaleSurvival <- list()
MaleSurvival <- list()
survivalperyear <- as.data.frame(table(MY_TABLE_perBirdYear$AliveNextYear, MY_TABLE_perBirdYear$BreedingYear, MY_TABLE_perBirdYear$Sex))
for (i in 2004:2015)
{FemaleSurvival[i] <-  survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'TRUE']/  (survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'TRUE']+survivalperyear$Freq[survivalperyear$Var3 == 0 & survivalperyear$Var2 == i & survivalperyear$Var1 == 'FALSE'] ) 
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

head(MY_TABLE_perBirdYear)





{#### fitness correlate of alternation

{## total provisioning rate

{# check dependent and explanatory variables

hist(MY_TABLE_perBrood$TotalProRate)
summary(MY_TABLE_perBrood$TotalProRate)
shapiro.test(MY_TABLE_perBrood$TotalProRate) 

boxcox(lm(TotalProRate ~  NbRinged + poly(Adev,1), data = MY_TABLE_perBrood))
hist(MY_TABLE_perBrood$TotalProRate^0.45)
shapiro.test(MY_TABLE_perBrood$TotalProRate^0.45) 

}

modFitnessAsProRate <- lmer(TotalProRate^0.45 ~  NbRinged + 
											poly(Adev,1) +
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
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
scatter.smooth(MY_TABLE_perBrood$Adev, resid(modFitnessAsProRate))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perBrood
d$fitted <- fitted(modFitnessAsProRate)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$Adev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="Adev") # polynomial ?

}


modFitnessAsProRate_poly <- lmer(TotalProRate^0.45 ~  NbRinged +
											poly(Adev,2) +
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
scatter.smooth(MY_TABLE_perBrood$Adev, resid(modFitnessAsProRate_poly))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perBrood
d$fitted <- fitted(modFitnessAsProRate_poly)
scatter.smooth(d$fitted, jitter(d$TotalProRate^0.45, 0.05),ylim=c(0, 100^0.45))
abline(0,1)	

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$Adev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="Adev") # polynomial ?

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
										data = MY_TABLE_perBrood)
										
summary(modFitnessAsChickMass) # Number of obs: 852, groups:  PairID, 436; SocialMumID, 287; SocialDadID, 276; BreedingYear, 12

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
scatter.smooth(d$NbRinged, resid(modFitnessAsChickMass))
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
scatter.smooth(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AvgMass", xlab="NbRinged")
scatter.smooth(d$Adev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AvgMass", xlab="Adev")

}


modFitnessAsResChickMass <- lmer(ResMassTarsus ~ NbRinged + 
												 MeanA + 
												 (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) ,
												 data = MY_TABLE_perBrood)
										
summary(modFitnessAsResChickMass) # Number of obs: 852, groups:  PairID, 436; SocialMumID, 287; SocialDadID, 276; BreedingYear, 12
# identical results as model above

{# model on MY_TABLE_perChick
	# here 'AvgOf' are averages within a ChickID when measured twice in the right time windows (age 11 to 14)
	
modFitnessAsChickMasslikeabove <- lmer(AvgOfMass ~ NbRinged + 
										MeanA + 
										AvgOfTarsus +
										(1|RearingBrood)+
										(1|SocialMumID)+ 
										(1|SocialDadID) +
										(1|PairID) + (1|BreedingYear) 
										#+ (1|dam) + (1|sire) + (1|GenPairID)
										,data = MY_TABLE_perChick)

summary(modFitnessAsChickMasslikeabove)	


modFitnessAsChickMasswithGenParents <- lmer(AvgOfMass ~ NbRinged + 
										MeanA + 
										AvgOfTarsus +
										(1|RearingBrood)+
										(1|SocialMumID)+ (1|SocialDadID) + 
										(1|PairID) + (1|BreedingYear) 
										+ (1|dam) + (1|sire) + (1|GenPairID)
										, data = MY_TABLE_perChick)

summary(modFitnessAsChickMasswithGenParents)

print(VarCorr(modFitnessAsChickMass),comp=c("Variance","Std.Dev."))
print(VarCorr(modFitnessAsChickMasswithGenParents),comp=c("Variance","Std.Dev."))
summary(modFitnessAsChickMass)$coefficients
summary(modFitnessAsChickMasswithGenParents)$coefficients
}

}

{# Parent survival

{# check dependent and explanatory variables
summary(MY_TABLE_perBirdYear$AliveNextYear)
scatter.smooth(MY_TABLE_perBirdYear$MeanAYear, MY_TABLE_perBirdYear$Age)
scatter.smooth(MY_TABLE_perBirdYear$MeanAYear[MY_TABLE_perBirdYear$Sex == 1], MY_TABLE_perBirdYear$Age[MY_TABLE_perBirdYear$Sex == 1])
table( MY_TABLE_perBirdYear$AliveNextYear[MY_TABLE_perBirdYear$Sex == 0])
table( MY_TABLE_perBirdYear$AliveNextYear[MY_TABLE_perBirdYear$Sex == 1])


}

modSurvival <- glmer(AliveNextYear ~ MeanAYear + Sex + Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear, family = "binomial" )
									
summary(modSurvival) # Number of obs: 1110, groups:  BirdID, 578; BreedingYear, 12


modSurvivalMale <- glmer(AliveNextYear ~ MeanAYear + Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$Sex == 1,], family = "binomial" )
									
summary(modSurvivalMale) # !! Model failed to converge !! Number of obs: 555, groups:  BirdID, 283; BreedingYear, 12


modSurvivalFemale <- glmer(AliveNextYear ~ MeanAYear + Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear[MY_TABLE_perBirdYear$Sex == 0,], family = "binomial" )
									
summary(modSurvivalFemale) # Number of obs: 555, groups:  BirdID, 295; BreedingYear, 12

modSurvival_SexAgeInteraction <- glmer(AliveNextYear ~ MeanAYear + Sex*Age +
									(1|BirdID) +
									#(1|PairID) + 
									(1|BreedingYear)
									, data = MY_TABLE_perBirdYear, family = "binomial" )
									
summary(modSurvival_SexAgeInteraction) # Number of obs: 1110, groups:  BirdID, 578; BreedingYear, 12




{# model assumptions checking >> residuals not normal !!!!!!

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsChickMass), resid(modFitnessAsChickMass))	
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
anova(modSurvival, modSurvival_withOverdispersionAccounted) # p = 0.38


# Mean of ranefs: should be zero
mean(unlist(ranef(modSurvival)$BirdID))
mean(unlist(ranef(modSurvival)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MeanA) & !is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass),]
scatter.smooth(d$NbRinged, resid(modFitnessAsChickMass))
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
scatter.smooth(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AvgMass", xlab="NbRinged")
scatter.smooth(d$Adev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AvgMass", xlab="Adev")

}


}

}

summary(modFitnessAsProRate)
summary(modFitnessAsChickMass)
summary(modSurvival)




#########################################
# replication Nagagawa et al 2007 study #
#########################################

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


# calculate Feeding visit rate per hour per chick
BirdProRate$Visit1RateHChick <- round(BirdProRate$Visit1RateH / BirdProRate$DVDInfoChickNb,2)

BirdProRate <- BirdProRate[!is.na(BirdProRate$RelTimeHrs),]

}

head(BirdProRate)


{#### repeatbility of provisioning rate

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

{#### repeatbility of provisioning rate per sex
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

{#### repeatbility of provisioning rate per chick like Shinichi did

modProRateRpt_perChick <- lmer(Visit1RateHChick ~ Sex +
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
									
summary(modProRateRpt_perChick)

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
