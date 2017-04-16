#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 15/04/2017
#	 commit: start cleaning up code to fit simulation of analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


{### remarks
# LastSeenAlive information needs to be updated manually when DB updated (especially with new pedigree)
# MY_tblBrood$Nb3 is the number of post fledgling
# MY_tblBrood Mass and tarsus info: the last measurement, at d12, when ringed. nMass, nTarsus, NbRinged should in principle be equal: maybe should consider small difference of age, i.e. include all brood or a standardized subsets
}

rm(list = ls(all = TRUE))

{### packages and options

library(dplyr) 
library(ggplot2)
library(lme4)
library(arm) # for the function invlogit
library(blmeco) # to check for overdispersion

options(scipen=999) # remove scientific notation e-
#options(scipen=0)

}


{### Get raw data from R_Selected&RandomizedData folder

# source('Alternation_DataSelection_DataSimulation.R')
# or :

SelectedData_folder <- "R_Selected&RandomizedData"

MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD.csv", sep="/")) # summary stats for all analyzed videos where both parents known and with expected alternation from simulation
MY_TABLE_perBrood <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perBrood.csv", sep="/")) # only recorded brood (summarizing MY_TABLE_perDVD per brood)
MY_TABLE_perChick <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perChick.csv", sep="/"))

}

head(MY_TABLE_perDVD)
head(MY_TABLE_perBrood)


{# create MY_TABLE_perDVD_long, with the column A and S having observed values in first half, and sim values in second half
MY_TABLE_perDVD_long <- rbind(MY_TABLE_perDVD,MY_TABLE_perDVD)

MY_TABLE_perDVD_long$Type <- c(rep("z_Obsv", nrow(MY_TABLE_perDVD)),rep("a_Sim", nrow(MY_TABLE_perDVD)))

MY_TABLE_perDVD_long$A[MY_TABLE_perDVD_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_long$MedAsimWithin[MY_TABLE_perDVD_long$Type == 'a_Sim']
MY_TABLE_perDVD_long$S[MY_TABLE_perDVD_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_long$MedSsimWithin[MY_TABLE_perDVD_long$Type == 'a_Sim']

MY_TABLE_perDVD_long$rowID <- seq(1:nrow(MY_TABLE_perDVD_long))
}

head(MY_TABLE_perDVD_long)



######################
# RANDOM VS OBSERVED #
######################

{All_A_long <- data.frame(

A=c(MY_TABLE_perDVD$Asorted, 
MY_TABLE_perDVD$A,
MY_TABLE_perDVD$Aswitch,
MY_TABLE_perDVD$MedAsimWithin,
MY_TABLE_perDVD$MedAsimAmong), 

Type = c(rep("a_sorted", nrow(MY_TABLE_perDVD)),
rep("a_Obsv", nrow(MY_TABLE_perDVD)),
rep("b_switch", nrow(MY_TABLE_perDVD)),
rep("c_within", nrow(MY_TABLE_perDVD)),
rep("d_among", nrow(MY_TABLE_perDVD))),

AMax = c(MY_TABLE_perDVD$AMax,MY_TABLE_perDVD$AMax,MY_TABLE_perDVD$AMax,MY_TABLE_perDVD$AMax,MY_TABLE_perDVD$AMax),

DVDRef = c(MY_TABLE_perDVD$DVDRef,MY_TABLE_perDVD$DVDRef,MY_TABLE_perDVD$DVDRef,MY_TABLE_perDVD$DVDRef,MY_TABLE_perDVD$DVDRef),

LineID = 1: nrow(MY_TABLE_perDVD)*5
)
}

head(All_A_long)

{All_S_long <- data.frame(

S=c(MY_TABLE_perDVD$S,
MY_TABLE_perDVD$MedSsimWithin,
MY_TABLE_perDVD$MedSsimAmong), 

Type = c(rep("a_Obsv", nrow(MY_TABLE_perDVD)),
rep("c_within", nrow(MY_TABLE_perDVD)),
rep("d_among", nrow(MY_TABLE_perDVD))),

SMax = c(MY_TABLE_perDVD$A,MY_TABLE_perDVD$MedAsimWithin,MY_TABLE_perDVD$MedAsimAmong), # this is an approximation, Smax should be the actual number of A in that specific observation.

DVDRef = c(MY_TABLE_perDVD$DVDRef,MY_TABLE_perDVD$DVDRef,MY_TABLE_perDVD$DVDRef),

LineID = 1: nrow(MY_TABLE_perDVD)*5
)
}

head(All_S_long)


{#### alternation

mod_A_RandomVsObs <- glmer(A ~ Type + (1|DVDRef), data = All_A_long, family = 'poisson')
summary(mod_A_RandomVsObs)

dispersion_glmer(mod_A_RandomVsObs) # < 1.4

mod_A_RandomVsObs_without_intercept <- glmer(A ~ -1 + Type  +(1|DVDRef) , data = All_A_long, family = 'poisson')
summary(mod_A_RandomVsObs_without_intercept)
exp(summary(mod_A_RandomVsObs_without_intercept)$coeff[,1])

	
{# model assumption checking 

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_A_RandomVsObs), resid(mod_A_RandomVsObs))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_A_RandomVsObs))
qqline(resid(mod_A_RandomVsObs))
qqnorm(unlist(ranef(mod_A_RandomVsObs))) 
qqline(unlist(ranef(mod_A_RandomVsObs)))

# qq-plot of deviance-residuals: should be normally distributed
qqnorm(residuals(mod_A_RandomVsObs, type="deviance"))
qqline(residuals(mod_A_RandomVsObs, type="deviance"))

# Mean of ranefs: should be zero
mean(unlist(ranef(mod_A_RandomVsObs)$DVDRef))

# residuals vs predictors
plot(as.factor(All_A_long$Type), resid(mod_A_RandomVsObs))
abline(h=0, lty=2)


}

}

summary(mod_A_RandomVsObs)


{#### synchrony

mod_S_RandomVsObs <- glmer(S ~ Type + (1|DVDRef), data = All_S_long, family = 'poisson')
summary(mod_S_RandomVsObs)

dispersion_glmer(mod_S_RandomVsObs) # < 1.4

mod_S_RandomVsObs_without_intercept <- glmer(S ~ -1 + Type  +(1|DVDRef) , data = All_S_long, family = 'poisson')
summary(mod_S_RandomVsObs_without_intercept)

{# model assumption checking 

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(mod_S_RandomVsObs), resid(mod_S_RandomVsObs))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_S_RandomVsObs))
qqline(resid(mod_S_RandomVsObs))
qqnorm(unlist(ranef(mod_S_RandomVsObs))) 
qqline(unlist(ranef(mod_S_RandomVsObs)))

# qq-plot of deviance-residuals: should be normally distributed
qqnorm(residuals(mod_S_RandomVsObs, type="deviance"))
qqline(residuals(mod_S_RandomVsObs, type="deviance"))

# Mean of ranefs: should be zero
mean(unlist(ranef(mod_S_RandomVsObs)$DVDRef))

# residuals vs predictors
plot(as.factor(All_S_long$Type), resid(mod_S_RandomVsObs))
abline(h=0, lty=2)


}

}

summary(mod_S_RandomVsObs)



##############
# PREDICTORS #
##############

# the type of model was decided following simulations (other script)

{#### predictors of alternation

{# check dependent and explanatory variables

cor.test(MY_TABLE_perDVD$DVDInfoAge,MY_TABLE_perDVD$DVDInfoChickNb) # cor = -0.10, p<0.001 
cor.test(MY_TABLE_perDVD$DVDInfoAge,MY_TABLE_perDVD$NbRinged) # cor = 0.05, p=0.06 
cor.test(MY_TABLE_perDVD$MumAge,MY_TABLE_perDVD$DadAge) # cor = 0.34, p *****   - assortative mating for age > take the mean of the 2 ?
cor.test(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$PairBroodNb) # cor = 0.65, p < 0.0001 ! > take one or the other variable ?
cor.test(MY_TABLE_perDVD$FPriorResidence,MY_TABLE_perDVD$MPriorResidence)

#hist(MY_TABLE_perDVD$DVDInfoAge) # very bimodal as the protocol is to measure d7 and d11, in between is when they "miss"

summary(MY_TABLE_perDVD$RelTimeHrs) # 6 NA's > if this covariate is use, reduce MY_TABLE_perDVD from those RelTimeHrs NAs
# add the average RelTimeHrs for those NA
MY_TABLE_perDVD_long$RelTimeHrs[is.na(MY_TABLE_perDVD_long$RelTimeHrs)] <- mean(MY_TABLE_perDVD_long$RelTimeHrs, na.rm=T)


#scatter.smooth(MY_TABLE_perDVD$NbAlternation,MY_TABLE_perDVD$RelTimeHrs)# linear 
#scatter.smooth(MY_TABLE_perDVD$RelTimeHrs,MY_TABLE_perDVD$NbAlternation)# linear
#scatter.smooth(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$NbAlternation/MY_TABLE_perDVD$NbAMax)

}

{# mod A

modA <- glmer(A ~  
	
	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*scale(DVDInfoChickNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
    Type*FPriorResidence +
	
	Type*scale(TotalProRate) +
	Type*scale(VisitRateDifference)+
		
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) 
	+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_long
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA)

}

{# model assumption checking

# residuals vs fitted: mean should constantly be zero: not quite ??
scatter.smooth(fitted(modA), resid(modA))
abline(h=0, lty=2)


# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modA))
qqline(resid(modA))
qqnorm(unlist(ranef(modA))) 
qqline(unlist(ranef(modA)))

# Mean of ranefs: should be zero
mean(unlist(ranef(modA)$BroodRef))
mean(unlist(ranef(modA)$SocialMumID))
mean(unlist(ranef(modA)$SocialDadID))
mean(unlist(ranef(modA)$PairID))

# residuals vs predictors
scatter.smooth(MY_TABLE_perDVD_long$ParentsAge, resid(modA))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD_long$HatchingDayAfter0401, resid(modA))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD_long$PairBroodNb, resid(modA))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD_long$DVDInfoChickNb, resid(modA))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD_long$ChickAgeCat, resid(modA))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD_long$RelTimeHrs, resid(modA))
abline(h=0, lty=2)		


}


}

summary(modA)


{#### predictors of synchrony

modS <- glmer(S ~ 
	
	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*scale(DVDInfoChickNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
    Type*FPriorResidence +
	
	Type*scale(TotalProRate) +
	Type*scale(VisitRateDifference)+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) 
	#+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_long
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modS)

{# model assumption checking

# residuals vs fitted: mean should constantly be zero: not quite !
scatter.smooth(fitted(modS), resid(modS))
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modS))
qqline(resid(modS))
qqnorm(unlist(ranef(modS))) 
qqline(unlist(ranef(modS)))

# Mean of ranefs: should be zero
mean(unlist(ranef(modS)$BroodRef))
mean(unlist(ranef(modS)$SocialMumID))
mean(unlist(ranef(modS)$SocialDadID))
mean(unlist(ranef(modS)$PairID))

# residuals vs predictors
scatter.smooth(MY_TABLE_perDVD_long$ParentsAge, resid(modS))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD_long$HatchingDayAfter0401, resid(modS))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD_long$PairBroodNb, resid(modS))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD_long$DVDInfoChickNb, resid(modS))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD_long$ChickAgeCat, resid(modS))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD_long$RelTimeHrs, resid(modS))
abline(h=0, lty=2)		


}
	
	
	
}

summary(modS)



######################
# FITNESS CORRELATES #
######################


head(MY_TABLE_perBrood)
cor.test(MY_TABLE_perBrood$MeanAdev, log((MY_TABLE_perBrood$MeanA+0.5)/(MY_TABLE_perBrood$MeanAsim+0.5)))
plot (MY_TABLE_perBrood$MeanAdev, log((MY_TABLE_perBrood$MeanA+0.5)/(MY_TABLE_perBrood$MeanAsim+0.5)))
cor.test(MY_TABLE_perBrood$MeanSdev, log((MY_TABLE_perBrood$MeanS+0.5)/(MY_TABLE_perBrood$MeanSsim+0.5)))
plot (MY_TABLE_perBrood$MeanSdev, log((MY_TABLE_perBrood$MeanS+0.5)/(MY_TABLE_perBrood$MeanSsim+0.5)))


head(MY_TABLE_perDVD)
cor.test(MY_TABLE_perDVD$Adev, log((MY_TABLE_perDVD$A+0.5)/(MY_TABLE_perDVD$MeanAsimWithin+0.5)))
plot (MY_TABLE_perDVD$Adev, log((MY_TABLE_perDVD$A+0.5)/(MY_TABLE_perDVD$MeanAsimWithin+0.5)))
cor.test(MY_TABLE_perDVD$Sdev, log((MY_TABLE_perDVD$S+0.5)/(MY_TABLE_perDVD$MeanSsimWithin+0.5)))
plot (MY_TABLE_perDVD$Sdev, log((MY_TABLE_perDVD$S+0.5)/(MY_TABLE_perDVD$MeanSsimWithin+0.5)))



{#### ChickSurvival ~ Alternation + Synchrony, brood

modChickSurvival <- glmer(cbind(NbRinged, NbHatched) ~ 	scale(MeanTotalProRate)+
														scale(MeanAdev)+
														scale(MeanSdev) +
														scale(HatchingDayAfter0401) +
														scale(PairBroodNb) +
														scale(MBroodNb) +
														scale(FBroodNb) +
														MPriorResidence +
														FPriorResidence +
														(1|SocialMumID)+ (1|SocialDadID) + 
														(1|PairID) + 
														(1|BreedingYear) 
														, data = MY_TABLE_perBrood
														, family = 'binomial')
										
summary(modChickSurvival)  

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero: not quite !
scatter.smooth(fitted(modChickSurvival), resid(modChickSurvival))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modChickSurvival))
qqline(resid(modChickSurvival))
qqnorm(unlist(ranef(modChickSurvival))) 
qqline(unlist(ranef(modChickSurvival)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(modChickSurvival)$SocialMumID))
# mean(unlist(ranef(modChickSurvival)$SocialDadID))
# mean(unlist(ranef(modChickSurvival)$PairID))
mean(unlist(ranef(modChickSurvival)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood
scatter.smooth(d$MeanAdev, resid(modChickSurvival)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)


# fitted vs all predictors
scatter.smooth(d$MeanAdev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$MeanSdev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$MeanTotalProRate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2)

}

}

summary(modChickSurvival) 


{#### ChickMass ~ Alternation + Synchrony, chicks

{# check dependent and explanatory variables
nrow(MY_TABLE_perBrood[ MY_TABLE_perBrood$NbRinged == 0 ,]) # 43 broods with no ringed chicks
nrow(MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,]) # 21 broods where ringed chicks but no mass nor tarsus: for some reasons were ringed not at the rigth age for comparable measurements)
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,] # 2 broods with ringed with mass but not tarsus

scatter.smooth(MY_TABLE_perBrood$AvgMass~ MY_TABLE_perBrood$AvgTarsus)

summary(MY_TABLE_perBrood$MixedBroodYN[!is.na(MY_TABLE_perBrood$sdResMassTarsus)])


# RQ: in MY_TABLE_perChick 'AvgOf' are not averages but simply the Mass and Tarsus of the chick as the minimum age (between 11 and 14) he was measured

MY_TABLE_perChick[MY_TABLE_perChick$NbRinged == 0,]
nrow(MY_TABLE_perChick[is.na(MY_TABLE_perChick$sire) | is.na(MY_TABLE_perChick$dam),]) # 35 chick with at least one genetic parent missing


}


modChickMass <- lmer(ResMassTarsus_perChick ~ MeanTotalProRate +
											HatchingDayAfter0401 +
											PairBroodNb +
											NbRinged + # brood size at d11 when measured
											MeanAdev + 
											MeanSdev +
											(1|RearingBrood)+
											#(1|SocialMumID)+ (1|SocialDadID) + 
											(1|PairID) + (1|BreedingYear) 
											+ (1|dam) + (1|sire) + (1|GenPairID)
											, data = MY_TABLE_perChick)
				

summary(modChickMass) 

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero: not quite !
scatter.smooth(fitted(modChickMass), resid(modChickMass))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modChickMass))
qqline(resid(modChickMass))
qqnorm(unlist(ranef(modChickMass))) 
qqline(unlist(ranef(modChickMass)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(modChickMass)$SocialMumID))
# mean(unlist(ranef(modChickMass)$SocialDadID))
# mean(unlist(ranef(modChickMass)$PairID))
mean(unlist(ranef(modChickMass)$BreedingYear))
mean(unlist(ranef(modChickMass)$dam))
mean(unlist(ranef(modChickMass)$sire))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modChickMass))), fitted (modChickMass))

# residuals vs predictors
d <- MY_TABLE_perChick[!is.na(MY_TABLE_perChick$ResMassTarsus_perChick) & !is.na(MY_TABLE_perChick$dam) & !is.na(MY_TABLE_perChick$sire),]
scatter.smooth(d$MeanAdev, resid(modChickMass)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)


# fitted vs all predictors
scatter.smooth(d$MeanAdev,fitted(modChickMass),  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$MeanSdev,fitted(modChickMass),  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$MeanTotalProRate,fitted(modChickMass),  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$HatchingDayAfter0401,fitted(modChickMass),  las=1, cex.lab=1.4, cex.axis=1.2)
plot(d$PairBroodNb,fitted(modChickMass),  las=1, cex.lab=1.4, cex.axis=1.2)
plot(d$NbRinged,fitted(modChickMass),  las=1, cex.lab=1.4, cex.axis=1.2)
										
}

}

summary(modChickMass) 


{#### ChickMassVariance ~ synchrony, brood

modChickMassVariance <- lmer(sdResMassTarsus ~ MeanTotalProRate+
												HatchingDayAfter0401+
												NbRinged +
												MixedBroodYN +	# this isn't quite correct										
												MeanAdev+
												MeanSdev + 
												PairBroodNb+
												(1|SocialMumID)+ (1|SocialDadID) + 
												(1|PairID) + (1|BreedingYear) 
												,data=MY_TABLE_perBrood)
summary(modChickMassVariance)

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero: not quite !
scatter.smooth(fitted(modChickMassVariance), resid(modChickMassVariance))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modChickMassVariance))
qqline(resid(modChickMassVariance))
qqnorm(unlist(ranef(modChickMassVariance))) 
qqline(unlist(ranef(modChickMassVariance)))

# Mean of ranefs: should be zero
mean(unlist(ranef(modChickMassVariance)$SocialMumID))
mean(unlist(ranef(modChickMassVariance)$SocialDadID))
mean(unlist(ranef(modChickMassVariance)$PairID))
mean(unlist(ranef(modChickMassVariance)$BreedingYear))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modChickMassVariance))), fitted (modChickMassVariance))

# residuals vs predictors
d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$sdResMassTarsus),]
scatter.smooth(d$MeanAdev, resid(modChickMassVariance)) # not linear !! > add poly term to model ?
abline(h=0, lty=2)


# fitted vs all predictors
scatter.smooth(d$MeanAdev,fitted(modChickMassVariance),  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$MeanSdev,fitted(modChickMassVariance),  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$MeanTotalProRate,fitted(modChickMassVariance),  las=1, cex.lab=1.4, cex.axis=1.2)
scatter.smooth(d$HatchingDayAfter0401,fitted(modChickMassVariance),  las=1, cex.lab=1.4, cex.axis=1.2)
plot(d$PairBroodNb,fitted(modChickMassVariance),  las=1, cex.lab=1.4, cex.axis=1.2)
plot(d$NbRinged,fitted(modChickMassVariance),  las=1, cex.lab=1.4, cex.axis=1.2)
										
}


}

summary(modChickMassVariance)





###########
# DIVORCE #
###########

{#### predictors of divorce

{# check dependent and explanatory variables 
nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MwillDivorce) & !is.na(MY_TABLE_perBrood$NbRinged),])
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$MwillDivorce) & !is.na(MY_TABLE_perBrood$NbRinged),] # when Social female was NA
MY_TABLE_perBrood[MY_TABLE_perBrood$SocialDadID == 4060,]
}


mod_MaleDivorce <- glmer(MwillDivorce~  scale(MeanSdev) + 
										scale(MeanAdev)	+
										scale(ParentsAge) + 
										scale(PairBroodNb) +
										scale(MeanTotalProRate) +  
										MnextNBsame + 
										scale(NbRinged) +
										(1|SocialDadID)
										#+(1|BreedingYear) 
										, data = MY_TABLE_perBrood
										, family="binomial")

									
summary(mod_MaleDivorce) # Number of obs: 680, groups:  SocialDadID, 223; BreedingYear, 12



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


## check for overdispersion
# mod_MaleDivorce_withOverdispersionAccounted <- glmer(MDivorce~MeanSynchroFeed + 
									##MeanA	+
									# scale(DadAge, scale=FALSE) + 
									# scale(PairBroodNb, scale=FALSE) +
									##MeanVisitRateDifference +  
									# MPriorResidence + 
									##MPrevNbRinged +
									 # (1|SocialDadID) + (1|BreedingYear) +(1|BroodRef)
									# , data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MDivorce),], family="binomial")
									
# summary(mod_MaleDivorce_withOverdispersionAccounted)
# anova(mod_MaleDivorce, mod_MaleDivorce_withOverdispersionAccounted)


# Mean of ranefs: should be zero
mean(unlist(ranef(mod_MaleDivorce)$SocialDadID))

# residuals vs predictors

# d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MDivorce) & !is.na(MY_TABLE_perBrood$MPrevNbRinged),]
# plot(d$MeanSynchroFeed, resid(mod_MaleDivorce))
# abline(h=0, lty=2)
# plot(d$PairBroodNb, resid(mod_MaleDivorce))
# abline(h=0, lty=2)
# plot(d$MPriorResidence, resid(mod_MaleDivorce))
# abline(h=0, lty=2)
# plot(d$MPrevNbRinged, resid(mod_MaleDivorce))
# abline(h=0, lty=2)

# # dependent variable vs fitted
# d$fitted <- fitted(mod_MaleDivorce)
# plot(d$fitted, d$MDivorce,ylim=c(0, 1))
# abline(0,1)	

# # fitted vs all predictors
# plot(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="MeanSynchroFeed")
# plot(d$PairBroodNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="PairBroodNb")
# plot(d$MPriorResidence,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="MPriorResidence")
# plot(d$MPrevNbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="MDivorce", xlab="MPrevNbRinged")

}



mod_FemaleDivorce <- glmer(FwillDivorce~scale(MeanSdev) + 
									scale(MeanAdev)	+
									scale(MumAge) + 
									scale(PairBroodNb) +
									scale(MeanTotalProRate) +  
									FnextNBsame + 
									scale(NbRinged) +
									(1|SocialMumID) 
									#+ (1|BreedingYear) 
									, data = MY_TABLE_perBrood
									, family="binomial")
									
summary(mod_FemaleDivorce) # Number of obs: 679, groups:  SocialMumID, 232; BreedingYear, 12

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


# # check for overdispersion
# mod_FemaleDivorce_withOverdispersionAccounted <- glmer(FDivorce~MeanSynchroFeed + 
									# # MeanA	+
									# #scale(DadAge, scale=FALSE) + 
									# scale(PairBroodNb, scale=FALSE) +
									# # MeanVisitRateDifference +  
									# FPriorResidence + FPrevNbRinged +
									 # (1|SocialMumID) + (1|BreedingYear) +(1|BroodRef)
									# , data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$FDivorce),], family="binomial")
									
# summary(mod_FemaleDivorce_withOverdispersionAccounted)
# anova(mod_FemaleDivorce, mod_FemaleDivorce_withOverdispersionAccounted) 


# Mean of ranefs: should be zero
mean(unlist(ranef(mod_FemaleDivorce)$SocialMumID))
mean(unlist(ranef(mod_FemaleDivorce)$BreedingYear))

# residuals vs predictors

d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$FwillDivorce),]
plot(d$MeanSynchroFeed, resid(mod_FemaleDivorce))
abline(h=0, lty=2)
plot(d$PairBroodNb, resid(mod_FemaleDivorce))
abline(h=0, lty=2)
plot(d$FnextNBsame, resid(mod_FemaleDivorce))
abline(h=0, lty=2)
plot(d$NbRinged, resid(mod_FemaleDivorce))
abline(h=0, lty=2)

# dependent variable vs fitted
d$fitted <- fitted(mod_FemaleDivorce)
plot(d$fitted, d$FDivorce,ylim=c(0, 1))
abline(0,1)	

# fitted vs all predictors
plot(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="MeanSynchroFeed")
plot(d$PairBroodNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="PairBroodNb")
plot(d$FnextNBsame,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="FnextNBsame")
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="FDivorce", xlab="FPrevNbRinged")
}

}	

summary(mod_MaleDivorce)	
summary(mod_FemaleDivorce)		




