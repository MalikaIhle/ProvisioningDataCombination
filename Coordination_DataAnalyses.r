#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 07/02/2019
#	 commit: cleaning up code to archive old analyses versions (corrected by joel)
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
library (multcomp)
library(MCMCglmm)
  
options(scipen=999) # remove scientific notation e-
#options(scipen=0)

}


{### Get raw data from R_Selected&RandomizedData folder

# source('Alternation_DataSelection_DataSimulation.R')
# or :

SelectedData_folder <- "R_Selected&RandomizedData"

MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD.csv", sep="/")) # summary stats for all analyzed videos where both parents known and with expected alternation from simulation
  # nrow(MY_TABLE_perDVD[MY_TABLE_perDVD$S == 0,])/nrow(MY_TABLE_perDVD)*100 #3%
#MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD_S05.csv", sep="/")) 
  # nrow(MY_TABLE_perDVD[MY_TABLE_perDVD$S == 0,])/nrow(MY_TABLE_perDVD)*100 #17%
  # summary(MY_TABLE_perDVD$S)
#MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD_S15.csv", sep="/")) 
  # nrow(MY_TABLE_perDVD[MY_TABLE_perDVD$S == 0,])/nrow(MY_TABLE_perDVD)*100 #4.6%
  # summary(MY_TABLE_perDVD$S)
#MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD_S25.csv", sep="/")) 
  # nrow(MY_TABLE_perDVD[MY_TABLE_perDVD$S == 0,])/nrow(MY_TABLE_perDVD)*100 #2.1%
  # summary(MY_TABLE_perDVD$S)

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
MY_TABLE_perDVD$MedAsimAmong,
MY_TABLE_perDVD$Agenerated), 

Type = c(rep("a_sorted", nrow(MY_TABLE_perDVD)),
rep("a_Obsv", nrow(MY_TABLE_perDVD)),
rep("b_switch", nrow(MY_TABLE_perDVD)),
rep("c_within", nrow(MY_TABLE_perDVD)),
rep("d_among", nrow(MY_TABLE_perDVD)),
rep("e_generated", nrow(MY_TABLE_perDVD))),

AMax = rep(MY_TABLE_perDVD$AMax, 6),

DVDRef = rep(MY_TABLE_perDVD$DVDRef,6),

LineID = 1: nrow(MY_TABLE_perDVD)*6
)
}

head(All_A_long)

{All_S_long <- data.frame(

S=c(MY_TABLE_perDVD$S,
MY_TABLE_perDVD$MedSsimWithin,
MY_TABLE_perDVD$MedSsimAmong,
MY_TABLE_perDVD$Sgenerated), 

Type = c(rep("a_Obsv", nrow(MY_TABLE_perDVD)),
rep("c_within", nrow(MY_TABLE_perDVD)),
rep("d_among", nrow(MY_TABLE_perDVD)),
rep("e_generated", nrow(MY_TABLE_perDVD))),

SMax = c(MY_TABLE_perDVD$A,MY_TABLE_perDVD$MedAsimWithin,MY_TABLE_perDVD$MedAsimAmong,MY_TABLE_perDVD$Agenerated), # this is an approximation, Smax should be the actual number of A in that specific observation.

DVDRef = rep(MY_TABLE_perDVD$DVDRef,4),

LineID = 1: nrow(MY_TABLE_perDVD)*4
)
}

head(All_S_long)


{#### alternation

mod_A_RandomVsObs <- glmer(A ~ Type + (1|DVDRef)
								, data = All_A_long
								, family = 'poisson')
summary(mod_A_RandomVsObs)

dispersion_glmer(mod_A_RandomVsObs) # < 1.4

mod_A_RandomVsObs_without_intercept <- glmer(A ~ -1 + Type  +(1|DVDRef) 
												, data = All_A_long
												, family = 'poisson')

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


summary(glht(mod_A_RandomVsObs, mcp(Type="Tukey")))


{#### synchrony

mod_S_RandomVsObs <- glmer(S ~ Type + (1|DVDRef)
								, data = All_S_long
								, family = 'poisson')
summary(mod_S_RandomVsObs)

dispersion_glmer(mod_S_RandomVsObs) # < 1.4

mod_S_RandomVsObs_without_intercept <- glmer(S ~ -1 + Type  +(1|DVDRef)
												, data = All_S_long
												, family = 'poisson')
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


summary(glht(mod_S_RandomVsObs, mcp(Type="Tukey"))) #sync = 2 : p sim vs among = 0.97 ; sync = 1.5 : p=0.83 ; sync=2.5 : p=1


##############
# PREDICTORS 
##############

# the type of model was decided following simulations (other script)


{#### predictors of alternation

{# check dependent and explanatory variables
# 
# cor.test(MY_TABLE_perDVD$DVDInfoAge,MY_TABLE_perDVD$DVDInfoChickNb) # cor = -0.10, p<0.001 
# cor.test(MY_TABLE_perDVD$DVDInfoAge,MY_TABLE_perDVD$NbRinged) # cor = 0.05, p=0.06 
# cor.test(MY_TABLE_perDVD$MumAge,MY_TABLE_perDVD$DadAge) # cor = 0.34, p *****   - assortative mating for age > take the mean of the 2 ? !!!! Highly pseudoreplicated
# cor.test(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$PairBroodNb) # cor = 0.65, p < 0.0001 ! > take one or the other variable ? !!!! Highly pseudoreplicated
# summary(MY_TABLE_perDVD$MPriorResidence == MY_TABLE_perDVD$FPriorResidence) # quite aliased
# cor.test(MY_TABLE_perDVD$MBroodNb,MY_TABLE_perDVD$FBroodNb)
# sunflowerplot(MY_TABLE_perDVD$MBroodNb,MY_TABLE_perDVD$FBroodNb)
# cor.test(MY_TABLE_perDVD$PairBroodNb,MY_TABLE_perDVD$FBroodNb)
# sunflowerplot(MY_TABLE_perDVD$PairBroodNb,MY_TABLE_perDVD$FBroodNb)
# cor.test(MY_TABLE_perDVD$PairBroodNb,MY_TABLE_perDVD$MBroodNb)
# sunflowerplot(MY_TABLE_perDVD$PairBroodNb,MY_TABLE_perDVD$MBroodNb)
# 
# 
# #hist(MY_TABLE_perDVD$DVDInfoAge) # very bimodal as the protocol is to measure d7 and d11, in between is when they "miss"
# 
# summary(MY_TABLE_perDVD$RelTimeHrs) # 6 NA's > if this covariate is use, reduce MY_TABLE_perDVD from those RelTimeHrs NAs
# 
# 
# 
# #scatter.smooth(MY_TABLE_perDVD$NbAlternation,MY_TABLE_perDVD$RelTimeHrs)# linear 
# #scatter.smooth(MY_TABLE_perDVD$RelTimeHrs,MY_TABLE_perDVD$NbAlternation)# linear
# #scatter.smooth(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$NbAlternation/MY_TABLE_perDVD$NbAMax)
# 
# cor.test(MY_TABLE_perDVD$MeanAsimWithin, MY_TABLE_perDVD$MedAsimWithin)
# cor.test(MY_TABLE_perDVD$MeanSsimWithin, MY_TABLE_perDVD$MedSsimWithin)
# 
}

{# mod A

  # add the average RelTimeHrs for those NA
  MY_TABLE_perDVD_long$RelTimeHrs[is.na(MY_TABLE_perDVD_long$RelTimeHrs)] <- mean(MY_TABLE_perDVD_long$RelTimeHrs, na.rm=T)


modA <- glmer(A ~

	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) +
	Type*scale(DVDInfoChickNb) +
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) +
	Type*MPriorResidence +

	Type*scale(TotalProRate) +
	Type*scale(VisitRateDifference)+

	(1|BroodRef) +
	(1|SocialMumID)+ (1|SocialDadID) +
	(1|PairID) +  # explained 0% of the variance
  (1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef)
  + (1|rowID) # for overdispersion > doesnt reduce overdispersion... ?? deviance = 17500 >> df ~3200 !
	, data = MY_TABLE_perDVD_long
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA)

dispersion_glmer(modA) # < 1.4


{# Joel attempt 
  
# MY_TABLE_perDVD_long$BroodRef <- as.factor(MY_TABLE_perDVD_long$BroodRef)
# MY_TABLE_perDVD_long$SocialMumID <- as.factor(MY_TABLE_perDVD_long$SocialMumID)
# MY_TABLE_perDVD_long$SocialDadID <- as.factor(MY_TABLE_perDVD_long$SocialDadID)
# MY_TABLE_perDVD_long$PairID <- as.factor(MY_TABLE_perDVD_long$PairID)
# MY_TABLE_perDVD_long$BreedingYear <- as.factor(MY_TABLE_perDVD_long$BreedingYear)
# MY_TABLE_perDVD_long$PairIDYear <- as.factor(MY_TABLE_perDVD_long$PairIDYear)
# MY_TABLE_perDVD_long$DVDRef <- as.factor(MY_TABLE_perDVD_long$DVDRef)

# write.csv(MY_TABLE_perDVD_long, file = "R_MY_TABLE_perDVD_long.csv", row.names = FALSE) 
  
# modAmcmcglmm <- MCMCglmm(A ~
#         Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
#         Type*scale(HatchingDayAfter0401) +
#         Type*scale(PairBroodNb) +
#         Type*scale(DVDInfoChickNb) +
#         Type*ChickAgeCat +
#         Type*scale(RelTimeHrs) +
#         Type*MPriorResidence +
#         Type*scale(TotalProRate) +
#         Type*scale(VisitRateDifference), random = ~  idh(Type):BroodRef +
#           idh(Type):SocialMumID+ 
#           idh(Type):SocialDadID +
#           idh(Type):PairID +  # explained 0% of the variance
#           idh(Type):BreedingYear + # explained 0% of the variance
#           idh(Type):PairIDYear 
#       + DVDRef
#       , data = MY_TABLE_perDVD_long
#       , family = 'poisson', nitt=20000, thin=10, burnin=10000
# )
# 
# summary(modAmcmcglmm)
}

}

{# model assumption checking
# # 
# # # residuals vs fitted: mean should constantly be zero: not quite ??
# # scatter.smooth(fitted(modA), resid(modA))
# # abline(h=0, lty=2)
# # 
# # 
# # # qqplots of residuals and ranefs: should be normally distributed
# # qqnorm(resid(modA))
# # qqline(resid(modA))
# # qqnorm(unlist(ranef(modA))) 
# # qqline(unlist(ranef(modA)))
# # 
# # # Mean of ranefs: should be zero
# # mean(unlist(ranef(modA)$BroodRef))
# # mean(unlist(ranef(modA)$SocialMumID))
# # mean(unlist(ranef(modA)$SocialDadID))
# # mean(unlist(ranef(modA)$PairID))
# # 
# # # residuals vs predictors
# # scatter.smooth(MY_TABLE_perDVD_long$ParentsAge, resid(modA))
# # abline(h=0, lty=2)
# # scatter.smooth(MY_TABLE_perDVD_long$HatchingDayAfter0401, resid(modA))
# # abline(h=0, lty=2)
# # plot(MY_TABLE_perDVD_long$PairBroodNb, resid(modA))
# # abline(h=0, lty=2)
# # plot(MY_TABLE_perDVD_long$DVDInfoChickNb, resid(modA))
# # abline(h=0, lty=2)	
# # plot(MY_TABLE_perDVD_long$ChickAgeCat, resid(modA))
# # abline(h=0, lty=2)	
# # scatter.smooth(MY_TABLE_perDVD_long$RelTimeHrs, resid(modA))
# # abline(h=0, lty=2)		
# # 
# # 
}

}

summary(modA)


{#### predictors of synchrony

modS <- glmer(S ~ 
	
	Type*scale(ParentsAge) + 
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*scale(DVDInfoChickNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
	
	Type*scale(TotalProRate) +
	Type*scale(VisitRateDifference)+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	(1|PairID) +  # explained 0% of the variance
	(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) 
	+ (1|rowID) # for overdispersion > doesnt help ... ??
	, data = MY_TABLE_perDVD_long
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modS)

dispersion_glmer(modS) # < 1.4

{# model assumption checking
# 
# # residuals vs fitted: mean should constantly be zero: not quite !
# scatter.smooth(fitted(modS), resid(modS))
# abline(h=0, lty=2)
# 
# # qqplots of residuals and ranefs: should be normally distributed
# qqnorm(resid(modS))
# qqline(resid(modS))
# qqnorm(unlist(ranef(modS))) 
# qqline(unlist(ranef(modS)))
# 
# # Mean of ranefs: should be zero
# mean(unlist(ranef(modS)$BroodRef))
# mean(unlist(ranef(modS)$SocialMumID))
# mean(unlist(ranef(modS)$SocialDadID))
# mean(unlist(ranef(modS)$PairID))
# 
# # residuals vs predictors
# scatter.smooth(MY_TABLE_perDVD_long$ParentsAge, resid(modS))
# abline(h=0, lty=2)
# scatter.smooth(MY_TABLE_perDVD_long$HatchingDayAfter0401, resid(modS))
# abline(h=0, lty=2)
# plot(MY_TABLE_perDVD_long$PairBroodNb, resid(modS))
# abline(h=0, lty=2)
# plot(MY_TABLE_perDVD_long$DVDInfoChickNb, resid(modS))
# abline(h=0, lty=2)	
# plot(MY_TABLE_perDVD_long$ChickAgeCat, resid(modS))
# abline(h=0, lty=2)	
# scatter.smooth(MY_TABLE_perDVD_long$RelTimeHrs, resid(modS))
# abline(h=0, lty=2)		
# 
# 
}
 	
	
	
}

summary(modS)



######################
# FITNESS CORRELATES
######################

### the deviation from randomness modeled above, with a poisson model is essentially:
### log (A) - log (Asim) = log (A/Asim)
{## since A can be 0, we add 0.5 to both numerator and denominator (Yamamura 1999)

MY_TABLE_perDVD$LogAdev <- log((MY_TABLE_perDVD$A+0.5)/(MY_TABLE_perDVD$MeanAsimWithin+0.5))
MY_TABLE_perDVD$LogSdev <- log((MY_TABLE_perDVD$S+0.5)/(MY_TABLE_perDVD$MeanSsimWithin+0.5))

MY_TABLE_perBrood <- merge(MY_TABLE_perBrood,
data.frame(summarise (group_by(MY_TABLE_perDVD, BroodRef),
MeanLogAdev = mean(LogAdev), 
MeanLogSdev = mean(LogSdev))), by='BroodRef')


head(MY_TABLE_perDVD)
cor.test(MY_TABLE_perDVD$Adev, MY_TABLE_perDVD$LogAdev)
plot (MY_TABLE_perDVD$Adev, MY_TABLE_perDVD$LogAdev)
cor.test(MY_TABLE_perDVD$Sdev, MY_TABLE_perDVD$LogSdev)
plot (MY_TABLE_perDVD$Sdev, MY_TABLE_perDVD$LogSdev)

head(MY_TABLE_perBrood)
cor.test(MY_TABLE_perBrood$MeanAdev, MY_TABLE_perBrood$MeanLogAdev)
plot (MY_TABLE_perBrood$MeanAdev, MY_TABLE_perBrood$MeanLogAdev)
cor.test(MY_TABLE_perBrood$MeanSdev, MY_TABLE_perBrood$MeanLogSdev)
plot (MY_TABLE_perBrood$MeanSdev, MY_TABLE_perBrood$MeanLogSdev)

head(MY_TABLE_perChick)
MY_TABLE_perChick <- merge(MY_TABLE_perChick,MY_TABLE_perBrood[,c("BroodRef","MeanLogAdev","MeanLogSdev")],by.x="RearingBrood", by.y="BroodRef")

}


{#### ChickSurvival ~ Alternation + Synchrony, brood

  modChickSurvival <- glmer(RingedYN ~ 
                              scale(MeanTotalProRate)+ scale(I(MeanTotalProRate^2))+
                              scale(NbRinged) +
                              scale(MeanLogAdev)+
                              scale(MeanLogSdev) +
                              scale(HatchingDayAfter0401) +
                              scale(PairBroodNb) +
                              MPriorResidence +
                              (1|PairID) + 
                              (1|BreedingYear) +
                              (1|BroodRef) +
                              (1|NatalBroodID)
                            , data = MY_TABLE_perChick_All
                            , family = 'binomial'
                            , control=glmerControl(optimizer = "bobyqa"))

  drop1(modChickSurvival, test="Chisq") # LRT
  
  dispersion_glmer(modChickSurvival) # 

}

summary(modChickSurvival) 


###########
# DIVORCE #
###########

{##### understand why Male and Female divorce are not matching for both partners

# all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
ExtractedData_folder <- "R_ExtractedData" 
MY_tblBroods <- read.csv(paste(ExtractedData_folder,"R_MY_tblBroods.csv", sep="/")) 

# just recorded broods : MY_TABLE_perBrood
nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$FwillDivorce) & !is.na(MY_TABLE_perBrood$MwillDivorce) 
                       & MY_TABLE_perBrood$FwillDivorce != MY_TABLE_perBrood$MwillDivorce
                       & !is.na(MY_TABLE_perBrood$BroodRef),]) # 68 where 'will divorce' doesn't match for both parents

nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$FwillDivorce) & !is.na(MY_TABLE_perBrood$MwillDivorce) 
                       & !is.na(MY_TABLE_perBrood$BroodRef),]) # 564 

# rules for individual divorce (MwillDivorce and FwillDivorce) (from data extraction script):
## TRUE = we know for sure the other partner is alive and the individual is breeding again but with another partner
## FALSE = the individual breed again with the same partner next (even if their partner has had other brood with someone else in between)
## NA: everything else, partner unidentified, no next brood.

# calculation of pair divorce
MY_TABLE_perBrood$PairDivorce <- NA
for (i in 1: nrow(MY_TABLE_perBrood)){
  # if both the same, no problem
  if (!is.na(MY_TABLE_perBrood$MwillDivorce[i]) & !is.na(MY_TABLE_perBrood$FwillDivorce[i]) 
      & MY_TABLE_perBrood$MwillDivorce[i] == MY_TABLE_perBrood$FwillDivorce[i])
  {MY_TABLE_perBrood$PairDivorce[i] <- MY_TABLE_perBrood$MwillDivorce[i]}
  # if one is TRUE, the other NA, the other is NA because he/she doesnt breed again or breed with someone unidentified; either way, divorce = TRUE
  if ((is.na(MY_TABLE_perBrood$MwillDivorce[i]) & !is.na(MY_TABLE_perBrood$FwillDivorce[i]) & MY_TABLE_perBrood$FwillDivorce[i] == TRUE) 
      | (!is.na(MY_TABLE_perBrood$MwillDivorce[i]) & MY_TABLE_perBrood$MwillDivorce[i] == TRUE & is.na(MY_TABLE_perBrood$FwillDivorce[i])))
  {MY_TABLE_perBrood$PairDivorce[i] <- TRUE}
  # if one is FALSE, the other NA, it measn the first one we know breed again with that smae partner again, but the other is NA because he/she breed with someone unidentified in between, depending on who that unidendify bird would be, divorce could be true or false, so let it be NA 
  if ((is.na(MY_TABLE_perBrood$MwillDivorce[i]) & !is.na(MY_TABLE_perBrood$FwillDivorce[i]) & MY_TABLE_perBrood$FwillDivorce[i] == FALSE) 
      | (!is.na(MY_TABLE_perBrood$MwillDivorce[i]) & MY_TABLE_perBrood$MwillDivorce[i] == FALSE & is.na(MY_TABLE_perBrood$FwillDivorce[i])))
  {MY_TABLE_perBrood$PairDivorce[i] <- NA}
  # if one is FALSE, the other TRUE: one is being faithful, the other plogamous, need to decide 'by hand' whether brood overlap (true polygamie), or not (divorce and getting back with their ex)
  if (!is.na(MY_TABLE_perBrood$MwillDivorce[i]) & !is.na(MY_TABLE_perBrood$FwillDivorce[i]) 
      & MY_TABLE_perBrood$MwillDivorce[i] != MY_TABLE_perBrood$FwillDivorce[i])
  {MY_TABLE_perBrood$PairDivorce[i] <- NA}
  
  }

# example of one is NA the other TRUE
nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$FwillDivorce) & is.na(MY_TABLE_perBrood$MwillDivorce) 
                       & !is.na(MY_TABLE_perBrood$BroodRef),]) # 32
MY_tblBroods[!is.na(MY_tblBroods$SocialMumID) & MY_tblBroods$SocialMumID == 1718,c('BroodRef', 'SocialMumID', 'SocialDadID', 'LayDate','NbHatched', 'NbRinged', 'FBroodNb','MBroodNb',  'FwillDivorce', 'MwillDivorce')]
MY_tblBroods[!is.na(MY_tblBroods$SocialDadID) & MY_tblBroods$SocialDadID == 1738,c('BroodRef', 'SocialMumID', 'SocialDadID', 'LayDate','NbHatched', 'NbRinged', 'FBroodNb','MBroodNb', 'FwillDivorce', 'MwillDivorce')]

# example of one is NA the other TRUE
nrow(MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$FwillDivorce) & !is.na(MY_TABLE_perBrood$MwillDivorce) 
                       & !is.na(MY_TABLE_perBrood$BroodRef),]) # 37
MY_tblBroods[!is.na(MY_tblBroods$SocialMumID) & MY_tblBroods$SocialMumID == 960,c('BroodRef', 'SocialMumID', 'SocialDadID', 'LayDate','NbHatched', 'NbRinged', 'FBroodNb','MBroodNb', 'FwillDivorce', 'MwillDivorce')]
MY_tblBroods[!is.na(MY_tblBroods$SocialDadID) & MY_tblBroods$SocialDadID == 650,c('BroodRef', 'SocialMumID', 'SocialDadID', 'LayDate','NbHatched', 'NbRinged', 'FBroodNb','MBroodNb', 'FwillDivorce', 'MwillDivorce')]

# cases to decide on how to replace NA by TRUE or FALSE
Polyandrous <- MY_TABLE_perBrood$SocialMumID[!is.na(MY_TABLE_perBrood$FwillDivorce) & !is.na(MY_TABLE_perBrood$MwillDivorce)
                 & MY_TABLE_perBrood$FwillDivorce == TRUE  &  MY_TABLE_perBrood$MwillDivorce == FALSE
                 & !is.na(MY_TABLE_perBrood$BroodRef)] #  1 polyandrous female?

Polygynous <- unique(MY_TABLE_perBrood$SocialDadID[!is.na(MY_TABLE_perBrood$FwillDivorce) & !is.na(MY_TABLE_perBrood$MwillDivorce) 
                   & MY_TABLE_perBrood$FwillDivorce == FALSE  &  MY_TABLE_perBrood$MwillDivorce == TRUE
                  & !is.na(MY_TABLE_perBrood$BroodRef)]) # 37 polygynous males?


  
BroodPolyandrous <- MY_tblBroods[!is.na(MY_tblBroods$SocialMumID) & MY_tblBroods$SocialMumID == 985,c('BroodRef', 'SocialMumID', 'SocialDadID', 'LayDate','NbHatched', 'NbRinged', 'FBroodNb', 'FwillDivorce', 'MwillDivorce')]
BroodPolyandrous <- BroodPolyandrous[order(BroodPolyandrous$FBroodNb),] # this is a true divorce (between season), even if then come back with ex.
BroodPolyandrous$RecordedYN <- BroodPolyandrous$BroodRef %in% MY_TABLE_perBrood$BroodRef

MY_TABLE_perBrood$PairDivorce[MY_TABLE_perBrood$BroodRef == 636] <- TRUE

BroodPolygynous <- MY_tblBroods[!is.na(MY_tblBroods$SocialDadID) & MY_tblBroods$SocialDadID %in% Polygynous, c('BroodRef', 'SocialMumID', 'SocialDadID', 'LayDate','NbHatched', 'NbRinged', 'MBroodNb', 'FwillDivorce', 'MwillDivorce')]
BroodPolygynous <- BroodPolygynous[order(BroodPolygynous$MBroodNb),] 
BroodPolygynous$RecordedYN <- BroodPolygynous$BroodRef %in% MY_TABLE_perBrood$BroodRef
split(BroodPolygynous, BroodPolygynous$SocialDadID)
  
  # 7900: in 2015 male has two females simultaneously 
  # 7814: in 2015 male has two females simultaneously 
  # 7267: in 2014 male has two females simultaneously 
  # 6816: in 2014 male has two females simultaneously 
  # 6270: in 2014 male has two females simultaneously 
  # 5675: in 2015 male has two females simultaneously 
  # 5489: in 2013 male has two females simultaneously 
  # 5466: in 2014 male has two females simultaneously 
  # 5461: in 2013 male has two females simultaneously 
  # 5202: end of season 2013, last brood with another female, then get back with initial female in 2014
  
  
  
}

{# male and female divorce in one model

mod_Divorce <- glmer(PairDivorce~scale(MeanLogSdev) + 
                             scale(MeanLogAdev)	+
                             scale(MumAge) + Scale(DadAge)+
                             scale(PairBroodNb) +
                             scale(MeanMVisit1RateH) +  scale(MeanFVisit1RateH) +
                             scale(NbRinged) +
                             (1|SocialMumID) + (1|SocialDadID)
                           + (1|BreedingYear) 
                           , data = MY_TABLE_perBrood
                           , family="binomial"
                           , control=glmerControl(optimizer = "bobyqa"))
}

summary(mod_Divorce) 
dispersion_glmer(mod_Divorce) # 
