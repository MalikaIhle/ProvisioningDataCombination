#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 20190924
#	 commit: add chick age
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
#library(MCMCglmm)
#library(RODBC) # to call DB
library(here)
library(boot) # for back trasnforming data for figures
require(gridExtra) # for multipanel figure
require(grid)
  
options(scipen=999) # remove scientific notation e-
#options(scipen=0)

}


{### Get raw data from R_Selected&RandomizedData folder

# source('Alternation_DataSelection_DataSimulation.R')
# or :

SelectedData_folder <- "R_Selected&RandomizedData"

MY_TABLE_perDVD <- read.csv(paste(here(), SelectedData_folder,"R_MY_TABLE_perDVD.csv", sep="/")) # summary stats for all analyzed videos where both parents known and with expected alternation from simulation
  # asked by reviewers
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

MY_TABLE_perBrood <- read.csv(paste(here(),SelectedData_folder,"R_MY_TABLE_perBrood.csv", sep="/")) # only recorded brood (summarizing MY_TABLE_perDVD per brood)

  # asked by reviewer
  # DVDoutlierInNestDur <- read.table("R_input/R_DVDoutlierInNestDur.txt", header=TRUE)
  # MY_TABLE_perDVD1000 <- read.csv(paste(here(), SelectedData_folder,"R_MY_TABLE_perDVD1000.csv", sep="/"))
  # cor.test(MY_TABLE_perDVD1000$MedAsimWithin,MY_TABLE_perDVD$A)
  # cor.test(MY_TABLE_perDVD1000$MedSsimWithin,MY_TABLE_perDVD$S)


MY_TABLE_perChick_All <- read.csv(paste(here(),SelectedData_folder,"R_MY_TABLE_perChick_All.csv", sep="/")) 

}

head(MY_TABLE_perDVD)
head(MY_TABLE_perBrood)
head(MY_TABLE_perChick_All)


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

S=c(MY_TABLE_perDVD$Ssorted,
    MY_TABLE_perDVD$S,
    MY_TABLE_perDVD$Sswitch,
    MY_TABLE_perDVD$MedSsimWithin,
    MY_TABLE_perDVD$MedSsimAmong,
    MY_TABLE_perDVD$Sgenerated),

Type = c(rep("a_sorted", nrow(MY_TABLE_perDVD)),
         rep("a_Obsv", nrow(MY_TABLE_perDVD)),
         rep("b_switch", nrow(MY_TABLE_perDVD)),
         rep("c_within", nrow(MY_TABLE_perDVD)),
         rep("d_among", nrow(MY_TABLE_perDVD)),
         rep("e_generated", nrow(MY_TABLE_perDVD))),

SMax = c(MY_TABLE_perDVD$Asorted,
         MY_TABLE_perDVD$A,
         MY_TABLE_perDVD$Aswitch,
         MY_TABLE_perDVD$MedAsimWithin,
         MY_TABLE_perDVD$MedAsimAmong,
         MY_TABLE_perDVD$Agenerated), # this is an approximation, Smax should be the actual number of A in that specific observation.

DVDRef = rep(MY_TABLE_perDVD$DVDRef,6),

LineID = 1: nrow(MY_TABLE_perDVD)*6
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
# PREDICTORS #
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



#################
# CHICK SURIVAL #
#################

{#### ChickSurvival ~ Alternation + Synchrony
  
  {## at the brood level, hatching to ringing: can't have cross fostered YN (include that biased effect since Xfost happens after death until age2/3)
  
  # modChickSurvival <- glmer(cbind(NbRinged, NbHatched-NbRinged) ~ 
  #                             scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
  #                             scale(NbHatched) +
  #                             scale(MeanLogAdev)+
  #                             scale(MeanLogSdev) +
  #                             scale(HatchingDayAfter0401) +
  #                             scale(PairBroodNb) +
  #                             XPriorResidence +
  #                             MixedBroodYN +
  #                            # (1|PairID) + 
  #                             (1|BreedingYear) 
  #                            #  +(1|BroodRef) 
  #                           , data = MY_TABLE_perBrood
  #                           , family = 'binomial'
  #                           , control=glmerControl(optimizer = "bobyqa"))
  # 
  # summary(modChickSurvival)  
  # drop1(modChickSurvival, test="Chisq") # LRT
  # dispersion_glmer(modChickSurvival) # 0.9524648
  }
  

  {# per chick age category:

  ## 5 to ringed with Coordination at day 6
#   modChickSurvival5toRinged <- glmer(RingedYN ~
#                               scale(MeanTotalProRate)+ 
#                               I(scale(MeanTotalProRate)^2)+
#                               scale(NbChickd5) +
#                               scale(MeanLogAdevAgeCat6)+
#                               scale(MeanLogSdevAgeCat6) +
#                               scale(HatchingDayAfter0401) +
#                               scale(PairBroodNb) +
#                               XPriorResidence +
#                               CrossFosteredYN +
#                               (1|PairID) +
#                               (1|BreedingYear) +
#                               (1|BroodRef) +
#                               (1|NatalBroodID)
#                               , data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]
#                               , family = 'binomial'
#                               , control=glmerControl(optimizer = "bobyqa")
#                               )
# 
#   summary(modChickSurvival5toRinged)
#   drop1(modChickSurvival5toRinged, test="Chisq") # LRT
# 
# nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6),])
# table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6)])
# 
# 
# table(MY_TABLE_perChick_All$WeightedAge5)
# table(MY_TABLE_perChick_All$WeightedAge12)
# table(MY_TABLE_perChick_All$RingedYN)
# table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE])
# nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge12 == FALSE & MY_TABLE_perChick_All$RingedYN == TRUE,])#64
# nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge12 == TRUE & MY_TABLE_perChick_All$WeightedAge5 == FALSE,])#146
# nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$RingedYN == TRUE & MY_TABLE_perChick_All$WeightedAge5 == FALSE,])#151
# nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,])#2373
# 
# length(unique(MY_TABLE_perChick_All$BroodRef[MY_TABLE_perChick_All$WeightedAge5 == TRUE &
#                                                is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6) &
#                                                !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat10)])) # 43
# 
# summary(MY_TABLE_perChick_All$MeanLogAdevAgeCat6[MY_TABLE_perChick_All$WeightedAge5 == TRUE])
#   hist(MY_TABLE_perChick_All$MeanLogAdevAgeCat6[MY_TABLE_perChick_All$WeightedAge5 == TRUE])
#   summary(MY_TABLE_perChick_All$MeanLogSdevAgeCat6[MY_TABLE_perChick_All$WeightedAge5 == TRUE])
#   hist(MY_TABLE_perChick_All$MeanLogSdevAgeCat6[MY_TABLE_perChick_All$WeightedAge5 == TRUE])
#   length(MY_TABLE_perChick_All$MeanLogAdevAgeCat6[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6)])
#   
# cor.test(MY_TABLE_perChick_All$MeanLogAdevAgeCat6, MY_TABLE_perChick_All$MeanLogSdevAgeCat6)
#   
# table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6)])
# table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdev)])

  }


## survival from age 5 to ringedYN with average Coordination: to exclude cross fostering bias
{
modChickSurvival5toRingedAverageCoordination <- glmer(RingedYN ~ scale(DurationSurvivalCheck) +
                                     scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
                                     scale(NbChickd5) + 
                                     scale(MeanLogAdev)+
                                     scale(MeanLogSdev) +
                                     scale(HatchingDayAfter0401) +
                                     scale(PairBroodNb) +
                                     XPriorResidence +
                                     CrossFosteredYN + # not significant
                                     (1|PairID) + 
                                     (1|BreedingYear) +
                                     (1|BroodRef) +
                                     (1|NatalBroodID)
                                   , data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]
                                   , family = 'binomial'
                                   , control=glmerControl(optimizer = "bobyqa"))


#summary(modChickSurvival5toRingedAverageCoordination)
#drop1(modChickSurvival5toRingedAverageCoordination, test="Chisq")

table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE])

# modChickSurvivalHatchedtoRingedAverageCoordination <- glmer(RingedYN ~ 
#                                                         scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
#                                                         scale(NbHatched) + 
#                                                         scale(MeanLogAdev)+
#                                                         scale(MeanLogSdev) +
#                                                         scale(HatchingDayAfter0401) +
#                                                         scale(PairBroodNb) +
#                                                         XPriorResidence +
#                                                         CrossFosteredYN + # extremely significant: only those that survived were crossfostered
#                                                         (1|PairID)  
#                                                         #  (1|BreedingYear) +
#                                                         # (1|BroodRef) +
#                                                        # (1|NatalBroodID)
#                                                       , data = MY_TABLE_perChick_All
#                                                       , family = 'binomial'
#                                                       , control=glmerControl(optimizer = "bobyqa"))
# 
# 
# summary(modChickSurvivalHatchedtoRingedAverageCoordination)


effects_ChickSurvival <- as.data.frame(cbind(est=invlogit(summary(modChickSurvival5toRingedAverageCoordination)$coeff[,1]),
                                             CIhigh=invlogit(summary(modChickSurvival5toRingedAverageCoordination)$coeff[,1]+summary(modChickSurvival5toRingedAverageCoordination)$coeff[,2]*1.96),
                                             CIlow=invlogit(summary(modChickSurvival5toRingedAverageCoordination)$coeff[,1]-summary(modChickSurvival5toRingedAverageCoordination)$coeff[,2]*1.96),
                                             SEhigh = invlogit(summary(modChickSurvival5toRingedAverageCoordination)$coeff[,1] + summary(modChickSurvival5toRingedAverageCoordination)$coeff[,2]),
                                             SElow = invlogit(summary(modChickSurvival5toRingedAverageCoordination)$coeff[,1] - summary(modChickSurvival5toRingedAverageCoordination)$coeff[,2])
))
effects_ChickSurvival$avSE <- (effects_ChickSurvival$SEhigh-effects_ChickSurvival$SElow)/2
effects_ChickSurvival <- effects_ChickSurvival*100
effects_ChickSurvival



#odds <- exp(cbind(OR=fixef(modChickSurvival5toRingedAverageCoordination), confint(modChickSurvival5toRingedAverageCoordination, parm="beta_")))[c(5,6),] 
#                         OR     2.5 %    97.5 %
# scale(MeanLogAdev) 0.8552787 0.7402355 0.9853105
# scale(MeanLogSdev) 1.0398193 0.9117151 1.1870856
# ran 20190731

#odds <- exp(cbind(OR=fixef(modChickSurvival5toRingedAverageCoordination), confint(modChickSurvival5toRingedAverageCoordination, parm="beta_")))[c(6,7),] 
#              OR     2.5 %    97.5 %
#scale(MeanLogAdev) 0.8571212 0.7420960 0.9874772
#scale(MeanLogSdev) 1.0218202 0.8958657 1.1658896
# ran 20190924

}

summary(modChickSurvival5toRingedAverageCoordination)


# check residuals
{
  mod <- modChickSurvival5toRingedAverageCoordination
  dat <- MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]
    
qqnorm(resid(mod)) # awful
qqline(resid(mod))

qqnorm(unlist(ranef(mod)$PairID))
qqline(unlist(ranef(mod)$PairID))


# residuals vs fitted values
plot(fitted(mod), resid(mod)) # awful
abline(h=0)

}


## figures
{
{### PR effect 
  
# plot(RingedYN ~ MeanTotalProRate,
#      data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,],
#      xlab="Average total provisioning rate per hour",
#      ylab="Survival likelihood",
#      pch=19)
# 
# curve(predict(glm(RingedYN ~
#                     poly(MeanTotalProRate,2),
#                   data=MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,],
#                   family = binomial(link="logit")),
#               data.frame(MeanTotalProRate=x),type="response"),
#       lty=1, lwd=2, col="blue",
#       add=TRUE)



### get PR range on transformed data
PR <- MY_TABLE_perChick_All$MeanTotalProRate[MY_TABLE_perChick_All$WeightedAge5 == TRUE]
transformed_PR_range <- (range(PR) - mean(PR))/sd(PR)

### create data along that range
x <- seq(transformed_PR_range[1],transformed_PR_range[2],length.out=1000)

### make y from model estimates (taken from table 1)
# summary(modChickSurvival5toRingedAverageCoordination)
y <- summary(modChickSurvival5toRingedAverageCoordination)$coeff[1,1] + summary(modChickSurvival5toRingedAverageCoordination)$coeff[3,1]*x + summary(modChickSurvival5toRingedAverageCoordination)$coeff[4,1]*x^2

### back transform x
x2 <- x*sd(PR) + mean(PR)

### back transform y (assuming that logit link function was used)
library(boot)
y2 <- inv.logit(y)

### plot line
plot(jitter(RingedYN, factor=0.1) ~ MeanTotalProRate,
     data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,],
     xlab="Mean total provisioning rate per hour",
     ylab="Offspring survival likelihood",
     pch=21,  col=alpha('black', 0.4))
lines(y2~x2, lty=1, lwd=2, col="blue")
}

{### Adev
  AdevSurv <- MY_TABLE_perChick_All$MeanLogAdev[MY_TABLE_perChick_All$WeightedAge5 == TRUE]
  transformed_Adev_range <- (range(AdevSurv) - mean(AdevSurv))/sd(AdevSurv)
  
  ### create data along that range
  x <- seq(transformed_Adev_range[1],transformed_Adev_range[2],length.out=1000)
  
  ### make y from model estimates (taken from table 1)
  # summary(modChickSurvival5toRingedAverageCoordination)
  y <- summary(modChickSurvival5toRingedAverageCoordination)$coeff[1,1] + summary(modChickSurvival5toRingedAverageCoordination)$coeff[5,1]*x 
  
  ### back transform x
  x2 <- x*sd(AdevSurv) + mean(AdevSurv)
  
  ### back transform y (assuming that logit link function was used)
  library(boot)
  y2 <- inv.logit(y)
  
  SurvAData <- data.frame(y2,x2)
  
  ### plot line
  par(mfrow=c(1,2))
 plot(jitter(RingedYN, factor=0.1) ~ MeanLogAdev,
       data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,],
       xlab="Alternation",
       ylab="Offspring survival likelihood",
       pch=21,  col=alpha('black', 0.4))
  lines(y2~x2, lty=1, lwd=2, col="blue")
}

{### S dev
SdevSurv <- MY_TABLE_perChick_All$MeanLogSdev[MY_TABLE_perChick_All$WeightedAge5 == TRUE]
transformed_Sdev_range <- (range(SdevSurv) - mean(SdevSurv))/sd(SdevSurv)

### create data along that range
x <- seq(transformed_Sdev_range[1],transformed_Sdev_range[2],length.out=1000)

### make y from model estimates (taken from table 1)
# summary(modChickSurvival5toRingedAverageCoordination)
y <- summary(modChickSurvival5toRingedAverageCoordination)$coeff[1,1] + summary(modChickSurvival5toRingedAverageCoordination)$coeff[6,1]*x 

### back transform x
x2 <- x*sd(SdevSurv) + mean(SdevSurv)

### back transform y (assuming that logit link function was used)
library(boot)
y2 <- inv.logit(y)

SurvSData <- data.frame(y2,x2)

### plot line
plot(jitter(RingedYN, factor=0.1) ~ MeanLogSdev,
     data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,],
     xlab="Synchrony",
     ylab="Offspring survival likelihood",
     pch=21,  col=alpha('black', 0.4))
lines(y2~x2, lty=2, lwd=2, col="blue")
  

}

}

}

##############
# CHICK MASS #
##############

{# see separate code modChickMass_andVariance_stan.R for the DHGLM

dd <- read.csv(paste(here(),SelectedData_folder,"R_MY_TABLE_perChick.csv", sep="/")) 
par(mfrow=c(1,2))
plot(AvgOfMass~MeanLogAdev,dd, pch=19, cex=0.5, col=alpha(1,0.5))
clip(min(dd$MeanLogAdev),max(dd$MeanLogAdev), 0, 30)
abline(22.298+mean(dd$MeanLogAdev),-0.048*sd(dd$MeanLogAdev), lwd=2, lty=2, col="blue")

plot(AvgOfMass~MeanLogSdev,dd, pch=19, cex=0.5, col=alpha(1,0.5))
clip(min(dd$MeanLogSdev),max(dd$MeanLogSdev), 0, 30)
abline(22.298+mean(dd$MeanLogSdev),-0.042*sd(dd$MeanLogSdev), lwd=2, lty=2, col="blue")
}


###########
# DIVORCE #
###########

{# male and female divorce in one model - exclude polygynous males
summary(MY_TABLE_perBrood) # polygynous males are part of the cases where PairdDivorce = NA
  
mod_Divorce <- glmer(PairDivorce~scale(MeanLogSdev) + 
                             scale(MeanLogAdev)	+
                             scale(MumAge) + scale(DadAge)+
                             scale(PairBroodNb) +
                             #scale(MeanMVisit1RateH) +  scale(MeanFVisit1RateH) +
                             scale(I(MeanMVisit1RateH+MeanFVisit1RateH))+
                              scale(I(abs(MeanMVisit1RateH-MeanFVisit1RateH)))+
                              scale(NbRinged) +
                             MixedBroodYN +
                             (1|SocialMumID)  
                         #  + (1|SocialDadID)
                          # + (1|BreedingYear) 
                           , data = MY_TABLE_perBrood
                           , family="binomial"
                         , control=glmerControl(optimizer = "bobyqa"))

  #summary(mod_Divorce) 
  #drop1(mod_Divorce, test = "Chisq")
  table(MY_TABLE_perBrood$PairDivorce)

  #oddsDivorce <- exp(cbind(OR=fixef(mod_Divorce), confint(mod_Divorce, parm="beta_")))[c(2,3),] 
  
  #                            OR     2.5 %   97.5 %
  # scale(MeanLogSdev) 0.9482971 0.7033124 1.276619
  # scale(MeanLogAdev) 1.0831807 0.8220911 1.437247
  # ran on 20190729
  
}

summary(mod_Divorce) 


{## figures

nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$PairDivorce),])

{### Adev
  AdevDiv <- MY_TABLE_perBrood$MeanLogAdev[!is.na(MY_TABLE_perBrood$PairDivorce)]
  transformed_AdevDiv_range <- (range(AdevDiv) - mean(AdevDiv))/sd(AdevDiv)
  
  ### create data along that range
  x <- seq(transformed_AdevDiv_range[1],transformed_AdevDiv_range[2],length.out=1000)
  
  ### make y from model estimates (taken from table 1)
  # summary(mod_Divorce)
  y <- summary(mod_Divorce)$coeff[1,1] + summary(mod_Divorce)$coeff[3,1]*x 
  
  ### back transform x
  x2 <- x*sd(AdevDiv) + mean(AdevDiv)
  
  ### back transform y (assuming that logit link function was used)
  library(boot)
  y2 <- inv.logit(y)
  
  DivAData <- data.frame(y2,x2)
  
  ### plot line
  par(mfrow=c(1,2))
   plot(jitter(as.numeric(PairDivorce), factor=0.1) ~ MeanLogAdev,
       data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$PairDivorce),],
       xlab="Log transformed deviation in alternation",
       ylab="Divorce likelihood",
       pch=21,  col=alpha('black', 0.4))
  lines(y2~x2, lty=1, lwd=2, col="blue")
}

{### S dev
  SdevDiv <- MY_TABLE_perBrood$MeanLogSdev[!is.na(MY_TABLE_perBrood$PairDivorce)]
  transformed_SdevDiv_range <- (range(SdevDiv) - mean(SdevDiv))/sd(SdevDiv)
  
  ### create data along that range
  x <- seq(transformed_SdevDiv_range[1],transformed_SdevDiv_range[2],length.out=1000)
  
  ### make y from model estimates (taken from table 1)
  # summary(mod_Divorce)
  y <- summary(mod_Divorce)$coeff[1,1] + summary(mod_Divorce)$coeff[3,1]*x 
  
  ### back transform x
  x2 <- x*sd(SdevDiv) + mean(SdevDiv)
  
  ### back transform y (assuming that logit link function was used)
  library(boot)
  y2 <- inv.logit(y)
  
  DivSData <- data.frame(y2,x2)
  
  ### plot line
  plot(jitter(as.numeric(PairDivorce), factor=0.1) ~ MeanLogSdev,
       data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$PairDivorce),],
       xlab="Log transformed deviation in synchrony",
       ylab="Divorce likelihood",
       pch=21,  col=alpha('black', 0.4))
  lines(y2~x2, lty=1, lwd=2, col="blue")
  
}

}



######################################
# composite figure for fitness paper #
######################################

{## this Warning message will appear without consequences for the plot
## In eval(family$initialize) : non-integer #successes in a binomial glm!

head(SurvAData)
head(SurvSData)
dd <- read.csv(paste(here(),SelectedData_folder,"R_MY_TABLE_perChick.csv", sep="/"))
head(dd)
head(DivAData)
head(DivSData)

summary(MY_TABLE_perChick_All$MeanLogAdev[MY_TABLE_perChick_All$WeightedAge5 == TRUE],dd$MeanLogAdev, MY_TABLE_perBrood$MeanLogAdev[!is.na(MY_TABLE_perBrood$PairDivorce)])
summary(MY_TABLE_perChick_All$MeanLogSdev[MY_TABLE_perChick_All$WeightedAge5 == TRUE],dd$MeanLogSdev, MY_TABLE_perBrood$MeanLogSdev[!is.na(MY_TABLE_perBrood$PairDivorce)])



SurvA <- 
  ggplot(aes(y = jitter(RingedYN, factor=0.1), x = MeanLogAdev)
                , data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]) +
  geom_point(pch=19,cex=2,col=alpha('black',0.25))+
  scale_x_continuous(limits = c(-0.73, 0.6))+
  geom_smooth(data = SurvAData, aes(x = x2, y = y2),
              method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, col = 'royalblue1', linetype = 1,size =1 ) +
  ylab("Offspring survival 
probability")+
  theme_classic()+
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_text(face="bold"),
    axis.text.y=element_text(size=7.5)
    )

SurvS <- 
  ggplot(aes(y = jitter(RingedYN, factor=0.1), x = MeanLogSdev)
         , data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]) +
  geom_point(pch=19,cex=2,col=alpha('black',0.25))+
  scale_x_continuous(limits = c(-1.3,1))+
  geom_smooth(data = SurvSData, aes(x = x2, y = y2),
              method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, col = 'royalblue1', linetype = 2,size =1 ) +
  theme_classic()+
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    axis.text.y=element_blank()
  )


MassA <- 
  ggplot(aes(y = AvgOfMass, x = MeanLogAdev), data = dd) +
  geom_point(pch=19,cex=2,col=alpha('black',0.25))+
  scale_x_continuous(limits = c(-0.73, 0.6))+
  geom_segment(aes(x = min(dd$MeanLogAdev), xend = max(dd$MeanLogAdev), 
                   y = 22.298+mean(dd$MeanLogAdev) -0.048*sd(dd$MeanLogAdev)*min(dd$MeanLogAdev),
                   yend = 22.298+mean(dd$MeanLogAdev) -0.048*sd(dd$MeanLogAdev)*max(dd$MeanLogAdev))
               , col= 'royalblue1',linetype = 2,size =1 )+
  ylab("Offspring mass 
(g)")+
  theme_classic()+
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_text(face="bold"),
    axis.text.y=element_text(size=7.5)
    )

MassS <- 
  ggplot(aes(y = AvgOfMass, x = MeanLogSdev), data = dd) +
  geom_point(pch=19,cex=2,col=alpha('black',0.25))+
  scale_x_continuous(limits = c(-1.3, 1))+
  geom_segment(aes(x = min(dd$MeanLogSdev), xend = max(dd$MeanLogSdev), 
                   y = 22.298+mean(dd$MeanLogSdev) -0.042*sd(dd$MeanLogSdev)*min(dd$MeanLogSdev),
                   yend = 22.298+mean(dd$MeanLogSdev) -0.042*sd(dd$MeanLogSdev)*max(dd$MeanLogSdev))
               , col= 'royalblue1',linetype = 2,size =1 )+
    theme_classic()+
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    axis.text.y=element_blank()
  )


DivA <- 
  ggplot(aes(y = jitter(as.numeric(PairDivorce), factor=0.1), x = MeanLogAdev)
         , data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$PairDivorce),]) +
  geom_point(pch=19,cex=2,col=alpha('black',0.25))+
  scale_x_continuous(limits = c(-0.73, 0.6), "Alternation")+
  geom_smooth(data = DivAData, aes(x = x2, y = y2),
              method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, col = 'royalblue1', linetype = 2,size =1 ) +
  ylab("Divorce 
probability")+
  theme_classic()+
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    axis.text.y=element_text(size=7.5)
  )

DivS <- 
  ggplot(aes(y = jitter(as.numeric(PairDivorce), factor=0.1), x = MeanLogSdev)
         , data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$PairDivorce),]) +
  geom_point(pch=19,cex=2,col=alpha('black',0.25))+
  scale_x_continuous(limits = c(-1.3, 1), "Synchrony")+
  geom_smooth(data = DivSData, aes(x = x2, y = y2),
              method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, col = 'royalblue1', linetype = 2,size =1 ) +
  theme_classic()+
  theme(
    panel.border = element_rect(colour = "black", fill=NA),
    axis.title.y = element_blank(),
    axis.text.y=element_blank(),
    axis.title.x = element_text(face="bold")
  )


gSurvA <- ggplotGrob(SurvA)
gSurvS <- ggplotGrob(SurvS)
gMassA <- ggplotGrob(MassA)
gMassS <- ggplotGrob(MassS)
gDivA <- ggplotGrob(DivA)
gDivS <- ggplotGrob(DivS)

firstcol = rbind(gSurvA,gMassA,gDivA, size = "last")
secondcol = rbind(gSurvS,gMassS,gDivS, size = "last")

g1 <- grid.arrange(firstcol)
g2 <- grid.arrange(secondcol)

setEPS() 
jpeg("Fig2.jpeg", height = 130, width = 85, units = 'mm', res=150)
grid.arrange(g1,g2,nrow = 1, ncol= 2, widths = c(1.4,1))
dev.off()
}




