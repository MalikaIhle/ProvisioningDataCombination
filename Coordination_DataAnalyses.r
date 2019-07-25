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
#library(MCMCglmm)
#library(RODBC) # to call DB
library(here)
  
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


{#### ChickSurvival ~ Alternation + Synchrony, brood
  
  
  modChickSurvival <- glmer(cbind(NbRinged, NbHatched-NbRinged) ~ 
                              scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
                              scale(NbHatched) +
                              scale(MeanLogAdev)+
                              scale(MeanLogSdev) +
                              scale(HatchingDayAfter0401) +
                              scale(PairBroodNb) +
                              XPriorResidence +
                              MixedBroodYN +
                             # (1|PairID) + 
                              (1|BreedingYear) 
                             #  +(1|BroodRef) 
                            , data = MY_TABLE_perBrood
                            , family = 'binomial'
                            , control=glmerControl(optimizer = "bobyqa"))
  
  summary(modChickSurvival)  
  drop1(modChickSurvival, test="Chisq") # LRT
  dispersion_glmer(modChickSurvival) # 0.9524648
                              

 
  
  effects_ChickSurvival <- as.data.frame(cbind(est=invlogit(summary(modChickSurvival)$coeff[,1]),
                                       CIhigh=invlogit(summary(modChickSurvival)$coeff[,1]+summary(modChickSurvival)$coeff[,2]*1.96),
                                       CIlow=invlogit(summary(modChickSurvival)$coeff[,1]-summary(modChickSurvival)$coeff[,2]*1.96),
                                      SEhigh = invlogit(summary(modChickSurvival)$coeff[,1] + summary(modChickSurvival)$coeff[,2]),
                                      SElow = invlogit(summary(modChickSurvival)$coeff[,1] - summary(modChickSurvival)$coeff[,2])
                                      ))
  effects_ChickSurvival$avSE <- (effects_ChickSurvival$SEhigh-effects_ChickSurvival$SElow)/2
  effects_ChickSurvival <- effects_ChickSurvival*100
  effects_ChickSurvival


  
  #odds <- exp(cbind(OR=fixef(modChickSurvival), confint(modChickSurvival, parm="beta_")))[c(5,6),] 
    #  OR     2.5 %    97.5 %
    # scale(MeanLogAdev) 0.887212 0.8078357 0.9739039
    # scale(MeanLogSdev) 1.045825 0.9515988 1.1499816
  

  
 

  # plot(RingedYN ~ MeanTotalProRate,
  #      data = MY_TABLE_perChick_All,
  #      xlab="Average total provisioning rate per hour",
  #      ylab="Survival likelihood",
  #      pch=19)
  # 
  # curve(predict(glm(RingedYN ~
  #                     poly(MeanTotalProRate,2),
  #                   data=MY_TABLE_perChick_All,
  #                   family = binomial(link="logit")),
  #               data.frame(MeanTotalProRate=x),type="response"),
  #       lty=1, lwd=2, col="blue",
  #       add=TRUE)


  
  ### get PR range on transformed data
  PR <- MY_TABLE_perChick_All$MeanTotalProRate
  transformed_PR_range <- (range(PR) - mean(PR))/sd(PR)
  
  ### create data along that range
  x <- seq(transformed_PR_range[1],transformed_PR_range[2],length.out=1000)
  
  ### make y from model estimates (taken from table 1)
 # summary(modChickSurvival)
  y <- summary(modChickSurvival)$coeff[1,1] + summary(modChickSurvival)$coeff[2,1]*x + summary(modChickSurvival)$coeff[3,1]*x^2
  
  ### back transform x
  x2 <- x*sd(PR) + mean(PR)
  
  ### back transform y (assuming that logit link function was used)
  library(boot)
  y2 <- inv.logit(y)
  
  ### plot line
  plot(jitter(RingedYN, factor=0.1) ~ MeanTotalProRate,
       data = MY_TABLE_perChick_All,
       xlab="Mean total provisioning rate per hour",
       ylab="Offspring survival likelihood",
       pch=21,  col=alpha('black', 0.4))
  lines(y2~x2, lty=1, lwd=2, col="blue")

  
}

summary(modChickSurvival) 

{# per chick age category

  ## 5 to ringed with Coordination at day 6
modChickSurvival5toRinged <- glmer(RingedYN ~ 
                           scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
                              scale(NbHatched) +
                              scale(MeanLogAdevAgeCat6)+
                               scale(MeanLogSdevAgeCat6) +
                            scale(HatchingDayAfter0401) +
                            scale(PairBroodNb) +
                           XPriorResidence +
                           CrossFosteredYN +
                          (1|PairID) + 
                            (1|BreedingYear) +
                            (1|BroodRef) +
                          (1|NatalBroodID)
                          , data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]
                           , family = 'binomial'
                          , control=glmerControl(optimizer = "bobyqa")
                          
)
  
  summary(modChickSurvival5toRinged)
  #drop1(modChickSurvival5toRinged, test="Chisq") # LRT

nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6),])
table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE & !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6)])


table(MY_TABLE_perChick_All$WeightedAge5)
table(MY_TABLE_perChick_All$WeightedAge12)
table(MY_TABLE_perChick_All$RingedYN)
table(MY_TABLE_perChick_All$RingedYN[MY_TABLE_perChick_All$WeightedAge5 == TRUE])
nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge12 == FALSE & MY_TABLE_perChick_All$RingedYN == TRUE,])#64
nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge12 == TRUE & MY_TABLE_perChick_All$WeightedAge5 == FALSE,])#146
nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$RingedYN == TRUE & MY_TABLE_perChick_All$WeightedAge5 == FALSE,])#151
nrow(MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,])#2373

length(unique(MY_TABLE_perChick_All$BroodRef[MY_TABLE_perChick_All$WeightedAge5 == TRUE & 
                                               is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat6) & 
                                               !is.na(MY_TABLE_perChick_All$MeanLogAdevAgeCat10)])) # 43
  



## 5 to ringed with average Coordination

modChickSurvival5toRingedAverageCoordination <- glmer(RingedYN ~ 
                                     scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
                                     scale(NbHatched) + # should be BS at day 5
                                     scale(MeanLogAdev)+
                                     scale(MeanLogSdev) +
                                     scale(HatchingDayAfter0401) +
                                     scale(PairBroodNb) +
                                     XPriorResidence +
                                     CrossFosteredYN +
                                     (1|PairID) + 
                                   #  (1|BreedingYear) +
                                    # (1|BroodRef) +
                                     (1|NatalBroodID)
                                   , data = MY_TABLE_perChick_All[MY_TABLE_perChick_All$WeightedAge5 == TRUE,]
                                   , family = 'binomial'
                                   , control=glmerControl(optimizer = "bobyqa")
                                   
)


summary(modChickSurvival5toRingedAverageCoordination)


modChickSurvivalHatchedtoRingedAverageCoordination <- glmer(RingedYN ~ 
                                                        scale(MeanTotalProRate)+ I(scale(MeanTotalProRate)^2)+
                                                        scale(NbHatched) + 
                                                        scale(MeanLogAdev)+
                                                        scale(MeanLogSdev) +
                                                        scale(HatchingDayAfter0401) +
                                                        scale(PairBroodNb) +
                                                        XPriorResidence +
                                                        CrossFosteredYN +
                                                        (1|PairID) + 
                                                        #  (1|BreedingYear) +
                                                        # (1|BroodRef) +
                                                        (1|NatalBroodID)
                                                      , data = MY_TABLE_perChick_All
                                                      , family = 'binomial'
                                                      , control=glmerControl(optimizer = "bobyqa")
                                                      
)


summary(modChickSurvivalHatchedtoRingedAverageCoordination)

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

  summary(mod_Divorce) 
  drop1(mod_Divorce, test = "Chisq")
  dispersion_glmer(mod_Divorce) # 0.90
  table(MY_TABLE_perBrood$PairDivorce)

  oddsDivorce <- exp(cbind(OR=fixef(mod_Divorce), confint(mod_Divorce, parm="beta_")))[c(2,3),] 
  
  #   OR     2.5 %   97.5 %
  #   scale(MeanLogSdev) 0.964058 0.7154698 1.296955
  #   scale(MeanLogAdev) 1.097456 0.8353895 1.454126
  
  
}

summary(mod_Divorce) 


