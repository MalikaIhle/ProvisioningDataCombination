#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 03/02/2017
#	 commit: upgrade analyses with Joel (cbind NbA, NbAmissed) + use of BLUPs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### remarks
# LastSeenAlive information needs to be updated manually when DB updated
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

{### Get raw data from R_Selected&SimulatedData folder

# source('Alternation_DataSelection_DataSimulation.R')
# or :

SelectedData_folder <- "R_Selected&SimulatedData"

MY_TABLE_perDVD <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perDVD.csv", sep="/")) # summary stats for all analyzed videos where both parents known and with expected alternation from simulation
MY_TABLE_perBrood <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perBrood.csv", sep="/")) # only recorded brood (summarizing MY_TABLE_perDVD per brood)
MY_TABLE_perChick <- read.csv(paste(SelectedData_folder,"R_MY_TABLE_perChick.csv", sep="/"))
#MY_TABLE_perBirdYear
SimulationOutput <- read.csv(paste(SelectedData_folder,"R_SimulationOutput.csv", sep="/"))
SimulationOutput_long <- read.csv(paste(SelectedData_folder,"R_SimulationOutput_long.csv", sep="/"))
SimulationOutput_long_median <- read.csv(paste(SelectedData_folder,"R_SimulationOutput_long_median.csv", sep="/"))
SimulationOutput_S_long_median <- read.csv(paste(SelectedData_folder,"R_SimulationOutput_S_long_median.csv", sep="/"))

}

head(MY_TABLE_perDVD)
head(MY_TABLE_perBrood)
head(SimulationOutput_long_median)

{# create a repeated table per DVD, with the column NbAlternation and NbS having observed values in first half, and sim values in second half
MY_TABLE_perDVD_Sim_long <- rbind(MY_TABLE_perDVD,MY_TABLE_perDVD)

MY_TABLE_perDVD_Sim_long$Type <- c(rep("z_Obsv", nrow(MY_TABLE_perDVD)),rep("a_Sim", nrow(MY_TABLE_perDVD)))

MY_TABLE_perDVD_Sim_long$NbAlternation[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_Sim_long$MedAsim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim']
MY_TABLE_perDVD_Sim_long$NbSynchro_ChickFeedingEquanim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim'] <- MY_TABLE_perDVD_Sim_long$MedSsim[MY_TABLE_perDVD_Sim_long$Type == 'a_Sim']

MY_TABLE_perDVD_Sim_long$rowID <- seq(1:nrow(MY_TABLE_perDVD_Sim_long))
}

head(MY_TABLE_perDVD_Sim_long)




######################
# RANDOM VS OBSERVED #
######################


{#### alternation

{# paired t.test > non-normality of the difference
 
SimulationOutput$DVDRef <- as.character(as.numeric(SimulationOutput$DVDRef))

hist(SimulationOutput$NbAlternation - SimulationOutput$Aswitch)
boxplot(SimulationOutput$NbAlternation - SimulationOutput$Aswitch)
qqnorm(SimulationOutput$NbAlternation - SimulationOutput$Aswitch)
qqline(SimulationOutput$NbAlternation - SimulationOutput$Aswitch)
shapiro.test(SimulationOutput$NbAlternation - SimulationOutput$Aswitch)

t.test(SimulationOutput$NbAlternation,SimulationOutput$Aswitch, paired=TRUE)


hist(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimWithin)
boxplot(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimWithin)
qqnorm(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimWithin)
qqline(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimWithin)
shapiro.test(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimWithin)

t.test(SimulationOutput$NbAlternation,SimulationOutput$MeanAsimWithin, paired=TRUE)


hist(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimAmong)
boxplot(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimAmong)
qqnorm(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimAmong)
qqline(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimAmong)
shapiro.test(SimulationOutput$NbAlternation - SimulationOutput$MeanAsimAmong)

t.test(SimulationOutput$NbAlternation,SimulationOutput$MeanAsimAmong, paired=TRUE)

}

{# lmer > heteroscedasticity

SimulationOutput_long$LineID <- as.character(1:nrow(SimulationOutput_long))
SimulationOutput_long$NbAlternation <- as.numeric(as.character(SimulationOutput_long$NbAlternation))


modRandomVsObs <- lmer( NbAlternation ~ Type + (1|DVDRef) , data = SimulationOutput_long)
summary(modRandomVsObs)


modRandomVsObs_without_intercept <- lmer(NbAlternation ~ -1 + Type + (1|DVDRef) , data = SimulationOutput_long)
summary(modRandomVsObs_without_intercept)


{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modRandomVsObs), resid(modRandomVsObs))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modRandomVsObs))
qqline(resid(modRandomVsObs))
qqnorm(unlist(ranef(modRandomVsObs))) 
qqline(unlist(ranef(modRandomVsObs)))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modRandomVsObs))),fitted(modRandomVsObs)) # quite not !!!!!!!

	#library(nlme)
	#modRandomVsObsnlme <- lme(NbAlternation ~ Type, random  =  ~1|DVDRef, data = SimulationOutput_long) # give the same output > heteroscedasticity no solved

	#modRandomVsObsglm <- glmer(NbAlternation ~ Type+ (1|DVDRef), family = 'poisson', data = SimulationOutput_long) does not run because NbA not integers

# Mean of ranefs: should be zero
mean(unlist(ranef(modRandomVsObs)$DVDRef))


# residuals vs predictors
plot(SimulationOutput_long$Type, resid(modRandomVsObs))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- SimulationOutput_long
d$fitted <- fitted(modRandomVsObs)
scatter.smooth(d$fitted, jitter(d$NbAlternation, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
boxplot(fitted~Type, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbAlternation", xlab="Type")

}

}

{# median and mean are extremely well correlated > use median for glmer
cor.test(as.numeric(as.character(SimulationOutput_long$NbAlternation[SimulationOutput_long$Type == '3_Within'])),
as.numeric(as.character(SimulationOutput_long_median$NbAlternation[SimulationOutput_long_median$Type == '3_Within'])))
plot(SimulationOutput_long$NbAlternation[SimulationOutput_long$Type == '3_Within'],SimulationOutput_long_median$NbAlternation[SimulationOutput_long_median$Type == '3_Within'])

cor.test(as.numeric(as.character(SimulationOutput_long$NbAlternation[SimulationOutput_long$Type == '4_Among'])),
as.numeric(as.character(SimulationOutput_long_median$NbAlternation[SimulationOutput_long_median$Type == '4_Among'])))
plot(SimulationOutput_long$NbAlternation[SimulationOutput_long$Type == '4_Among'],SimulationOutput_long_median$NbAlternation[SimulationOutput_long_median$Type == '4_Among'])
}

{# glmer poisson > fine

modRandomVsObs_glmer <- glmer( NbAlternation ~ Type + (1|DVDRef), data = SimulationOutput_long_median, family = 'poisson')
summary(modRandomVsObs_glmer)

dispersion_glmer(modRandomVsObs_glmer) # 0.63 < 1.4

modRandomVsObs_glmer_without_intercept <- glmer(NbAlternation ~ -1 + Type + (1|DVDRef) , data = SimulationOutput_long_median, family = 'poisson')
summary(modRandomVsObs_glmer_without_intercept)
exp(summary(modRandomVsObs_glmer_without_intercept)$coeff[,1])

	# gaussian for comparison
	modRandomVsObs_glmer_without_intercept_lmer <- lmer(NbAlternation ~ -1 + Type + (1|DVDRef) , data = SimulationOutput_long_median)
	summary(modRandomVsObs_glmer_without_intercept_lmer)$coeff[,1]

	
{# model assumption checking : all ok

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modRandomVsObs_glmer), resid(modRandomVsObs_glmer))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modRandomVsObs_glmer))
qqline(resid(modRandomVsObs_glmer))
qqnorm(unlist(ranef(modRandomVsObs_glmer))) 
qqline(unlist(ranef(modRandomVsObs_glmer)))

# qq-plot of deviance-residuals: should be normally distributed
qqnorm(residuals(modRandomVsObs_glmer, type="deviance"))
qqline(residuals(modRandomVsObs_glmer, type="deviance"))

# Mean of ranefs: should be zero
mean(unlist(ranef(modRandomVsObs_glmer)$DVDRef))

# residuals vs predictors
plot(SimulationOutput_long_median$Type, resid(modRandomVsObs_glmer))
abline(h=0, lty=2)


}

}

{# glmer binomial > the best one for plotting A out of A Max in percentage

modRandomVsObs_outofAmax_glmer <- glmer(cbind(NbAlternation,NbAMax-NbAlternation) ~ Type + (1|DVDRef) , data = SimulationOutput_long_median, family = 'binomial')
summary(modRandomVsObs_outofAmax_glmer)
	
{# model assumption checking : ok-ish ?

# residuals vs fitted: mean should constantly be zero: not quite ?
scatter.smooth(fitted(modRandomVsObs_outofAmax_glmer), resid(modRandomVsObs_outofAmax_glmer))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modRandomVsObs_outofAmax_glmer))
qqline(resid(modRandomVsObs_outofAmax_glmer))
qqnorm(unlist(ranef(modRandomVsObs_outofAmax_glmer))) 
qqline(unlist(ranef(modRandomVsObs_outofAmax_glmer)))

# qq-plot of deviance-residuals: should be normally distributed
qqnorm(residuals(modRandomVsObs_outofAmax_glmer, type="deviance"))
qqline(residuals(modRandomVsObs_outofAmax_glmer, type="deviance"))

# Mean of ranefs: should be zero
mean(unlist(ranef(modRandomVsObs_outofAmax_glmer)$DVDRef))

# residuals vs predictors
plot(SimulationOutput_long_median$Type, resid(modRandomVsObs_outofAmax_glmer))
abline(h=0, lty=2)
}

{# plot

modRandomVsObs_outofAmax_glmer_without_intercept <- glmer(cbind(NbAlternation,NbAMax-NbAlternation) ~ -1+Type + (1|DVDRef), data = SimulationOutput_long_median, family = 'binomial')

estimatesNbAoutofNbAMax <- summary(modRandomVsObs_outofAmax_glmer_without_intercept)$coeff
estimatesNbAoutofNbAMax

summary_A_AMax_est <- cbind(Type = rownames(data.frame(est = invlogit(estimatesNbAoutofNbAMax[,1])*100)),
data.frame(est = invlogit(estimatesNbAoutofNbAMax[,1])*100),
data.frame(selow = invlogit(estimatesNbAoutofNbAMax[,1]-estimatesNbAoutofNbAMax[,2]*1.96)*100),
data.frame(seup = invlogit(estimatesNbAoutofNbAMax[,1]+estimatesNbAoutofNbAMax[,2]*1.96)*100))


Fig_A_AMax_est <- {ggplot(data=summary_A_AMax_est, aes(x=Type, y=est))+
xlab(NULL)+
ylab("Number of alternations realized out of the maximum possible (%)\n")+

geom_errorbar(aes(ymin=selow, ymax=seup),na.rm=TRUE)+
geom_point(size = 3) +

scale_y_continuous(breaks =seq(60,80, by = 2),limits = c(60,80)) +
scale_x_discrete(labels = c('Observed', 'Switch', 'Within', 'Among'))+

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

Fig_A_AMax_est 
# these are not the row data like in Selection/Simulation script, but are the back transformed estimates of the binomial glmer model + 95% CI 
# > Joel says it's wrong (one would need to use delta method to back transform variance)

}

summary(modRandomVsObs_outofAmax_glmer)

{#### synchrony

{# gmer binomial cbind(NbS,NbAMax-NbS)
	
mod_S_RandomVsObs_outofAmax_glmer <- glmer( cbind(NbS,NbAMax-NbS) ~ Type
																	+(1|DVDRef)
																	, data = SimulationOutput_S_long_median, 
																	family = 'binomial')
																	
summary(mod_S_RandomVsObs_outofAmax_glmer)

dispersion_glmer(mod_S_RandomVsObs_outofAmax_glmer) # 0.8 < 1.6

}

{# model assumption checking : ok-ish ?

# residuals vs fitted: mean should constantly be zero: not quite ?
scatter.smooth(fitted(mod_S_RandomVsObs_outofAmax_glmer), resid(mod_S_RandomVsObs_outofAmax_glmer))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(mod_S_RandomVsObs_outofAmax_glmer))
qqline(resid(mod_S_RandomVsObs_outofAmax_glmer))
qqnorm(unlist(ranef(mod_S_RandomVsObs_outofAmax_glmer))) 
qqline(unlist(ranef(mod_S_RandomVsObs_outofAmax_glmer)))

# qq-plot of deviance-residuals: should be normally distributed
qqnorm(residuals(mod_S_RandomVsObs_outofAmax_glmer, type="deviance"))
qqline(residuals(mod_S_RandomVsObs_outofAmax_glmer, type="deviance"))

# Mean of ranefs: should be zero
mean(unlist(ranef(mod_S_RandomVsObs_outofAmax_glmer)$DVDRef))

# residuals vs predictors
plot(SimulationOutput_S_long_median$Type, resid(mod_S_RandomVsObs_outofAmax_glmer))
abline(h=0, lty=2)
}

{# plot Fig_S_AMax_est

mod_S_RandomVsObs_outofAmax_glmer_without_intercept <- glmer(cbind(NbS,NbAMax-NbS) ~ -1+Type + (1|DVDRef) + (1|LineID), data = SimulationOutput_S_long_median, family = 'binomial')

estimatesNbSoutofNbAMax <- summary(mod_S_RandomVsObs_outofAmax_glmer_without_intercept)$coeff
estimatesNbSoutofNbAMax

summary_S_AMax_est <- cbind(Type = rownames(data.frame(est = invlogit(estimatesNbSoutofNbAMax[,1])*100)),
data.frame(est = invlogit(estimatesNbSoutofNbAMax[,1])*100),
data.frame(selow = invlogit(estimatesNbSoutofNbAMax[,1]-estimatesNbSoutofNbAMax[,2]*1.96)*100),
data.frame(seup = invlogit(estimatesNbSoutofNbAMax[,1]+estimatesNbSoutofNbAMax[,2]*1.96)*100))

Fig_S_AMax_est <- {ggplot(data=summary_S_AMax_est, aes(x=Type, y=est))+
xlab(NULL)+
ylab("Number of synchronous visits realized out of the maximum possible (%)\n")+

geom_errorbar(aes(ymin=selow, ymax=seup),na.rm=TRUE)+
geom_point(size = 3) +

scale_y_continuous(breaks =seq(5,13, by = 2),limits = c(5,13)) +
scale_x_discrete(labels = c('Observed', 'Within', 'Among'))+

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

summary(mod_S_RandomVsObs_outofAmax_glmer)



##############
# PREDICTORS #
##############


{#### predictors of alternation

{# check dependent and explanatory variables

cor.test(MY_TABLE_perDVD$DVDInfoAge,MY_TABLE_perDVD$DVDInfoChickNb) # cor = -0.10, p<0.001 
cor.test(MY_TABLE_perDVD$DVDInfoAge,MY_TABLE_perDVD$NbRinged) # cor = 0.05, p=0.06 
cor.test(MY_TABLE_perDVD$MumAge,MY_TABLE_perDVD$DadAge) # cor = 0.34, p *****   - assortative mating for age > take the mean of the 2 ?
cor.test(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$PairBroodNb) # cor = 0.65, p < 0.0001 ! > take one or the other variable ?

#hist(MY_TABLE_perDVD$DVDInfoAge) # very bimodal as the protocol is to measure d7 and d11, in between is when they "miss"

summary(MY_TABLE_perDVD$RelTimeHrs) # 6 NA's > if this covariate is use, reduce MY_TABLE_perDVD from those RelTimeHrs NAs
#scatter.smooth(MY_TABLE_perDVD$NbAlternation,MY_TABLE_perDVD$RelTimeHrs)# linear 
#scatter.smooth(MY_TABLE_perDVD$RelTimeHrs,MY_TABLE_perDVD$NbAlternation)# linear
#scatter.smooth(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$NbAlternation/MY_TABLE_perDVD$NbAMax)

}

{# modA

modA <- glmer(cbind(NbAlternation, NbAMax-NbAlternation)~  
	scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	scale(HatchingDayAfter0401) +
	scale(PairBroodNb) + 
	scale(DVDInfoChickNb) + 
	ChickAgeCat +
	scale(RelTimeHrs) + 
	MPriorResidence +
    FPriorResidence +
	
	#+ TotalProRate + VisitRateDifference+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) # for overdispersion
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
	, family = 'binomial'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA) # Number of obs: 1593, groups:  BroodRef, 869; PairIDYear: 546 ; PairID, 443; SocialMumID, 290; SocialDadID, 280; BreedingYear, 12

summary(modA)$coeff
summary(modA2)$coeff

dispersion_glmer(modA) # 1.2 <1.4, but when DVDRef added, captures quite some variance ?

BlupsBroodRefModA <- cbind(unique(MY_TABLE_perDVD$BroodRef[!is.na(MY_TABLE_perDVD$RelTimeHrs)]), invlogit(ranef(modA)$BroodRef[,1])) 
colnames(BlupsBroodRefModA) <- c('BroodRef','blupsA')
}

{# model assumptions checking: residuals vs fitted not good at all

# residuals vs fitted: mean should constantly be zero: NOT AT ALL ! <<<<<<<<<<<<<<<<<
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
mean(unlist(ranef(modA)$BreedingYear))

# residuals vs predictors
scatter.smooth(MY_TABLE_perDVD$ParentsAge[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD$HatchingDayAfter0401[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD$PairBroodNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD$DVDInfoChickNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD$ChickAgeCat[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$RelTimeHrs[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modA))
abline(h=0, lty=2)		


}

{# modA_withinIndAgeEffect

modA_withinIndAgeEffect <- glmer(cbind(NbAlternation, NbAMax-NbAlternation)~  

	#scale(meanMumAge, scale=FALSE) + 
	#scale(DeltaMumAge, scale=FALSE) +
	scale(meanDadAge, scale=FALSE) + 
	scale(DeltaDadAge, scale=FALSE) +
	
	scale(HatchingDayAfter0401) +
	#scale(PairBroodNb) + 
	scale(DVDInfoChickNb) + 
	ChickAgeCat +
	scale(RelTimeHrs) + 
	MPriorResidence +
    FPriorResidence +
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)+
	(1|DVDRef) # for overdispersion
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
	, family = 'binomial'
	,control=glmerControl(optimizer = "bobyqa")
	)
	
summary(modA_withinIndAgeEffect)

}



modA_off <- glmer(NbAlternation~  
	scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	scale(HatchingDayAfter0401) +
	scale(PairBroodNb) + 
	scale(DVDInfoChickNb) + 
	ChickAgeCat +
	scale(RelTimeHrs) + 
	MPriorResidence +
    FPriorResidence +
	
	#+ TotalProRate + VisitRateDifference+
	
	offset(log(NbAMax))+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) # for overdispersion
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)
summary(modA_off)




{# mod A_long
modA_long <- glmer(NbAlternation ~  
	
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
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long)


modA_long_off <- glmer(NbAlternation ~  
	
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
	
	offset(log(NbAMax))+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) 
	+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long)$coeff
summary(modA_long_off)$coeff
#$coef[c(2,13:22),]


{# exploration of similarity of models

MY_TABLE_perDVD$MedAsim2 <- with(MY_TABLE_perDVD, ifelse(MedAsim==0,1,MedAsim))
MY_TABLE_perDVD_Sim_long$NbAlternation2 <- with(MY_TABLE_perDVD_Sim_long, ifelse(NbAlternation==0,1,NbAlternation))



modA_long_log <- lmer(log(NbAlternation2) ~  
	
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
#	+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
#	, family = 'poisson'
#	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long_log)$coef[c(2,13:22),]


plot(summary(modA_long)$coef[c(2,13:22),1], summary(modA_long_log)$coef[c(2,13:22),1],col=0)
abline(0,1)
text(summary(modA_long)$coef[c(2,13:22),1], summary(modA_long_log)$coef[c(2,13:22),1], 1:11)
abline(0,1)


modA_off_med_log <- lmer(log(NbAlternation)~  
	scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	scale(HatchingDayAfter0401) +
	scale(PairBroodNb) + 
	scale(DVDInfoChickNb) + 
	ChickAgeCat +
	scale(RelTimeHrs) + 
	MPriorResidence +
    FPriorResidence +
	
	scale(TotalProRate) + 
	scale(VisitRateDifference)+

	offset(log(MedAsim2))+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	#+ (1|DVDRef) # for overdispersion
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
	#, family = 'poisson'
	#,control=glmerControl(optimizer = "bobyqa")
	#, offset=log(MedAsim2)
	)

summary(modA_off_med_log)$coef


sunflowerplot(NbAlternation~MedAsim, MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])



modA_long_gauss <- lmer(NbAlternation ~  
	
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
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	#, family = 'poisson'
	#,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long_gauss)



modA_long_ratio <- lmer((NbAlternation/NbAMax)) ~  
	
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
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	#, family = 'poisson'
	#,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long_ratio)





modA_long_ratio_off <- lmer(I(NbAlternation/NbAMax) ~  
	
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
	
	offset(log(NbAMax))+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) 
	#+ (1|rowID) # for overdispersion
	, weights= MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]$NbAMax
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	#, family = 'poisson'
	#,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long_ratio_off)







modA_long_off <- glmer(NbAlternation ~  
	
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
	
	offset(log(NbAMax))+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) 
	+ (1|rowID) # for overdispersion
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long_off)







modA_long_cbind <- glmer(cbind(NbAlternation,NbAMax-NbAlternation) ~  
	
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
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'binomial'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modA_long_cbind)

summary(modA_long_cbind)$coef[13:22,]
summary(modA_long_ratio_off)$coef[13:22,]

}

}

}


##http://stats.stackexchange.com/questions/189115/fitting-a-binomial-glmm-glmer-to-a-response-variable-that-is-a-proportion-or-f
summary(modA)
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=BlupsBroodRefModA, by='BroodRef', all.x=TRUE)

{#### predictors of the deviation in alternation from randomization within file

scatter.smooth(MY_TABLE_perDVD$NbAlternation, MY_TABLE_perDVD$Adev)

modAdev <- lmer( Adev ~ scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
						scale(HatchingDayAfter0401) +
						scale(PairBroodNb) + 
						scale(DVDInfoChickNb) + 
						ChickAgeCat +
						scale(RelTimeHrs) + 
						MPriorResidence +
						FPriorResidence +
						
						#scale(TotalProRate)+
						scale(MFVisit1)+
						scale(VisitRateDifference)+
						
						
						(1|BroodRef) + 
						(1|SocialMumID)+ (1|SocialDadID) + 
						#(1|PairID) +  # explained 0% of the variance
						(1|BreedingYear)  
						# + (1|PairIDYear) # explained 0% of the variance
						, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])
summary(modAdev)

summary(modAdev)
summary(modA_long_gauss)$coef[c(2,13:22),]


BlupsBroodRefModAdev <- cbind(unique(MY_TABLE_perDVD$BroodRef[!is.na(MY_TABLE_perDVD$RelTimeHrs)]), invlogit(ranef(modAdev)$BroodRef[,1])) 
colnames(BlupsBroodRefModAdev) <- c('BroodRef','blupsAdev')

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero: ok -ish
scatter.smooth(fitted(modAdev), resid(modAdev))
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modAdev))
qqline(resid(modAdev))
qqnorm(unlist(ranef(modAdev))) 
qqline(unlist(ranef(modAdev)))

# Mean of ranefs: should be zero
mean(unlist(ranef(modAdev)$BroodRef))
mean(unlist(ranef(modAdev)$SocialMumID))
mean(unlist(ranef(modAdev)$SocialDadID))
mean(unlist(ranef(modAdev)$PairID))
mean(unlist(ranef(modAdev)$BreedingYear))

# homogeneity of variance
scatter.smooth(sqrt(abs(resid(modAdev))),fitted(modAdev)) 

# residuals vs predictors
scatter.smooth(MY_TABLE_perDVD$ParentsAge[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modAdev))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD$HatchingDayAfter0401[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modAdev))
abline(h=0, lty=2)
plot(MY_TABLE_perDVD$DVDInfoChickNb[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modAdev))
abline(h=0, lty=2)	
plot(MY_TABLE_perDVD$ChickAgeCat[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modAdev))
abline(h=0, lty=2)	
scatter.smooth(MY_TABLE_perDVD$RelTimeHrs[!is.na(MY_TABLE_perDVD$RelTimeHrs)], resid(modAdev))
abline(h=0, lty=2)	

# dependent variable vs fitted
d <- MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
d$fitted <- fitted(modAdev)
scatter.smooth(d$fitted, jitter(d$Adev^1.2, 0.05),ylim=c(0, 100^1.2))
abline(0,1)	

# fitted vs all predictors
scatter.smooth(d$ParentsAge,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ParentsAge")
scatter.smooth(d$HatchingDayAfter0401,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="HatchingDayAfter0401")
boxplot(fitted~ChickAgeCat, d, ylim=c(0, 100), las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="ChickAgeCat")
plot(d$DVDInfoChickNb,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="DVDInfoChickNb")
scatter.smooth(d$RelTimeHrs,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="AlternationValue", xlab="RelTimeHrs")	


}

}

summary(modAdev)
MY_TABLE_perBrood <- merge(x=MY_TABLE_perBrood, y=BlupsBroodRefModAdev, by='BroodRef', all.x=TRUE)


{#### predictors of synchrony

{# check dependent and explanatory variables 

Correl_A_S <-  {ggplot(data=MY_TABLE_perDVD, aes(y=NbSynchro_ChickFeedingEquanim,x=NbAlternation) )+ 
							geom_point()  +
							geom_smooth(method = "lm") +
							geom_abline(intercept=0,slope=0.5)+
							geom_abline(intercept=0,slope=1)}

hist(MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim, breaks =length(unique(MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim)))

# summary when synchro 0 vs non-zero
summary(MY_TABLE_perDVD[MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim == 0,c("MVisit1","FVisit1","VisitRateDifference","TotalProRate","NbAlternation","DVDInfoChickNb")])
summary(MY_TABLE_perDVD[MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim != 0,c("MVisit1","FVisit1","VisitRateDifference","TotalProRate","NbAlternation","DVDInfoChickNb")])

}


modS <- glmer(cbind(NbSynchro_ChickFeedingEquanim, NbAMax-NbSynchro_ChickFeedingEquanim)~  
	scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur become signi negative, if PBDur removed, ParentAge even more signi Neg)
	scale(HatchingDayAfter0401) +
	scale(PairBroodNb) + 
	scale(DVDInfoChickNb) + 
	ChickAgeCat +
	scale(RelTimeHrs) + 
	MPriorResidence +
    FPriorResidence +
	##offset(I(log(MFVisit1)))+
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	#(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	(1|PairIDYear)
	+ (1|DVDRef) # for overdispersion
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),]
	, family = 'binomial'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modS)
	
	
	
modS_long <- glmer(NbSynchro_ChickFeedingEquanim ~ 
	
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
	, data = MY_TABLE_perDVD_Sim_long[!is.na(MY_TABLE_perDVD_Sim_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)

summary(modS_long)

	
	
	
}


{#### predictors of the deviation in synchrony from randomization within file

scatter.smooth(MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim, MY_TABLE_perDVD$Sdev)


modSdev <- lmer(Sdev~  
	scale(MFVisit1) + # this is strongly correlated to VisitRateDifference and with chickNb and this is mathematically linked to Sync score
	scale(ParentsAge) +
	scale(HatchingDayAfter0401) + 
	scale(PairBroodNb) + 
	scale(DVDInfoChickNb) + 
	FPriorResidence +
	MPriorResidence+
	ChickAgeCat + 
	scale(RelTimeHrs) + 
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) +
	(1|PairID) + 
	(1|BreedingYear) 
	# + (1|PairIDYear) # explained 0% of the variance
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),])

summary(modSdev)


}



######################
# FITNESS CORRELATES #
######################


{#### total provisioning rate ~ Alternation

modFitnessAsTotalProvisioning <- glmer(MFVisit1 ~ scale(NbRinged) 
												+ scale(NbAlternation/NbAMax)*Type # the deviation to max, isn't different between observed and sim (random) values
												+ offset(log(EffectiveTime))
												+ (1|SocialMumID)+ (1|SocialDadID) 
												+ (1|PairID) + (1|BreedingYear)
												#+ (1|PairIDYear)
												#+ (1|rowID) # overdispersion
												+ (1|DVDRef) # each DVD twice: once with alternation observed, once with simulated alternation (averaged)
												, data = MY_TABLE_perDVD_Sim_long
												, family = 'poisson'
												,control=glmerControl(optimizer = "bobyqa")										
												)
											
summary(modFitnessAsTotalProvisioning) # interaction not signi.



modFitnessAsTotalProvisioning <- glmer(MFVisit1 ~ scale(NbRinged) 
												+ scale(Adev) # the deviation to max, isn't different between observed and sim (random) values
												+ offset(log(EffectiveTime))
												+ (1|SocialMumID)+ (1|SocialDadID) 
												+ (1|PairID) + (1|BreedingYear)
												#+ (1|PairIDYear)
												#+ (1|rowID) # overdispersion
												+ (1|DVDRef) # each DVD twice: once with alternation observed, once with simulated alternation (averaged)
												, data = MY_TABLE_perDVD
												, family = 'poisson'
												,control=glmerControl(optimizer = "bobyqa")										
												)
											
summary(modFitnessAsTotalProvisioning) # interaction not signi.

{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsTotalProvisioning), resid(modFitnessAsTotalProvisioning))	# not quite ??
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modFitnessAsTotalProvisioning))
qqline(resid(modFitnessAsTotalProvisioning))
qqnorm(unlist(ranef(modFitnessAsTotalProvisioning)))
qqline(unlist(ranef(modFitnessAsTotalProvisioning)))

# qq-plot of deviance-residuals: should be normally distributed
qqnorm(residuals(modFitnessAsTotalProvisioning, type="deviance"))
qqline(residuals(modFitnessAsTotalProvisioning, type="deviance"))

# Mean of ranefs: should be zero
mean(unlist(ranef(modFitnessAsTotalProvisioning)$SocialMumID))
mean(unlist(ranef(modFitnessAsTotalProvisioning)$SocialDadID))
mean(unlist(ranef(modFitnessAsTotalProvisioning)$PairID))
mean(unlist(ranef(modFitnessAsTotalProvisioning)$BreedingYear))

# residuals vs predictors
plot(MY_TABLE_perDVD_Sim_long$NbRinged, resid(modFitnessAsTotalProvisioning))
abline(h=0, lty=2)
scatter.smooth(MY_TABLE_perDVD_Sim_long$NbAlternation, resid(modFitnessAsTotalProvisioning))
abline(h=0, lty=2)

# dependent variable vs fitted
d <- MY_TABLE_perDVD_Sim_long
d$fitted <- fitted(modFitnessAsTotalProvisioning)
scatter.smooth(d$fitted, jitter(d$TotalProRate, 0.05),ylim=c(0, 100))
abline(0,1)	

# fitted vs all predictors
plot(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbRinged")
scatter.smooth(d$NbAlternation,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="TotalProRate", xlab="NbAlternation")

}


}

{#### mean chick mass ~ Alternation + Synchrony

{# check dependent and explanatory variables
nrow(MY_TABLE_perBrood[ MY_TABLE_perBrood$NbRinged == 0 ,]) # 44 broods with no ringed chicks
nrow(MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,]) # 21 broods where ringed chicks but no mass nor tarsus: for some reasons were ringed not at the rigth age for comparable measurements)
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$AvgTarsus) & !is.na(MY_TABLE_perBrood$AvgMass) & MY_TABLE_perBrood$NbRinged != 0 ,] # 2 broods with ringed with mass but not tarsus

scatter.smooth(MY_TABLE_perBrood$AvgMass~ MY_TABLE_perBrood$AvgTarsus)
scatter.smooth(MY_TABLE_perBrood$MeanTotalProRate,MY_TABLE_perBrood$blups)

head(MY_TABLE_perChick)
head(MY_TABLE_perBrood)
}

# RQ: in MY_TABLE_perChick 'AvgOf' are not averages but simply the Mass and Tarsus of the chick as the minimum age (between 11 and 14) he was measured

modFitnessAsChickMasswithGenParents_Dev <- lmer(AvgOfMass ~ AvgOfTarsus +
															HatchingDayAfter0401+
															PairBroodNb+
															NbRinged + 
															MeanTotalProRate +
															MeanAdev + 
															MeanSdev+
															(1|RearingBrood)+
															#(1|SocialMumID)+ (1|SocialDadID) + 
															(1|PairID) + (1|BreedingYear) 
															+ (1|dam) + (1|sire) + (1|GenPairID)
															, data = MY_TABLE_perChick)
												

summary(modFitnessAsChickMasswithGenParents_Dev) 


modFitnessAsChickMasswithGenParents_blups <- lmer(AvgOfMass ~ AvgOfTarsus +
															HatchingDayAfter0401+
															PairBroodNb+
															NbRinged + 
															MeanTotalProRate +
															MeanAdev + 
															MeanSdev+
															(1|RearingBrood)+
															#(1|SocialMumID)+ (1|SocialDadID) + 
															(1|PairID) + (1|BreedingYear) 
															+ (1|dam) + (1|sire) + (1|GenPairID)
															, data = MY_TABLE_perChick)

}

{#### number of chicks ringed ~ Alternation + Synchrony



summary(MY_TABLE_perBrood$MixedBroodYN)

modFitnessAsNbRinged <- glmer(cbind(NbRinged, NbHatched) ~ 	MeanTotalProRate+
															MeanA+
															HatchingDayAfter0401 +
															PairBroodNb +
															MBroodNb+
															FBroodNb +
															MPriorResidence +
															FPriorResidence +
															# MixedBroodYN + is NA if not chicked measured...
															# (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
															(1|BreedingYear) , data = MY_TABLE_perBrood, family = 'binomial')
										
summary(modFitnessAsNbRinged)  

  

modFitnessAsNbRinged_blups <- glmer(cbind(NbRinged, NbHatched) ~ 	MeanTotalProRate+
															blups+
															HatchingDayAfter0401 +
															PairBroodNb +
															MBroodNb+
															FBroodNb +
															MPriorResidence +
															FPriorResidence +
															# MixedBroodYN + is NA if not chicked measured...
															# (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
															(1|BreedingYear) , data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$blups),], family = 'binomial')
										
summary(modFitnessAsNbRinged_blups)  
                                                                  



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



modFitnessAsNbRinged_ADev <- lmer(NbRinged ~ TotalProRate+
											MeanAdev+
											MeanSdev+
											PairBroodNb+
											DadAge +
											MumAge +
											HatchingDayAfter0401 +
											#MBroodNb+
											#FBroodNb +
											MPriorResidence +
											FPriorResidence +
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
											(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(modFitnessAsNbRinged_ADev) # in ppt 20160707

sunflowerplot(MY_TABLE_perBrood$MumAge, MY_TABLE_perBrood$DadAge)
cor.test(MY_TABLE_perBrood$MumAge,MY_TABLE_perBrood$DadAge) # r=0.34 *****
scatter.smooth(MY_TABLE_perBrood$NbRinged~MY_TABLE_perBrood$HatchingDayAfter0401)



{# model assumptions checking

# residuals vs fitted: mean should constantly be zero
scatter.smooth(fitted(modFitnessAsNbRinged_ADev), resid(modFitnessAsNbRinged_ADev))	
abline(h=0, lty=2)

# qqplots of residuals and ranefs: should be normally distributed
qqnorm(resid(modFitnessAsNbRinged_ADev))
qqline(resid(modFitnessAsNbRinged_ADev))
qqnorm(unlist(ranef(modFitnessAsNbRinged_ADev))) 
qqline(unlist(ranef(modFitnessAsNbRinged_ADev)))

# Mean of ranefs: should be zero
# mean(unlist(ranef(modFitnessAsNbRinged_ADev)$SocialMumID))
# mean(unlist(ranef(modFitnessAsNbRinged_ADev)$SocialDadID))
# mean(unlist(ranef(modFitnessAsNbRinged_ADev)$PairID))
mean(unlist(ranef(modFitnessAsNbRinged_ADev)$BreedingYear))

##residuals vs predictors
# d <- MY_TABLE_perBrood
# scatter.smooth(d$MeanAdev, resid(modFitnessAsNbRinged_ADev)) # not linear !! > add poly term to model ?
# abline(h=0, lty=2)

##dependent variable vs fitted
# d$fitted <- fitted(modFitnessAsNbRinged_ADev)
# scatter.smooth(d$fitted, jitter(d$NbRinged, 0.05),ylim=c(0, 10))
# abline(0,1)	

##fitted vs all predictors
# scatter.smooth(d$MeanAdev,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="NbRinged", xlab="MeanAdev")

}

}

{#### variance in chick mass ~ synchrony

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
#mean(unlist(ranef(mod_Sync_sdResMassTarsus)$SocialMumID))
#mean(unlist(ranef(mod_Sync_sdResMassTarsus)$SocialDadID))
mean(unlist(ranef(mod_Sync_sdResMassTarsus)$PairID))
mean(unlist(ranef(mod_Sync_sdResMassTarsus)$BreedingYear))

# residuals vs predictors
d <- MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$sdResMassTarsus),]

boxplot(d$MixedBroodYN, resid(mod_Sync_sdResMassTarsus))
abline(h=0, lty=2)
# scatter.smooth(d$NbRinged, resid(mod_Sync_sdResMassTarsus))
# abline(h=0, lty=2)
# scatter.smooth(d$MeanSynchroFeed, resid(mod_Sync_sdResMassTarsus))
# abline(h=0, lty=2)	
	

# # dependent variable vs fitted
# d$fitted <- fitted(mod_Sync_sdResMassTarsus)
# scatter.smooth(d$fitted, jitter(d$sdResMassTarsus, 0.05),ylim=c(0, 5))

# # fitted vs all predictors
# boxplot(d$MixedBroodYN,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="sdResMassTarsus", xlab="MixedBroodYN")
# scatter.smooth(d$NbRinged,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="sdResMassTarsus", xlab="NbRinged")
# scatter.smooth(d$MeanSynchroFeed,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="sdResMassTarsus", xlab="MeanSynchroFeed")


}


mod_Sdev_sdResMassTarsus <- lmer(sdResMassTarsus ~ MixedBroodYN +
											HatchingDayAfter0401+
											TotalProRate+
											NbRinged + 
											MeanAdev+
											MeanSdev + 
											PairBroodNb+
											(1|SocialMumID)+ (1|SocialDadID) + 
											(1|PairID) + (1|BreedingYear) ,data=MY_TABLE_perBrood)
summary(mod_Sdev_sdResMassTarsus) # Number of obs: 680, groups:  PairID, 378; SocialMumID, 263; SocialDadID, 253; BreedingYear, 12



}

summary(mod_Sync_sdResMassTarsus)


{#### proportion of synchronous visits where female enters first > repeatability within pair could induce alternation

mod_proportionSexStartSynchro <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ #MFmeanDuration+TotalProRate + 
													(1|BroodRef) +
													(1|PairID)
													 #+(1|DVDRef) 
													, data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")

summary(mod_proportionSexStartSynchro)

mod_proportionSexStartSynchro <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~  
													#MFmeanDuration+TotalProRate + 
													(1|BroodRef) +
													(1|PairID)
													 #+(1|DVDRef) 
													, data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")

summary(mod_proportionSexStartSynchro)



{# model assumptions checking

# # check for overdispersion
# mod_proportionSexStartSynchro_overdisp <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ MFmeanDuration+TotalProRate + 
												# (1|BroodRef) +
												# (1|PairID)
												 # +(1|DVDRef) 
												# , data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")
# summary(mod_proportionSexStartSynchro_overdisp)
# anova(mod_proportionSexStartSynchro_overdisp,mod_proportionSexStartSynchro)

## check for overdispersion
# mod_proportionSexStartSynchro_overdisp <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ MFmeanDuration+TotalProRate + 
												# (1|BroodRef) +
												# (1|PairID)
												 # +(1|DVDRef) 
												# , data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")
# summary(mod_proportionSexStartSynchro_overdisp)
# anova(mod_proportionSexStartSynchro_overdisp,mod_proportionSexStartSynchro)

# qqplots residuals and ranef
qqnorm(resid(mod_proportionSexStartSynchro))
qqline(resid(mod_proportionSexStartSynchro))
qqnorm(unlist(ranef(mod_proportionSexStartSynchro)))	
qqline(unlist(ranef(mod_proportionSexStartSynchro)))

# residuals vs fitted					?
scatter.smooth(fitted(mod_proportionSexStartSynchro), resid(mod_proportionSexStartSynchro))
abline(h=0, lty=2)

# # residuals vs predictors		
# scatter.smooth(MY_TABLE_perDVD$MFmeanDuration[MY_TABLE_perDVD$SynchronyFeedValue >0], resid(mod_proportionSexStartSynchro))
# abline(h=0, lty=2)
# scatter.smooth(MY_TABLE_perDVD$TotalProRate[MY_TABLE_perDVD$SynchronyFeedValue >0], resid(mod_proportionSexStartSynchro))
# abline(h=0, lty=2)

# # data vs. fitted ?							
# d <- MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >0,]
# d$fitted <- fitted(mod_proportionSexStartSynchro)
# scatter.smooth(d$fitted, jitter(d$NbSynchroFemaleStart/(d$NbSynchroFemaleStart+d$NbSynchroMaleStart), 0.05),ylim=c(0, 1))
# abline(0,1)	

# # data and fitted against all predictors
# scatter.smooth(d$MFmeanDuration,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="proportion of synchronous visits where female enters first", xlab="MFmeanDuration")	
# scatter.smooth(d$TotalProRate,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="proportion of synchronous visits where female enters first", xlab="TotalProRate")	

}

}

summary(mod_proportionSexStartSynchro)



###########
# DIVORCE #
###########

{#### consequence of behavioural compatiblity or fitness in term of divorce

{# check dependent and explanatory variables 
nrow(MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$MwillDivorce) & !is.na(MY_TABLE_perBrood$NbRinged),])
MY_TABLE_perBrood[is.na(MY_TABLE_perBrood$MwillDivorce) & !is.na(MY_TABLE_perBrood$NbRinged),] # when Social female was NA
MY_TABLE_perBrood[MY_TABLE_perBrood$SocialDadID == 4060,]
MY_tblBroods[!is.na(MY_tblBroods$SocialDadID) & MY_tblBroods$SocialDadID == 4060,]

}


mod_MaleDivorce <- glmer(MwillDivorce~  scale(MeanSdev, scale=FALSE) + 
									scale(MeanAdev, scale=FALSE)	+
									scale(DadAge, scale=FALSE) + 
									scale(PairBroodNb, scale=FALSE) +
									scale(MeanFVisit1RateH, scale=FALSE) +  
									MnextNBsame + 
									scale(NbRinged, scale=FALSE) +
									(1|SocialDadID) + (1|SocialMumID) +(1|BreedingYear) 
									, data = MY_TABLE_perBrood, family="binomial")

									
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
mean(unlist(ranef(mod_MaleDivorce)$BreedingYear))

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



mod_FemaleDivorce <- glmer(FwillDivorce~scale(MeanSdev, scale=FALSE) + 
									scale(MeanAdev, scale=FALSE)	+
									scale(MumAge, scale=FALSE) + 
									scale(PairBroodNb, scale=FALSE) +
									scale(MeanMVisit1RateH, scale=FALSE) +  
									FnextNBsame + 
									scale(NbRinged, scale=FALSE) +
									 (1|SocialMumID) + (1|SocialDadID) + (1|BreedingYear) 
									, data = MY_TABLE_perBrood, family="binomial")
									
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




