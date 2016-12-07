#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 07/12/2016
#	 last modif : 07/12/2016
#	 commit: clean up DataAnalyses script
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
library(lme4)

}

{### Get raw data (from source() or R_output folder)

{# output csv files

# source('Alternation_DataSelection_DataSimulation.R')
# or :

DataDryad_folder <- "C:/Users/Malika/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/DataDryad"

MY_TABLE_perDVD
MY_TABLE_perBrood
MY_TABLE_perChick
MY_TABLE_perBirdYear


}


}



###############
# ALTERNATION #
###############

{#### Predictors of alternation

{# check dependent and explanatory variables

cor.test(MY_TABLE_perDVD$ChickAge,MY_TABLE_perDVD$DVDInfoChickNb) # cor = -0.08, p<0.001 
cor.test(MY_TABLE_perDVD$ChickAge,MY_TABLE_perDVD$NbRinged) # cor = 0.06, p=0.01 
cor.test(MY_TABLE_perDVD$ParentsAge,MY_TABLE_perDVD$PairBroodNb) # cor = 0.63, p < 0.0001 ! > take one or the other variable
cor.test(MY_TABLE_perDVD$DiffVisit1Rate, MY_TABLE_perDVD$MFVisit1RateH) # r=0.46
#scatter.smooth(MY_TABLE_perDVD$DiffVisit1Rate, MY_TABLE_perDVD$MFVisit1RateH)
#sunflowerplot(MY_TABLE_perDVD$DiffVisit1Rate, MY_TABLE_perDVD$MFVisit1RateH)

#scatter.smooth(MY_TABLE_perDVD$MFVisit1RateH~ MY_TABLE_perDVD$DVDInfoChickNb)
#sunflowerplot(MY_TABLE_perDVD$MFVisit1RateH~ MY_TABLE_perDVD$DVDInfoChickNb)

#cor.test(MY_TABLE_perDVD$MFVisit1RateH,MY_TABLE_perDVD$DVDInfoChickNb)

summary(MY_TABLE_perDVD$RelTimeHrs) # 6 NA's > if this covariate is use, reduce MY_TABLE_perDVD from those RelTimeHrs NAs
#scatter.smooth(MY_TABLE_perDVD$AlternationValue,MY_TABLE_perDVD$RelTimeHrs)# linear ? >linear enough to keep it as it is ?
#scatter.smooth(MY_TABLE_perDVD$RelTimeHrs,MY_TABLE_perDVD$AlternationValue)# linear ? >linear enough to keep it as it is ?
#hist(MY_TABLE_perDVD$AlternationValue)


boxcoxAlternationValue <- boxcox(lm(AlternationValue ~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE), data = MY_TABLE_perDVD))

boxcoxAlternationValue$x[boxcoxAlternationValue$y == max(boxcoxAlternationValue$y)] # 1.23

}

{# modA

modA <- lmer(AlternationValue^1.2~  
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	#MFVisit1RateH+
	scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
	# M or F PriorResidence NS
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
scatter.smooth(d$fitted, jitter(d$AlternationValue^1.2, 0.05),ylim=c(0, 100^1.2))
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

{# modA_withinIndAgeEffect

modA_withinIndAgeEffect <- lmer(AlternationValue^1.2~  

	scale(meanMumAge, scale=FALSE) + 
	scale(DeltaMumAge, scale=FALSE) +
	scale(meanDadAge, scale=FALSE) + 
	scale(DeltaDadAge, scale=FALSE) +
	#scale(MumAge, scale=FALSE)+
	#scale(DadAge, scale=FALSE)+
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

# see graph in paragraph 'create MY_TABLE_perBirdYear', 'get mean Alternation per year per BirdID'

}

}

summary(modA)

{### predictors Adev

scatter.smooth(MY_TABLE_perDVD$AlternationValue, MY_TABLE_perDVD$Adev)

modAdev <- lmer(Adev~ 	#scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
						MumAge+ DadAge+
						scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
						scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
						scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
						ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
						DiffVisit1Rate +  
						MFVisit1RateH+
						scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
						MPriorResidence+
						FPriorResidence +
						(1|BroodRef) + 
						(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
						# + (1|PairIDYear) # explain 0% of the variance
						, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])

summary(modAdev) # in ppt 20160707
}

{### predictors ratioObsvMax

hist(MY_TABLE_perDVD$RatioObsvMax)
scatter.smooth(MY_TABLE_perDVD$RatioObsvMax~MY_TABLE_perDVD$MFVisit1RateH)
scatter.smooth(MY_TABLE_perDVD$RatioObsvMax~MY_TABLE_perDVD$DiffVisit1Rate)


modRatioObsvMax <- lmer(RatioObsvMax~  MFVisit1RateH+
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + # Kat&Ben's paper: date (how was it transformed to be numeric?)
	scale(PairBroodNb, scale=FALSE) + # Kat&Ben's paper: pbdur in years (but long-tailed tits have one brood a year, sparrows, several)
	scale(DVDInfoChickNb, scale=FALSE) + # Kat&Ben's paper: use brood size d11, maybe they didn't check nest on day of recording ?
	ChickAgeCat + # rather than continuous because field protocol > measure d7 and d11, in between is when they "miss"
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + # Kat&Ben's paper: time to nearest minute (how was it transformed to be numeric?)
	# M or F PriorResidence NS
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) # this is additional compared to  Kat&Ben's paper
	# + (1|PairIDYear) # explain 0% of the variance
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs),])

summary(modRatioObsvMax)
}

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

modA_MCMCglmm <- MCMCglmm(AlternationValue^1.2~1+ParentsAge+HatchingDayAfter0401+PairBroodNb+DVDInfoChickNb+ChickAgeCat+DiffVisit1Rate+RelTimeHrs,
												random = ~BroodRef+SocialMumID+SocialDadID+PairID+BreedingYear,
												data=MY_TABLE_perDVD_wihoutNA,
												prior = prior_modA_MCMCglmm)
												#thin   = 1000,
												#burnin = 20000,
												#nitt   = 120000) # for 100 models (add one zero to nitt to get 1000 models)

summary(modA_MCMCglmm)
# plot(modA_MCMCglmm$VCV)
# autocorr(modA_MCMCglmm$VCV)
# plot(modA_MCMCglmm$Sol)

# plot.acfs <- function(x) {
  # n <- dim(x)[2]
  # par(mfrow=c(ceiling(n/2),2), mar=c(3,2,3,0))
  # for (i in 1:n) {
    # acf(x[,i], lag.max=100, main=colnames(x)[i])
    # grid()
  # }
# }
# plot.acfs(modA_MCMCglmm$VCV)


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

summary(modA)


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


summary(lmer(MeanAdev~TotalProRate+(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear), data = MY_TABLE_perBrood))
summary(lmer(Adev~MFVisit1RateH+(1|BroodRef)+(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear), data = MY_TABLE_perDVD))



modFitnessAsProRate <- lmer(TotalProRate^0.45 ~ scale(HatchingDayAfter0401, scale=FALSE) + 
												MPriorResidence +
												FPriorResidence+
												PairBroodNb +
												MeanAdev+
												(1|SocialDadID)+ 
												(1|SocialMumID) +
												(1|BreedingYear)
												 + (1|PairID)
												, data = MY_TABLE_perBrood, REML=FALSE)
summary(modFitnessAsProRate)


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

scatter.smooth(MY_TABLE_perBrood$MeanAdev, MY_TABLE_perBrood$TotalProRate)

}


modFitnessAsProRate_poly_onDVD <- lmer(MFVisit1RateH^0.45 ~  DVDInfoChickNb +
											poly(Adev,2) +
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear)
											# + (1|PairIDYear) # explain 0% of the variance
											, data = MY_TABLE_perDVD)
											
summary(modFitnessAsProRate_poly_onDVD)



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


modFitnessAsResChickMass_Adev <- lmer(ResMassTarsus ~ NbRinged + TotalProRate +
												MeanAdev + 
												#HatchingDayAfter0401 +
												#PairBroodNb +
												 (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) ,
												 data = MY_TABLE_perBrood[!is.na(MY_TABLE_perBrood$ResMassTarsus),])
										
summary(modFitnessAsResChickMass_Adev) 


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

modFitnessAsChickMassRedidualswithGenParents <- lmer(ResMassTarsus_perChick ~ NbRinged +  TotalProRate +
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


modFitnessAsChickMassRedidualswithGenParents_ADev <- lmer(ResMassTarsus_perChick ~ 
												HatchingDayAfter0401+
												PairBroodNb+
												NbRinged + 
												TotalProRate +
												MeanAdev + 
												MeanSdev+
												(1|RearingBrood)+
												#(1|SocialMumID)+ (1|SocialDadID) + 
												(1|PairID) + (1|BreedingYear) 
												+ (1|dam) + (1|sire) + (1|GenPairID)
												, data = MY_TABLE_perChick[!is.na(MY_TABLE_perChick$ResMassTarsus_perChick),])

summary(modFitnessAsChickMassRedidualswithGenParents_ADev) # added to ppt 20160708


}

}

{## number of chicks ringed

hist(MY_TABLE_perBrood$NbRinged)

# modFitnessAsNbRinged <- glmer(NbRinged ~ #scale(MeanA, scale=FALSE) + 
										# poly(MeanA,2)+
										# # (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										# (1|BreedingYear) , data = MY_TABLE_perBrood, family="poisson")

modFitnessAsNbRinged <- lmer(NbRinged ~ #scale(MeanA, scale=FALSE) + 
										#MeanAdev+
										TotalProRate+
										MeanA+
										HatchingDayAfter0401 +
										PairBroodNb +
										MBroodNb+
										FBroodNb +
										# (1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(modFitnessAsNbRinged)



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


hist(MY_TABLE_perBrood$RatioNbRingedNbHatched)
hist(MY_TABLE_perBrood$RatioNbRingedNbHatched)


modPercSurvivedChick <- lmer(RatioNbRingedNbHatched ~ MeanA + 
										TotalProRate+
										PairBroodNb+
										DadAge +
										MumAge +
										HatchingDayAfter0401 +
										MBroodNb+
										FBroodNb +
										MPriorResidence +
										FPriorResidence +
										(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
										(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(modPercSurvivedChick)

scatter.smooth(MY_TABLE_perBrood$RatioNbRingedNbHatched~MY_TABLE_perBrood$MeanA)


modPercSurvivedChick_ADev <- lmer(RatioNbRingedNbHatched ~ TotalProRate+
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
										
summary(modPercSurvivedChick_ADev) # in ppt 20160707

scatter.smooth(MY_TABLE_perBrood$RatioNbRingedNbHatched~MY_TABLE_perBrood$MeanAdev)




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


#check for overdispersion
# modSurvival_withOverdispersionAccounted <- glmer(AliveNextYear ~ MeanAYear + Sex + Age +
									# (1|BirdID) +
									# (1|BreedingYear)+
									# (1|BirdIDYear) # overdispersion parameter
									# , data = MY_TABLE_perBirdYear, family = "binomial" )
# summary(modSurvival_withOverdispersionAccounted)
# anova(modSurvival, modSurvival_withOverdispersionAccounted) # p = 0.6037


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





#############
# SYNCHRONY #
#############

{# compare to simulation correlation A-S

# ggplot(data=MY_TABLE_perDVD, aes(y=NbSynchro_ChickFeedingEquanim,x=NbAlternation) ) + 
							# geom_point() + 
							# geom_smooth(method = "lm") +
							# geom_abline(intercept=0,slope=0.5)+
							# geom_abline(intercept=0,slope=1)
							
							
# ggplot(data=MY_TABLE_perDVD, aes(y=NbSynchro_LessConspicuous,x=NbAlternation) ) + 
							# geom_point() + 
							# geom_smooth(method = "lm") +
							# geom_abline(intercept=0,slope=0.5)+
							# geom_abline(intercept=0,slope=1)	

# ggplot(data=MY_TABLE_perDVD, aes(y=SynchronyFeedValue,x=AlternationValue) ) + 
							# geom_point() + 
							# geom_smooth(method = "lm") +
							# geom_abline(intercept=0,slope=0.5)+
							# geom_abline(intercept=0,slope=1)
							
							
# ggplot(data=MY_TABLE_perDVD, aes(y=SynchronyMvtValue,x=AlternationValue) ) + 
							# geom_point() + 
							# geom_smooth(method = "lm") +
							# geom_abline(intercept=0,slope=0.5)+
							# geom_abline(intercept=0,slope=1)							
							

# hist(MY_TABLE_perDVD$AlternationValue)
}

{#### predictors of synchrony

{# check dependent and explanatory variables 
#hist(MY_TABLE_perDVD$SynchronyFeedValue, breaks =length(unique(MY_TABLE_perDVD$SynchronyFeedValue)))
hist(MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim, breaks =length(unique(MY_TABLE_perDVD$NbSynchro_ChickFeedingEquanim)))

table(MY_TABLE_perDVD$SynchronyFeedValue)

#scatter.smooth(MY_TABLE_perDVD$MFVisit1,MY_TABLE_perDVD$DiffVisit1Rate )
cor.test(MY_TABLE_perDVD$MFVisit1,MY_TABLE_perDVD$DiffVisit1Rate)

#scatter.smooth(MY_TABLE_perDVD$SynchronyFeedValue~MY_TABLE_perDVD$MFVisit1 )



# summary when synchro 0 vs non-zero

summary(MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue == 0,c("MVisit1","FVisit1","DiffVisit1Rate","MFVisit1RateH","NbAlternation","DVDInfoChickNb")])
summary(MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue != 0,c("MVisit1","FVisit1","DiffVisit1Rate","MFVisit1RateH","NbAlternation","DVDInfoChickNb")])

}

{# synchrony score > assumptions model weird

modS <- lmer(SynchronyFeedValue~  
	scale(MFVisit1, scale=FALSE) + # this is strongly correlated to DiffVisit1Rate and with chickNb and this is mathematically linked to Sync score
	scale(ParentsAge, scale=FALSE) + # this is strongly correlated to PairBroodNb
	scale(HatchingDayAfter0401, scale=FALSE) + 
	scale(PairBroodNb, scale=FALSE) + 
	scale(DVDInfoChickNb, scale=FALSE) + 
	ChickAgeCat + 
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + 
	(1|BroodRef) + 
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

## Mean of ranefs: should be zero
#mean(unlist(ranef(modS)$BroodRef))
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
ranefs_modS_nb_glmmadmb_PairID$ranefs <- as.numeric(as.character(ranefs_modS_nb_glmmadmb_PairID$ranefs))

# pairs that have twice zero for synchrony
MeanSynchronyFeedValue_perPair <- as.data.frame(MY_TABLE_perDVD %>% group_by(PairID) %>% summarise(mean(SynchronyFeedValue)))
colnames(MeanSynchronyFeedValue_perPair) <- c("PairID", "MeanSynchronyFeedValue")
MeanSynchronyFeedValue_perPair[MeanSynchronyFeedValue_perPair$MeanSynchronyFeedValue == 0,]

ranefs_modS_nb_glmmadmb_PairID$PairswithMeanS0 <- ifelse(ranefs_modS_nb_glmmadmb_PairID$PairID %in%  MeanSynchronyFeedValue_perPair$PairID[MeanSynchronyFeedValue_perPair$MeanSynchronyFeedValue == 0], 0, 1)
ggplot(ranefs_modS_nb_glmmadmb_PairID, aes(PairID,ranefs, colour = as.factor(PairswithMeanS0))) + geom_point()


}

{## Gamma hurdle model with continuous data : NOT WORKING
# http://seananderson.ca/2014/05/18/gamma-hurdle.html

# MY_TABLE_perDVD$SynchroFeed_non_zero <- ifelse(MY_TABLE_perDVD$SynchronyFeedValue > 0, 1, 0)
# ggplot(MY_TABLE_perDVD, aes(DVDRef, SynchronyFeedValue, colour = as.factor(SynchroFeed_non_zero))) + geom_point()

# modS1_Logistic <- glmer(SynchroFeed_non_zero ~ MFVisit1 +# this is strongly correlated to DiffVisit1Rate
												# ParentsAge + # this is strongly correlated to PairBroodNb
												##HatchingDayAfter0401 + 
												##PairBroodNb + 
												# DVDInfoChickNb + 
												# ChickAgeCat + 
												# DiffVisit1Rate +  
												##RelTimeHrs + 
												##(1|BroodRef) + 
												##(1|SocialMumID)+ 
												# (1|SocialDadID) + 
												# (1|PairID)
												#+(1|BreedingYear) 
												# , data = MY_TABLE_perDVD, family = binomial(link = logit))

# summary(modS1_Logistic)

# modS2_Gamma <- glm(SynchronyFeedValue ~ # MFVisit1 +# this is strongly correlated to DiffVisit1Rate and this is mathematically linked to Sync score
										# ParentsAge + # this is strongly correlated to PairBroodNb
										##HatchingDayAfter0401 + 
										##PairBroodNb + 
										# DVDInfoChickNb + 
										# ChickAgeCat + 
										# DiffVisit1Rate 
										##scale(RelTimeHrs, scale=FALSE) + 
										##(1|BroodRef) + 
										##(1|SocialMumID)+ (1|SocialDadID) 
										##+(1|PairID) 
										##+ (1|BreedingYear) 
										# , data = subset(MY_TABLE_perDVD, MY_TABLE_perDVD$SynchroFeed_non_zero == 1), family = Gamma(link = log))

# summary(modS2_Gamma)	# can't make the glmer to converge
}
	
}

summary(modS_nb_glmmadmb)

{# predictors of Sdev

scatter.smooth(MY_TABLE_perDVD$SynchronyFeedValue, MY_TABLE_perDVD$Sdev)


modSdev <- lmer(Sdev~  
	scale(MFVisit1, scale=FALSE) + # this is strongly correlated to DiffVisit1Rate and with chickNb and this is mathematically linked to Sync score
	MumAge + 
	DadAge +
	scale(HatchingDayAfter0401, scale=FALSE) + 
	scale(PairBroodNb, scale=FALSE) + 
	scale(DVDInfoChickNb, scale=FALSE) + 
	FPriorResidence +
	MPriorResidence+
	ChickAgeCat + 
	DiffVisit1Rate +  
	scale(RelTimeHrs, scale=FALSE) + 
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + (1|BreedingYear) 
	, data = MY_TABLE_perDVD[!is.na(MY_TABLE_perDVD$RelTimeHrs) & !is.na(MY_TABLE_perDVD$ParentsAge),])

summary(modSdev) # in ppt 20160707

}


{#### fitness benefits of synchrony > meaningless ?

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


mod_Sync_FitnessAsNbRinged <- lmer(NbRinged ~ scale(MeanSynchroFeed, scale=FALSE) + 
												scale(TotalProRate, scale=FALSE) +
												scale(PairBroodNb, scale=FALSE) +
												#(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
												(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(mod_Sync_FitnessAsNbRinged)

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
mod_Sync_nb_FitnessAsNbRinged <- lmer(NbRinged ~ scale(MeanSynchroFeed_nb, scale=FALSE) +
												scale(TotalProRate, scale=FALSE) + 
												scale(PairBroodNb, scale=FALSE) +
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


{# Sdev

mod_Sdev_FitnessAsNbRinged <- lmer(NbRinged ~ MeanSdev+
											scale(TotalProRate, scale=FALSE) + 
											scale(PairBroodNb, scale=FALSE) +
											(1|SocialMumID)+ (1|SocialDadID) + (1|PairID) + 
											(1|BreedingYear) , data = MY_TABLE_perBrood)
										
summary(mod_Sdev_FitnessAsNbRinged)



}

}

}

summary(mod_Sync_FitnessAsNbRinged)


{#### proportion of synchronous visits where female enters first > repeatability within pair could induce alternation

mod_proportionSexStartSynchro <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ #MFmeanDuration+MFVisit1RateH + 
													(1|BroodRef) +
													(1|PairID)
													 #+(1|DVDRef) 
													, data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")

summary(mod_proportionSexStartSynchro)

mod_proportionSexStartSynchro <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~  
													#MFmeanDuration+MFVisit1RateH + 
													(1|BroodRef) +
													(1|PairID)
													 #+(1|DVDRef) 
													, data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")

summary(mod_proportionSexStartSynchro)



{# model assumptions checking

# # check for overdispersion
# mod_proportionSexStartSynchro_overdisp <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ MFmeanDuration+MFVisit1RateH + 
												# (1|BroodRef) +
												# (1|PairID)
												 # +(1|DVDRef) 
												# , data=MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >3,], family ="binomial")
# summary(mod_proportionSexStartSynchro_overdisp)
# anova(mod_proportionSexStartSynchro_overdisp,mod_proportionSexStartSynchro)

## check for overdispersion
# mod_proportionSexStartSynchro_overdisp <- glmer(cbind(NbSynchroFemaleStart,NbSynchroMaleStart) ~ MFmeanDuration+MFVisit1RateH + 
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
# scatter.smooth(MY_TABLE_perDVD$MFVisit1RateH[MY_TABLE_perDVD$SynchronyFeedValue >0], resid(mod_proportionSexStartSynchro))
# abline(h=0, lty=2)

# # data vs. fitted ?							
# d <- MY_TABLE_perDVD[MY_TABLE_perDVD$SynchronyFeedValue >0,]
# d$fitted <- fitted(mod_proportionSexStartSynchro)
# scatter.smooth(d$fitted, jitter(d$NbSynchroFemaleStart/(d$NbSynchroFemaleStart+d$NbSynchroMaleStart), 0.05),ylim=c(0, 1))
# abline(0,1)	

# # data and fitted against all predictors
# scatter.smooth(d$MFmeanDuration,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="proportion of synchronous visits where female enters first", xlab="MFmeanDuration")	
# scatter.smooth(d$MFVisit1RateH,d$fitted,  las=1, cex.lab=1.4, cex.axis=1.2, ylab="proportion of synchronous visits where female enters first", xlab="MFVisit1RateH")	

}

}

summary(mod_proportionSexStartSynchro)

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
									##MeanDiffVisit1Rate +  
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
									# # MeanDiffVisit1Rate +  
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



