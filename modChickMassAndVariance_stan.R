#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Joel PICK      joel.l.pick@gmail.com
#	 Analyse provisioning data sparrows: chick mass and variance in Stan
#	 Start : July 2019
#	 last modif : 20190822
#	 commit: cleaning up code to run in Malika's github repo environement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

#options(width=Sys.getenv("COLUMNS"), stringsAsFactors=FALSE)
options(stringsAsFactors=FALSE)

library(MCMCglmm)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(MasterBayes)
library(here)
library(rstudioapi)

## function to make pedigree for stan
factorisePed <- function(pedigree, unknown=0){
    new_ped <- data.frame(
        1:nrow(pedigree), 
        ifelse(is.na(pedigree[,2]),unknown,match(pedigree[,2], pedigree[,1])), 
        ifelse(is.na(pedigree[,3]),unknown,match(pedigree[,3], pedigree[,1]))
        )
    colnames(new_ped) <- colnames(pedigree)[1:3]

    return(new_ped)
}


## whether to run model or previous run
run_model <- FALSE


## load in data
dd <- read.csv(paste(here(),"R_Selected&RandomizedData/R_MY_TABLE_perChick.csv", sep="/")) 
head(dd)
nrow(dd)

## remove those without a tarsus length
dd <- subset(dd, !is.na(AvgOfTarsus))

## scale continuous variables 
dd$AvgOfTarsusS <- as.numeric(scale(dd$AvgOfTarsus))
dd$MeanTotalProRateS <- as.numeric(scale(dd$MeanTotalProRate))
dd$HatchingDayAfter0401S <- as.numeric(scale(dd$HatchingDayAfter0401))
dd$PairBroodNbS <- as.numeric(scale(dd$PairBroodNb))
dd$NbRingedS <- as.numeric(scale(dd$NbRinged))
dd$MeanLogAdevS  <- as.numeric(scale(dd$MeanLogAdev))
dd$MeanLogSdevS <- as.numeric(scale(dd$MeanLogSdev))
dd$XPriorResidence <-as.factor(dd$XPriorResidence)
dd$CrossFosteredYN <-as.factor(dd$CrossFosteredYN)

## make random effects integers from 1 to n levels
dd$Rearing_id <- as.numeric(as.factor(dd$RearingBrood))
dd$Natal_id <- as.numeric(as.factor(dd$NatalBrood))
dd$Pair_id <- as.numeric(as.factor(dd$PairID))
dd$Year_id <- as.numeric(as.factor(dd$BreedingYear))

## data for within brood variance analysis
dd_rearing <- aggregate(cbind(AvgOfMass,MeanLogSdevS,NbRingedS)~Rearing_id,dd,mean)

## fixed effects design matrix for mean
X<-model.matrix(AvgOfMass~ 
	AvgOfTarsusS +
	MeanTotalProRateS +
	I(MeanTotalProRateS^2)+
	HatchingDayAfter0401S +
	PairBroodNbS +
	NbRingedS +
	MeanLogAdevS + 
	MeanLogSdevS +
	XPriorResidence + 
	CrossFosteredYN,dd
	)

## fixed effects design matrix for within brood variance
XV<-model.matrix(AvgOfMass~ 
	NbRingedS +
	MeanLogSdevS +
	NbRingedS : MeanLogSdevS
	,dd_rearing #not dd
	)

if(run_model){

	## import and format pedigree
	pedigree <-  read.table(file= paste("R_input/Pedigree_20160309.txt", sep="/"), sep='\t', header=T)  ## !!! to update when new pedigree !!! 
	
	ped <- insertPed(prunePed(orderPed(pedigree[,1:3]), dd$ChickID, make.base = TRUE))
	head(ped)
	ped2 <- rbind(data.frame(id=dd$ChickID[! dd$ChickID %in% ped[,1]], dam=NA, sire=NA), ped)

	## scaler of Mendelian sampling variance
	MSV <- inverseA(factorisePed(ped2[,1:3], unknown=NA))$dii

	## pedigree for stan
	stan_ped <- factorisePed(ped2[,1:3])

	## indexes for groups of individuals with different known relationships, for implementation of animal model in stan
	NoParents <- which(stan_ped[,2]==0 & stan_ped[,3]==0)
	ParentsOffspring <- which(stan_ped[,1] %in% c(stan_ped[,2],stan_ped[,3]) & !stan_ped[,1] %in% NoParents)
	ParentsNoOffspring <- which(!stan_ped[,1] %in% c(NoParents,ParentsOffspring))
	length(NoParents); length(ParentsOffspring); length(ParentsNoOffspring)

	animal_id <- stan_ped[match(as.character(dd$ChickID), ped2$id),"id"]

	stan_ped2 <- rbind(c(0,-1,-1), stan_ped)+1

	stan_data <- list(
		N=nrow(dd),
		y=dd$AvgOfMass, 
		J=ncol(X), 
		X=X,
		K=ncol(XV), 
		XV=XV,
		MSV=c(1,MSV),

		N_Rearing=length(unique(dd$RearingBrood)),
		N_Natal=length(unique(dd$NatalBrood)),
		N_Pair=length(unique(dd$PairID)),
		N_Year=length(unique(dd$BreedingYear)),
		N_Ped = nrow(stan_ped2), 
		N_NoParents=length(NoParents), 
		N_ParentsOffspring=length(ParentsOffspring), 
		N_ParentsNoOffspring=length(ParentsNoOffspring), 

		Rearing_id = dd$Rearing_id,
		Natal_id = dd$Natal_id,
		Pair_id = dd$Pair_id,
		Year_id = dd$Year_id,
		animal_id = animal_id+1, 
		dam = stan_ped2$dam,
		sire = stan_ped2$sire,
		NoParents=NoParents+1, 
		ParentsOffspring=ParentsOffspring+1,
		ParentsNoOffspring=ParentsNoOffspring+1
		)

	stanModel <- stan_model(file = paste0(here(),"/R_input/DHGLM_animal_model_reduced.stan"))

	mod_stan <- sampling(
		stanModel, 
		data = stan_data, 
		pars =c("beta", "betaV", "sigma_Natal", "sigma_Rearing", "sigma_Year", "sigma_Pair", "sigma_A", "sigma_Rearing_V"),
		chains=4, 
		iter = 30000, 
		warmup = 15000)
	save(X,XV,mod_stan, file= paste0(here(),"/stanModDHGLM_MI",format(Sys.time(), "%Y%m%d_%H%M"),".Rdata"))
}else{
	load(paste0(here(),"/Figures&Rmd/stanModDHGLM20190730_1412.Rdata"))
}

## summary and diagnostics
summary(mod_stan)$summary[,c(4,1,8,9,10)]
rstan::traceplot(mod_stan, pars=c("sigma_Natal", "sigma_Rearing", "sigma_Year", "sigma_Pair", "sigma_A", "sigma_Rearing_V","lp__"))
pairs(mod_stan, pars=c("sigma_Natal", "sigma_Rearing", "sigma_Year", "sigma_Pair", "sigma_A", "sigma_Rearing_V","lp__"))
rstan::traceplot(mod_stan, pars=c("betaV"))
rstan::traceplot(mod_stan, pars=c("beta"))

## extract chains
out <- extract(mod_stan, permute=FALSE)

## fixed effects table for mean
fixed_mean <- round(cbind(summary(mod_stan)$summary[1:11,c(1,4,8)],pMCMC=apply(out[,,1:11], 3, function(x) if(sum(x>0)>sum(x<0)){sum(x<0)/length(x)}else{sum(x>0)/length(x)})*2
),3)
rownames(fixed_mean) <- colnames(X)

## fixed effects table for within brood variance
fixed_var <- round(cbind(summary(mod_stan)$summary[12:15,c(1,4,8)],pMCMC=apply(out[,,12:15], 3, function(x) if(sum(x>0)>sum(x<0)){sum(x<0)/length(x)}else{sum(x>0)/length(x)})*2
),3)
rownames(fixed_var) <- colnames(XV)

## random effects table
RE <- round(summary(mod_stan)$summary[16:21,c(1,4,8)],3)
