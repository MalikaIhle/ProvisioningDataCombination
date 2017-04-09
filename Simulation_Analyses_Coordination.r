#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE & Joel PICK  malika_ihle@hotmail.fr & joel.l.pick@gmail.com
#	 Simulation to help decide which analyses to perfom on coordination in provisioning in pairs of sparrows
#	 Start : 02/02/2017
#	 last modif : 22/03/2017
#	 commit: calling one datafile + preparation for running on iceberg
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))



{# Packages
library(pbapply)
library(lme4)
library(MASS)
options(scipen=999) # to remove scientific notation e-
# options(scipen=0) # to put it back
library(tidyr)
library(dplyr) 
library(ggplot2)
require(gridExtra)
require(grid)
}

{# Get real data as selected for DataAnalyses.R

RawInterfeeds <- read.csv(paste("R_Selected&RandomizedData","R_RawInterfeeds.csv", sep="/")) 
MY_TABLE_perDVD <- read.csv(paste("R_Selected&RandomizedData","R_MY_TABLE_perDVD.csv", sep="/")) 


}

{# Functions

check_integer <- function(x) {x == round(x)}
median_integer <- function(x) {if (check_integer(median(x)) == TRUE) {return(median(x))} else {return(as.integer(median(x) +sample(c(0.5,-0.5),1)))}}

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 

Create_one_nest_watch <- function (x){

MaleVisits <- sort(runif(x$MaleP,0,VideoLength)) # MaleP is male number of provisioning visits
FemaleVisits <- sort(runif(x$FemaleP,0,VideoLength))
MaleIntervals <- c(0,diff(MaleVisits))
FemaleIntervals <- c(0,diff(FemaleVisits))

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- x$DVDRef
dat 

}

Create_one_fully_sorted_nest_watch <- function (x){

MaleVisits <- sort(runif(x$MaleP,0,VideoLength)) # MaleP is male number of provisioning visits
FemaleVisits <- sort(runif(x$FemaleP,0,VideoLength))
MaleIntervals <- sort(c(0,diff(MaleVisits))) # interval associated to the first visit is zero like in real dataset
FemaleIntervals <- sort(c(0,diff(FemaleVisits)))
MaleVisits <- MaleVisits[1] + cumsum(MaleIntervals) # recalculate Tstart based on sorted intervals
FemaleVisits <- FemaleVisits[1] + cumsum(FemaleIntervals)

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- x$DVDRef
dat 

}

Convert_CN_2_r <- function(oneCN){
rseq <- seq(from=0,to=1, length.out=max(CN))
r <- rseq[oneCN]
return(r)
}

Create_one_partially_sorted_nest_watch <- function (x){

MaleVisits <- sort(runif(x$MaleP,0,VideoLength)) # MaleP is male number of provisioning visits
FemaleVisits <- sort(runif(x$FemaleP,0,VideoLength))

if (length(MaleVisits) == 1 | length(FemaleVisits) == 1){
Create_one_fully_sorted_nest_watch(x)}

# pick one sex at random to be fully sorted
else{sex_fully_sorted <- sample(c(1,0),1) 

if (sex_fully_sorted == 1) {
MaleIntervals <- sort(c(0,diff(MaleVisits))) # first interval is zero like in real data
MaleVisits <- MaleVisits[1] + cumsum(MaleIntervals) # recalculate Tstart based on sorted intervals
}
if (sex_fully_sorted == 0) {
FemaleIntervals <- sort(c(0,diff(FemaleVisits)))
FemaleVisits <- MaleVisits[1] + cumsum(FemaleIntervals)
}

# the other sex to be partially sorted
if(sex_fully_sorted == 0) {sex_partially_sorted <- 1} else(sex_partially_sorted <- 0)
rCNint <- Convert_CN_2_r(x$CN) # strength of the sorting = correlation to CN

if (sex_partially_sorted == 1) {
MaleIntervals <- sort(diff(MaleVisits))
seq_intervalNb <- 1:length(MaleIntervals)
sorting_parameter <- rCNint*seq_intervalNb + rnorm(length(seq_intervalNb), 0, sqrt(1-rCNint^2))
MaleVisits <- MaleVisits[1] + cumsum(MaleIntervals[order(sorting_parameter)])# recalculate Tstart based on partially sorted intervals
MaleIntervals <- c(0,diff(MaleVisits)) # first interval is zero like in real data
}
if (sex_partially_sorted == 0) {
FemaleIntervals <- sort(diff(FemaleVisits))
seq_intervalNb <- 1:length(FemaleIntervals)
sorting_parameter <- rCNint*seq_intervalNb + rnorm(length(seq_intervalNb), 0, sqrt(1-rCNint^2))
FemaleVisits <- FemaleVisits[1] + cumsum(FemaleIntervals[order(sorting_parameter)])# recalculate Tstart based on partially sorted intervals
FemaleIntervals <- c(0,diff(FemaleVisits)) # first interval is zero like in real data
}

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),
						cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- x$DVDRef
dat 

}
}


Calculate_AMax <- function(x){
if (x$MaleP == x$FemaleP) {x$AMax <- x$TotalP - x$DiffP -1} else {x$AMax <- x$TotalP - x$DiffP}
return(x)
}

Calculate_A_S_one_nest_watch <- function(x){
A <- sum(diff(x$Sex)!=0)
S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= syncint)
data.frame(cbind(DVDRef=unique(x$DVDRef),A,S))
}

Calculate_A_one_nest_watch <- function(x){
A <- sum(diff(x$Sex)!=0)
A
}

Calculate_S_one_nest_watch <- function(x){
S <- S <- sum(diff(x$Sex)!=0 & diff(x$Tstart) <= syncint)
S
}


Randomize_one_nest_watch <-  function(x){

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
}

Randomize_Data_WithinFile_and_Calculate_A_S <- function(RawData) { 

SimData <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),Randomize_one_nest_watch))
SimData_A <- do.call(rbind,lapply(split(SimData,SimData$DVDRef), Calculate_A_one_nest_watch ))
SimData_S <- do.call(rbind,lapply(split(SimData,SimData$DVDRef), Calculate_S_one_nest_watch ))

return(rbind(SimData_A, SimData_S)) # the 'length(unique(DVDRef))' first row are Asim, the other half are Ssim
}


Generate_TP <- function(nPR, sdlogPR,rCNTP){

MalePexp <- rlnorm(nPR, meanlog = meanlogPR, sdlog = sdlogPR ) # expected number of visits
FemalePexp <- rlnorm(nPR, meanlog = meanlogPR, sdlog = sdlogPR )

MaleP <- NULL
FemaleP <- NULL

for (i in 1: length(MalePexp)){ # for my analyses I selected videos where both partners visited at least once
MaleP5 <- rpois(5, MalePexp[i])  # potential realized number of visits including the poisson distributed error
MaleP[i] <- sample(MaleP5[MaleP5>0],1)  # picking one potential MaleP that is not zero (excluded in observed data)
FemaleP5 <- rpois(5, FemalePexp[i])
FemaleP[i] <- sample(FemaleP5[FemaleP5>0],1)
}

return(data.frame(DVDRef = seq(1:nPR), TotalP = MaleP+FemaleP, DiffP = abs(FemaleP-MaleP), FemaleP = FemaleP, MaleP = MaleP))

}

Generate_TP_correlated_to_CN <- function(nPR, sdlogPR,rCNTP, CN){
maleE <- rnorm(nPR,0, sqrt(sdlogPR^2*(1-rCNTP^2)))
femaleE <- rnorm(nPR,0, sqrt(sdlogPR^2*(1-rCNTP^2)))

MalePexp <- exp(aCNTP + bCNTP * CN + maleE)
FemalePexp <- exp(aCNTP + bCNTP * CN + femaleE)

MaleP <- NULL
FemaleP <- NULL

for (i in 1: length(MalePexp)){ # for my analyses I selected videos where both partners visited at least once
MaleP5 <- rpois(5, MalePexp[i])
MaleP[i] <- sample(MaleP5[MaleP5>0],1)
FemaleP5 <- rpois(5, FemalePexp[i])
FemaleP[i] <- sample(FemaleP5[FemaleP5>0],1)
}

return(data.frame(DVDRef = seq(1:nPR), CN = CN, TotalP = MaleP+FemaleP, DiffP = abs(FemaleP-MaleP), FemaleP = FemaleP, MaleP = MaleP))

}


Shape_results <- function(MY_Results, TypeNumber){

sorted_results <- lapply(MY_Results, function(x){ 
subx <- x[,-1] 
subx <- subx[ , order(names(subx))] 
cbind(Factor=x[,1], subx)
})

Signi_results <- lapply(sorted_results, function(x){
x_p <- x[,-(1:((ncol(x)+1)/2))]
x_e <- x[,c(-1,-(((ncol(x)-1)/2+2):ncol(x)))]

cbind(x_p < 0.05, x_e)
})

results_Sign_compiled <- Reduce('+',Signi_results)/NreplicatesSimulation[TypeNumber]
results_Sign_compiled <- cbind(results_Sign_compiled[,c(-(((ncol(results_Sign_compiled))/2+1):ncol(results_Sign_compiled)))]*100,
										results_Sign_compiled[,-(1:((ncol(results_Sign_compiled))/2))])
results_Sign_compiled <- round(results_Sign_compiled,2)
results_PercentageFactorSignificant <- data.frame(Factor=MY_Results[[1]]$Factor,results_Sign_compiled)

return(results_PercentageFactorSignificant)

}

}

{# Parameter values for above functions

VideoLength <- 90

nPR <- nrow(MY_TABLE_perDVD)
avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))

meanlogPR <- log(avPR) # pass on the log scale to be able to add Poisson distributed error
sdlogPR <- sqrt(log(1 + sdPR^2/avPR^2))

CN <- MY_TABLE_perDVD$DVDInfoChickNb     # fixed given data for chick number
rCNTP <- 0.6

bCNTP <- (rCNTP * sdlogPR)/ sd(CN)
aCNTP <- meanlogPR - mean(CN)*bCNTP

syncint <- 2 # I tried 10 ; 5 ; 2 ; 0.5  I had a priori chosen 0.5 but this gives false-postivie results (too many zeros leading to false convergence ?), for 2, 5, 10 this works well
}

set.seed(21)

NreplicatesWithinFileRandomization <- 10
NreplicatesSimulation <- 1

{# Simulation 1: Take observed CN ; generate TP either correlated to CN or not ; with or without an effect of CN on A (i.e. with CN = sorting parameter of intervals)

Generate_TP_randomize_data_and_analyse <-function(autocorrelation, correlation_CN_TP){

# generate TP correlated to existing CN and create MY_TABLE_per_DVD

if (correlation_CN_TP == 'No'){
MY_TABLE_per_DVD <- Generate_TP(nPR, sdlogPR,rCNTP)
MY_TABLE_per_DVD$CN <- sample(CN)
}

if (correlation_CN_TP == 'Yes'){
MY_TABLE_per_DVD <- Generate_TP_correlated_to_CN(nPR, sdlogPR,rCNTP,CN)
}

MY_TABLE_per_DVD <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Calculate_AMax))
head(MY_TABLE_per_DVD)

# create nest watches

if (autocorrelation == 'none'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_nest_watch))
}

if (autocorrelation == 'partial'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_partially_sorted_nest_watch))
}

if (autocorrelation == 'full'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_fully_sorted_nest_watch))
}

head(full_dat)

# Randomization Within nest watch, within individual
A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S(full_dat),simplify=FALSE ) )
# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef)))) 
# second half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# summarise observed and simulated coordination
summary_A_S_observed <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),Calculate_A_S_one_nest_watch))
summary_A_S_randomized <- data.frame(DVDRef = out_Asim_within_df[,1], 
									MedAsim = apply(out_Asim_within_df[,-1],1,median_integer), # Median are used for having integer for poisson models
									MedSsim= apply(out_Ssim_within_df[,-1],1,median_integer),
									MeanAsim = apply(out_Asim_within_df[,-1],1,mean),
									MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))

{# create table short and long

# short table
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_observed, by='DVDRef')
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_randomized, by='DVDRef')
MY_TABLE_per_DVD$Adev <- MY_TABLE_per_DVD$A - MY_TABLE_per_DVD$MedAsim
MY_TABLE_per_DVD$Sdev <- MY_TABLE_per_DVD$S - MY_TABLE_per_DVD$MedSsim 
head(MY_TABLE_per_DVD)

# long table
MY_TABLE_per_DVD_long <- rbind(MY_TABLE_per_DVD, MY_TABLE_per_DVD)
# first half is 'simulated', second half is 'observed'
MY_TABLE_per_DVD_long$Type <- c(rep('a_Sim', nrow(MY_TABLE_per_DVD)), rep('z_Obsv', nrow(MY_TABLE_per_DVD)))
MY_TABLE_per_DVD_long$A[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- MY_TABLE_per_DVD_long$MedAsim[MY_TABLE_per_DVD_long$Type == 'a_Sim'] # A sim is MedAsim
MY_TABLE_per_DVD_long$S[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- MY_TABLE_per_DVD_long$MedSsim[MY_TABLE_per_DVD_long$Type == 'a_Sim']
MY_TABLE_per_DVD_long$rowID <- seq(1:nrow(MY_TABLE_per_DVD_long))
head(MY_TABLE_per_DVD_long)
tail(MY_TABLE_per_DVD_long)

}

{# analyses

modA_simple <- glmer(A~ scale(CN) + scale(TotalP) + scale(DiffP)+(1|DVDRef) ,data=MY_TABLE_per_DVD,family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modA <- glmer(A~ Type*scale(CN) + Type*scale(TotalP) + Type*scale(DiffP) +(1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modS <- glmer(S~ Type*scale(CN) + Type*scale(TotalP) + Type*scale(DiffP) +(1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modAdev <- lm(Adev~ scale(CN) + scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modSdev <- lm(Sdev~ scale(CN) + scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modAbin <- glmer(cbind(A, AMax-A) ~ Type*scale(CN) + Type*scale(TotalP) + Type*scale(DiffP) +(1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'binomial') 
modABen <- lm(I(A/(TotalP-1)) ~ scale(DiffP) + scale(CN), data=MY_TABLE_per_DVD) 

}

{# results

resultsGLMER <- data.frame(Factor = rownames(summary(modA)$coeff), 
						e_modA = summary(modA)$coeff[,1], 
						p_modA = round(summary(modA)$coeff[,4],3),
						e_modS = summary(modS)$coeff[,1], 
						p_modS = round(summary(modS)$coeff[,4],3),
						e_modAbin = summary(modAbin)$coeff[,1], 
						p_modAbin = round(summary(modAbin)$coeff[,4],3))
rownames(resultsGLMER) <- NULL
				
resultsLM <- data.frame(Factor = rownames(summary(modAdev)$coeff), 
						e_modAdev = summary(modAdev)$coeff[,1], 
						p_modAdev = round(summary(modAdev)$coeff[,4],3),
						e_modSdev = summary(modSdev)$coeff[,1], 
						p_modSdev = round(summary(modSdev)$coeff[,4],3),
						e_modA_simple = summary(modA_simple)$coeff[,1], 
						p_modA_simple = round(summary(modA_simple)$coeff[,4],3))
rownames(resultsLM) <- NULL

resultsBen <- data.frame(Factor = rownames(summary(modABen)$coeff),					
						e_modABen  = summary(modABen)$coeff[,1],
						p_modA_Ben = round(summary(modABen)$coeff[,4],3)	
						)
rownames(resultsBen) <- NULL

results <- merge(resultsGLMER, resultsLM, by='Factor', all.x=TRUE)
results <- merge(results, resultsBen, by='Factor', all.x=TRUE)


}

return(list(results))
}

result_no_autocor_no_cor <- pbreplicate(NreplicatesSimulation,Generate_TP_randomize_data_and_analyse('none','No')) # sim_1_1
result_no_autocor_corCN <- pbreplicate(NreplicatesSimulation,Generate_TP_randomize_data_and_analyse('none','Yes')) # sim_1_2
result_full_autocor_no_cor <- pbreplicate(NreplicatesSimulation,Generate_TP_randomize_data_and_analyse('full','No'))# sim_1_3
result_full_autocor_corCN <- pbreplicate(NreplicatesSimulation,Generate_TP_randomize_data_and_analyse('full','Yes'))# sim_1_4
result_partial_autocor_no_cor <- pbreplicate(NreplicatesSimulation,Generate_TP_randomize_data_and_analyse('partial','No')) # sim_1_5
result_partial_autocor_corCN <- pbreplicate(NreplicatesSimulation,Generate_TP_randomize_data_and_analyse('partial','Yes'))# sim_1_6


Results_Sim_1 <- c(
list(Shape_results(result_no_autocor_no_cor)),
list(Shape_results(result_no_autocor_corCN)),
list(Shape_results(result_partial_autocor_no_cor)),
list(Shape_results(result_partial_autocor_corCN)),
list(Shape_results(result_full_autocor_no_cor)),
list(Shape_results(result_full_autocor_corCN)))

{# outputs

{## Nrand = 20, Nsim = 700, sync int = 0.5

# > Shape_results(result_no_autocor_no_cor)
                    # Factor      p_modA p_modA_simple   p_modAbin p_modAdev      p_modS p_modSdev         e_modA e_modA_simple      e_modAbin    e_modAdev        e_modS     e_modSdev
# 1              (Intercept) 100.0000000    100.000000 100.0000000  9.571429 100.0000000 89.142857  2.60483024646  2.6128870483  0.62943948545  0.036006533  0.7621881037  0.1194232422
# 2                scale(CN)   0.8571429      3.571429   1.0000000  5.142857   0.2857143  4.714286 -0.00031289728 -0.0006104923  0.00001781527 -0.004229667 -0.0006390490 -0.0006670589
# 3             scale(DiffP) 100.0000000    100.000000 100.0000000 12.142857 100.0000000 10.428571 -0.19706578487 -0.1907153603  0.50596782917 -0.045183800 -0.2018723112  0.0041424174
# 4            scale(TotalP) 100.0000000    100.000000 100.0000000 13.142857 100.0000000 19.142857  0.43882437396  0.4231687578 -0.19526064708  0.019400093  0.7109607461 -0.0282661821
# 5               Typez_Obsv   0.0000000            NA   0.4285714        NA  83.8571429        NA  0.00253861644            NA  0.00572226552           NA  0.0621844673            NA
# 6     Typez_Obsv:scale(CN)   0.0000000            NA   0.0000000        NA   0.2857143        NA -0.00028951653            NA -0.00080236850           NA -0.0001767647            NA
# 7  Typez_Obsv:scale(DiffP)   0.0000000            NA   0.8571429        NA   0.4285714        NA -0.00215128648            NA -0.00703103388           NA  0.0066624011            NA
# 8 Typez_Obsv:scale(TotalP)   0.0000000            NA   0.4285714        NA  17.4285714        NA  0.00007037868            NA -0.00030264864           NA -0.0270257218            NA

# > Shape_results(result_no_autocor_corCN)
                    # Factor p_modA p_modA_simple   p_modAbin p_modAdev      p_modS p_modSdev         e_modA e_modA_simple     e_modAbin    e_modAdev       e_modS     e_modSdev
# 1              (Intercept)    100           100 100.0000000 12.571429 100.0000000 86.857143  2.62745146734    2.63935892  0.5292597412  0.042807289  0.778854509  0.1188138216
# 2                scale(CN)    100           100   1.0000000  4.571429  99.7142857  4.285714  0.03962019149    0.04347558 -0.0004753185  0.001832702  0.082953048 -0.0028057448
# 3             scale(DiffP)    100           100 100.0000000  9.714286 100.0000000 11.142857 -0.13508611846   -0.12712027  0.3986553802 -0.035366825 -0.130313154  0.0003955507
# 4            scale(TotalP)    100           100 100.0000000 10.571429 100.0000000 17.714286  0.43109052170    0.40428713 -0.1760524571  0.014532418  0.694931753 -0.0269963953
# 5               Typez_Obsv      0            NA   1.7142857        NA  80.7142857        NA  0.00309943375            NA  0.0077443076           NA  0.061250979            NA
# 6     Typez_Obsv:scale(CN)      0            NA   0.0000000        NA   0.5714286        NA -0.00009594692            NA -0.0001798294           NA -0.011118879            NA
# 7  Typez_Obsv:scale(DiffP)      0            NA   0.2857143        NA   0.2857143        NA -0.00149673596            NA -0.0037872301           NA  0.002523535            NA
# 8 Typez_Obsv:scale(TotalP)      0            NA   0.2857143        NA   2.0000000        NA -0.00022630826            NA -0.0012877870           NA -0.017843765            NA

# > Shape_results(result_partial_autocor_no_cor)
                    # Factor    p_modA p_modA_simple p_modAbin p_modAdev    p_modS p_modSdev      e_modA e_modA_simple   e_modAbin e_modAdev      e_modS  e_modSdev
# 1              (Intercept) 100.00000           100 100.00000       100 100.00000       100  2.56846133    2.74179078  0.52067800  3.079346  0.72364184  1.5452665
# 2                scale(CN)  25.14286           100  49.71429       100  33.85714       100  0.01102557    0.07941813  0.02412257  1.216972  0.02823590  0.5448820
# 3             scale(DiffP) 100.00000           100 100.00000       100 100.00000       100 -0.22207513   -0.27318761  0.40747310 -2.215145 -0.22649440 -0.6988953
# 4            scale(TotalP) 100.00000           100 100.00000       100 100.00000       100  0.47291743    0.51898420 -0.11398535  3.348504  0.77096291  1.7897549
# 5               Typez_Obsv 100.00000            NA 100.00000        NA 100.00000        NA  0.16652183            NA  0.56021754        NA  0.41701994         NA
# 6     Typez_Obsv:scale(CN) 100.00000            NA 100.00000        NA 100.00000        NA  0.06849234            NA  0.28844535        NA  0.12049067         NA
# 7  Typez_Obsv:scale(DiffP) 100.00000            NA  71.14286        NA  49.00000        NA -0.06273255            NA -0.05539350        NA -0.03497615         NA
# 8 Typez_Obsv:scale(TotalP) 100.00000            NA 100.00000        NA  59.14286        NA  0.06590150            NA  0.25273048        NA  0.03606849         NA

# > Shape_results(result_partial_autocor_corCN)
                    # Factor p_modA p_modA_simple  p_modAbin p_modAdev     p_modS p_modSdev      e_modA e_modA_simple   e_modAbin  e_modAdev      e_modS  e_modSdev
# 1              (Intercept)    100           100 100.000000       100 100.000000 100.00000  2.58824596     2.7868724  0.43483474  4.0358931  0.72284253  2.0506550
# 2                scale(CN)    100           100  66.571429       100 100.000000  56.28571  0.05393606     0.1135108  0.03531841  0.7842518  0.11405236  0.1529490
# 3             scale(DiffP)    100           100 100.000000       100 100.000000 100.00000 -0.15201406    -0.2013129  0.34087195 -2.3708760 -0.14857229 -0.6919257
# 4            scale(TotalP)    100           100 100.000000       100 100.000000 100.00000  0.46491292     0.5044226 -0.12010872  4.6174386  0.76070179  2.6589192
# 5               Typez_Obsv    100            NA 100.000000        NA 100.000000        NA  0.19189841            NA  0.63326558         NA  0.46662661         NA
# 6     Typez_Obsv:scale(CN)    100            NA 100.000000        NA  96.714286        NA  0.05853477            NA  0.23394504         NA  0.07189730         NA
# 7  Typez_Obsv:scale(DiffP)    100            NA   8.857143        NA  25.285714        NA -0.05743193            NA  0.00217888         NA -0.02441761         NA
# 8 Typez_Obsv:scale(TotalP)    100            NA 100.000000        NA   3.857143        NA  0.05967761            NA  0.27584622         NA  0.01665913         NA

# > Shape_results(result_full_autocor_no_cor)
                    # Factor      p_modA p_modA_simple   p_modAbin  p_modAdev      p_modS p_modSdev         e_modA e_modA_simple     e_modAbin    e_modAdev         e_modS     e_modSdev
# 1              (Intercept) 100.0000000    100.000000 100.0000000 100.000000 100.0000000       100  2.60109835158  2.8262267330  0.6402344182  4.044336428  0.73792839659  1.6684562489
# 2                scale(CN)   0.1428571      4.857143   0.5714286   4.857143   0.1428571         6 -0.00018998323 -0.0002743124 -0.0003562106 -0.001608348 -0.00011472462  0.0004305754
# 3             scale(DiffP) 100.0000000    100.000000 100.0000000 100.000000 100.0000000       100 -0.20362204123 -0.2708783482  0.5178803652 -2.764605245 -0.21542249090 -0.7025157405
# 4            scale(TotalP) 100.0000000    100.000000 100.0000000 100.000000 100.0000000       100  0.45056157983  0.5007366690 -0.2009595712  3.889506873  0.75018880317  1.8352579365
# 5               Typez_Obsv 100.0000000            NA 100.0000000         NA 100.0000000        NA  0.21980205510            NA  0.8218710846           NA  0.46181633844            NA
# 6     Typez_Obsv:scale(CN)   0.0000000            NA   5.4285714         NA   0.1428571        NA -0.00009025152            NA -0.0007143799           NA  0.00006594763            NA
# 7  Typez_Obsv:scale(DiffP) 100.0000000            NA  12.4285714         NA  39.8571429        NA -0.07637036702            NA -0.0115101150           NA -0.03265708034            NA
# 8 Typez_Obsv:scale(TotalP) 100.0000000            NA 100.0000000         NA  30.5714286        NA  0.06609419646            NA  0.2373313972           NA  0.02830575456            NA

# > Shape_results(result_full_autocor_corCN)
                    # Factor p_modA p_modA_simple   p_modAbin p_modAdev      p_modS p_modSdev       e_modA e_modA_simple    e_modAbin   e_modAdev       e_modS  e_modSdev
# 1              (Intercept)    100           100 100.0000000 100.00000 100.0000000 100.00000  2.621382260    2.86104128  0.535913596  4.55555441  0.748536004  1.8781666
# 2                scale(CN)    100           100   0.5714286  13.57143  99.2857143  56.57143  0.037395100    0.04526714 -0.001658412 -0.07371365  0.076490856 -0.1524972
# 3             scale(DiffP)    100           100 100.0000000 100.00000 100.0000000 100.00000 -0.141665182   -0.19751735  0.409299386 -2.47458144 -0.142661376 -0.5770583
# 4            scale(TotalP)    100           100 100.0000000 100.00000 100.0000000 100.00000  0.448166273    0.49513107 -0.179196084  4.63018873  0.743537398  2.3329311
# 5               Typez_Obsv    100            NA 100.0000000        NA 100.0000000        NA  0.234510635            NA  0.827491267          NA  0.474481228         NA
# 6     Typez_Obsv:scale(CN)      0            NA  14.7142857        NA   0.1428571        NA  0.006417069            NA  0.018566688          NA -0.001458202         NA
# 7  Typez_Obsv:scale(DiffP)    100            NA  24.1428571        NA  19.1428571        NA -0.062091153            NA  0.020762948          NA -0.022264495         NA
# 8 Typez_Obsv:scale(TotalP)    100            NA 100.0000000        NA  14.1428571        NA  0.063587767            NA  0.229458559          NA  0.025450693         NA
}


{## Nrand = 20, Nsim = 20, sync int = 2

# > Shape_results(result_no_autocor_no_cor)
                    # Factor p_modA p_modA_Ben p_modA_simple p_modAbin p_modAdev p_modS p_modSdev         e_modA e_modA_simple    e_modABen     e_modAbin   e_modAdev        e_modS    e_modSdev
# 1              (Intercept)    100        100           100       100        10    100        10  2.60374569393  2.6116453542  0.449507290  0.6304051471  0.03567990  1.9201594050  0.050270758
# 2                scale(CN)      0          5             0         0         5      0         0  0.00009004037  0.0009112402  0.001163768  0.0009855683  0.01569086  0.0001819707  0.011225199
# 3             scale(DiffP)    100        100           100       100         5    100         5 -0.19089292330 -0.1856186583 -0.065902600  0.5000218157 -0.06347929 -0.1987243310 -0.035905754
# 4            scale(TotalP)    100         NA           100       100        15    100         5  0.43349509478  0.4183628854           NA -0.1956094172  0.02815446  0.6402084848  0.004000917
# 5               Typez_Obsv      0         NA            NA         0        NA      0        NA  0.00239826941            NA           NA  0.0043599632          NA  0.0082016205           NA
# 6     Typez_Obsv:scale(CN)      0         NA            NA         0        NA      0        NA  0.00106478579            NA           NA  0.0029240054          NA  0.0013429119           NA
# 7  Typez_Obsv:scale(DiffP)      0         NA            NA         0        NA      0        NA -0.00325418457            NA           NA -0.0114126627          NA -0.0022203254           NA
# 8 Typez_Obsv:scale(TotalP)      0         NA            NA         0        NA      0        NA  0.00045095607            NA           NA  0.0009991389          NA -0.0027217814           NA

# > Shape_results(result_no_autocor_corCN)
                    # Factor p_modA p_modA_Ben p_modA_simple p_modAbin p_modAdev p_modS p_modSdev        e_modA e_modA_simple   e_modABen     e_modAbin    e_modAdev        e_modS    e_modSdev
# 1              (Intercept)    100        100           100       100         5    100         5  2.6261609913    2.63822376  0.46501370  0.5283822657  0.043742479  1.9315260730  0.049699158
# 2                scale(CN)    100        100           100         0         0    100         5  0.0377219958    0.04224232  0.01554426 -0.0001698994  0.011322431  0.0642128325  0.009157457
# 3             scale(DiffP)    100        100           100       100         5    100         5 -0.1347367739   -0.12607787 -0.05480666  0.4002291681 -0.027683588 -0.1384888839 -0.007794497
# 4            scale(TotalP)    100         NA           100       100         5    100        10  0.4308729500    0.40306802          NA -0.1706669423  0.002716867  0.6399346313 -0.028725571
# 5               Typez_Obsv      0         NA            NA         0        NA      0        NA  0.0032289440            NA          NA  0.0083071253           NA  0.0089364095           NA
# 6     Typez_Obsv:scale(CN)      0         NA            NA         0        NA      0        NA  0.0005280569            NA          NA  0.0014929034           NA -0.0004176266           NA
# 7  Typez_Obsv:scale(DiffP)      0         NA            NA         0        NA      0        NA -0.0010895077            NA          NA -0.0025372075           NA -0.0000723588           NA
# 8 Typez_Obsv:scale(TotalP)      0         NA            NA         0        NA      0        NA -0.0008783092            NA          NA -0.0031169562           NA -0.0040326521           NA

# > Shape_results(result_partial_autocor_no_cor)
                    # Factor p_modA p_modA_Ben p_modA_simple p_modAbin p_modAdev p_modS p_modSdev      e_modA e_modA_simple   e_modABen   e_modAbin e_modAdev      e_modS e_modSdev
# 1              (Intercept)    100        100           100       100       100    100       100  2.57040664    2.74384309  0.51586162  0.52404058  3.073375  1.88643915  2.628520
# 2                scale(CN)     25        100           100        50       100     30       100  0.01050781    0.07911351  0.04005612  0.02456910  1.230748  0.01566594  1.013166
# 3             scale(DiffP)    100        100           100       100       100    100       100 -0.22198005   -0.27130705 -0.08497874  0.40633724 -2.176177 -0.23260244 -1.403926
# 4            scale(TotalP)    100         NA           100       100       100    100       100  0.47368136    0.51875379          NA -0.11350969  3.309304  0.68945499  2.768635
# 5               Typez_Obsv    100         NA            NA       100        NA    100        NA  0.16685491            NA          NA  0.56685187        NA  0.25843335        NA
# 6     Typez_Obsv:scale(CN)    100         NA            NA       100        NA    100        NA  0.06878446            NA          NA  0.29112705        NA  0.09178639        NA
# 7  Typez_Obsv:scale(DiffP)    100         NA            NA        60        NA    100        NA -0.06096616            NA          NA -0.04059641        NA -0.04261800        NA
# 8 Typez_Obsv:scale(TotalP)    100         NA            NA       100        NA    100        NA  0.06459445            NA          NA  0.24649974        NA  0.03737430        NA

# > Shape_results(result_partial_autocor_corCN)
                    # Factor p_modA p_modA_Ben p_modA_simple p_modAbin p_modAdev p_modS p_modSdev      e_modA e_modA_simple   e_modABen    e_modAbin  e_modAdev      e_modS  e_modSdev
# 1              (Intercept)    100        100           100       100       100    100       100  2.59521456     2.7942363  0.54583257  0.437748234  4.0559868  1.89461128  3.4228039
# 2                scale(CN)    100        100           100        85       100    100       100  0.05416827     0.1119440  0.09191707  0.039372042  0.7600433  0.08971190  0.5480689
# 3             scale(DiffP)    100        100           100       100       100    100       100 -0.15233557    -0.2017685 -0.08133176  0.342620683 -2.3911717 -0.15616322 -1.5334289
# 4            scale(TotalP)    100         NA           100       100       100    100       100  0.46471368     0.5055177          NA -0.121922700  4.6512290  0.68294329  3.9433865
# 5               Typez_Obsv    100         NA            NA       100        NA    100        NA  0.19258564            NA          NA  0.638927714         NA  0.28510578         NA
# 6     Typez_Obsv:scale(CN)    100         NA            NA       100        NA    100        NA  0.05660859            NA          NA  0.228033553         NA  0.06757036         NA
# 7  Typez_Obsv:scale(DiffP)    100         NA            NA         5        NA    100        NA -0.05746155            NA          NA  0.003138606         NA -0.04027319         NA
# 8 Typez_Obsv:scale(TotalP)    100         NA            NA       100        NA     55        NA  0.06060907            NA          NA  0.284506167         NA  0.02631513         NA

# > Shape_results(result_full_autocor_no_cor)
                    # Factor p_modA p_modA_Ben p_modA_simple p_modAbin p_modAdev p_modS p_modSdev        e_modA e_modA_simple      e_modABen     e_modAbin    e_modAdev        e_modS    e_modSdev
# 1              (Intercept)    100        100           100       100       100    100       100  2.6060317177   2.831937754  0.56299137383  0.6421449834  4.064410349  1.9167172183  3.229572804
# 2                scale(CN)      0          5            10         0        15      0        25 -0.0002285533  -0.001028047 -0.00003617729  0.0009803349 -0.009908654 -0.0009473257  0.001054099
# 3             scale(DiffP)    100        100           100       100       100    100       100 -0.2041038920  -0.270373341 -0.09657325927  0.5194933806 -2.775468909 -0.2192147207 -1.594224466
# 4            scale(TotalP)    100         NA           100       100       100    100       100  0.4499889399   0.498153545             NA -0.2002060318  3.889212891  0.6695685500  3.126687252
# 5               Typez_Obsv    100         NA            NA       100        NA    100        NA  0.2206963739            NA             NA  0.8275732596           NA  0.3175498134           NA
# 6     Typez_Obsv:scale(CN)      0         NA            NA        10        NA      0        NA -0.0008696453            NA             NA -0.0027682129           NA -0.0001418715           NA
# 7  Typez_Obsv:scale(DiffP)    100         NA            NA         0        NA    100        NA -0.0754864126            NA             NA -0.0061163046           NA -0.0438236694           NA
# 8 Typez_Obsv:scale(TotalP)    100         NA            NA       100        NA    100        NA  0.0640660615            NA             NA  0.2293667966           NA  0.0298202132           NA

# > Shape_results(result_full_autocor_corCN)
                    # Factor p_modA p_modA_Ben p_modA_simple p_modAbin p_modAdev p_modS p_modSdev       e_modA e_modA_simple   e_modABen     e_modAbin   e_modAdev       e_modS  e_modSdev
# 1              (Intercept)    100        100           100       100       100    100       100  2.624735628    2.86420600  0.58638692  0.5364213015  4.55803249  1.923407709  3.5461191
# 2                scale(CN)    100        100           100         0         5    100        35  0.036476312    0.04449428  0.05034675 -0.0008293478 -0.06052328  0.061983518 -0.1210297
# 3             scale(DiffP)    100        100           100       100       100    100       100 -0.142140274   -0.19722641 -0.08599844  0.4135276895 -2.47078440 -0.148680301 -1.3693367
# 4            scale(TotalP)    100         NA           100       100       100    100       100  0.450426131    0.49704006          NA -0.1804873614  4.62815718  0.672368405  3.7868298
# 5               Typez_Obsv    100         NA            NA       100        NA    100        NA  0.234270465            NA          NA  0.8302623602          NA  0.323773684         NA
# 6     Typez_Obsv:scale(CN)      0         NA            NA        10        NA      0        NA  0.006658687            NA          NA  0.0203479083          NA  0.002034227         NA
# 7  Typez_Obsv:scale(DiffP)    100         NA            NA        30        NA    100        NA -0.061284495            NA          NA  0.0290988589          NA -0.035066633         NA
# 8 Typez_Obsv:scale(TotalP)    100         NA            NA       100        NA     80        NA  0.063422640            NA          NA  0.2262242339          NA  0.030850473         NA
} 


}

}

Results_Sim_1


{# Simulation 2: Take observed CN ; take observed TP ; generate visits ; with or without an effect of CN on A (i.e. with CN = sorting parameter of intervals)

{# get the real MY_TABLE_per_DVD 

MY_TABLE_per_DVD <- MY_TABLE_perDVD[,c('DVDRef', 'MVisit1','FVisit1',"DVDInfoChickNb")]
colnames(MY_TABLE_per_DVD) <- c('DVDRef','MaleP','FemaleP', 'CN')
MY_TABLE_per_DVD$TotalP <- MY_TABLE_per_DVD$MaleP + MY_TABLE_per_DVD$FemaleP # total number of visits for that video
MY_TABLE_per_DVD$DiffP <- abs(MY_TABLE_per_DVD$MaleP - MY_TABLE_per_DVD$FemaleP)
MY_TABLE_per_DVD <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Calculate_AMax))
head(MY_TABLE_per_DVD)

}

head(MY_TABLE_per_DVD)

Generate_visits_randomize_them_and_analyse <-function(autocorrelation){

# create nest watches

if (autocorrelation == 'none'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_nest_watch))}

if (autocorrelation == 'partial'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_partially_sorted_nest_watch))}

if (autocorrelation == 'full'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_fully_sorted_nest_watch))}

head(full_dat)

# Randomization Within nest watch, within individual
A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S(full_dat),simplify=FALSE ) )
# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef)))) 
# second half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# summarise observed and simulated coordination
summary_A_S_observed <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),Calculate_A_S_one_nest_watch))
summary_A_S_randomized <- data.frame(DVDRef = out_Asim_within_df[,1], 
									MedAsim = apply(out_Asim_within_df[,-1],1,median_integer), # Median are used for having integer for poisson models
									MedSsim= apply(out_Ssim_within_df[,-1],1,median_integer),
									MeanAsim = apply(out_Asim_within_df[,-1],1,mean),
									MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))

{# create table short and long

# short table
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_observed, by='DVDRef')
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_randomized, by='DVDRef')
MY_TABLE_per_DVD$Adev <- MY_TABLE_per_DVD$A - MY_TABLE_per_DVD$MedAsim
MY_TABLE_per_DVD$Sdev <- MY_TABLE_per_DVD$S - MY_TABLE_per_DVD$MedSsim 
head(MY_TABLE_per_DVD)

# long table
MY_TABLE_per_DVD_long <- rbind(MY_TABLE_per_DVD, MY_TABLE_per_DVD)
# first half is 'simulated', second half is 'observed'
MY_TABLE_per_DVD_long$Type <- c(rep('a_Sim', nrow(MY_TABLE_per_DVD)), rep('z_Obsv', nrow(MY_TABLE_per_DVD)))
MY_TABLE_per_DVD_long$A[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- MY_TABLE_per_DVD_long$MedAsim[MY_TABLE_per_DVD_long$Type == 'a_Sim'] # A sim is MedAsim
MY_TABLE_per_DVD_long$S[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- MY_TABLE_per_DVD_long$MedSsim[MY_TABLE_per_DVD_long$Type == 'a_Sim']
MY_TABLE_per_DVD_long$rowID <- seq(1:nrow(MY_TABLE_per_DVD_long))
head(MY_TABLE_per_DVD_long)
tail(MY_TABLE_per_DVD_long)

}

{# analyses

modA_simple <- glmer(A~ scale(CN) + scale(TotalP) + scale(DiffP)+(1|DVDRef) ,data=MY_TABLE_per_DVD,family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modA <- glmer(A~ Type*scale(CN) + Type*scale(TotalP) + Type*scale(DiffP) +(1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modS <- glmer(S~ Type*scale(CN) + Type*scale(TotalP) + Type*scale(DiffP) +(1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'poisson',control=glmerControl(optimizer = "bobyqa")) 
modAdev <- lm(Adev~ scale(CN) + scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modSdev <- lm(Sdev~ scale(CN) + scale(TotalP) + scale(DiffP) ,data=MY_TABLE_per_DVD) 
modAbin <- glmer(cbind(A, AMax-A) ~ Type*scale(CN) + Type*scale(TotalP) + Type*scale(DiffP) +(1|DVDRef), data=MY_TABLE_per_DVD_long, family = 'binomial') 
modABen <- lm(I(A/(TotalP-1)) ~ scale(DiffP) + scale(CN), data=MY_TABLE_per_DVD) 

}

{# results

resultsGLMER <- data.frame(Factor = rownames(summary(modA)$coeff), 
						e_modA = summary(modA)$coeff[,1], 
						p_modA = round(summary(modA)$coeff[,4],3),
						e_modS = summary(modS)$coeff[,1], 
						p_modS = round(summary(modS)$coeff[,4],3),
						e_modAbin = summary(modAbin)$coeff[,1], 
						p_modAbin = round(summary(modAbin)$coeff[,4],3))
rownames(resultsGLMER) <- NULL
				
resultsLM <- data.frame(Factor = rownames(summary(modAdev)$coeff), 
						e_modAdev = summary(modAdev)$coeff[,1], 
						p_modAdev = round(summary(modAdev)$coeff[,4],3),
						e_modSdev = summary(modSdev)$coeff[,1], 
						p_modSdev = round(summary(modSdev)$coeff[,4],3),
						e_modA_simple = summary(modA_simple)$coeff[,1], 
						p_modA_simple = round(summary(modA_simple)$coeff[,4],3))
rownames(resultsLM) <- NULL

resultsBen <- data.frame(Factor = rownames(summary(modABen)$coeff),					
						e_modABen  = summary(modABen)$coeff[,1],
						p_modA_Ben = round(summary(modABen)$coeff[,4],3)	
						)
rownames(resultsBen) <- NULL

results <- merge(resultsGLMER, resultsLM, by='Factor', all.x=TRUE)
results <- merge(results, resultsBen, by='Factor', all.x=TRUE)


}

return(list(results))

}

result_no_autocor_Sim2 <- pbreplicate(NreplicatesSimulation,Generate_visits_randomize_them_and_analyse('none'))
result_partial_autocor_Sim2 <- pbreplicate(NreplicatesSimulation,Generate_visits_randomize_them_and_analyse('partial'))
result_full_autocor_Sim2 <- pbreplicate(NreplicatesSimulation,Generate_visits_randomize_them_and_analyse('full'))

Results_Sim_2 <- c(
list(Shape_results(result_no_autocor_no_cor_Sim2)),
list(Shape_results(result_partial_autocor_corCN_Sim2)),
list(Shape_results(result_full_autocor_corCN_Sim2)))

}

Results_Sim_2



{# Simulation 3: Randomized real dataset instead of generating data

{## get the full real MY_TABLE_perDVD 
MY_TABLE_per_DVD <- MY_TABLE_perDVD[,-which(names(MY_TABLE_perDVD) %in% c('A','S','Aswitch','MeanAsimWithin','MeanAsimAmong','MedAsimWithin','MedAsimAmong','MeanSsimWithin','MeanSsimAmong','MedSsimWithin','MedSsimAmong','Adev','Sdev'))]

MY_TABLE_per_DVD$TotalP <- MY_TABLE_per_DVD$FVisit1+ MY_TABLE_per_DVD$MVisit1
MY_TABLE_per_DVD$DiffP <- abs(MY_TABLE_per_DVD$FVisit1- MY_TABLE_per_DVD$MVisit1)
MY_TABLE_per_DVD <- dplyr::rename(MY_TABLE_per_DVD, ChickNb = DVDInfoChickNb)

MY_TABLE_per_DVD$BroodRef <- as.factor(MY_TABLE_per_DVD$BroodRef)
MY_TABLE_per_DVD$SocialDadID <- as.factor(MY_TABLE_per_DVD$SocialDadID)
MY_TABLE_per_DVD$SocialMumID <- as.factor(MY_TABLE_per_DVD$SocialMumID)
MY_TABLE_per_DVD$PairID <- as.factor(MY_TABLE_per_DVD$PairID)
MY_TABLE_per_DVD$BreedingYear <- as.factor(MY_TABLE_per_DVD$BreedingYear)

head(MY_TABLE_per_DVD)
}

Randomize_real_data_re_randomize_them_and_analyse <-function(){

# randomise real nest watches from RawInterfeeds to create 'observed' dataset
full_dat <- do.call(rbind,lapply(split(RawInterfeeds, RawInterfeeds$DVDRef),Randomize_one_nest_watch))
head(full_dat)

# Randomization Within nest watch, within individual
A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S(full_dat),simplify=FALSE ) )
# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef)))) 
# second half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# summarise observed and simulated coordination
summary_A_S_observed <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),Calculate_A_S_one_nest_watch))
summary_A_S_randomized <- data.frame(DVDRef = out_Asim_within_df[,1], 
									MedAsim = apply(out_Asim_within_df[,-1],1,median_integer), # Median are used for having integer for poisson models
									MedSsim= apply(out_Ssim_within_df[,-1],1,median_integer),
									MeanAsim = apply(out_Asim_within_df[,-1],1,mean),
									MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))

{# create table short and long

# short table
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_observed, by='DVDRef')
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_randomized, by='DVDRef')
head(MY_TABLE_per_DVD)

# long table
MY_TABLE_per_DVD_long <- rbind(MY_TABLE_per_DVD, MY_TABLE_per_DVD)
# first half is 'simulated', second half is 'observed'
MY_TABLE_per_DVD_long$Type <- c(rep('a_Sim', nrow(MY_TABLE_per_DVD)), rep('z_Obsv', nrow(MY_TABLE_per_DVD)))
MY_TABLE_per_DVD_long$A[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- MY_TABLE_per_DVD_long$MedAsim[MY_TABLE_per_DVD_long$Type == 'a_Sim'] # A sim is MedAsim
MY_TABLE_per_DVD_long$S[MY_TABLE_per_DVD_long$Type == 'a_Sim'] <- MY_TABLE_per_DVD_long$MedSsim[MY_TABLE_per_DVD_long$Type == 'a_Sim']
MY_TABLE_per_DVD_long$rowID <- seq(1:nrow(MY_TABLE_per_DVD_long))
head(MY_TABLE_per_DVD_long)
tail(MY_TABLE_per_DVD_long)

}

{# analyses

{modA <- glmer(A ~  
	
	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
    Type*FPriorResidence +
	
	Type*scale(ChickNb) + 
	Type*scale(TotalP) +
	Type*scale(DiffP)+
		
	(1|BroodRef) + 	
	(1|SocialMumID)+ (1|SocialDadID) + 
	(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	#(1|PairIDYear)
	+ (1|DVDRef) 
	#+ (1|rowID) # for overdispersion
	, data = MY_TABLE_per_DVD_long[!is.na(MY_TABLE_per_DVD_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)
}
	
{modS <- glmer(S ~ 
	
	Type*scale(ParentsAge) + # this is strongly correlated to PairBroodNb (if removed, PBDur still negative NS, if PBDur removed, ParentAge signi Neg)
	Type*scale(HatchingDayAfter0401) +
	Type*scale(PairBroodNb) + 
	Type*ChickAgeCat +
	Type*scale(RelTimeHrs) + 
	Type*MPriorResidence +
    Type*FPriorResidence +
	
	Type*scale(ChickNb) +
	Type*scale(TotalP) +
	Type*scale(DiffP)+
	
	(1|BroodRef) + 
	(1|SocialMumID)+ (1|SocialDadID) + 
	(1|PairID) +  # explained 0% of the variance
	#(1|BreedingYear) + # explained 0% of the variance
	#(1|PairIDYear)
	+ (1|DVDRef) 
	#+ (1|rowID) # for overdispersion
	, data = MY_TABLE_per_DVD_long[!is.na(MY_TABLE_per_DVD_long$RelTimeHrs),]
	, family = 'poisson'
	,control=glmerControl(optimizer = "bobyqa")
	)	
}

}

{# results

results <- data.frame(Factor = rownames(summary(modA)$coeff), 
						e_modA = summary(modA)$coeff[,1], 
						p_modA = round(summary(modA)$coeff[,4],3),
						e_modS = summary(modS)$coeff[,1], 
						p_modS = round(summary(modS)$coeff[,4],3))
rownames(results) <- NULL

}


return(list(results))

}

result_no_autocor_observed_cor_Sim3 <- pbreplicate(NreplicatesSimulation,Randomize_real_data_re_randomize_them_and_analyse())

Results_Sim_3 <- Shape_results(result_no_autocor_observed_cor_Sim3)

}

Results_Sim_3


set.seed(21)

{# Graphs after running one replicate of simulation 1 with each of the 6 sets of parameter

Generate_TP_randomize_data_and_give_data <-function(autocorrelation, correlation_CN_TP){

# generate TP correlated to existing CN and create MY_TABLE_per_DVD

if (correlation_CN_TP == 'No'){
MY_TABLE_per_DVD <- Generate_TP(nPR, sdlogPR,rCNTP)
MY_TABLE_per_DVD$CN <- sample(CN)
}

if (correlation_CN_TP == 'Yes'){
MY_TABLE_per_DVD <- Generate_TP_correlated_to_CN(nPR, sdlogPR,rCNTP,CN)
}

MY_TABLE_per_DVD <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Calculate_AMax))
head(MY_TABLE_per_DVD)

# create nest watches

if (autocorrelation == 'none'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_nest_watch))
}

if (autocorrelation == 'partial'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_partially_sorted_nest_watch))
}

if (autocorrelation == 'full'){
full_dat <- do.call(rbind,lapply(split(MY_TABLE_per_DVD,MY_TABLE_per_DVD$DVDRef), Create_one_fully_sorted_nest_watch))
}

head(full_dat)

# Randomization Within nest watch, within individual
A_S_within_randomization <- do.call(cbind,replicate(NreplicatesWithinFileRandomization,Randomize_Data_WithinFile_and_Calculate_A_S(full_dat),simplify=FALSE ) )
# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef)))) 
# second half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# summarise observed and simulated coordination
summary_A_S_observed <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),Calculate_A_S_one_nest_watch))
summary_A_S_randomized <- data.frame(DVDRef = out_Asim_within_df[,1], 
									MedAsim = apply(out_Asim_within_df[,-1],1,median_integer), # Median are used for having integer for poisson models
									MedSsim= apply(out_Ssim_within_df[,-1],1,median_integer),
									MeanAsim = apply(out_Asim_within_df[,-1],1,mean),
									MeanSsim= apply(out_Ssim_within_df[,-1],1,mean))

{# create table short and long

# short table
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_observed, by='DVDRef')
MY_TABLE_per_DVD <- merge(MY_TABLE_per_DVD, summary_A_S_randomized, by='DVDRef')
MY_TABLE_per_DVD$Adev <- MY_TABLE_per_DVD$A - MY_TABLE_per_DVD$MedAsim
MY_TABLE_per_DVD$Sdev <- MY_TABLE_per_DVD$S - MY_TABLE_per_DVD$MedSsim 
head(MY_TABLE_per_DVD)

# long table
MY_TABLE_per_DVD_long <- rbind(MY_TABLE_per_DVD, MY_TABLE_per_DVD)
# first half is 'simulated', second half is 'observed'
MY_TABLE_per_DVD_long$Type <- c(rep('Sim', nrow(MY_TABLE_per_DVD)), rep('Obsv', nrow(MY_TABLE_per_DVD)))
MY_TABLE_per_DVD_long$A[MY_TABLE_per_DVD_long$Type == 'Sim'] <- MY_TABLE_per_DVD_long$MedAsim[MY_TABLE_per_DVD_long$Type == 'Sim'] # A sim is MedAsim
MY_TABLE_per_DVD_long$S[MY_TABLE_per_DVD_long$Type == 'Sim'] <- MY_TABLE_per_DVD_long$MedSsim[MY_TABLE_per_DVD_long$Type == 'Sim']
MY_TABLE_per_DVD_long$rowID <- seq(1:nrow(MY_TABLE_per_DVD_long))
head(MY_TABLE_per_DVD_long)
tail(MY_TABLE_per_DVD_long)

}

return(MY_TABLE_per_DVD_long)
}

MY_TABLE_per_DVD_long_none_yes <- Generate_TP_randomize_data_and_give_data('none', 'Yes')
MY_TABLE_per_DVD_long_full_yes <- Generate_TP_randomize_data_and_give_data('full', 'Yes')
MY_TABLE_per_DVD_long_partial_yes <- Generate_TP_randomize_data_and_give_data('partial', 'Yes')

{## summarize data per cat for plotting

Change_x_to_Cat <- function(x) {
x$CN_Cat <- cut(x$CN, c(0,1,2,3,4,7), labels = c(1:4,"5+"),include.lowest=TRUE)
x$TotalP_Cat <- cut(x$TotalP,c(seq(0,80,20),150), labels = c(seq(10,70,20),"90+"), include.lowest=TRUE)
x}

MY_TABLE_per_DVD_long_none_yes <- Change_x_to_Cat(MY_TABLE_per_DVD_long_none_yes)
MY_TABLE_per_DVD_long_full_yes <- Change_x_to_Cat(MY_TABLE_per_DVD_long_full_yes)
MY_TABLE_per_DVD_long_partial_yes <- Change_x_to_Cat(MY_TABLE_per_DVD_long_partial_yes)

summarize_TP <- function(x) {data.frame(summarise ((x %>% group_by(Type,TotalP_Cat)),
				Amean = mean(A),
				Alower = Amean - sd(A)/sqrt(n())*1.96,
				Aupper = Amean + sd(A)/sqrt(n())*1.96,
				NbFiles = n()))}

summarize_CN <- function(x) {data.frame(summarise ((x %>% group_by(Type,CN_Cat)),
				Amean = mean(A),
				Alower = Amean - sd(A)/sqrt(n())*1.96,
				Aupper = Amean + sd(A)/sqrt(n())*1.96,
				NbFiles = n()))	}

summary_TP_none_yes <- summarize_TP(MY_TABLE_per_DVD_long_none_yes)	
summary_CN_none_yes <- summarize_CN(MY_TABLE_per_DVD_long_none_yes)	
summary_TP_full_yes <- summarize_TP(MY_TABLE_per_DVD_long_full_yes)	
summary_CN_full_yes <- summarize_CN(MY_TABLE_per_DVD_long_full_yes)	
summary_TP_partial_yes <- summarize_TP(MY_TABLE_per_DVD_long_partial_yes)	
summary_CN_partial_yes <- summarize_CN(MY_TABLE_per_DVD_long_partial_yes)

}	

{## plot with cor A

{plot1y <- ggplot(aes(y = Amean, x = TotalP_Cat, col=Type), data = summary_TP_none_yes) + 
geom_point()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
scale_y_continuous(limits = c(0, 70), breaks =seq(5,65, by = 10))+
xlab(NULL)+
ylab("Number of alternated visits")+
scale_color_manual(values = rep(c('black', 'dimgrey'),7), labels=c("Observed", "Random"))+ 
theme_classic()+
theme(
legend.justification= c(0,1),
legend.position = c(0.01,0.99),
legend.title =element_blank(),
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(margin=margin(0,10,0,0)) ,
axis.ticks.y=element_blank(),
axis.text.x=element_text(color="white"),
axis.title.x = element_blank(),
axis.ticks.x=element_blank(),
plot.margin = unit(c(0.1,0.2,0,0.2), "cm"))
}

{plot2y <- ggplot(aes(y = Amean, x = CN_Cat, col = Type), data = summary_CN_none_yes) + 
geom_point()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
scale_y_continuous(limits = c(0, 70), breaks =seq(5,65, by = 10))+
xlab(NULL)+
ylab(NULL)+
scale_color_manual(values = rep(c('black', 'dimgrey'),7))+
theme_classic()+
theme(
legend.position="none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(angle=0),
axis.text.y=element_blank(),
axis.ticks.y=element_blank(),
axis.text.x=element_text(color="white"),
axis.title.x = element_blank(),
axis.ticks.x=element_blank(),
plot.margin = unit(c(0.1,0.2,0,0.1), "cm"))
}

{plot3y <- ggplot(aes(y = Amean, x = TotalP_Cat, col = Type), data = summary_TP_full_yes) + 
geom_point()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
scale_y_continuous(limits = c(0, 70), breaks =seq(5,65, by = 10))+
xlab(NULL)+
ylab("Number of alternated visits")+
scale_color_manual(values = rep(c('black', 'dimgrey'),7))+ 
theme_classic()+
theme(
legend.position = "none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(margin=margin(0,10,0,0)),
axis.ticks.y=element_blank(),
axis.title.x = element_blank(),
axis.text.x=element_text(color="white"),
axis.ticks.x=element_blank(),
plot.margin = unit(c(0,0.2,0,0.2), "cm"))
}

{plot4y <- ggplot(aes(y = Amean, x = CN_Cat, col = Type), data = summary_CN_full_yes) + 
geom_point()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
scale_y_continuous(limits = c(0, 70), breaks =seq(5,65, by = 10))+
xlab(NULL)+
ylab(NULL)+
scale_color_manual(values = rep(c('black', 'dimgrey'),7))+
theme_classic()+
theme(
legend.position="none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(angle=0),
axis.ticks.y=element_blank(),
axis.text.y=element_blank(),
axis.title.x = element_blank(),
axis.text.x=element_text(color="white"),
axis.ticks.x=element_blank(),
plot.margin = unit(c(0,0.2,0,0.1), "cm"))
}

{plot5y <- ggplot(aes(y = Amean, x = TotalP_Cat, col = Type), data = summary_TP_partial_yes) + 
geom_point()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
scale_y_continuous(limits = c(0, 70), breaks =seq(5,65, by = 10))+
xlab(NULL)+
ylab("Number of alternated visits")+
scale_color_manual(values = rep(c('black', 'dimgrey'),7))+ 
theme_classic()+
theme(
legend.position = "none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(margin=margin(0,10,0,0)),
axis.ticks.y=element_blank(),
axis.title.x = element_blank(),
axis.ticks.x=element_blank(),
plot.margin = unit(c(0,0.2,0.1,0.2), "cm"))
}

{plot6y <- ggplot(aes(y = Amean, x = CN_Cat, col = Type), data = summary_CN_partial_yes) + 
geom_point()+
geom_errorbar(aes(ymin=Alower, ymax=Aupper, col=Type),na.rm=TRUE)+
scale_y_continuous(limits = c(0, 70), breaks =seq(5,65, by = 10))+
xlab(NULL)+
ylab(NULL)+
scale_color_manual(values = rep(c('black', 'dimgrey'),7))+
theme_classic()+
theme(
legend.position="none",
panel.border = element_rect(colour = "black", fill=NA), 
axis.title.y=element_text(angle=0, hjust=1),
axis.text.y=element_blank(),
axis.ticks.y=element_blank(),
axis.title.x = element_blank(),
axis.ticks.x=element_blank(),
plot.margin = unit(c(0,0.2,0.1,0.1), "cm"))
}

{blank1y <-ggplot()+
scale_x_continuous(limits = c(0,10))+
scale_y_continuous(limits = c(0,10), breaks=seq(0,10,10))+
ylab("N")+

annotate("text", x = 5, y = 5, label = "Total number of visits", hjust = 0.5, angle=0, color="black")+
theme_classic()+

theme(
#panel.border = element_rect(colour = "red", fill=NA),
axis.title.y=element_text(color="white", angle=(90)),
axis.text.y=element_text(color="white"),
axis.ticks.y=element_blank(),
axis.title.x = element_blank(),
axis.text.x=element_blank(),
axis.ticks.x=element_blank(),
#axis.line = element_line(colour = "green"),
axis.line = element_blank(),
plot.margin = unit(c(0,0.2,0,0.2), "cm"))

}

{blank2y <-ggplot()+
scale_x_continuous(limits = c(0, 10))+
scale_y_continuous(limits = c(0, 10))+

annotate("text", x = 5, y = 5, label = "Number of chicks",  hjust = 0.5, angle=0)+
theme_classic()+

theme(
#panel.border = element_rect(colour = "red", fill=NA),
axis.title.y=element_blank(),
axis.text.y=element_blank(),
axis.ticks.y=element_blank(),
axis.title.x = element_blank(),
axis.text.x=element_blank(),
axis.ticks.x=element_blank(),
#axis.line = element_line("orange"),
axis.line = element_blank(),
plot.margin = unit(c(0,0.2,0,0.1), "cm"))
}


{### nested plotting structuring

g1y <- ggplotGrob(plot1y)
g2y <- ggplotGrob(plot2y)
g3y <- ggplotGrob(plot3y)
g4y <- ggplotGrob(plot4y)
g5y <- ggplotGrob(plot5y)
g6y <- ggplotGrob(plot6y)
gblank1y <- ggplotGrob(blank1y)
gblank2y <- ggplotGrob(blank2y)

firstrowy = cbind(g1y, g2y, size = "last")
secondrowy = cbind(g3y, g4y, size = "last")
thirdrowy = cbind(g5y, g6y, size = "last")
bottomrowy = cbind(gblank1y, gblank2y, size = "last")

g1 <- grid.arrange(firstrowy)
g2 <- grid.arrange(secondrowy)
g3 <- grid.arrange(thirdrowy)
gbottom <- grid.arrange(bottomrowy)

gg1 <- grid.arrange(textGrob("Scenario 1:
Observed alternation 
simulated to be
random"),g1,ncol =2, widths = c(1.5,4))

gg2 <- grid.arrange(textGrob("Scenario 2:
Observed alternation 
simulated to be
higher than random
due to 
autocorrelation"),g2,ncol =2, widths = c(1.5,4))

gg3 <- grid.arrange(textGrob("Scenario 3:
Observed alternation 
simulated to be
higher than random
due to an effect
of the number of chicks"),g3,ncol =2, widths = c(1.5,4))

ggbottom <- grid.arrange(textGrob(""), gbottom,ncol =2, widths = c(1.5,4)) 

}

grid.arrange(gg1,gg2,gg3,ggbottom,nrow = 4, ncol= 1, heights = c(4,4,4,0.5))

}

length(MY_TABLE_per_DVD_long_none_yes$TotalP[MY_TABLE_per_DVD_long_none_yes$TotalP >100])
length(MY_TABLE_per_DVD_long_full_yes$TotalP[MY_TABLE_per_DVD_long_full_yes$TotalP >100])
length(MY_TABLE_per_DVD_long_partial_yes$TotalP[MY_TABLE_per_DVD_long_partial_yes$TotalP >100])

length(MY_TABLE_per_DVD_long_none_yes$CN[MY_TABLE_per_DVD_long_none_yes$CN >5])
length(MY_TABLE_per_DVD_long_full_yes$CN[MY_TABLE_per_DVD_long_full_yes$CN >5])
length(MY_TABLE_per_DVD_long_partial_yes$CN[MY_TABLE_per_DVD_long_partial_yes$CN >5])


}




