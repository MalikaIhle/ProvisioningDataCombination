#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Joel PICK & Malika IHLE    joel.l.pick@gmail.com   malika_ihle@hotmail.fr
#	 Simulation to help decide which analyses to perfom on provisioning data sparrows
#	 Start : 02/02/2017
#	 last modif : 11/02/2017
#	 commit: one function (create fulldataset) to test all analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{### Packages
library(pbapply)
library(lme4)
options(scipen=999)
}



## to test the link between alternation and total provisioning rate


Generate_data_randomize_them_and_analyse <-function(){

{# generate data

avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8
VideoLength <- 90

# Joel suggested to pass on the log scale to be able to add Poisson distributed error, I assume (??)
meanlog <- log(avPR)
sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))

full_dat <- list()

for (i in 1:1662){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog ) # expected number of visits
FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
MaleP <- rpois(1, MalePexp) # realized number of visits including the poisson distributed error
FemaleP <- rpois(1, FemalePexp)
TotalP <- MaleP + FemaleP # total number of visits for that video
DiffP <- abs(MaleP - FemaleP)

MaleVisits <- sort(runif(MaleP,0,VideoLength))
FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
MaleIntervals <- c(diff(MaleVisits),90-MaleVisits[length(MaleVisits)])
FemaleIntervals <- c(diff(FemaleVisits),90-FemaleVisits[length(FemaleVisits)])

dat <- data.frame(rbind(cbind(Tstart = MaleVisits, Sex = rep(1, length(MaleVisits)),Interval=MaleIntervals),cbind(Tstart = FemaleVisits,Sex = rep(0, length(FemaleVisits)), Interval=FemaleIntervals)))
dat <- dat[order(dat$Tstart),]
dat$DVDRef <- i
if (TotalP - DiffP >1) {full_dat[[i]] <- dat}

}

full_dat <- do.call(rbind,full_dat)
}

{### Randomization Within nest watch, within individual

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
Randomize_Data_WithinFile_and_Calculate_A_S_fun <- function(RawData) { 

RandomizeData_oneSplit <-  function(x){

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

SimData <- do.call(rbind,lapply(split(RawData, RawData$DVDRef),RandomizeData_oneSplit))
rownames(SimData) <- NULL

## Calculate Alternation within each DVD

SimData_Calculate_A <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
Asim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]) # NbAlternation
return(Asim)
}

SimData_A <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_A ))

## Calculate Synchrony within each DVD

SimData_Calculate_S <- function(x){
x <- x[order(x$Tstart),]
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
x$NextTstartafterhalfminTstart <-  c(x$Tstart[-1],NA) <= x$Tstart +0.5 &  c(x$Tstart[-1],NA) >= x$Tstart # second arrive shortly after first visit (can share time in the nest box or not) > can assess chick feeding/state of hunger + less conspicuous?
Ssim <- length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame) 
		& x$NextTstartafterhalfminTstart == TRUE & !is.na(x$NextTstartafterhalfminTstart)])
return(Ssim)
}

SimData_S <- do.call(rbind,lapply(X=split(SimData,SimData$DVDRef),FUN= SimData_Calculate_S ))

# output: Asim of each DVD (first half of the rows), and Ssim of each DVD (second half of the rows)
return(rbind(SimData_A, SimData_S)) # the length(unique(DVDRef)) first row are Asim, the other half are Ssim
}

A_S_within_randomization <- do.call(cbind,replicate(10,Randomize_Data_WithinFile_and_Calculate_A_S_fun(full_dat),simplify=FALSE ) )

# first half are A sim
out_Asim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), head(A_S_within_randomization,length(unique(full_dat$DVDRef))))

# second Half are S sim
out_Ssim_within_df <- data.frame(DVDRef = unique(full_dat$DVDRef), tail(A_S_within_randomization,length(unique(full_dat$DVDRef))))

}

{# create table long

median_integer <- function(x) {as.integer(median(x) +sample(c(0.5,-0.5),1))}
SimOut <- data.frame(DVDRef = out_Asim_within_df[,1], A = apply(out_Asim_within_df[,-1],1,median_integer))
			

sumarize_one_DVD <- function(x){

MaleP <-nrow(x[x$Sex==1,])
FemaleP <-nrow(x[x$Sex==0,])
TotalP <- MaleP+FemaleP
DiffP <- abs(MaleP-FemaleP)

A <- sum(diff(x$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
#S <- sum(diff(xxxx$Sex)!=0 & diff(x$Visits) <= 0.5)

summary_DVD <- data.frame(cbind(DVDRef=unique(x$DVDRef),TotalP,DiffP,MaxA,A))

summary_DVD

}


summary_all_DVD <- do.call(rbind,lapply(split(full_dat,full_dat$DVDRef),FUN=sumarize_one_DVD))

SimOut <- merge (x=SimOut, y= summary_all_DVD[,c('DVDRef', 'TotalP', 'DiffP', 'MaxA')], by='DVDRef', all.x=TRUE)
SimOut$Type <- 'a_Sim'
summary_all_DVD$Type <- 'z_Obsv'

head(summary_all_DVD)
head(SimOut)

fulldat_long <- rbind(summary_all_DVD,SimOut)
fulldat_long$rowID <- seq(1:nrow(fulldat_long))

}

{# analyse 

modA <- glm(A~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'poisson') # [,1] 
modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'binomial') # [,2] 
modTotalP_AMax <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) # [,3] 
modTotalP <- glmer(TotalP ~ scale(A)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long) # [,4] 

}


return(cbind(summary(modA)$coef[5:6,4], # [,1] 
summary(modAoutofAmax)$coef[5:6,4], # [,2] 
rbind(summary(modTotalP_AMax)$coef[4,4], NA), # [,3] 
rbind(summary(modTotalP)$coef[4,4], NA))) # [,4] 

# should look like this: 
                       # [,1]       [,2]      [,3]         [,4]
# Typez_Obsv:TotalP 0.4103925 0.05045313 0.2873707 0.0004660565
# Typez_Obsv:DiffP  0.7379561 0.03139404        NA           NA

}


n <- 100

modcoeff <- pbreplicate(n,Generate_data_randomize_them_and_analyse())
modcoeff_Sign <- modcoeff < 0.05

Typez_Obsv_TotalP <- NULL
Typez_Obsv_DiffP <- NULL

for (i in 1:n)
{
Typez_Obsv_TotalP[1] <- sum(modcoeff_Sign[1,1,1],modcoeff_Sign[1,1,2])/n
Typez_Obsv_TotalP[2] <- sum(modcoeff_Sign[1,2,1],modcoeff_Sign[1,2,2])/n
Typez_Obsv_TotalP[3] <- sum(modcoeff_Sign[1,3,1],modcoeff_Sign[1,3,2])/n
Typez_Obsv_TotalP[4] <- sum(modcoeff_Sign[1,4,1],modcoeff_Sign[1,4,2])/n

Typez_Obsv_DiffP[1] <- sum(modcoeff_Sign[2,1,1],modcoeff_Sign[2,1,2])/n
Typez_Obsv_DiffP[2] <- sum(modcoeff_Sign[2,2,1],modcoeff_Sign[2,2,2])/n
Typez_Obsv_DiffP[3] <- NA
Typez_Obsv_DiffP[4] <- NA

out <- rbind(Typez_Obsv_TotalP,Typez_Obsv_DiffP)

}

out # percentage of time where effect is significant. should be < 0.05 to be acceptably significant by chance.

# with n = 100
                  # [,1] [,2] [,3] [,4]
# Typez_Obsv_TotalP    0 0.02    0 0.02
# Typez_Obsv_DiffP     0 0.00   NA   NA







