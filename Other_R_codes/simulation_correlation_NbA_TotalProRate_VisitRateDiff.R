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

# avPR <- 15 
# sdPR <- 8 
# VideoLength <- 90


create_fulldat_and_analyse <- function(avPR,sdPR,VideoLength, ASP){

meanlog <- log(avPR)
sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))

create_DVD <- function(){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog )
FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
MaleP <- rpois(1, MalePexp)
FemaleP <- rpois(1, FemalePexp)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP - FemaleP)

MaleVisits <- sort(runif(MaleP,0,VideoLength))
FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
DVD <- data.frame(rbind(cbind(Visits = MaleVisits, Sex = rep(1, length(MaleVisits))),cbind(Visits = FemaleVisits,Sex = rep(0, length(FemaleVisits)))))
DVD <- DVD[order(DVD$Visits),]

A <- sum(diff(DVD$Sex)!=0)
if (MaleP == FemaleP){MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP)) -1} else {MaxA <- MaleP + FemaleP - (abs(MaleP - FemaleP))}
S <- sum(diff(DVD$Sex)!=0 & diff(DVD$Visits) <= 0.5)
dat <- data.frame(cbind(TotalP,DiffP,MaxA,A,S))

dat
}

fulldat <- data.frame(matrix(data=unlist(pbreplicate(3000, create_DVD())), 3000,5, byrow = TRUE))
colnames(fulldat) <- c('TotalP','DiffP','MaxA','A','S')
fulldat$DVDRef <- seq(1:nrow(fulldat))


if(ASP == 'A')
{mod <- glm(cbind(A, MaxA-A)~ TotalP + DiffP, data=fulldat, family = 'binomial')
return(summary(mod)$coeff[2:3,4])}

if(ASP == 'S')
{mod <- glm(cbind(S, MaxA-S)~ TotalP + DiffP, data=fulldat, family = 'binomial')
return(summary(mod)$coeff[2:3,4])}

if(ASP == 'P')
{
# A 'simulated' (another one random)
fulldat2 <- data.frame(matrix(data=unlist(pbreplicate(3000, create_DVD())), 3000,5, byrow = TRUE))
colnames(fulldat2) <- c('TotalP','DiffP','MaxA','A','S')
fulldat2$DVDRef <- seq(1:nrow(fulldat2))

fulldat_long <- rbind(fulldat, fulldat2)
fulldat_long$Type <- c(rep("Obsv", nrow(fulldat)),rep("Sim", nrow(fulldat2)))
fulldat_long$rowID <- seq(1:nrow(fulldat_long))

mod <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=fulldat_long)
return(summary(mod)$coef[4,4])}

}

modcoeff_A <- pbreplicate(100,create_fulldat_and_analyse(15,8,90,'A'))
modcoeff_A_Sign <- modcoeff_A < 0.05
apply(modcoeff_A_Sign, 1, sum)/100


modcoeff_S <- pbreplicate(100,create_fulldat_and_analyse(15,8,90,'S'))
modcoeff_S_Sign <- modcoeff_S < 0.05
apply(modcoeff_S_Sign, 1, sum)/100


modcoeff_P <- pbreplicate(100,create_fulldat_and_analyse(15,8,90,'P'))
modcoeff_P_Sign <- modcoeff_P < 0.05
sum(modcoeff_P_Sign)/1000








# as poisson with type* rather than cbind

Generate_data_randomize_them_and_analyse <-function(model){

{# generate data

avPR <- 15 
sdPR <- 8
VideoLength <- 90

meanlog <- log(avPR)
sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))

full_dat <- list()

for (i in 1:1662){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog )
FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
MaleP <- rpois(1, MalePexp)
FemaleP <- rpois(1, FemalePexp)
TotalP <- MaleP + FemaleP
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

# analyse 

if (model == 'A')
{
modA <- glm(A~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'poisson')
return(summary(modA)$coef[5:6,4])
}
if(model=='cbindA')
{
modAoutofAmax <- glm(cbind(A, MaxA-A) ~ Type*TotalP + Type*DiffP, data=fulldat_long, family = 'binomial')
return(summary(modAoutofAmax)$coef[5:6,4])
}

}


modcoeff_A <- pbreplicate(100,Generate_data_randomize_them_and_analyse('A'))
modcoeff_A_Sign <- modcoeff_A < 0.05
apply(modcoeff_A_Sign, 1, sum)/100 
# Typez_Obsv:TotalP  Typez_Obsv:DiffP 
#             0.08              0.00

modcoeff_A <- pbreplicate(10,Generate_data_randomize_them_and_analyse('cbindA'))
modcoeff_A_Sign <- modcoeff_A < 0.05
apply(modcoeff_A_Sign, 1, sum)/10



