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













