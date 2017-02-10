#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Joel PICK & Malika IHLE    joel.l.pick@gmail.com   malika_ihle@hotmail.fr
#	 Simulation to help decide which analyses to perfom on provisioning data sparrows
#	 Start : 02/02/2017
#	 last modif : 09/02/2017
#	 commit: add interaction term
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{### Packages
library(pbapply)
library(lme4)
options(scipen=999)
}

{### Predict Alternation

# mimicking our analysis to predict Nb of Alternation

sim <- function(){
MaleP <- rpois(1000, 10)
FemaleP <- rpois(1000, 10)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(1000,MaxA,0.5)
mod <- glm(cbind(A,MaxA-A) ~TotalP + DiffP, family = 'binomial')
summary(mod)$coef[2:3,4]
}

simOut <- replicate(1000, sim())
simOutSign <- simOut<0.05
apply(simOutSign, 1, sum)/1000 # this doesn't get significant more than by chance



# mimicking Ben's analysis on alternation score (Nb of alternation / total provisioning rate)

sim2 <- function(){
MaleP <- rpois(1000, 10)
FemaleP <- rpois(1000, 10)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(1000,MaxA,0.5)
mod <- lm(I(A/(TotalP-1)) ~ DiffP)
summary(mod)$coef[2,4]
}

simOut2 <- replicate(1000, sim2())
simOutSign2 <- simOut2<0.05
sum(simOutSign2)/1000 # this leads to significant results more than by chance due to mathematical relationship between dependent and explanatory variables

}

{### Predict provisioning rate as the fitness trait, while including variation in provisioning rate

n <- 1000
avPRobs <-15 # this is the average of provisioning rate observed in the data
sdPRobs <- 8 # it's SD
meanlog <- log(avPRobs)
sdlog <-  sqrt(log(1 + sdPRobs^2/avPRobs^2))


# mimicking Ben's analysis TotalProRate ~ Ascore or A

simProRate <- function(){

MalePexp <- rlnorm(n, meanlog = meanlog, sdlog = sdlog )
FemalePexp <- rlnorm(n, meanlog = log(avPRobs), sdlog = sqrt(log(1 + sdPRobs^2/avPRobs^2)) )
MaleP <- rpois(n, MalePexp)
FemaleP <- rpois(n, FemalePexp)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(n,MaxA,0.6) # observed = random
modreverse <- glm(TotalP ~ A/MaxA, family = 'poisson')
summary(modreverse)$coef[2,4]

}

simProRateOut <- pbreplicate(1000, simProRate())

simProRateOutSign <- simProRateOut < 0.05
sum(simProRateOutSign)/1000 # doing it that way will ALWAYS lead to significant results.


# micmicking our analysis: TotalProRate ~ A*Type(sim or obsv)

simProRate <- function(){

MalePexp <- rlnorm(n, meanlog = meanlog, sdlog = sdlog )
FemalePexp <- rlnorm(n, meanlog = log(avPRobs), sdlog = sqrt(log(1 + sdPRobs^2/avPRobs^2)) )
MaleP <- rpois(n, MalePexp)
FemaleP <- rpois(n, FemalePexp)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(n,MaxA,0.6) # 'observed' (one random)
DVDRef <- seq(1:length(A))

dat <- data.frame(cbind(TotalP,A,MaxA, DVDRef))
dat_long <- rbind(dat, dat)
dat_long$Type <- c(rep("Obsv", nrow(dat)),rep("Sim", nrow(dat)))
dat_long$A[dat_long$Type == 'Sim'] <- rbinom(n,MaxA,0.6) # 'simulated' (another one random)
dat_long$rowID <- seq(1:nrow(dat_long))

modreverse <- glmer(TotalP ~ I(A/MaxA)*Type + (1|DVDRef), family = 'poisson', data=dat_long)
summary(modreverse)$coef[4,4]

}

simProRateOut <- pbreplicate(1000, simProRate())

simProRateOutSign <- simProRateOut < 0.05
sum(simProRateOutSign)/1000 # does not get significant ever (A observed and A simulated are both random and therefore 'equal', when comparing their slope they do not differ)

}







### Predict Synchrony 

avPRobs <-15 # this is the average of provisioning rate observed in the data
sdPRobs <- 8 # it's SD
meanlog <- log(avPRobs)
sdlog <-  sqrt(log(1 + sdPRobs^2/avPRobs^2))
VideoLength <- 90

sim_S <- function(){

create_DVD <- function(){

MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog )
FemalePexp <- rlnorm(1, meanlog = log(avPRobs), sdlog = sqrt(log(1 + sdPRobs^2/avPRobs^2)) )
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

modS <- glm(cbind(S, MaxA-S)~ TotalP + DiffP, data=fulldat, family = 'binomial')
summary(modS)$coeff[2:3,4]
}

sim_S_Out <- pbreplicate(1000, sim_S())
sim_S_OutSign <- sim_S_Out < 0.05
apply(sim_S_OutSign, 1, sum)/1000



head(fulldat)










