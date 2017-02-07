## Joel Pick 20170202
# how is NbAlternation out of Nb of alternation maximum (NbA observed , Nb A missed)
# is mathematically linked with Total number of visits and the difference between number of visits


# mimicking our analysis
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
apply(simOutSign, 1, sum)/1000


hist(A)

# mimicking Ben's analysis
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
sum(simOutSign2)/1000




MaleP <- rpois(1000, 10)
FemaleP <- rpois(1000, 10)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(1000,MaxA,0.5)
mod <- glm(cbind(A,MaxA-A) ~ TotalP + DiffP, family = 'binomial')
modoff <- glm(A~TotalP + DiffP + offset(log(MaxA)), family = 'quasipoisson')

summary(mod)$coef
summary(modoff)$coef
 

exp(-0.002)
library(arm)
invlogit(-0.004)





# to analyse provisioning rate as the fitness trait
simProRate <- function(){
MaleP <- rpois(1000, 10)
FemaleP <- rpois(1000, 10)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(1000,MaxA,0.6) # observed = random
#mod <- glm(cbind(A,MaxA-A) ~TotalP + DiffP, family = 'binomial')
#summary(mod)$coef[2:3,4]
modreverse <- glm(TotalP ~ I(A/MaxA), family = 'poisson')
summary(modreverse)$coef[2,4]
}


simProRateOut <- replicate(10000, simProRate())
simProRateOutSign <- simProRateOut<0.05
sum(simProRateOutSign)/10000








n <- 1000

# to analyse provisioning rate as the fitness trait
simProRate <- function(n=1000){

meanlog <- log(15)
MalePexp <- rlnorm(n, meanlog=log(15), sdlog=sqrt(log(1 + 8^2/15^2)) )
FemalePexp <- rlnorm(n, meanlog=log(15), sdlog=sqrt(log(1 + 8^2/15^2)))
MaleP <- rpois(n, MalePexp)
FemaleP <- rpois(n, FemalePexp)
TotalP <- MaleP + FemaleP
DiffP <- abs(MaleP-FemaleP)
MaxA <- TotalP - DiffP
A <- rbinom(n,MaxA,0.6) # observed = random
#mod <- glm(cbind(A,MaxA-A) ~TotalP + DiffP, family = 'binomial')
#summary(mod)$coef[2:3,4]
modreverse <- glm(TotalP ~ A/MaxA, family = 'poisson')
summary(modreverse)$coef[2,4]
}

library(pbapply)

simProRateOut <- pbreplicate(1000, simProRate())

simProRateOutSign <- simProRateOut < 0.05
sum(simProRateOutSign)/1000


hist(MaleP)
mean(MaleP)
sd(MaleP)











