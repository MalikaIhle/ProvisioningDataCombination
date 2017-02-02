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










