#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Simulation necessary correlation between alternation and synchrony
#	 Start : 02/06/2016			last modif : 02/06/2016
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)

nbvisitfemale <-  rpois(1000, lambda=7)
nbvisitmale <-  rpois(1000, lambda=7)
totalnbvisit <- nbvisitfemale+ nbvisitmale
diffnbvisit <- abs(nbvisitfemale-nbvisitmale)
dtf <- data.frame(totalnbvisit,nbvisitfemale,nbvisitmale,diffnbvisit)


hist(nbvisitfemale)
hist(nbvisitmale)
hist(totalnbvisit)
hist(diffnbvisit)


# absolute number of maximum alternation FAmax is determined by the minimum of the two provisioning rates
for (i in 1:nrow(dtf)){
if(dtf$diffnbvisit[i] == 0 )
{dtf$FAmax[i] <- min(dtf$nbvisitfemale[i],dtf$nbvisitmale[i])*2-1}
else
{dtf$FAmax[i] <- min(dtf$nbvisitfemale[i],dtf$nbvisitmale[i])*2}

}

ggplot(data=dtf, aes(y=FAmax,x=totalnbvisit)) + geom_point() + geom_smooth(method = "lm") + geom_abline(intercept = 0, slope =1)
summary(lm(FAmax ~ totalnbvisit, data=dtf))


# alternation score is therefore determined by totalnbvisit
dtf$A <- round(dtf$FAmax / (dtf$totalnbvisit-1) *100)

hist(dtf$A)
ggplot(data=dtf, aes(y=A,x=totalnbvisit)) + geom_point() + geom_smooth(method = "lm")
summary(lm(A ~ totalnbvisit, data=dtf))

## version 1: when synchrony does not increase by chance as provisioning rate increase
# absolute number of synchrony FS is between 0 and absolute number of maximum alternation FAmax
for (i in 1: nrow(dtf)){
dtf$x[i] <- sample(0:dtf$FAmax[i], 1) # x is the portion of FAmax that will be substracted from FAmax to calculate FS
}

dtf$FS <- (dtf$FAmax - dtf$x) 
ggplot(data=dtf, aes(y=FS,x=FAmax)) + geom_point() + geom_smooth(method = "lm") + geom_abline(intercept = 0, slope =0.5)
summary(lm(FS ~ FAmax, data=dtf))


# synchrony score is therefore determined by alternation
dtf$S <- round(dtf$FS / (dtf$totalnbvisit -1)*100)

ggplot(data=dtf, aes(y=S,x=A)) + geom_point() + geom_smooth(method = "lm") + geom_abline(intercept = 0, slope = 0.5)
summary(lm(S ~ A, data=dtf))

# is this regression between S and A different from 0.5 ?
summary(lm(S ~ A + offset(A*0.5), data=dtf)) # No


# Ben's model on synchrony, when synchrony does not increase by chance as provisioning rate increase
summary(lm(S ~ A + totalnbvisit, data = dtf)) # quite wrong residuals !
summary(lm(FS ~ FAmax + totalnbvisit, data = dtf))

summary(lm(log(S+1) ~ A + totalnbvisit, data = dtf))
ggplot(data=dtf, aes(y=log(S+1),x=A)) + geom_point() + geom_smooth(method = "lm") 


## version 2: when synchrony does increase by chance as provisioning rate increase
# this is because when totalnbvisit increases, interfeed intervals are getting smaller so synchrony by chance more likely
# like earlier: FS is between 0 and FAmax, FAmax being itself constrained by totalnbvisit

for (i in 1: nrow(dtf)){
if (dtf$totalnbvisit[i] > 14) # when total provisioning rate is higher than average
dtf$x2[i] <- sample(0:dtf$FAmax[i], 1, prob = (length(0:dtf$FAmax[i]):1))  # x is the portion of FAmax that will be substracted from FAmax to calculate FS: linearly more likely to be close to zero
if (dtf$totalnbvisit[i] <= 14)  # when total provisioning rate is lower than average (and therefore FAmax also lower)
dtf$x2[i] <- sample(0:dtf$FAmax[i], 1, prob = (1:length(0:dtf$FAmax[i]))) # x is the portion of FAmax that will be substracted from FAmax to calculate FS: linearly more likely to be close to FAmax
}

hist(dtf$x2[dtf$totalnbvisit > 14])
hist(dtf$x2[dtf$totalnbvisit <= 14])

dtf$FS2 <- (dtf$FAmax - dtf$x2) 
ggplot(data=dtf, aes(y=FS2,x=FAmax)) + geom_point() + geom_smooth(method = "lm") + geom_abline(intercept = 0, slope =0.5)
summary(lm(FS2 ~ FAmax, data=dtf))


# synchrony score is therefore determined by alternation
dtf$S2 <- round(dtf$FS2 / (dtf$totalnbvisit-1) *100)

ggplot(data=dtf, aes(y=S2,x=A)) + geom_point() + geom_smooth(method = "lm") + geom_abline(intercept = 0, slope = 0.5)
summary(lm(S2 ~ A, data=dtf))

# is this regression between S and A different from 0.5 ?
summary(lm(S2 ~ A + offset(A*0.5), data=dtf)) # depends


# Ben's model on synchrony, when synchrony does increase by chance as provisioning rate increase
summary(lm(S2 ~ A + totalnbvisit, data = dtf))



summary(lm(FS2 ~ FAmax + totalnbvisit, data = dtf))









