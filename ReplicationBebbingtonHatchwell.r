#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Analyse provisioning data sparrows
#	 Start : 15/04/2015
#	 last modif : 15/04/2016  
#	 commit: simulation Kat style
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list = ls(all = TRUE))

# source('COMPILATION_PROVISIONING.R')



############################################ 
# replication Bebbington & Hatchwell study #
############################################

head(RawFeedingVisits,32)
head(MY_tblParentalCare)
library(dplyr)
library(ggplot2)

{### simulation alternation

{## calculation top 5% of feeding rates for each sex 

summary(MY_tblParentalCare$FVisit1RateH)
summary(MY_tblParentalCare$MVisit1RateH)
dev.new()
par(mfrow=c(2,1)) 
hist(MY_tblParentalCare$FVisit1RateH, xlim=c(0,50), ylim = c(0,1000))
hist(MY_tblParentalCare$MVisit1RateH, xlim=c(0,50), ylim = c(0,1000))

quantile(MY_tblParentalCare$FVisit1RateH[!is.na(MY_tblParentalCare$FVisit1RateH)], c(0.05,0.95))
quantile(MY_tblParentalCare$MVisit1RateH[!is.na(MY_tblParentalCare$MVisit1RateH)], c(0.05,0.95))
}

{## Get all simulated combinations of individuals with specific provisioning rates, and calculate their alternation

{# Create RawInterfeeds and split per sex and select provisioning rates from 3 to 22
RawInterfeeds <- merge(x= RawFeedingVisits[,c('DVDRef','Sex','Interval')], y=MY_tblParentalCare[,c('DVDRef','MVisit1RateH', 'FVisit1RateH','DiffVisit1Rate','AlternationValue')] , by='DVDRef', all.x=TRUE)

MRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','Interval','MVisit1RateH')], RawInterfeeds$Sex == 1)
MRawInterfeeds322 <- MRawInterfeeds[MRawInterfeeds$MVisit1RateH >=3 & MRawInterfeeds$MVisit1RateH <=22,]
FRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','Interval','FVisit1RateH')], RawInterfeeds$Sex == 0)
FRawInterfeeds322 <- FRawInterfeeds[FRawInterfeeds$FVisit1RateH >=3 & FRawInterfeeds$FVisit1RateH <=22,]
}

{# Randomise the interfeed intervals within individuals of the same sex that have the same visit rate
FShuffledInterfeeds322 <- FRawInterfeeds322[-1] %>% group_by(FVisit1RateH) %>% mutate(Interval=sample(Interval))
MShuffledInterfeeds322 <- MRawInterfeeds322[-1] %>% group_by(MVisit1RateH) %>% mutate(Interval=sample(Interval))
}

{# create one simulated df per sex per visit rate, with shuffled intervals associated to a SimID of length 'visit rate - 1'

SimFemale <- list ()

for (i in 3:22)
{
# one group of visit rate at a time
SimFemale[[i]] <- filter(FShuffledInterfeeds322, FVisit1RateH == i)
# add SimID to (visit rate - 1) visits
SimFemale[[i]] <- mutate(SimFemale[[i]], SimID = rep(1:((nrow(SimFemale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimFemale[[i]])))
# Shuffle the SimID
SimFemale[[i]]<-mutate(SimFemale[[i]], SimID = sample(SimID)) # sample without replacement
# sort (not needed but easier to look at output)
SimFemale[[i]]<-arrange(SimFemale[[i]],SimID)
# Calculate cumulative sum for each SimID
SimFemale[[i]]<-SimFemale[[i]]%>%
  group_by(SimID)%>%
  mutate(CumInt = cumsum(Interval))

}


SimMale <- list ()

for (i in 3:22)
{
# one group of visit rate at a time
SimMale[[i]] <- filter(MShuffledInterfeeds322, MVisit1RateH == i)
# add SimID to (visit rate - 1) visits
SimMale[[i]] <- mutate(SimMale[[i]], SimID = rep(1:((nrow(SimMale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimMale[[i]])))
# Shuffle the SimID
SimMale[[i]]<-mutate(SimMale[[i]], SimID = sample(SimID)) # sample without replacement
# sort
SimMale[[i]]<-arrange(SimMale[[i]],SimID)
# Calculate cumulative sum for each SimID
SimMale[[i]]<-SimMale[[i]]%>%
  group_by(SimID)%>%
  mutate(CumInt = cumsum(Interval))

}


SimMale <- do.call(rbind,SimMale)
SimFemale <- do.call(rbind,SimFemale)
SimData <- bind_rows(SimMale, SimFemale) # different from rbind as it binds two df with different columns, adding NAs
SimData[is.na(SimData)] <- 0
}

head(SimData)

{# create MiFj: 400 dataframe of combine male visit * female visit rate
# all individuals of one sex of one visit rate are reused for each combination involving this visit rate

MiFj <- list()
i = rep(3:22, each = 20) # male visit rate
j = rep((3:22), 20) # female visit rate
  
for (k in 1:400) # 400 combination
{ 
MiFj[[k]]<-SimData%>%
filter(MVisit1RateH==i[k] | FVisit1RateH==j[k])%>%
arrange(SimID, CumInt) 
}

AllMiFj <- do.call(rbind, MiFj)
nrow(AllMiFj) # 1075820
}

{# add running OverallSimID and select combinations with both sex, with the full number of intervals for a given provisioning rates
AllMiFj$OverallSimID <- cumsum(AllMiFj$SimID != c(0,head(AllMiFj$SimID,-1))) # shift all SimID from 1, get a running number changing at each mismatch between the original vector of SimID and the shifted one
AllMiFj$Sex <- as.numeric(as.character(AllMiFj$Sex))

AllMiFj_splitperOverallSimID <- split(AllMiFj, AllMiFj$OverallSimID)

AllMiFj_splitperOverallSimID_fun <- function(x){
return(c(
length(x$Sex[x$Sex==0]), 
length(x$Sex[x$Sex==1])
))
}

AllMiFj_splitperOverallSimID_out1 <- lapply(AllMiFj_splitperOverallSimID,FUN= AllMiFj_splitperOverallSimID_fun )
AllMiFj_splitperOverallSimID_out2 <- data.frame(rownames(do.call(rbind,AllMiFj_splitperOverallSimID_out1)),do.call(rbind, AllMiFj_splitperOverallSimID_out1))
 
rownames(AllMiFj_splitperOverallSimID_out2 ) <- NULL
colnames(AllMiFj_splitperOverallSimID_out2 ) <- c('OverallSimID','NbF', 'NbM')
 
# remove all OverSimID where one sex not present
AllMiFj <- AllMiFj[ ! AllMiFj$OverallSimID %in% AllMiFj_splitperOverallSimID_out2$OverallSimID[AllMiFj_splitperOverallSimID_out2$NbF == 0 | AllMiFj_splitperOverallSimID_out2$NbM == 0] ,]
nrow(AllMiFj) # 778147
 
# rename OverallSimID to have it continuous
AllMiFj$OverallSimID <- cumsum(AllMiFj$SimID != c(0,head(AllMiFj$SimID,-1)))

# write.table(AllMiFj, file = "AllMiFj.xls", col.names=TRUE, sep='\t') # 20160412
}

head(AllMiFj)

{# calculate alternation for each combination of individuals with specific provisioning rates

FinalMiFj <- group_by(AllMiFj,OverallSimID)

SimulatedSummaryKat <- summarise(FinalMiFj,
                            tt = n(), # what we have here are interfeeds > if we want numbers of feeds add 2 ?							
                            F = sum(diff(Sex)!=0),
                            A = round((F/(tt-1))*100,2),# what we have are interfeeds > ?
                            MVisitRate = max(MVisit1RateH),## added this for bootstrapping per category - this allows removing lines with 0 ?
                            FVisitRate = max(FVisit1RateH),## added this for bootstrapping per category
                            MFVisitRate = paste(max(MVisit1RateH),max(FVisit1RateH), sep="-"), ## added this for bootstrapping per category
                            VisitRateDifference= abs(max(MVisit1RateH)-max(FVisit1RateH)))

							
tail(as.data.frame(SimulatedSummaryKat),60)				
freqCombination <- arrange(count(SimulatedSummaryKat, MFVisitRate), n)
nrow(freqCombination) # 400 combinations

}

}

head(SimulatedSummaryKat)

{## bootstraping as a simple sampling of A with replacement

SimulatedSummaryKat_splitperMFVisiteRate <- split(SimulatedSummaryKat[,c('A','MFVisitRate' )], SimulatedSummaryKat$MFVisitRate)
x <- SimulatedSummaryKat_splitperMFVisiteRate[[1]]

SimulatedSummaryKat_splitperMFVisiteRate_fun <- function(x) {
base::sample(x$A, replace=TRUE) # sample with replacement, size = original size

return()  increase to 10 000
}

SimulatedSummaryKat_splitperMFVisiteRcate_out1 <- lapply(SimulatedSummaryKat_splitperMFVisiteRate,SimulatedSummaryKat_splitperMFVisiteRate_fun)
SimulatedSummaryKat_splitperMFVisiteRcate_out2 <- do.call(rbind, SimulatedSummaryKat_splitperMFVisiteRcate_out1)

Aboot_MFVisitRate <- data.frame(rep(rownames(SimulatedSummaryKat_splitperMFVisiteRcate_out2),each=10000)) #  increase to 10 000
Aboot_A <- data.frame(unlist(SimulatedSummaryKat_splitperMFVisiteRcate_out1))

Aboot <- cbind(Aboot_MFVisitRate,Aboot_A)
rownames(Aboot) <- NULL
colnames(Aboot) <- c('MFVisitRate', 'A')

Aboot <- merge(x=Aboot, y= unique(SimulatedSummaryKat[,c('MFVisitRate','VisitRateDifference')]), all.x=TRUE, by='MFVisitRate')


}

tail(Aboot,30)

{# summary Aboot
# per visit rate difference like in the paper					
Aboot_perVisitRateDiff <- group_by(Aboot, VisitRateDifference)

Summary_Aboot_perVisitRateDiff <- summarise (Aboot_perVisitRateDiff,
					Amean = mean(A),
					Alower = Amean - sd(A)/sqrt(n())*1.96,
					Aupper = Amean + sd(A)/sqrt(n())*1.96)
}

Summary_Aboot_perVisitRateDiff

{# summary Aobserved
# per visit rate difference like in the paper
MY_tblParentalCare_forA <- MY_tblParentalCare[!is.na(MY_tblParentalCare$AlternationValue),]
MY_tblParentalCare_perVisitRateDiff <- group_by(MY_tblParentalCare_forA, DiffVisit1Rate)

Summary_MY_tblParentalCare_perVisitRateDiff <- summarise (MY_tblParentalCare_perVisitRateDiff,
					Amean = mean(AlternationValue),
					Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96)
					
Summary_MY_tblParentalCare_perVisitRateDiff20 <- Summary_MY_tblParentalCare_perVisitRateDiff[1:20,]
Summary_MY_tblParentalCare_perVisitRateDiff20 <- dplyr::rename(Summary_MY_tblParentalCare_perVisitRateDiff20,VisitRateDifference= DiffVisit1Rate)

}

Summary_MY_tblParentalCare_perVisitRateDiff20

{# combine observed and expected and plot

# per visit rate difference like in the paper
Summary_MY_tblParentalCare_perVisitRateDiff20$Type <- "Observed"
Summary_Aboot_perVisitRateDiff$Type <- "Expected"

VisitRateDiff_Amean <- rbind(Summary_Aboot_perVisitRateDiff, Summary_MY_tblParentalCare_perVisitRateDiff20)

Fig1 <- ggplot(data=VisitRateDiff_Amean, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "Mean Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean$Amean, n = 9)) +  
  theme_classic()
  
}

}

dev.new()
Fig1


xx <- SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == 0]
xx <- SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == 12]

xxMean <- function(xx, i) mean(xx[i])
samplemean <- function(x, d) {
  return(mean(x[d]))
}

library(boot)



xxsample <- sample(xx,size = 10000, replace = TRUE)
mean(xxsample)

boot_xx <- boot(xx, samplemean, R=10000)
boot_ci <- boot.ci(boot_xx, type='norm')


#using the boot function
#first need to turn the alternation column of zerodiff into a vector
zero.diff <- zero.diff[,"alternation"]

mean.zero <- function(zero.diff, i) mean(zero.diff[i])
#now we can run the bootstrap for 10000 iterations

boot.zero <- boot(zero.diff, mean.zero, R=10000)
boot.zero
hist(boot.zero$t)

boot.ci(boot.zero)
upper.zero <- quantile(boot.zero$t, 0.975)