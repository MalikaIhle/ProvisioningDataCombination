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
library(boot)

### simulation alternation

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

{## bootstrap A from SimulatedSummaryKat within each visit rate difference

samplemean <- function(x, d) {return(mean(x[d]))}

Aboot <- data.frame(matrix(,data=NA, nrow=20, ncol=4))
colnames(Aboot) <- c('VisitRateDifference','Amean','Alower','Aupper')
Aboot$VisitRateDifference <- c(0:19)

for (i in 1:20)
{
Aboot$Amean[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$t0
Aboot$Alower[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$normal[2] 
Aboot$Aupper[i] <- boot.ci(boot(SimulatedSummaryKat$A[SimulatedSummaryKat$VisitRateDifference == i-1], samplemean, R=10000), type='norm')$normal[3] 
}
}

Aboot

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
Aboot$Type <- "Expected"

VisitRateDiff_Amean <- rbind(Aboot, Summary_MY_tblParentalCare_perVisitRateDiff20)

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



dev.new()
Fig1



# Description of Kat’ simulation described in the paper page 3 + supp figure:
 
# Alternation score fore observed nest watches:
# A = F/ (t-1)
# with F the number of alternation and t the number of feeding visits
# for a female provisioning rate x = 7 and a male provisioning rate y = 10, the number of feeding visits is 17 (if one hour was watched).

# Simulation steps:
# 1) select the provisioning rates for individuals of either sexes that are not too infrequent (remove the extreme low and high values of x or y)
# 2) extract all interfeed intervals for all individuals of a same provisioning rate and of a same sex and randomize them
# 3) (see supp fig) create all combinations of female and male with provisioning rate x and y by sampling without replacement x-1 female interfeed intervals and y-1 male interfeed intervals. Several simulated nest watches are possible per each provisioning rate combination, depending on the number of individuals with these provisioning rates that were observed . Simulated nest watches are created until one of the pool of interfeed intervals per each provisioning rate per sex is empty. Each pool of interfeed intervals, for instance the one for female with provisioning rate x, are reuse for each combination involving x.
# 4) (see supp fig) the cumulative sum of interfeed interval are calculated for each sex separately, and then rows are merged and sorted 

# 5) Alternation score for those simulated nest watch is then calculated with the formula:
# A = F/ (t-1)
# with F the number of alternation and t the number of feeding interval
# for a female provisioning rate x = 7 and a male provisioning rate y = 10, the number of feeding intervals is 15.

# 6) within each combination of provisioning rate combination, 10000 bootstraps of alternation scores were ran (this is what the paper says, but in fact the code Kat sent shows that the bootstrapping was made at a latter stage, see below)
# 7) All simulated alternation scores were pooled into groups of visit rate differences (absolute value of x minus y) [and the bootstrapping happened here in Kat’s code – but maybe it does not matter)
# 8) All observed alternation scores were also pooled into groups of visit rate differences and compared to the simulated one leading to Fig. 1.

# My point on view on this:
# 1) their selection seems arbitrary, I suggest we remove the extreme 5% quantile.
# 2) what about non-independence of some nest watch because of same MID or FID or PairID ? Is it ok because they will then be more represented in both observed and simulated nest watches ? But their individuality is anyway disrupted, for instance for a same provisioning rate, a bird can be consistently very regular or can be very irregular. I think this argues for randomizing within nest watch.
# 3) what if we randomly select several large intervals and spill over the hour ? Also, in our case, we also calculated visit rate per hour although we typically record for 90 min, so we maybe have more variance in interfeed intervals to select from when picking ‘visit rate-1’ interfeed intervals.
# 4) (see supp fig) intervals length are considered like the starting time of feeding visits, and like if the first visit of the male and the female were both at time zero. I think maybe it is better again to shuffle intervals within a file, keeping the firs time start of this file.
# 5) the formula to calculate A is different for observed and simulated nest watches I believe, and the maximum A that can be obtained for a same combination of x and y is systematically lower for the simulated ones as soon as the provisioning rate difference (absolute value of x minus y) is larger than 1 (see excel file attached).
# 8) and 1) 
# for simulation : selection of individuals that have a provisioning rate not extreme, then pooled into visit rate differences, only with pairs whose individuals have non extreme provisioning rate.
# in observed values: we take every individuals, every combinations, so for a same visit rate difference, individuals can have very extreme provisioning rates. I think observed combinations where one of the sex has an extreme value for provisioning rate should be remove for comparison with simulated data.

# I don’t know how this impact Kat’s results, but I will now try to run simulations within files because I can better argue for it, and maybe compare both outputs to let her know.





