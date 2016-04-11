#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Andrew Jones + Malika Ihle
#	 Simulation alternation provisioning
#	 Start : 07/04/2016
#	 last modif : 07/04/2016  
#	 commit: integrate Andrews code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))
library(ggplot2)
library(plyr); library(dplyr)
library(RODBC)

Fullprovisioning <- read.table("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination\\R_RawFeedingVisits.txt", header=T)
#Fullprovisioning <- read.csv("C:\\Users\\Andrew Jones\\Documents\\University\\Level 4\\Project\\R_RawFeedingVisits.csv")


# Group data by video file
by_pair <-group_by(Fullprovisioning, DVDRef)


{######################################################

# For each pair calculates the number of visits (count), the number of alternations, and the
# alternation rate.
Summarydata<-summarise(by_pair,
                       count = n(),
                       malecount = length(Sex[Sex==1]),
                       femalecount = length(Sex[Sex==0]),
                       number_alternations= sum(diff(Sex)!=0),
                       alternation_rate= (number_alternations/(count-1)),
                       alternationpercent= (alternation_rate*100))
	
	
conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")				   
#conDB= odbcConnectAccess("C:\\Users\\Andrew Jones\\Documents\\University\\Level 4\\Project\\Database Copy\\Database0.74_20160322_MI\\SparrowData.mdb")

BroodsPerPair <- sqlQuery(conDB, "
                          SELECT tblBroods.SocialMumID, tblBroods.SocialDadID, tblBroods.SocialMumCertain, tblBroods.SocialDadCertain, tblBroods.BroodRef, tblDVDInfo.Situation, tblDVDInfo.DVDRef, tblDVDInfo.DVDNumber, tblDVD_XlsFiles.Filename, tblDVDInfo.Age, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.OffspringNo, tblParentalCare.EffectTime
FROM ((tblBroods INNER JOIN tblDVDInfo ON tblBroods.BroodRef = tblDVDInfo.BroodRef) INNER JOIN tblDVD_XlsFiles ON tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef) INNER JOIN tblParentalCare ON (tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef) AND (tblDVDInfo.DVDRef = tblParentalCare.DVDRef)
                          WHERE (((tblBroods.SocialMumCertain)=Yes) AND ((tblBroods.SocialDadCertain)=Yes) AND ((tblDVDInfo.Situation)=4));")
close(conDB) # closes connection to db 



### 30/03
# Remove VJ0141 as mix up with videos on hard drive/excels/database (NOTE-changed from deleting VJ0139 on 31/03)
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="VJ0141")
# Remove what duplicates there are, for now remove the broodref (so both results) CHANGE WHEN SORTED
BroodsPerPair <- subset(BroodsPerPair, BroodRef!="48")
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="50195") #added 31/03
BroodsPerPair <- subset(BroodsPerPair, DVDNumber!="40256") #added 31/03

# Merge the alternation summary data with the BroodsPerPair query from the database:

Merged<-merge(Summarydata, BroodsPerPair, "DVDRef") # merging by 'DVDRef'

# Creating new Columns
# This creates a "PairID" 
Merged <- transform(Merged, PairID = as.numeric(interaction(SocialMumID, SocialDadID, drop=TRUE)))

# Create "visit rate" for both Male and Female
# Per hour to standardise
# Visit rate per hour = Number of visits/Effect time * 60
Merged <- transform(Merged, male_visit_rate = (malecount/EffectTime)*60)
Merged <- transform(Merged, female_visit_rate = (femalecount/EffectTime)*60)
Merged <- transform(Merged, visit_rate_difference = abs(male_visit_rate - female_visit_rate))

# Check distributions
malevisit<- ggplot(Merged, aes(x= male_visit_rate))+
  geom_histogram(binwidth=1)+
  theme_classic()

femalevisit<- ggplot(Merged, aes(x= female_visit_rate))+
  geom_histogram(binwidth=1)+
  theme_classic()

visitdiff<- ggplot(Merged, aes(x= visit_rate_difference))+
  geom_histogram(binwidth=1)+
  theme_classic()
  
  
# Round the visit rates to nearest whole number to get whole number differences
# Create new columns
Merged <- transform(Merged, round_male_visit_rate = round(male_visit_rate))
Merged <- transform(Merged, round_female_visit_rate = round(female_visit_rate))
Merged <- transform(Merged, visit_rate_diff_after_rounding = abs(round_male_visit_rate - round_female_visit_rate))


# Visit Rate Difference ---------------------------------------------------

# Scatter plot of all the points 
ggplot(Merged, aes(x=visit_rate_diff_after_rounding, y=alternation_rate))+
  geom_point()+
  ylim(0,1)+
  xlab("Visit rate difference")+
  ylab("Alternation rate")+
  theme_classic()
  
  
  # This will summarise the data by the difference in visit rate, giving mean alternation etc
# Enables a graph to be plotted with error bars
VisitRateSum<-summarise(group_by(Merged, visit_rate_diff_after_rounding),
                        meanalternation = mean(alternation_rate),
                        SDalt= sd(alternation_rate),
                        SampleSize= length(alternation_rate),
                        SE= SDalt/sqrt(SampleSize),
                        lwr= meanalternation-SE,
                        upr= meanalternation+SE)

# Plots the mean alternation for each difference in rate
ggplot(VisitRateSum, aes(x=visit_rate_diff_after_rounding, y=meanalternation))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lwr, ymax=upr))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  ylim(0,0.8)+
  theme_classic()
# This section calculates the difference between parents' visit rates and investigates how this
# affects alternation

}


# Interfeed Intervals -----------------------------------------------------
# Calculate interfeed intervals
males<-filter(by_pair, Sex==1)
females<-filter(by_pair, Sex==0)

males<-males %>%
  group_by(DVDRef) %>%
  mutate(interfeed_interval = c(0,diff(TstartFeedVisit)))

females<-females %>%
  group_by(DVDRef) %>%
  mutate(interfeed_interval = c(0,diff(TstartFeedVisit)))


females<-left_join(females, Merged, by="DVDRef")
males<-left_join(males, Merged, by="DVDRef")

mfinterfeed<-bind_rows(females,males)
mfinterfeed<-group_by(mfinterfeed, DVDRef)
mfinterfeed<- select(mfinterfeed, Sex, interfeed_interval, round_male_visit_rate, round_female_visit_rate, DVDRef, BroodRef, PairID)

maleinterfeed<-filter(mfinterfeed, Sex == 1)
maleinterfeed<-select(maleinterfeed, -round_female_visit_rate)

femaleinterfeed<-filter(mfinterfeed, Sex == 0)
femaleinterfeed<-select(femaleinterfeed, -round_male_visit_rate)

# Based on distribution of the rounded visit rates, I decided to do between 3 and 14 visits.
# This results in 12 visit rates for males and females giving 144 possible outcomes

# Filter the interfeed dataframes to only those where visit rate is 3 to 14 inclusive.
femaleinterfeed314<-filter(femaleinterfeed, round_female_visit_rate <= 14 & round_female_visit_rate >= 3)
maleinterfeed314<-filter(maleinterfeed, round_male_visit_rate <= 14 & round_male_visit_rate >= 3)



# Randomise the interfeed intervals within individuals that have the same visit rate
femaleinterfeed314sh<-femaleinterfeed314 %>% group_by(round_female_visit_rate) %>% mutate(interfeed_interval=sample(interfeed_interval))
maleinterfeed314sh<-maleinterfeed314 %>% group_by(round_male_visit_rate) %>% mutate(interfeed_interval=sample(interfeed_interval))

# Example to test to start with.
# Make df for female rate = 5, and assign 4 rows to a "Simulated Female"
femaleinterfeedsim5<-filter(femaleinterfeed314sh, round_female_visit_rate == 5)
femaleinterfeedsim5<-mutate(femaleinterfeedsim5, SimFemale = rep(1:((nrow(femaleinterfeedsim5)/4)+1), each = 4, len = nrow(femaleinterfeedsim5)))
# Shuffle the simulated female ID
femaleinterfeedsim5<-mutate(femaleinterfeedsim5, SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
femaleinterfeedsim5<-femaleinterfeedsim5%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

# Need to do this for each sex and all visit rates 3 - 14 (24 times in total)
#SimFemale3 means a dataframe of all simulated females visiting at rate 3

# females

SimFemale <- list ()

for (i in 3:14)
{
  
SimFemale[[i]]<-filter(femaleinterfeed314sh, round_female_visit_rate == i)
SimFemale[[i]]<-mutate(SimFemale[[i]], SimFemale = rep(1:((nrow(SimFemale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimFemale[[i]])))
# Shuffle the simulated female ID
SimFemale[[i]]<-mutate(SimFemale[[i]], SimFemale = sample(SimFemale))
# Calculate cumulative sum for each SimFemale
SimFemale[[i]]<-SimFemale[[i]]%>%
  group_by(SimFemale)%>%
  mutate(FemCumulative = cumsum(interfeed_interval))

}


# males

SimMale <- list ()

for (i in 3:14)
{
  
  SimMale[[i]]<-filter(maleinterfeed314sh, round_male_visit_rate == i)
  SimMale[[i]]<-mutate(SimMale[[i]], SimMale = rep(1:((nrow(SimMale[[i]])/(i-1))+1), each = (i-1), len = nrow(SimMale[[i]])))
  # Shuffle the simulated male ID
  SimMale[[i]]<-mutate(SimMale[[i]], SimMale = sample(SimMale))
  # Calculate cumulative sum for each SimMale
  SimMale[[i]]<-SimMale[[i]]%>%
    group_by(SimMale)%>%
    mutate(MaleCumulative = cumsum(interfeed_interval))
  
}



# Join together by Sex
SimulatedMales<-do.call(rbind,SimMale)
SimulatedFemales<-do.call(rbind,SimFemale)

# Select necessary rows and rename
SimulatedMales<- rename(SimulatedMales, MaleVisitRate = round_male_visit_rate)
SimulatedMales<- rename(SimulatedMales, SimID = SimMale)
SimulatedMales<- rename(SimulatedMales, Interval = MaleCumulative)
SimulatedMales<- select(SimulatedMales, SimID, Sex, MaleVisitRate, Interval)

SimulatedFemales<- rename(SimulatedFemales, FemaleVisitRate = round_female_visit_rate)
SimulatedFemales<- rename(SimulatedFemales, SimID = SimFemale)
SimulatedFemales<- rename(SimulatedFemales, Interval = FemCumulative)
SimulatedFemales<- select(SimulatedFemales, SimID, Sex, FemaleVisitRate, Interval)

SimulatedData<-bind_rows(SimulatedMales, SimulatedFemales) # are not list anymore
SimulatedData$MaleVisitRate<- ifelse(is.na(SimulatedData$MaleVisitRate), 0, SimulatedData$MaleVisitRate)
SimulatedData$FemaleVisitRate<- ifelse(is.na(SimulatedData$FemaleVisitRate), 0, SimulatedData$FemaleVisitRate)
SimulatedData<- mutate(SimulatedData, VisitDifference=abs(MaleVisitRate-FemaleVisitRate))


MiFj <- list()
i = rep(3:14, each = 12) # male visit rate
j = rep((3:14), 12) # female visit rate
  
for (k in 1:144) # 144 combination
{ 
MiFj[[k]]<-SimulatedData%>%
filter(MaleVisitRate==i[k] | FemaleVisitRate==j[k])%>%
arrange(SimID, Interval) 
}


MergedSimData<- do.call(rbind, MiFj)
MergedSimData<-mutate(MergedSimData, OverallSimID=cumsum(SimID != c(".NOTHING.",head(SimID,-1))))


# write.table(MergedSimData, file = "R_MergedSimDataRStudio.xls", col.names=TRUE, sep='\t') # 20160407



FinalSimData<-group_by(MergedSimData, OverallSimID)

SimulatedSummary<-summarise(FinalSimData,
                            count = n(),
                            malecount = length(Sex[Sex==1]),
                            femalecount = length(Sex[Sex==0]),
                            number_alternations= sum(diff(Sex)!=0),
                            alternation_rate= (number_alternations/(count-1)),
                            alternationpercent= (alternation_rate*100),
                            MVisitRate = max(MaleVisitRate),## added this for bootstrapping per category - this allows removing lines with 0 ?
                            FVisitRate = max(FemaleVisitRate),## added this for bootstrapping per category
                            MFVisitRate = paste(max(MaleVisitRate),max(FemaleVisitRate), sep="-"), ## added this for bootstrapping per category
                            VisitRateDifference= abs(max(MaleVisitRate)-max(FemaleVisitRate))) ### this poss where prob arise 20:46//now works 20:56



## bootstraping = simple sample with replacement ?

boot_per_ij_bootnb <- list()

for (bootnb in 1:10) # increase to 10 0000
{
boot_per_ij <- list()
i = rep(3:14, each = 12) # male visit rate
j = rep((3:14), 12) # female visit rate

for (k in 1:144) # 144 combination
{ 
boot_per_ij[[k]] <- SimulatedSummary[,c('alternation_rate', 'MVisitRate','FVisitRate','MFVisitRate' )]%>%
  filter(MVisitRate==i[k] & FVisitRate==j[k])%>%
  mutate(alternation_rate=sample(alternation_rate, replace=TRUE))
}

boot_per_ij_bootnb[[bootnb]] <- do.call(rbind, boot_per_ij)

}

ALLboot_per_ij_bootnb <- do.call(rbind, boot_per_ij_bootnb)






AltSim<-select(SimulatedSummary, OverallSimID, alternation_rate, alternationpercent, MFVisitRate,VisitRateDifference)

### Make summary for each difference in rate
SimByRateSum<-summarise(group_by(AltSim, VisitRateDifference),
                        meanalternation = mean(alternation_rate),
                        SDalt= sd(alternation_rate),
                        SampleSize= length(alternation_rate),
                        SE= SDalt/sqrt(SampleSize),
                        lwr= meanalternation-SE,
                        upr= meanalternation+SE)

simplot<- ggplot(SimByRateSum, aes(x=VisitRateDifference, y=meanalternation))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lwr, ymax=upr))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  ylim(0,1)+
  theme_classic()

# Combine them
VisitRateSum<- rename(VisitRateSum, VisitRateDifference = visit_rate_diff_after_rounding)
VisitRateSum<- mutate(VisitRateSum, Type= rep("Observed", nrow(VisitRateSum)))
SimByRateSum<- mutate(SimByRateSum, Type= rep("Expected", nrow(SimByRateSum)))
CombinedExpObs<- bind_rows(VisitRateSum, SimByRateSum)

ggplot(data=CombinedExpObs, aes(x=VisitRateDifference, y=meanalternation, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lwr, ymax=upr))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  theme_classic()

# Above graph has all of the observed, but to compare just the ones we did simulation for:
CombinedExpObsL14<- filter(CombinedExpObs, VisitRateDifference<=14)
CombinedExpObsL14$Type<-factor(CombinedExpObsL14$Type)
ggplot(data=CombinedExpObsL14, aes(x=VisitRateDifference, y=meanalternation, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=lwr, ymax=upr))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("black", "grey"), labels=c("95% Expected", "Mean Observed"))+
  scale_x_continuous(breaks = pretty(CombinedExpObsL14$VisitRateDifference, n = 15)) +
  scale_y_continuous(breaks = pretty(CombinedExpObsL14$meanalternation, n = 8))+
  theme_classic()
