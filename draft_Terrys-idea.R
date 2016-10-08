#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Terry's idea: put one bird to unit, rescale other accordingly
#	 Start : 05/10/2016
#	 last modif : 06/10/2016
#	 commit: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{#### remarks
# I will have to neglect the time birds spend within the nest box, which might not be neglecteable 
# hist(MY_RawFeedingVisits$TendFeedVisit - MY_RawFeedingVisits$TstartFeedVisit, 40)
# summary(MY_RawFeedingVisits$TendFeedVisit - MY_RawFeedingVisits$TstartFeedVisit)

# I will have to select files where both birds visit at least twice, to scale at least an intervisit interval

# I pick the standard bird to be the first arriving at the nest within that nest watch. 

# if the first bird is not the last, the other bird last visit whose time during the last visit is not overlapping
}

{#### packages
library(dplyr)
library(ggplot2)
}

{#### functions

{# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

}

}

{#### selection valid files

{### copy paste from AlternationSynchronyDataAnalyses code to get same file selection

{### Get raw data (from source() or R_output folder)


{# output csv files

# source('COMPILATION_PROVISIONING.R')
# or :

output_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_output"

MY_tblParentalCare <- read.csv(paste(output_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblBroods <- read.csv(paste(output_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
MY_tblDVDInfo <- read.csv(paste(output_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(output_folder,"R_MY_RawFeedingVisits.csv", sep="/")) # OF directly followed by IN are merged into one feeding visits ; will be used for simulation
}

{# input txt files

input_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_input"

FedBroods <-  read.table(file= paste(input_folder,"FedBroods.txt", sep="/"), sep='\t', header=T)  ## from Ian Cleasby 20160531

}


}

{### select valid video files for studying behavioural compatibility in chick provisioning

list_non_valid_DVDRef <- 
c(
MY_tblParentalCare$DVDRef[!(MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 10 files with no visits at all + 2 files with no feeding visits at all
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)],# 6 - where 0 chicks
MY_tblDVDInfo$DVDRef[ ! MY_tblDVDInfo$ChickAge >5 & MY_tblDVDInfo$DVDInfoChickNb > 0 & (MY_tblDVDInfo$DVDRef)%in%(MY_RawFeedingVisits$DVDRef) ],# 171 - where still brooding (age <=5) and with chicks and with feeding visit
MY_tblParentalCare$DVDRef[(MY_tblParentalCare$MVisit1 ==0 | MY_tblParentalCare$FVisit1 ==0 )& MY_tblDVDInfo$DVDInfoChickNb > 0 & MY_tblDVDInfo$ChickAge >5  & (MY_tblParentalCare$DVDRef)%in%(MY_RawFeedingVisits$DVDRef)], # 153 - one sex did not visit for feeding despite having chicks above age 5
MY_tblDVDInfo$DVDRef[ !MY_tblDVDInfo$BroodRef %in% MY_tblBroods$BroodRef],# 2 DVD where both parents unidentified
MY_tblDVDInfo$DVDRef[MY_tblDVDInfo$BroodRef %in% unlist(FedBroods)] # 106 extra files for 48 broods (the 49th: 980 already excluded as only female visited) fed by Ian 
)


length(unique(list_non_valid_DVDRef)) # 450 

MY_tblDVDInfo <- MY_tblDVDInfo[ ! MY_tblDVDInfo$DVDRef %in% list_non_valid_DVDRef,]
MY_tblParentalCare <- MY_tblParentalCare[ ! MY_tblParentalCare$DVDRef %in% list_non_valid_DVDRef,]
MY_RawFeedingVisits  <- MY_RawFeedingVisits[ ! MY_RawFeedingVisits$DVDRef %in% list_non_valid_DVDRef,]

}

}

#MY_RawFeedingVisits$Sex <- as.factor(MY_RawFeedingVisits$Sex)
MY_RawFeedingVisits$DVDRef <- as.factor(MY_RawFeedingVisits$DVDRef)

{### keep files with at least 2 visit per sex

split_MY_RawFeedingVisits_perDVD <- split(MY_RawFeedingVisits,MY_RawFeedingVisits$DVDRef)

split_MY_RawFeedingVisits_perDVD_fun <- function(x) {
return(c(nrow(x[x$Sex == 0,]), nrow(x[x$Sex == 1,]), nrow(x)))
}

out1_split_MY_RawFeedingVisits_perDVD <- lapply(split_MY_RawFeedingVisits_perDVD,split_MY_RawFeedingVisits_perDVD_fun)
out2_split_MY_RawFeedingVisits_perDVD <- data.frame(rownames(do.call(rbind,out1_split_MY_RawFeedingVisits_perDVD)),do.call(rbind, out1_split_MY_RawFeedingVisits_perDVD))
nrow(out2_split_MY_RawFeedingVisits_perDVD)	# 1662
rownames(out2_split_MY_RawFeedingVisits_perDVD) <- NULL
colnames(out2_split_MY_RawFeedingVisits_perDVD) <- c('DVDRef','CoutSex0','CoutSex1','Nrow')

DVDRefToExclude <- out2_split_MY_RawFeedingVisits_perDVD$DVDRef[out2_split_MY_RawFeedingVisits_perDVD$CoutSex0 <2 | out2_split_MY_RawFeedingVisits_perDVD$CoutSex1 <2]


MY_RawFeedingVisits <- data.frame(MY_RawFeedingVisits[!MY_RawFeedingVisits$DVDRef %in% DVDRefToExclude,])

out3_split_MY_RawFeedingVisits_perDVD <- data.frame(out2_split_MY_RawFeedingVisits_perDVD[!out2_split_MY_RawFeedingVisits_perDVD$DVDRef %in% DVDRefToExclude,])
out3_split_MY_RawFeedingVisits_perDVD$splitID <- seq_along(out3_split_MY_RawFeedingVisits_perDVD$DVDRef)
head(out3_split_MY_RawFeedingVisits_perDVD)

MY_RawFeedingVisits <- MY_RawFeedingVisits[,c('DVDRef', 'TstartFeedVisit','Sex','Interval' )]
MY_RawFeedingVisits <- merge(MY_RawFeedingVisits, out3_split_MY_RawFeedingVisits_perDVD[,c('DVDRef','splitID')])
MY_RawFeedingVisits <- MY_RawFeedingVisits[order(MY_RawFeedingVisits$DVDRef),]

}


}

head(MY_RawFeedingVisits)

{#### plot some raw data


# plotting time in the nest

plot9randomgraphsTimeInNB <- function(){
p <- NULL

for (j in 1:9)  {
for (i in sample(1:length(split_MY_RawFeedingVisits_perDVD),9))
{
p[[j]] <-ggplot(split_MY_RawFeedingVisits_perDVD[[i]], aes(colour=as.factor(Sex))) + 
		geom_segment(aes(x=TstartFeedVisit, xend=TendFeedVisit, y=as.factor(Sex), yend=as.factor(Sex)), size=3) +
		xlab("Duration") +
		geom_text(x=max(split_MY_RawFeedingVisits_perDVD[[i]]$TendFeedVisit)/2, 
				y= 0.5,label=unique(split_MY_RawFeedingVisits_perDVD[[i]]$DVDRef), colour='black')+
		theme(legend.position="none")
}
}

multiplot(p[[1]],p[[2]],p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], cols=3)

}

set.seed(10)
plot9randomgraphsTimeInNB()

# plotting visits, considering they do not have duration (i.e. how 'intervals' were calculated)

plot9randomgraphs <- function(){
p <- NULL

for (j in 1:9)  {
for (i in sample(1:length(split_MY_RawFeedingVisits_perDVD),9))
{
p[[j]] <-ggplot(split_MY_RawFeedingVisits_perDVD[[i]], aes(colour=as.factor(Sex))) + 
		geom_segment(aes(x=TstartFeedVisit, xend=TstartFeedVisit+0.5, y=as.factor(Sex), yend=as.factor(Sex)), size=3) +
		xlab("Duration") +
		geom_text(x=max(split_MY_RawFeedingVisits_perDVD[[i]]$TendFeedVisit)/2, 
				y= 0.5,label=unique(split_MY_RawFeedingVisits_perDVD[[i]]$DVDRef), colour='black')+
		theme(legend.position="none")
}
}

multiplot(p[[1]],p[[2]],p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], cols=3)

}

set.seed(10)
dev.new()
plot9randomgraphs()

}


{###### scale visits

split_MY_RawFeedingVisits_per_splitID <- split(MY_RawFeedingVisits,MY_RawFeedingVisits$splitID)

out_scaling <- list()
out_scaling_for_plotting <- list()

options(warn=2) # so that loop breaks if one file doesn't work through all functions: call 'j' to know which one and check it out

for (j in 1:length(unique(MY_RawFeedingVisits$splitID)))
{

x <- split_MY_RawFeedingVisits_per_splitID[[j]]
	# examples with different issues to solve
	# x <- split_MY_RawFeedingVisits_per_splitID[[2]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[778]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[432]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[7]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[1202]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[17]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[13]]
	# StandardizingSex <- 1	
	# x <- split_MY_RawFeedingVisits_per_splitID[[84]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[1488]]
	
StandardizingSex <- sample(c(0,1),1) # pick a random sex as the sdandardizing sex

x_StandardizingSex = subset(x, Sex == StandardizingSex)
x_OtherSex = subset(x, Sex != StandardizingSex)

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$TstartFeedVisit[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$TstartFeedVisit[-1],NA)	

		# for the standardizing sex, all intervals will be set to its initial mean interval
		multiplicator <-  mean(x_StandardizingSex$Interval[-1])/x_StandardizingSex$Interval[-1]

# to solve problem caused by two consecutive visits of the standardizing bird at the same Tstart (issie with scoring to the 10th of minutes...)
# need to exclude multiplicator == 'Inf' that arise for intervals == 0
ScaledInterval <- median(x_StandardizingSex$Interval[-1]*multiplicator, na.rm=TRUE) # all scaled intervals (interval*multiplicator) are the same, but those that are 'Inf' excluded by taking the 'median' of this vector
multiplicator[which(is.infinite(multiplicator))] <- multiplicator[which(is.infinite(multiplicator))-1] # replace the 'Inf' multiplicator by the multiplicator just previous to it, that is the one with the same initial Tstart

# to recalculate ScaledTstart, start from the initial first Tstart and add up scaled intervals (cumulative sum)
x_StandardizingSex$ScaledInterval <- c(0,(rep(ScaledInterval, nrow(x_StandardizingSex)-1))) # the scaled Interval for the standardizing sex is always the same, hence the repeat function
x_StandardizingSex$ScaledTstart <- x_StandardizingSex$TstartFeedVisit[1] + cumsum(x_StandardizingSex$ScaledInterval) 


		# create vector of times on each foraging trip (all 10th of minute in between two Tstart from the same sex)
		StandardizingSex_trip = mapply(FUN = function(TstartFeedVisit, NextTstart) {  
		if (TstartFeedVisit==NextTstart) 
		{return (TstartFeedVisit)} 
		if (TstartFeedVisit!=NextTstart)	
		{return(list(((TstartFeedVisit*10) : (NextTstart*10-1))/10))}}, # (the *10 and then /10 are the easiest way to construct thenths of minutes)
		TstartFeedVisit = x_StandardizingSex$TstartFeedVisit[-nrow(x_StandardizingSex)], 
		NextTstart = x_StandardizingSex$NextTstart[-nrow(x_StandardizingSex)])
		
		OtherSex_trip = mapply(FUN = function(TstartFeedVisit, NextTstart) {  
		if (TstartFeedVisit==NextTstart) 
		{return (TstartFeedVisit)} 
		if (TstartFeedVisit!=NextTstart)	
		{return(list(((TstartFeedVisit*10) : (NextTstart*10-1))/10))}}, 
		TstartFeedVisit = x_OtherSex$TstartFeedVisit[-nrow(x_OtherSex)], 
		NextTstart = x_OtherSex$NextTstart[-nrow(x_OtherSex)])
		

		# check for the list entry of the other sex how many of the numbers also occur for the first sex (here the standadirzing sex)
		# this gives you the number of tenths-of-minutes that both birds were foraging at the same time
		outK <- NULL
		outKI<- list()
		
		for (i in 1:length(OtherSex_trip)){
		for (k in 1:length(StandardizingSex_trip)){
		outK[k] <- length(which(OtherSex_trip[[i]] %in% StandardizingSex_trip[[k]])) # stored the number of 10th of minutes from the other sex i trip that overlaps with all k trips from the standardising sex
		}
		outKI[[i]] <- sum(outK*multiplicator)/10 # there is one multiplicator per standardizing sex trip ; this is the scaled interval fro the other sex for this i trip
		}
		
# recalculate ScaledTstart from those ScaledIntervals for the other sex
x_OtherSex$ScaledInterval <- c(0,do.call(rbind,outKI))
x_OtherSex$ScaledTstart <- x_OtherSex$TstartFeedVisit[1] +cumsum(x_OtherSex$ScaledInterval)

# recreate x with Tstart and scaledTstart for both sexes
x <-rbind(x_StandardizingSex, x_OtherSex)
x <-x[,-which(names(x)%in%c("NextTstart"))]
x <- x[order(x$TstartFeedVisit),]

# to solve edges with no overlap: modify some ScaledInterval and ScaledTstart from x
FirstSex <- x$Sex[1] # who is the first sex to visit
LastSex <- x$Sex[nrow(x)] # who is the last sex to visit

if (StandardizingSex != FirstSex){ # if StandardizingSex is not the FirstSex, the first intervals of the others sex can't already be standardized, they do not fully overlap with the intervals of the standardizing sex, and are therefore left intact, unstandardized

# add to the first overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$TstartFeedVisit == min(x$TstartFeedVisit[x$Sex==FirstSex & x$TstartFeedVisit >=min(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$TstartFeedVisit == min(x$TstartFeedVisit[x$Sex==FirstSex & x$TstartFeedVisit >=min(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])]+
min(x$TstartFeedVisit[x$Sex==StandardizingSex]) - max(x$TstartFeedVisit[x$Sex==FirstSex & x$TstartFeedVisit <= min(x$TstartFeedVisit[x$Sex==StandardizingSex])])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$TstartFeedVisit <= min(x$TstartFeedVisit[x$Sex==StandardizingSex]) & x$Sex==FirstSex] <- 
x$Interval[x$TstartFeedVisit <= min(x$TstartFeedVisit[x$Sex==StandardizingSex]) & x$Sex==FirstSex] 

# recalculate the Tstart for the first overlapping trip of the other sex and for the exra non overlapping trips of the other sex from the beginning of the nest watch
x$ScaledTstart[x$Sex==FirstSex] <- x$TstartFeedVisit[1] + cumsum(x$ScaledInterval[x$Sex==FirstSex])

}

if (StandardizingSex != LastSex){ # if Standardazing is not the LastSex: there is no interval for the other sex to overlap with at the end, those ones can't be standardized, and are therefore 'left' intact unstandardized

# add to the last overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$TstartFeedVisit == min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$TstartFeedVisit == min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])]+
min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ]) - max(x$TstartFeedVisit[x$Sex==StandardizingSex])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$TstartFeedVisit > min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])] <- 
x$Interval[x$TstartFeedVisit > min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])]

# recalculate the Tstart for the last overlapping trip of the other sex and for the exra non overlapping trips of the other sex from the end of the nest watch
x$ScaledTstart[x$TstartFeedVisit >= min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])] <-
x$ScaledTstart[x$TstartFeedVisit == max(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit <=max(x$TstartFeedVisit[x$Sex==StandardizingSex])]) & x$Sex==LastSex ] +
cumsum(x$ScaledInterval[x$TstartFeedVisit >= min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])])

}

# round calculated numbers
x$ScaledTstart <- round(x$ScaledTstart,1)
x$ScaledInterval <- round(x$ScaledInterval,1)


out_scaling[[j]] <- x	# store x with raw and scaled Tsart & Intervals for each video file j


# reshape x for plotting
x_raw <- x[,c('splitID','DVDRef','TstartFeedVisit','Sex')]
x_raw$Type <- 'Original'
colnames(x_raw)[which(names(x_raw) == "TstartFeedVisit")] <- "Tstart"
	
x_scaled <- x[,c('splitID','DVDRef','ScaledTstart','Sex')]
x_scaled$Type <- 'Scaled'
colnames(x_scaled)[which(names(x_scaled) == "ScaledTstart")] <- "Tstart"	
x_scaled$Sex[x_scaled$Sex == 0 & x_scaled$Type == "Scaled"] <- -1
x_scaled$Sex[x_scaled$Sex == 1 & x_scaled$Type == "Scaled"] <- 2

x_for_plotting <- rbind(x_raw,x_scaled)
x_for_plotting <- x_for_plotting[order(x_for_plotting$DVDRef),]
rownames(x_for_plotting) <- NULL

# add colours for plotting depending on standardizing sex (random for each j)
x_for_plotting$Colours[x_for_plotting$Sex == 0] <- "orange"
x_for_plotting$Colours[x_for_plotting$Sex == 1] <- "green"

if(StandardizingSex == 0){
x_for_plotting$Colours[x_for_plotting$Sex == -1] <- "black"
x_for_plotting$Colours[x_for_plotting$Sex == 2] <- "deepskyblue"
}

if(StandardizingSex == 1){
x_for_plotting$Colours[x_for_plotting$Sex == -1] <- "red"
x_for_plotting$Colours[x_for_plotting$Sex == 2] <- "black"
}

out_scaling_for_plotting[[j]] <- x_for_plotting

}
	
MY_RawFeedingVisits_scaled <- do.call(rbind, out_scaling)
MY_RawFeedingVisits_scaled_for_plotting <- do.call(rbind, out_scaling_for_plotting)

}

head(MY_RawFeedingVisits_scaled,20)
head(MY_RawFeedingVisits_scaled_for_plotting,50)

{#### plot scaled visit near raw visits

split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID <- split(MY_RawFeedingVisits_scaled_for_plotting,MY_RawFeedingVisits_scaled_for_plotting$splitID)

plot9randomgraphs_scaled <- function(){
p <- NULL

for (j in 1:9)  {
for (i in sample(1:length(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID),9))
{

p[[j]] <-ggplot(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]])  + 
		geom_segment(aes(x=Tstart, xend=Tstart+0.5, y=as.factor(Sex), yend=as.factor(Sex)), size=3, colour =split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$Colours) +
		xlab("Nest watch duration") + ylab("Sex")+ scale_y_discrete(breaks=c("-1","0","1","2"),
        labels=c("S0", "0", "1","S1"))+
		geom_text(x=(max(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$Tstart) + 0.5)/2, 
				y= 2.5,label=unique(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$DVDRef), colour='black')+
		theme(legend.position="none") 
}
}

multiplot(p[[1]],p[[2]],p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], cols=3)

}

set.seed(10)
dev.new()
plot9randomgraphs_scaled()

}







