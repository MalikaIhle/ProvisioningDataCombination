#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Terry's idea: put one bird to unit, rescale other accordingly
#	 Start : 05/10/2016
#	 last modif : 19/10/2016
#	 commit: modify scaling function so that identical Tstart for a same sex is incremented by 0.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

{#### remarks
# I will have to neglect the time birds spend within the nest box, which might not be neglecteable 
# hist(MY_RawFeedingVisits$TendFeedVisit - MY_RawFeedingVisits$TstartFeedVisit, 40)
# summary(MY_RawFeedingVisits$TendFeedVisit - MY_RawFeedingVisits$TstartFeedVisit)

# I will have to select files where both birds visit at least twice, to scale at least an intervisit interval

# if the standardizing bird is not the first and or last, the other bird first and or last visit are not overlapping: time left non-standardized

# by rescaling with one sex or the other, NbalternationScaled is different within 41% of the files (min =1, med = 2, max = 8)
# as well as dofferent from the observed alternation in the original nest watch
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

sample_vector <- function(x,...){if(length(x)==1) x else sample(x,replace=F)} 
 
 as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
}

{#### Get raw data & select valid files

{### Get raw data (from source() or R_output folder)

{# output csv files

output_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_output"

MY_tblParentalCare <- read.csv(paste(output_folder,"R_MY_tblParentalCare.csv", sep="/")) # summary stats for all analyzed videos
MY_tblBroods <- read.csv(paste(output_folder,"R_MY_tblBroods.csv", sep="/")) # all broods unless bot parents are unidentified, even those when one social parent not identified, even those not recorded
MY_tblDVDInfo <- read.csv(paste(output_folder,"R_MY_tblDVDInfo.csv", sep="/")) # metadata for all analysed videos
MY_RawFeedingVisits <- read.csv(paste(output_folder,"R_MY_RawFeedingVisits.csv", sep="/")) # OF directly followed by IN are merged into one feeding visits ; will be used for simulation
AllMiFj <-  read.csv(paste(output_folder,"R_MY_AllMiFj.csv", sep="/")) # among nest watch randomisation output from Alternation Data Analyses (Kat style simulation)
}

{# input txt files

input_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_input"

FedBroods <-  read.table(file= paste(input_folder,"FedBroods.txt", sep="/"), sep='\t', header=T)  ## from Ian Cleasby 20160531

}


}

{### select valid video files for studying behavioural compatibility in chick provisioning
# copy paste from AlternationSynchronyDataAnalyses code to get same file selection

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


{#### scale visits for multiple random set of standardizing sex
split_MY_RawFeedingVisits_per_splitID <- split(MY_RawFeedingVisits,MY_RawFeedingVisits$splitID)

scaling_function <- function(x,StandardizingSex) {

x_StandardizingSex = subset(x, Sex == StandardizingSex)
x_OtherSex = subset(x, Sex != StandardizingSex)

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$TstartFeedVisit[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$TstartFeedVisit[-1],NA)	

{# modify all Tstart of visits that have the same Tstart as the previous visit (add 0.1)
x_StandardizingSex$Interval[x_StandardizingSex$TstartFeedVisit ==
x_StandardizingSex$TstartFeedVisit[x_StandardizingSex$NextTstart == x_StandardizingSex$TstartFeedVisit & !is.na(x_StandardizingSex$NextTstart)]][2] <- 0.1

x_StandardizingSex$TstartFeedVisit[x_StandardizingSex$TstartFeedVisit ==
x_StandardizingSex$TstartFeedVisit[x_StandardizingSex$NextTstart == x_StandardizingSex$TstartFeedVisit & !is.na(x_StandardizingSex$NextTstart)]][2] <- 
x_StandardizingSex$TstartFeedVisit[x_StandardizingSex$NextTstart == x_StandardizingSex$TstartFeedVisit & !is.na(x_StandardizingSex$NextTstart)]+0.1

x_StandardizingSex$Interval <- c(0,diff(x_StandardizingSex$TstartFeedVisit))


x_OtherSex$Interval[x_OtherSex$TstartFeedVisit ==
x_OtherSex$TstartFeedVisit[x_OtherSex$NextTstart == x_OtherSex$TstartFeedVisit & !is.na(x_OtherSex$NextTstart)]][2] <- 0.1

x_OtherSex$TstartFeedVisit[x_OtherSex$TstartFeedVisit ==
x_OtherSex$TstartFeedVisit[x_OtherSex$NextTstart == x_OtherSex$TstartFeedVisit & !is.na(x_OtherSex$NextTstart)]][2] <- 
x_OtherSex$TstartFeedVisit[x_OtherSex$NextTstart == x_OtherSex$TstartFeedVisit & !is.na(x_OtherSex$NextTstart)]+0.1

x_OtherSex$Interval <- c(0,diff(x_OtherSex$TstartFeedVisit))

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$TstartFeedVisit[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$TstartFeedVisit[-1],NA)	

}

		# for the standardizing sex, all intervals will be set to its initial mean interval
		multiplicator <-  mean(x_StandardizingSex$Interval[-1])/x_StandardizingSex$Interval[-1]


# to recalculate ScaledTstart, start from the initial first Tstart and add up scaled intervals (cumulative sum)
x_StandardizingSex$ScaledInterval <- c(0,(rep(median(x_StandardizingSex$Interval[-1]*multiplicator), nrow(x_StandardizingSex)-1))) # the scaled Interval for the standardizing sex is always the same, hence the repeat function ; the use of median instead of unique is because of rounding that make identical number tiny different
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
x <- x[order(as.numeric(rownames(x))),] # x[order(x$TstartFeedVisit, -x$TendFeedVisit),] this sorting wasnt precise enough for those 7 cases where both Tstart and Tend are identical between two visits.
 

# to solve edges with no overlap: modify some ScaledInterval and ScaledTstart from x
FirstSex <- x$Sex[1] # who is the first sex to visit
LastSex <- x$Sex[nrow(x)] # who is the last sex to visit

if (StandardizingSex != FirstSex){ # if StandardizingSex is not the FirstSex, the first intervals of the others sex can't already be standardized, they do not fully overlap with the intervals of the standardizing sex, and are therefore left intact, unstandardized

if(sum(x_OtherSex$ScaledInterval) >0){
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

if(sum(x_OtherSex$ScaledInterval) ==0){

x$ScaledInterval[x$Sex == FirstSex] <- x$Interval[x$Sex == FirstSex]
x$ScaledTstart[x$Sex == FirstSex] <- x$ScaledTstart[x$Sex == FirstSex][1] +cumsum(x$ScaledInterval[x$Sex == FirstSex])

}

}

if (StandardizingSex != LastSex){ # if Standardazing is not the LastSex: there is no interval for the other sex to overlap with at the end, those ones can't be standardized, and are therefore 'left' intact unstandardized

if(sum(x_OtherSex$ScaledInterval) >0){
# add to the last overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$TstartFeedVisit == min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$TstartFeedVisit == min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])]+
min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ]) - max(x$TstartFeedVisit[x$Sex==StandardizingSex])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$TstartFeedVisit > min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])] <- 
x$Interval[x$TstartFeedVisit > min(x$TstartFeedVisit[x$Sex==LastSex & x$TstartFeedVisit >=max(x$TstartFeedVisit[x$Sex==StandardizingSex]) ])]

# recalculate the Tstart for the last sex
x$ScaledTstart[x$Sex==LastSex] <- x$TstartFeedVisit[x$Sex==LastSex][1] +cumsum(x$ScaledInterval[x$Sex==LastSex])
}

if(sum(x_OtherSex$ScaledInterval) ==0){
x$ScaledInterval[x$Sex == LastSex] <- x$Interval[x$Sex == LastSex]
x$ScaledTstart[x$Sex == LastSex] <- x$ScaledTstart[x$Sex == LastSex][1] +cumsum(x$ScaledInterval[x$Sex == LastSex])

}
}

# round calculated numbers
x$ScaledTstart <- round(x$ScaledTstart,1)
x$ScaledInterval <- round(x$ScaledInterval,1)

return(x)
}

Reshape_function_for_plotting <- function(x,StandardizingSex) {

x_raw <- x[,c('splitID','DVDRef','TstartFeedVisit','Sex')]
x_raw$Type <- 'Original'
colnames(x_raw)[which(names(x_raw) == "TstartFeedVisit")] <- "Tstart"
	
x_scaled <- x[,c('splitID','DVDRef','ScaledTstart','Sex')]
x_scaled$Type <- 'Scaled'
colnames(x_scaled)[which(names(x_scaled) == "ScaledTstart")] <- "Tstart"	
x_scaled$Sex[x_scaled$Sex == 0 & x_scaled$Type == "Scaled"] <- -1
x_scaled$Sex[x_scaled$Sex == 1 & x_scaled$Type == "Scaled"] <- 2

x_for_plotting <- rbind(x_raw,x_scaled)
x_for_plotting <- x_for_plotting[order(x_for_plotting$splitID),]
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

return(x_for_plotting)
}
 
# all files with standardizing sex = 0
out_scaling_list_0 <- lapply(X=split_MY_RawFeedingVisits_per_splitID,FUN=scaling_function, StandardizingSex = 0)
out_scaling_list_for_plotting_0 <- lapply(X=out_scaling_list_0,FUN=Reshape_function_for_plotting, StandardizingSex = 0)

MY_RawFeedingVisits_scaled_0 <- do.call(rbind, out_scaling_list_0)
MY_RawFeedingVisits_scaled_for_plotting_0 <- do.call(rbind, out_scaling_list_for_plotting_0)

# all files with standardizing sex = 1
out_scaling_list_1 <- lapply(split_MY_RawFeedingVisits_per_splitID,FUN=scaling_function, StandardizingSex = 1)
out_scaling_list_for_plotting_1 <- lapply(out_scaling_list_1,FUN=Reshape_function_for_plotting, StandardizingSex = 1)

MY_RawFeedingVisits_scaled_1 <- do.call(rbind, out_scaling_list_1)
MY_RawFeedingVisits_scaled_for_plotting_1 <- do.call(rbind, out_scaling_list_for_plotting_1)

}

head(MY_RawFeedingVisits_scaled_0,20)
head(MY_RawFeedingVisits_scaled_for_plotting_0,50)
head(MY_RawFeedingVisits_scaled_1,20)
head(MY_RawFeedingVisits_scaled_for_plotting_1,50)

{### Calculate A score for observed scaled data with standardizing sex being 0 then 1

{# calculate Nb of Alternation for each file

MY_RawFeedingVisits_scaled_split_fun = function(x) {
x <- x[order(x$ScaledTstart),] # for 41% of the files, this change slightly the order of max 4 visits, and therefore NbAlternation is different than NbAlternaitonScaled for those
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
return(c(as.character(unique(x$DVDRef)), length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]))) #NbAlternationScaled
}

# all files with standardizing sex = 0
MY_RawFeedingVisits_scaled_split_0 <- split(MY_RawFeedingVisits_scaled_0,MY_RawFeedingVisits_scaled_0$splitID)

out1_MY_RawFeedingVisits_scaled_split_0 <-lapply(MY_RawFeedingVisits_scaled_split_0,MY_RawFeedingVisits_scaled_split_fun)
out2_MY_RawFeedingVisits_scaled_split_0 <- data.frame(rownames(do.call(rbind,out1_MY_RawFeedingVisits_scaled_split_0)),do.call(rbind, out1_MY_RawFeedingVisits_scaled_split_0))
rownames(out2_MY_RawFeedingVisits_scaled_split_0) <- NULL
colnames(out2_MY_RawFeedingVisits_scaled_split_0) <- c('splitID','DVDRef','NbAlternationScaled')

# all files with standardizing sex = 1
MY_RawFeedingVisits_scaled_split_1 <- split(MY_RawFeedingVisits_scaled_1,MY_RawFeedingVisits_scaled_1$splitID)

out1_MY_RawFeedingVisits_scaled_split_1 <-lapply(MY_RawFeedingVisits_scaled_split_1,MY_RawFeedingVisits_scaled_split_fun)
out2_MY_RawFeedingVisits_scaled_split_1 <- data.frame(rownames(do.call(rbind,out1_MY_RawFeedingVisits_scaled_split_1)),do.call(rbind, out1_MY_RawFeedingVisits_scaled_split_1))
rownames(out2_MY_RawFeedingVisits_scaled_split_1) <- NULL
colnames(out2_MY_RawFeedingVisits_scaled_split_1) <- c('splitID','DVDRef','NbAlternationScaled')

out2_MY_RawFeedingVisits_scaled_split_01 <- rbind(out2_MY_RawFeedingVisits_scaled_split_0,out2_MY_RawFeedingVisits_scaled_split_1)
}

head(out2_MY_RawFeedingVisits_scaled_split_01)

{# calculate AlternationValue for each file

MY_tblParentalCare_scaled <- merge(x= out2_MY_RawFeedingVisits_scaled_split_01, y=MY_tblParentalCare[,c('DVDRef','MVisit1','FVisit1','DiffVisit1Rate')], all.x=TRUE, by='DVDRef')
MY_tblParentalCare_scaled$splitID <-  as.numeric(as.character(MY_tblParentalCare_scaled$splitID))
MY_tblParentalCare_scaled$NbAlternationScaled <-  as.numeric(as.character(MY_tblParentalCare_scaled$NbAlternation))
MY_tblParentalCare_scaled <- MY_tblParentalCare_scaled[order(MY_tblParentalCare_scaled$splitID),]

MY_tblParentalCare_scaled$AlternationValueScaled <- round(MY_tblParentalCare_scaled$NbAlternationScaled/(MY_tblParentalCare_scaled$MVisit1 + MY_tblParentalCare_scaled$FVisit1 -1) *100,1)
}

tail(MY_tblParentalCare_scaled)

{# average NbAlternationScaled and AlternationValueScaled accross standardizing sex = 0 and then 1

MY_tblParentalCare_scaled_01 <- data.frame(summarise (group_by(MY_tblParentalCare_scaled, DVDRef),
					splitID = unique(splitID),
					NbAlternationScaledAv = mean(NbAlternationScaled),
					DiffNbAlternationScaled = max(NbAlternationScaled)-min(NbAlternationScaled),
					DiffVisit1Rate = unique(DiffVisit1Rate),
					AlternationValueScaledAv = mean(AlternationValueScaled)))

MY_tblParentalCare_scaled_01 <- MY_tblParentalCare_scaled_01[order(MY_tblParentalCare_scaled_01$splitID),]

# Nb files where NbAlternation Value Scaled is diferent with different standardizing sex
# length(MY_tblParentalCare_scaled_01$DVDRef[MY_tblParentalCare_scaled_01$DiffNbAlternationScaled !=0]) #670/1619 = 41%
# summary(MY_tblParentalCare_scaled_01$DiffNbAlternationScaled[MY_tblParentalCare_scaled_01$DiffNbAlternationScaled !=0])
}

tail(MY_tblParentalCare_scaled_01)

{# summary Aobserved per VisitRateDifference

MY_tblParentalCare_scaled_perVisitRateDiff_01 <- group_by(MY_tblParentalCare_scaled_01, DiffVisit1Rate)

Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01 <- summarise (MY_tblParentalCare_scaled_perVisitRateDiff_01,
					Amean = mean(AlternationValueScaledAv),
					Alower = Amean - sd(AlternationValueScaledAv)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValueScaledAv)/sqrt(n())*1.96,
					NbFiles = n())
					
Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01 <- dplyr::rename(Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01,VisitRateDifference= DiffVisit1Rate)
Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01 <- as.data.frame(Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01)

}


}

Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01

{#### simulation alternation: shuffling intervals within files 
# I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

{## creation of i simulated dataset (and calculation of i Asim) for each j file, twice: when standardising sex is 0 then 1

simulation_nestwatch_function <- function(x) {

x <- x[order(x$ScaledTstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

out_Asim_i <- list()

for (i in 1:100) # to increase up to 1000
{

x0sim <- x0
x1sim <- x1

x0sim$ScaledInterval <- c(0, sample_vector(x0sim$ScaledInterval[-1]))
x0sim$ScaledTstart <- c(x0sim$ScaledTstart[1],x0sim$ScaledTstart[-nrow(x0sim)]+x0sim$ScaledInterval[-1])

x1sim$ScaledInterval <- c(0, sample_vector(x1sim$ScaledInterval[-1]))
x1sim$ScaledTstart <- c(x1sim$ScaledTstart[1],x1sim$ScaledTstart[-nrow(x1sim)]+x1sim$ScaledInterval[-1])

xsim <- rbind(x0sim,x1sim)
xsim <- xsim[order(xsim$ScaledTstart),]
xsim$NextSexSame <- c(xsim$Sex[-1],NA) == xsim$Sex

Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)
out_Asim_i[i] <- Asim


}

return((unlist(out_Asim_i)))
}

# all files with standardizing sex = 0
MY_RawFeedingVisits_scaled_for_Sim_0 <- MY_RawFeedingVisits_scaled_0[,c('splitID','DVDRef','ScaledTstart','Sex','ScaledInterval')]
MY_RawFeedingVisits_scaled_for_Sim_split_0 <- split(MY_RawFeedingVisits_scaled_for_Sim_0,MY_RawFeedingVisits_scaled_for_Sim_0$splitID)
out_Asim_j_0 <- lapply(MY_RawFeedingVisits_scaled_for_Sim_split_0,simulation_nestwatch_function)
out_Asim_0 <- do.call(rbind, out_Asim_j_0)

# all files with standardizing sex = 1
MY_RawFeedingVisits_scaled_for_Sim_1 <- MY_RawFeedingVisits_scaled_1[,c('splitID','DVDRef','ScaledTstart','Sex','ScaledInterval')]
MY_RawFeedingVisits_scaled_for_Sim_split_1 <- split(MY_RawFeedingVisits_scaled_for_Sim_1,MY_RawFeedingVisits_scaled_for_Sim_1$splitID)
out_Asim_j_1 <- lapply(MY_RawFeedingVisits_scaled_for_Sim_split_1,simulation_nestwatch_function)
out_Asim_1 <- do.call(rbind, out_Asim_j_1)

}

head(out_Asim_0)
head(out_Asim_1)

{# out A sim summary

out_Asim_df_0 <- data.frame(DVDRef = unique(MY_RawFeedingVisits_scaled_for_Sim_0$DVDRef), out_Asim_0)
out_Asim_df_1 <- data.frame(DVDRef = unique(MY_RawFeedingVisits_scaled_for_Sim_1$DVDRef), out_Asim_1)
out_Asim_df <- rbind(out_Asim_df_0,out_Asim_df_1)
out_Asim_df <- merge(x=out_Asim_df, y= MY_tblParentalCare[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

out_Asim_df_perDiffVisit1Rate <- split(out_Asim_df,out_Asim_df$DiffVisit1Rate)

out_Asim_df_perDiffVisit1Rate_fun <- function(x) {

x <- x[,-1]
x <- x[,-ncol(x)]
v <- unlist(list(x))

return(c(
mean(v), # Amean
mean(v) - sd(v)/sqrt(length(v))*1.96, # Alower
mean(v) + sd(v)/sqrt(length(v))*1.96, # Aupper
nrow(x) # NbFiles
))
}

out_Asim_df_perDiffVisit1Rate_out1_01 <- lapply(out_Asim_df_perDiffVisit1Rate,out_Asim_df_perDiffVisit1Rate_fun)
out_Asim_df_perDiffVisit1Rate_out2_01 <- data.frame(rownames(do.call(rbind,out_Asim_df_perDiffVisit1Rate_out1_01)),do.call(rbind, out_Asim_df_perDiffVisit1Rate_out1_01))

nrow(out_Asim_df_perDiffVisit1Rate_out2_01)	# 32
rownames(out_Asim_df_perDiffVisit1Rate_out2_01) <- NULL
colnames(out_Asim_df_perDiffVisit1Rate_out2_01) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

out_Asim_df_perDiffVisit1Rate_out2_01

{# A: for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01$Type <- "Observed"
out_Asim_df_perDiffVisit1Rate_out2_01$Type <- "Expected"


VisitRateDiff_Amean_scaled_01 <- as.data.frame(rbind( Summary_MY_tblParentalCare_scaled_perVisitRateDiff_01[1:21,],out_Asim_df_perDiffVisit1Rate_out2_01[1:21,] ))
VisitRateDiff_Amean_scaled_01$VisitRateDifference <- as.numeric(VisitRateDiff_Amean_scaled_01$VisitRateDifference)

Fig1Scaled_01 <- ggplot(data=VisitRateDiff_Amean_scaled_01, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("#009E73", "black"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_scaled_01$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_scaled_01$Amean, n = 9)) +  
  theme_classic()
  
}

}

VisitRateDiff_Amean_scaled_01
dev.new()
Fig1Scaled_01

{### MeanAsimScaled (accross both standardizing sex) for each file

{# average Asim scaled a accross standardizing sex = 0 and then 1

MeanAsimScale_perDVDRef_perSdSex <- cbind(as.numeric(as.character(out_Asim_df[,1])), rowMeans(out_Asim_df[-ncol(out_Asim_df)][-1])) # remove last and first column
MeanAsimScale_perDVDRef_perSdSex <- data.frame(MeanAsimScale_perDVDRef_perSdSex[order(MeanAsimScale_perDVDRef_perSdSex[,1]),])
colnames(MeanAsimScale_perDVDRef_perSdSex) <- c('DVDRef', 'MeanAsimScale')

MeanAsimScale_perDVDRef <- data.frame(summarise(group_by(MeanAsimScale_perDVDRef_perSdSex, DVDRef),
							MeanAsimScaled01 = mean(MeanAsimScale)))
}

head(MeanAsimScale_perDVDRef)

{### add Mean A sim to MY_tblParentalCare_scaled_01
MY_tblParentalCare_scaled_01 <- merge(x= MY_tblParentalCare_scaled_01, y=MeanAsimScale_perDVDRef, by='DVDRef', all.x =TRUE)
MY_tblParentalCare_scaled_01 <- MY_tblParentalCare_scaled_01[order(MY_tblParentalCare_scaled_01$splitID),]			  
}

}

head(MY_tblParentalCare_scaled_01)


{#### timeline plots 

{## plotting time in the nest

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
dev.new()
plot9randomgraphsTimeInNB()
}

{## plotting visits, considering they do not have duration (i.e. how 'intervals' were calculated)

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

{## plotting scaled visits + raw visits

plot9randomgraphs_scaled <- function(dfsplit){
p <- NULL

for (j in 1:9)  {
for (i in sample(1:length(unique(MY_RawFeedingVisits$DVDRef)),9))
{

p[[j]] <-ggplot(dfsplit[[i]])  + 
		geom_segment(aes(x=Tstart, xend=Tstart+0.5, y=as.factor(Sex), yend=as.factor(Sex)), size=3, colour =dfsplit[[i]]$Colours) +
		xlab("Nest watch duration") + ylab("Sex")+ scale_y_discrete(breaks=c("-1","0","1","2"),
        labels=c("S0", "0", "1","S1"))+
		geom_text(x=(max(dfsplit[[i]]$Tstart) + 0.5)/2, 
				y= 2.5,label=unique(dfsplit[[i]]$DVDRef), colour='black')+
		theme(legend.position="none") 
}
}

multiplot(p[[1]],p[[2]],p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], cols=3)

}

split_MY_RawFeedingVisits_scaled_for_plotting_0_per_splitID <- split(MY_RawFeedingVisits_scaled_for_plotting_0,MY_RawFeedingVisits_scaled_for_plotting_0$splitID)
split_MY_RawFeedingVisits_scaled_for_plotting_1_per_splitID <- split(MY_RawFeedingVisits_scaled_for_plotting_1,MY_RawFeedingVisits_scaled_for_plotting_1$splitID)


set.seed(10)
dev.new()
plot9randomgraphs_scaled(dfsplit = split_MY_RawFeedingVisits_scaled_for_plotting_0_per_splitID)

set.seed(10)
dev.new()
plot9randomgraphs_scaled(dfsplit = split_MY_RawFeedingVisits_scaled_for_plotting_1_per_splitID)

}

}



## 20161010
# write.csv(VisitRateDiff_Amean_scaled,file = paste(output_folder,"R_MY_VisitRateDiff_Amean_scaled.csv", sep="/"), row.names = FALSE) # 20161010
# write.csv(MY_tblParentalCare_scaled,file = paste(output_folder,"R_MY_tblParentalCare_scaled.csv", sep="/"), row.names = FALSE) # 20161010

## 20161012
# write.csv(VisitRateDiff_Amean_scaled_01,file = paste(output_folder,"R_MY_VisitRateDiff_Amean_scaled_01.csv", sep="/"), row.names = FALSE)
# write.csv(MY_tblParentalCare_scaled_01,file = paste(output_folder,"R_MY_tblParentalCare_scaled_01.csv", sep="/"), row.names = FALSE)




{### comparison Alternation in original and scaled nest watches

head(out2_MY_RawFeedingVisits_scaled_split_1)
head(out2_MY_RawFeedingVisits_scaled_split_0)

NbAlternation_scaled_01 <- merge(x=out2_MY_RawFeedingVisits_scaled_split_0, y= out2_MY_RawFeedingVisits_scaled_split_1[,c("splitID","NbAlternationScaled")], by="splitID")
head(NbAlternation_scaled_01)
All_versions_NbAlternation <- merge(x=NbAlternation_scaled_01,y= MY_tblParentalCare[,c('DVDRef', 'NbAlternation')])
head(All_versions_NbAlternation)

All_versions_NbAlternation$NbAlternationScaled.x <- as.numeric.factor(All_versions_NbAlternation$NbAlternationScaled.x)
All_versions_NbAlternation$NbAlternationScaled.y <- as.numeric.factor(All_versions_NbAlternation$NbAlternationScaled.y)
All_versions_NbAlternation$DVDRef<- as.numeric.factor(All_versions_NbAlternation$DVDRef)
All_versions_NbAlternation$DiffOrig0 <- All_versions_NbAlternation$NbAlternation - All_versions_NbAlternation$NbAlternationScaled.x
All_versions_NbAlternation$DiffOrig1 <- All_versions_NbAlternation$NbAlternation - All_versions_NbAlternation$NbAlternationScaled.y
All_versions_NbAlternation$DiffScaled01 <- All_versions_NbAlternation$NbAlternationScaled.x - All_versions_NbAlternation$NbAlternationScaled.y
head(All_versions_NbAlternation)

summary(All_versions_NbAlternation$DiffOrig0)
summary(All_versions_NbAlternation$DiffOrig1)

nrow(All_versions_NbAlternation[All_versions_NbAlternation$DiffOrig0 !=0 | All_versions_NbAlternation$DiffOrig1 !=0 ,])/1619*100 #41.5
file_with_Observed_Alternation_different <- unique(All_versions_NbAlternation$DVDRef[All_versions_NbAlternation$DiffOrig0 !=0 | All_versions_NbAlternation$DiffOrig1 !=0])

head(MY_RawFeedingVisits)
MY_RawFeedingVisits_toCheck <- MY_RawFeedingVisits[,c('DVDRef','TstartFeedVisit')]
MY_RawFeedingVisits_toCheck$nextTstart <- c(MY_RawFeedingVisits_toCheck$TstartFeedVisit[-1],0)
file_with_duplicated_times <- unique(MY_RawFeedingVisits_toCheck$DVDRef[MY_RawFeedingVisits_toCheck$TstartFeedVisit == MY_RawFeedingVisits_toCheck$nextTstart])

{## plotting selected scaled visits + raw visits

dfsplit <- split_MY_RawFeedingVisits_scaled_for_plotting_0_per_splitID
#dfsplit <- split_MY_RawFeedingVisits_scaled_for_plotting_1_per_splitID
selectedsplitID <- c(268,16,1540,1617,1322,932,199,12,1163)
p <- NULL
for (i in selectedsplitID)
{

p[[i]] <-ggplot(dfsplit[[i]])  + 
		geom_segment(aes(x=Tstart, xend=Tstart+0.5, y=as.factor(Sex), yend=as.factor(Sex)), size=3, colour =dfsplit[[i]]$Colours) +
		xlab("Nest watch duration") + ylab("Sex")+ scale_y_discrete(breaks=c("-1","0","1","2"),
        labels=c("S0", "0", "1","S1"))+
		geom_text(x=(max(dfsplit[[i]]$Tstart) + 0.5)/2, 
				y= 2.5,label=unique(dfsplit[[i]]$DVDRef), colour='black')+
		theme(legend.position="none") 
}

dev.new()
multiplot(p[[268]],p[[16]],p[[1540]], p[[1617]], p[[1322]], p[[932]], p[[199]], p[[12]], p[[1163]], cols=3)

}


file_with_Observed_Alternation_different[!file_with_Observed_Alternation_different %in%file_with_duplicated_times]
file_with_duplicated_times[!file_with_duplicated_times %in%file_with_Observed_Alternation_different]

All_versions_NbAlternation[All_versions_NbAlternation$DVDRef == 1012,]
MY_RawFeedingVisits_scaled_1[MY_RawFeedingVisits_scaled_1$DVDRef == 1012,]
MY_RawFeedingVisits_scaled_0[MY_RawFeedingVisits_scaled_0$DVDRef == 1012,]

}



{######### simulation scaling effect on alternation (i.e. when one bird becomes completely regular)

{## Nest watch simulations, keep first Tstart, shuffle interval AMONG individual same sex same visit rate, replicate x times each DVDRef

# Select RawInterfeeds per sex for relevant files (not in list_non_valid_DVDRef nor DVDRefToExclude) 
RawInterfeeds <- merge(x= MY_RawFeedingVisits[,c('DVDRef','Sex','TstartFeedVisit','Interval')], 
                       y=MY_tblParentalCare[,c('DVDRef','MVisit1RateH', 'FVisit1RateH','DiffVisit1Rate','AlternationValue')] , 
                       by='DVDRef', all.x=TRUE)

MRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','TstartFeedVisit','Interval','MVisit1RateH')], RawInterfeeds$Sex == 1)
FRawInterfeeds <- subset(RawInterfeeds[,c('DVDRef','Sex','TstartFeedVisit','Interval','FVisit1RateH')], RawInterfeeds$Sex == 0)

# remove the first line with interval (=0) from each file for each sex before shuffling interval
FRawInterfeeds322toShuffle <- do.call(rbind,lapply(X=split(FRawInterfeeds,FRawInterfeeds$DVDRef), FUN=function(x){return(x[-1,])}))
rownames(FRawInterfeeds322toShuffle) <- NULL
head(FRawInterfeeds322toShuffle)

MRawInterfeeds322toShuffle <- do.call(rbind,lapply(X=split(MRawInterfeeds,MRawInterfeeds$DVDRef), FUN=function(x){return(x[-1,])}))
rownames(MRawInterfeeds322toShuffle) <- NULL

# save first Tstart of each file and each sex  (with interval = 0)
FRawFirstTstart <- do.call(rbind,lapply(X=split(FRawInterfeeds,FRawInterfeeds$DVDRef), FUN=function(x){return(x[1,])}))
rownames(FRawFirstTstart) <- NULL
head(FRawFirstTstart)

MRawFirstTstart <- do.call(rbind,lapply(X=split(MRawInterfeeds,MRawInterfeeds$DVDRef), FUN=function(x){return(x[1,])}))
rownames(MRawFirstTstart) <- NULL

# function that simulate all nest watches once with identical first Tstarts and shuffled interval among individual of same sex and same visit rate

randomization_among_nest_watch_fun <- function(x,y){

# shuffled intervals among individuals of the same sex that have the same visit rate
FShuffledInterfeeds322 <- data.frame(x %>% group_by(FVisit1RateH) %>% mutate(Interval=sample(Interval)))
MShuffledInterfeeds322 <- data.frame(y %>% group_by(MVisit1RateH) %>% mutate(Interval=sample(Interval)))

# add first Tstart
SimFemale <- rbind(FRawFirstTstart,FShuffledInterfeeds322)
SimFemale <- SimFemale[order(SimFemale$DVDRef),]

SimMale <- rbind(MRawFirstTstart,MShuffledInterfeeds322)
SimMale <- SimMale[order(SimMale$DVDRef),]

# recalculate TstartFeedVisit
SimFemale <- do.call(rbind,lapply(X=split(SimFemale,SimFemale$DVDRef), FUN=function(x){x$TstartFeedVisit <- x$TstartFeedVisit[1] + cumsum(x$Interval)
return(x)}))
rownames(SimFemale) <- NULL

SimMale <- do.call(rbind,lapply(X=split(SimMale,SimMale$DVDRef), FUN=function(x){x$TstartFeedVisit <- x$TstartFeedVisit[1] + cumsum(x$Interval)
return(x)}))
rownames(SimMale) <- NULL

# bind together
SimData <- data.frame(bind_rows(SimMale, SimFemale)) # different from rbind as it binds two df with different columns, adding NAs
SimData[is.na(SimData)] <- 0
SimData <- SimData[order(SimData$DVDRef,SimData$TstartFeedVisit),]
rownames(SimData) <- NULL

return(SimData)

}

# replicate that function and add splitID unique per DVDRef-replication
set.seed(10)
SimData <- do.call( rbind, replicate(10, randomization_among_nest_watch_fun(FRawInterfeeds322toShuffle, MRawInterfeeds322toShuffle), simplify=FALSE ) )
SimData <- SimData[!SimData$DVDRef %in% DVDRefToExclude,] # those files where visits from one sex < 2visits
SimData$splitID <- rep(1:length(rle(as.numeric(as.character(SimData$DVDRef)))$lengths), times=rle(as.numeric(as.character(SimData$DVDRef)))$lengths)


}

head(SimData,50)

{## calculate A for each Simulated file

SimData_split <- split(SimData,SimData$splitID)

SimData_split_fun = function(x) {
x <- x[order(x$TstartFeedVisit),] 
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
return(c(
as.character(unique(x$DVDRef)), 
length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]),#NbAlternation
nrow(x[x$Sex == 1,]), # MVisit1
nrow(x[x$Sex == 0,]) # FVisit1
))
}

out1_SimData_split <-lapply(SimData_split,SimData_split_fun)
out2_SimData_split <- data.frame(rownames(do.call(rbind,out1_SimData_split)),do.call(rbind, out1_SimData_split))
rownames(out2_SimData_split) <- NULL
colnames(out2_SimData_split) <- c('splitID','DVDRef','NbAlternation', 'MVisit1','FVisit1')

{# calculate AlternationValue for each file

MY_tblParentalCare_sim <- out2_SimData_split
MY_tblParentalCare_sim$splitID <-  as.numeric(as.character(MY_tblParentalCare_sim$splitID))
MY_tblParentalCare_sim$NbAlternation <-  as.numeric(as.character(MY_tblParentalCare_sim$NbAlternation))
MY_tblParentalCare_sim$MVisit1 <-  as.numeric(as.character(MY_tblParentalCare_sim$MVisit1))
MY_tblParentalCare_sim$FVisit1 <-  as.numeric(as.character(MY_tblParentalCare_sim$FVisit1))
MY_tblParentalCare_sim <- MY_tblParentalCare_sim[order(MY_tblParentalCare_sim$splitID),]

MY_tblParentalCare_sim$DiffVisit1Rate <- abs(round(MY_tblParentalCare_sim$FVisit1 - MY_tblParentalCare_sim$MVisit1))

MY_tblParentalCare_sim$AlternationValue <- round(MY_tblParentalCare_sim$NbAlternation/(MY_tblParentalCare_sim$MVisit1 + MY_tblParentalCare_sim$FVisit1 -1) *100,1)

}

tail(MY_tblParentalCare_sim)


{# summary Aobserved per VisitRateDifference

MY_tblParentalCare_sim_perVisitRateDiff <- group_by(MY_tblParentalCare_sim, DiffVisit1Rate)

Summary_MY_tblParentalCare_sim_perVisitRateDiff <- summarise (MY_tblParentalCare_sim_perVisitRateDiff,
				Amean = mean(AlternationValue),
				Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
				Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
				NbFiles = n())
				
Summary_MY_tblParentalCare_sim_perVisitRateDiff <- dplyr::rename(Summary_MY_tblParentalCare_sim_perVisitRateDiff,VisitRateDifference= DiffVisit1Rate)
Summary_MY_tblParentalCare_sim_perVisitRateDiff <- as.data.frame(Summary_MY_tblParentalCare_sim_perVisitRateDiff)

}

}

Summary_MY_tblParentalCare_sim_perVisitRateDiff


{#### simulation alternation: shuffling intervals within files 
# I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

{## creation of i simulated dataset (and calculation of i Asim) for each j file, twice: when standardising sex is 0 then 1

simulation_nestwatch_function_in_original_scale <- function(x) {

x <- x[order(x$TstartFeedVisit),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

out_Asim_i <- list()

for (i in 1:100) # to increase up to 1000
{

x0sim <- x0
x1sim <- x1

x0sim$Interval <- c(0, sample_vector(x0sim$Interval[-1]))
x0sim$TstartFeedVisit <- c(x0sim$TstartFeedVisit[1],x0sim$TstartFeedVisit[-nrow(x0sim)]+x0sim$Interval[-1])

x1sim$Interval <- c(0, sample_vector(x1sim$Interval[-1]))
x1sim$TstartFeedVisit <- c(x1sim$TstartFeedVisit[1],x1sim$TstartFeedVisit[-nrow(x1sim)]+x1sim$Interval[-1])

xsim <- rbind(x0sim,x1sim)
xsim <- xsim[order(xsim$TstartFeedVisit),]
xsim$NextSexSame <- c(xsim$Sex[-1],NA) == xsim$Sex

Asim <- round( ( sum(diff(xsim$Sex)!=0) / (nrow(xsim) -1) ) *100   ,2)
out_Asim_i[i] <- Asim


}

return((unlist(out_Asim_i)))
}

# all files
SimData_for_Sim <- SimData[,c('splitID','DVDRef','TstartFeedVisit','Sex','Interval')]
SimData_for_Sim_split <- split(SimData_for_Sim,SimData_for_Sim$splitID)
out_Asim_j_sim <- lapply(SimData_for_Sim_split,simulation_nestwatch_function_in_original_scale)
out_Asim_sim <- do.call(rbind, out_Asim_j_sim)

}

head(out_Asim_sim)

{# out A sim summary

out_Asim_sim_df <- data.frame(splitID = unique(SimData_for_Sim$splitID), out_Asim_sim)
out_Asim_sim_df <- merge(x=out_Asim_sim_df, y= MY_tblParentalCare_sim_scaled_01[,c('splitID','DiffVisit1Rate')], by='splitID', all.x =TRUE)

out_Asim_sim_df_perDiffVisit1Rate <- split(out_Asim_sim_df,out_Asim_sim_df$DiffVisit1Rate)

out1_out_Asim_sim_df_perDiffVisit1Rate <- lapply(out_Asim_sim_df_perDiffVisit1Rate,out_Asim_df_perDiffVisit1Rate_fun)
out2_out_Asim_sim_df_perDiffVisit1Rate <- data.frame(rownames(do.call(rbind,out1_out_Asim_sim_df_perDiffVisit1Rate)),do.call(rbind, out1_out_Asim_sim_df_perDiffVisit1Rate))

nrow(out2_out_Asim_sim_df_perDiffVisit1Rate)	
rownames(out2_out_Asim_sim_df_perDiffVisit1Rate) <- NULL
colnames(out2_out_Asim_sim_df_perDiffVisit1Rate) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

out2_out_Asim_sim_df_perDiffVisit1Rate

{# A: for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_sim_perVisitRateDiff$Type <- "Observed"
out2_out_Asim_sim_df_perDiffVisit1Rate$Type <- "Expected"


VisitRateDiff_Amean_Sim_Sim <- as.data.frame(rbind( Summary_MY_tblParentalCare_sim_perVisitRateDiff[1:21,],out2_out_Asim_sim_df_perDiffVisit1Rate[1:21,] ))
VisitRateDiff_Amean_Sim_Sim$VisitRateDifference <- as.numeric(VisitRateDiff_Amean_Sim_Sim$VisitRateDifference)

Fig1Sim_Sim <- ggplot(data=VisitRateDiff_Amean_Sim_Sim, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("#009E73", "black"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_Sim_Sim$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_Sim_Sim$Amean, n = 9)) +  
  theme_classic()
  
}



}

dev.new()
Fig1Sim_Sim

{## scaling simulating data (once with standardizing sex = 0, then 1)

SimData_per_splitID <- split(SimData,SimData$splitID)

out_scaling_list_SimData_0 <- lapply(X=SimData_per_splitID,FUN=scaling_function, StandardizingSex = 0)
SimData_scaled_0 <- do.call(rbind, out_scaling_list_SimData_0)
out_scaling_list_for_plotting_SimData_0 <- lapply(X=out_scaling_list_SimData_0,FUN=Reshape_function_for_plotting, StandardizingSex = 0)
SimData_scaled_for_plotting_0 <- do.call(rbind, out_scaling_list_for_plotting_SimData_0)

out_scaling_list_SimData_1 <- lapply(X=SimData_per_splitID,FUN=scaling_function, StandardizingSex = 1)
SimData_scaled_1 <- do.call(rbind, out_scaling_list_SimData_1)
out_scaling_list_for_plotting_SimData_1 <- lapply(X=out_scaling_list_SimData_1,FUN=Reshape_function_for_plotting, StandardizingSex = 1)
SimData_scaled_for_plotting_1 <- do.call(rbind, out_scaling_list_for_plotting_SimData_1)

}

head(SimData_scaled_0,20)
head(SimData_scaled_for_plotting_0,50)
head(SimData_scaled_1,20)
head(SimData_scaled_for_plotting_1,50)

{## plot scaled and raw simulated data
split_SimData_scaled_for_plotting_0 <- split(SimData_scaled_for_plotting_0,SimData_scaled_for_plotting_0$splitID)
split_SimData_scaled_for_plotting_1 <- split(SimData_scaled_for_plotting_1,SimData_scaled_for_plotting_1$splitID)

set.seed(10)
dev.new()
plot9randomgraphs_scaled(dfsplit = split_SimData_scaled_for_plotting_0)

set.seed(10)
dev.new()
plot9randomgraphs_scaled(dfsplit = split_SimData_scaled_for_plotting_1)
}


{### Calculate A score for observed simulated scaled data with standardizing sex being 0 then 1

{# calculate Nb of Alternation for each file
# all files with standardizing sex = 0
SimData_scaled_0_split <- split(SimData_scaled_0,SimData_scaled_0$splitID)

SimData_scaled_0_split_fun = function(x) {
x <- x[order(x$ScaledTstart),] 
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
return(c(
as.character(unique(x$DVDRef)), 
length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]),#NbAlternationScaled
nrow(x[x$Sex == 1,]), # MVisit1
nrow(x[x$Sex == 0,]) # FVisit1
))
}

out1_SimData_scaled_0_split <-lapply(SimData_scaled_0_split,SimData_scaled_0_split_fun)
out2_SimData_scaled_0_split <- data.frame(rownames(do.call(rbind,out1_SimData_scaled_0_split)),do.call(rbind, out1_SimData_scaled_0_split))
rownames(out2_SimData_scaled_0_split) <- NULL
colnames(out2_SimData_scaled_0_split) <- c('splitID','DVDRef','NbAlternationScaled', 'MVisit1','FVisit1')

# all files with standardizing sex = 1
SimData_scaled_1_split <- split(SimData_scaled_1,SimData_scaled_1$splitID)

out1_SimData_scaled_1_split <-lapply(SimData_scaled_1_split,SimData_scaled_0_split_fun)
out2_SimData_scaled_1_split <- data.frame(rownames(do.call(rbind,out1_SimData_scaled_1_split)),do.call(rbind, out1_SimData_scaled_1_split))
rownames(out2_SimData_scaled_1_split) <- NULL
colnames(out2_SimData_scaled_1_split) <- c('splitID','DVDRef','NbAlternationScaled', 'MVisit1','FVisit1')

MY_tblParentalCare_sim_scaled <- rbind(out2_SimData_scaled_0_split,out2_SimData_scaled_1_split)
}

head(MY_tblParentalCare_sim_scaled)

{# calculate AlternationValue for each  file

MY_tblParentalCare_sim_scaled$splitID <-  as.numeric(as.character(MY_tblParentalCare_sim_scaled$splitID))
MY_tblParentalCare_sim_scaled$NbAlternationScaled <-  as.numeric(as.character(MY_tblParentalCare_sim_scaled$NbAlternation))
MY_tblParentalCare_sim_scaled$MVisit1 <-  as.numeric(as.character(MY_tblParentalCare_sim_scaled$MVisit1))
MY_tblParentalCare_sim_scaled$FVisit1 <-  as.numeric(as.character(MY_tblParentalCare_sim_scaled$FVisit1))

MY_tblParentalCare_sim_scaled$DiffVisit1Rate <- abs(round(MY_tblParentalCare_sim_scaled$FVisit1 - MY_tblParentalCare_sim_scaled$MVisit1))

MY_tblParentalCare_sim_scaled$AlternationValueScaled <- round(MY_tblParentalCare_sim_scaled$NbAlternationScaled/(MY_tblParentalCare_sim_scaled$MVisit1 + MY_tblParentalCare_sim_scaled$FVisit1 -1) *100,1)

MY_tblParentalCare_sim_scaled <- MY_tblParentalCare_sim_scaled[order(MY_tblParentalCare_sim_scaled$splitID),]

}

tail(MY_tblParentalCare_sim_scaled)
summary(MY_tblParentalCare_sim_scaled$DVDRef)

{# average NbAlternationScaled and AlternationValueScaled accross standardizing sex = 0 and then 1

MY_tblParentalCare_sim_scaled_01 <- data.frame(summarise (group_by(MY_tblParentalCare_sim_scaled, splitID), # same DVDRef simulated several time, each have unique splitID
					NbAlternationScaledAv = mean(NbAlternationScaled),
					DiffNbAlternationScaled = max(NbAlternationScaled)-min(NbAlternationScaled),
					DiffVisit1Rate = unique(DiffVisit1Rate),
					AlternationValueScaledAv = mean(AlternationValueScaled)))

MY_tblParentalCare_sim_scaled_01 <- MY_tblParentalCare_sim_scaled_01[order(MY_tblParentalCare_sim_scaled_01$splitID),]

# Nb files where NbAlternation Value Scaled is diferent with different standardizing sex
# length(MY_tblParentalCare_sim_scaled_01$splitID[MY_tblParentalCare_sim_scaled_01$DiffNbAlternationScaled !=0]) #12029/16190 = 74%
# summary(MY_tblParentalCare_sim_scaled_01$DiffNbAlternationScaled[MY_tblParentalCare_sim_scaled_01$DiffNbAlternationScaled !=0])
}

tail(MY_tblParentalCare_sim_scaled_01)

{# summary Aobserved per VisitRateDifference

MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01 <- group_by(MY_tblParentalCare_sim_scaled_01, DiffVisit1Rate)

Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01 <- summarise (MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01,
					Amean = mean(AlternationValueScaledAv),
					Alower = Amean - sd(AlternationValueScaledAv)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValueScaledAv)/sqrt(n())*1.96,
					NbFiles = n())
					
Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01 <- dplyr::rename(Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01,VisitRateDifference= DiffVisit1Rate)
Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01 <- as.data.frame(Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01)

}


}


{#### simulation alternation in scaled files: shuffling intervals within files 
# I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

{## creation of i simulated dataset (and calculation of i Asim) for each j file, twice: when standardising sex is 0 then 1

# all files with standardizing sex = 0
SimData_scaled_0_for_Sim_0 <- SimData_scaled_0[,c('splitID','DVDRef','ScaledTstart','Sex','ScaledInterval')]
split_SimData_scaled_0_for_Sim_0 <- split(SimData_scaled_0_for_Sim_0,SimData_scaled_0_for_Sim_0$splitID)
out_Asim_j_0_sim <- lapply(split_SimData_scaled_0_for_Sim_0,simulation_nestwatch_function)
out_Asim_0_sim <- do.call(rbind, out_Asim_j_0_sim)

# all files with standardizing sex = 1
SimData_scaled_1_for_Sim_1 <- SimData_scaled_1[,c('splitID','DVDRef','ScaledTstart','Sex','ScaledInterval')]
split_SimData_scaled_1_for_Sim_1 <- split(SimData_scaled_1_for_Sim_1,SimData_scaled_1_for_Sim_1$splitID)
out_Asim_j_1_sim <- lapply(split_SimData_scaled_1_for_Sim_1,simulation_nestwatch_function)
out_Asim_1_sim <- do.call(rbind, out_Asim_j_1_sim)

}

head(out_Asim_0_sim)
head(out_Asim_1_sim)

{# out A sim summary

out_Asim_df_0_sim <- data.frame(splitID = unique(SimData_scaled_0_for_Sim_0$splitID), out_Asim_0_sim)
out_Asim_df_1_sim <- data.frame(splitID = unique(SimData_scaled_1_for_Sim_1$splitID), out_Asim_1_sim)
out_Asim_df_sim <- rbind(out_Asim_df_0_sim,out_Asim_df_1_sim)
out_Asim_df_sim <- merge(x=out_Asim_df_sim, y= MY_tblParentalCare_sim_scaled_01[,c('splitID','DiffVisit1Rate')], by='splitID', all.x =TRUE)

out_Asim_df_perDiffVisit1Rate_sim <- split(out_Asim_df_sim,out_Asim_df_sim$DiffVisit1Rate)

out1_out_Asim_df_perDiffVisit1Rate_sim <- lapply(out_Asim_df_perDiffVisit1Rate_sim,out_Asim_df_perDiffVisit1Rate_fun)
out2_out_Asim_df_perDiffVisit1Rate_sim <- data.frame(rownames(do.call(rbind,out1_out_Asim_df_perDiffVisit1Rate_sim)),do.call(rbind, out1_out_Asim_df_perDiffVisit1Rate_sim))

nrow(out2_out_Asim_df_perDiffVisit1Rate_sim)	# 43
rownames(out2_out_Asim_df_perDiffVisit1Rate_sim) <- NULL
colnames(out2_out_Asim_df_perDiffVisit1Rate_sim) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

out2_out_Asim_df_perDiffVisit1Rate_sim

{# A: for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01$Type <- "Observed"
out2_out_Asim_df_perDiffVisit1Rate_sim$Type <- "Expected"


VisitRateDiff_Amean_scaled_01_sim <- as.data.frame(rbind( Summary_MY_tblParentalCare_sim_scaled_01_perVisitRateDiff_01[1:21,],out2_out_Asim_df_perDiffVisit1Rate_sim[1:21,] ))
VisitRateDiff_Amean_scaled_01_sim$VisitRateDifference <- as.numeric(VisitRateDiff_Amean_scaled_01_sim$VisitRateDifference)

Fig1Scaled_01 <- ggplot(data=VisitRateDiff_Amean_scaled_01_sim, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("#009E73", "black"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_scaled_01_sim$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_scaled_01_sim$Amean, n = 9)) +  
  theme_classic()
  
}

}

VisitRateDiff_Amean_scaled_01_sim
dev.new()
Fig1Scaled_01


}



## simulation scaling effect on alternation with generated data

{## make data frame with data from 1000 different videos

visits <- function(){
	visits <- rgamma(100, rate=0.281,shape=1.08)
	csVisits <- cumsum(visits)
	outVisits <- csVisits[csVisits<90]
	return(outVisits)
	}

males <- as.data.frame(do.call(rbind, lapply(1:1000, function(x) data.frame(DVDRef=x, TstartFeedVisit=visits()))))
females <- as.data.frame(do.call(rbind, lapply(1:1000, function(x) data.frame(DVDRef=x, TstartFeedVisit=visits()))))
males$Sex <- 1
females$Sex <-0

males <- as.data.frame(do.call(rbind, lapply(split(males,males$DVDRef),function(x) {x$Interval <- c(0,diff(x$TstartFeedVisit))
return(x)})))
females <- as.data.frame(do.call(rbind, lapply(split(females,females$DVDRef),function(x) {x$Interval <- c(0,diff(x$TstartFeedVisit))
return(x)})))

allnestwatches <- rbind(males,females)
allnestwatches <- allnestwatches[order(allnestwatches$DVD,allnestwatches$TstartFeedVisit),]


}

head(allnestwatches)

{## calculate observed alternation

allnestwatches_split <- split(allnestwatches,allnestwatches$DVDRef)

calculate_alternation = function(x) {
x <- x[order(x$TstartFeedVisit),] 
x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex
return(c(
length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]),#NbAlternation
nrow(x[x$Sex == 1,]), # MVisit1
nrow(x[x$Sex == 0,]) # FVisit1
))
}

out1_allnestwatches_split <- lapply(allnestwatches_split,calculate_alternation)
out2_allnestwatches_split <- data.frame(rownames(do.call(rbind,out1_allnestwatches_split)),do.call(rbind, out1_allnestwatches_split))
rownames(out2_allnestwatches_split) <- NULL
colnames(out2_allnestwatches_split) <- c('DVD','NbAlternation', 'MVisit1','FVisit1')


out2_allnestwatches_split$DiffVisit1Rate <- abs(round(out2_allnestwatches_split$FVisit1 - out2_allnestwatches_split$MVisit1))
out2_allnestwatches_split$AlternationValue<- round(out2_allnestwatches_split$NbAlternation/(out2_allnestwatches_split$MVisit1 + out2_allnestwatches_split$FVisit1 -1) *100,1)

MY_tblParentalCare_generated <- out2_allnestwatches_split
}

head(MY_tblParentalCare_generated)

{## scale observed data and alternation

out_scaling_allnestwatches_split_0 <- lapply(X=allnestwatches_split,FUN=scaling_function, StandardizingSex = 0)
allnestwatches_scaled_0 <- do.call(rbind, out_scaling_allnestwatches_split_0)
out_scaling_list_SimData_1 <- lapply(X=allnestwatches_split,FUN=scaling_function, StandardizingSex = 1)
allnestwatches_scaled_1 <- do.call(rbind, out_scaling_list_SimData_1)

{### Calculate A score for observed scaled data with standardizing sex being 0 then 1


# all files with standardizing sex = 0
allnestwatches_scaled_0_split_0 <- split(allnestwatches_scaled_0,allnestwatches_scaled_0$DVDRef)

out1_allnestwatches_scaled_0_split_0 <-lapply(allnestwatches_scaled_0_split_0,SimData_split_fun)
out2_allnestwatches_scaled_0_split_0 <- data.frame(rownames(do.call(rbind,out1_allnestwatches_scaled_0_split_0)),do.call(rbind, out1_allnestwatches_scaled_0_split_0))
rownames(out2_allnestwatches_scaled_0_split_0) <- NULL
colnames(out2_allnestwatches_scaled_0_split_0) <- c('splitID','DVDRef','NbAlternationScaled', 'MVisit1','FVisit1')

# all files with standardizing sex = 1
allnestwatches_scaled_1_split_1 <- split(allnestwatches_scaled_1,allnestwatches_scaled_1$DVDRef)

out1_allnestwatches_scaled_1_split_1 <-lapply(allnestwatches_scaled_1_split_1,SimData_split_fun)
out2_allnestwatches_scaled_1_split_1 <- data.frame(rownames(do.call(rbind,out1_allnestwatches_scaled_1_split_1)),do.call(rbind, out1_allnestwatches_scaled_1_split_1))
rownames(out2_allnestwatches_scaled_1_split_1) <- NULL
colnames(out2_allnestwatches_scaled_1_split_1) <- c('splitID','DVDRef','NbAlternationScaled', 'MVisit1','FVisit1')

out2_allnestwatches_scaled_1_split_01 <- rbind(out2_allnestwatches_scaled_0_split_0,out2_allnestwatches_scaled_1_split_1)
}

head(out2_allnestwatches_scaled_1_split_01)

{# calculate AlternationValue for each file

MY_tblParentalCare_generated_scaled <- out2_allnestwatches_scaled_1_split_01
MY_tblParentalCare_generated_scaled$NbAlternationScaled <-  as.numeric(as.character(MY_tblParentalCare_generated_scaled$NbAlternationScaled))
MY_tblParentalCare_generated_scaled$MVisit1 <-  as.numeric(as.character(MY_tblParentalCare_generated_scaled$MVisit1))
MY_tblParentalCare_generated_scaled$FVisit1 <-  as.numeric(as.character(MY_tblParentalCare_generated_scaled$FVisit1))
MY_tblParentalCare_generated_scaled <- MY_tblParentalCare_generated_scaled[order(MY_tblParentalCare_generated_scaled$splitID),]

MY_tblParentalCare_generated_scaled$DiffVisit1Rate <- abs(round(MY_tblParentalCare_generated_scaled$FVisit1 - MY_tblParentalCare_generated_scaled$MVisit1))

MY_tblParentalCare_generated_scaled$AlternationValueScaled <- round(MY_tblParentalCare_generated_scaled$NbAlternationScaled/(MY_tblParentalCare_generated_scaled$MVisit1 + MY_tblParentalCare_generated_scaled$FVisit1 -1) *100,1)
MY_tblParentalCare_generated_scaled <- MY_tblParentalCare_generated_scaled[-1]

}

tail(MY_tblParentalCare_generated_scaled)

{# average NbAlternationScaled and AlternationValueScaled accross standardizing sex = 0 and then 1

MY_tblParentalCare_generated_scaled_01 <- data.frame(summarise (group_by(MY_tblParentalCare_generated_scaled, DVDRef),
					NbAlternationScaledAv = mean(NbAlternationScaled),
					DiffNbAlternationScaled = max(NbAlternationScaled)-min(NbAlternationScaled),
					DiffVisit1Rate = unique(DiffVisit1Rate),
					AlternationValueScaledAv = mean(AlternationValueScaled)))

MY_tblParentalCare_generated_scaled_01 <- MY_tblParentalCare_generated_scaled_01[order(MY_tblParentalCare_generated_scaled_01$DVDRef),]

# Nb files where NbAlternation Value Scaled is diferent with different standardizing sex
# length(MY_tblParentalCare_scaled_01$DVDRef[MY_tblParentalCare_scaled_01$DiffNbAlternationScaled !=0]) #673/1619 = 42%
# summary(MY_tblParentalCare_scaled_01$DiffNbAlternationScaled[MY_tblParentalCare_scaled_01$DiffNbAlternationScaled !=0])
}

tail(MY_tblParentalCare_generated_scaled_01)

{# summary Aobserved per VisitRateDifference

MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01 <- group_by(MY_tblParentalCare_generated_scaled_01, DiffVisit1Rate)

Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01 <- summarise (MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01,
					Amean = mean(AlternationValueScaledAv),
					Alower = Amean - sd(AlternationValueScaledAv)/sqrt(n())*1.96,
					Aupper = Amean + sd(AlternationValueScaledAv)/sqrt(n())*1.96,
					NbFiles = n())
					
Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01 <- dplyr::rename(Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01,VisitRateDifference= DiffVisit1Rate)
Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01 <- as.data.frame(Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01)

}

}

head(allnestwatches_scaled_0)
head(allnestwatches_scaled_1)
Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01

{#### simulation alternation: shuffling intervals within files 
# I kept the time of the first visit of both male and female in each file, and randomized subsequent intervals

{## creation of i simulated dataset (and calculation of i Asim) for each j file, twice: when standardising sex is 0 then 1

# all files with standardizing sex = 0
allnestwatches_scaled_0_for_Sim <- allnestwatches_scaled_0[,c('DVDRef','ScaledTstart','Sex','ScaledInterval')]
allnestwatches_scaled_0_for_Sim_split_0 <- split(allnestwatches_scaled_0_for_Sim,allnestwatches_scaled_0_for_Sim$DVDRef)
out_Agenerated_scaled_j_0 <- lapply(allnestwatches_scaled_0_for_Sim_split_0,simulation_nestwatch_function)
out_Agenerated_scaled_0 <- do.call(rbind, out_Agenerated_scaled_j_0)

# all files with standardizing sex = 1
allnestwatches_scaled_1_for_Sim <- allnestwatches_scaled_1[,c('DVDRef','ScaledTstart','Sex','ScaledInterval')]
allnestwatches_scaled_1_for_Sim_split_1 <- split(allnestwatches_scaled_1_for_Sim,allnestwatches_scaled_1_for_Sim$DVDRef)
out_Agenerated_scaled_j_1 <- lapply(allnestwatches_scaled_1_for_Sim_split_1,simulation_nestwatch_function)
out_Agenerated_scaled_1 <- do.call(rbind, out_Agenerated_scaled_j_1)

}

head(out_Agenerated_scaled_0)
head(out_Agenerated_scaled_1)

{# out A sim summary

out_Agenerated_scaled_0_df <- data.frame(DVDRef = unique(allnestwatches_scaled_0$DVDRef), out_Agenerated_scaled_0)
out_Agenerated_scaled_1_df <- data.frame(DVDRef = unique(allnestwatches_scaled_1$DVDRef), out_Agenerated_scaled_1)
out_Agenerated_scaled_df <- rbind(out_Agenerated_scaled_0_df,out_Agenerated_scaled_1_df)
out_Agenerated_scaled_df <- merge(x=out_Agenerated_scaled_df, y= MY_tblParentalCare_generated_scaled_01[,c('DVDRef','DiffVisit1Rate')], by='DVDRef', all.x =TRUE)

out_Agenerated_scaled_dff_perDiffVisit1Rate <- split(out_Agenerated_scaled_df,out_Agenerated_scaled_df$DiffVisit1Rate)

out1_Agenerated_scaled_dff_perDiffVisit1Rate_01 <- lapply(out_Agenerated_scaled_dff_perDiffVisit1Rate,out_Asim_df_perDiffVisit1Rate_fun)
out2_Agenerated_scaled_dff_perDiffVisit1Rate_01 <- data.frame(rownames(do.call(rbind,out1_Agenerated_scaled_dff_perDiffVisit1Rate_01)),do.call(rbind, out1_Agenerated_scaled_dff_perDiffVisit1Rate_01))

nrow(out2_Agenerated_scaled_dff_perDiffVisit1Rate_01)	# 20
rownames(out2_Agenerated_scaled_dff_perDiffVisit1Rate_01) <- NULL
colnames(out2_Agenerated_scaled_dff_perDiffVisit1Rate_01) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

}

out2_Agenerated_scaled_dff_perDiffVisit1Rate_01

{# A: for the moment cut at 20 visit rate difference in both randomized and observed, and plot

Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01$Type <- "Observed"
out2_Agenerated_scaled_dff_perDiffVisit1Rate_01$Type <- "Expected"


VisitRateDiff_Amean_generated__scaled_01 <- as.data.frame(rbind( Summary_MY_tblParentalCare_generated_scaled_01_perVisitRateDiff_01[1:19,],out2_Agenerated_scaled_dff_perDiffVisit1Rate_01[1:19,] ))
VisitRateDiff_Amean_generated__scaled_01$VisitRateDifference <- as.numeric(VisitRateDiff_Amean_generated__scaled_01$VisitRateDifference)

Fig1_generated_Scaled_01 <- ggplot(data=VisitRateDiff_Amean_generated__scaled_01, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("#009E73", "black"), labels=c("95% Expected", "95% Observed"))+
  scale_x_continuous(breaks = pretty(VisitRateDiff_Amean_generated__scaled_01$VisitRateDifference, n = 12)) +
  scale_y_continuous(breaks = pretty(VisitRateDiff_Amean_generated__scaled_01$Amean, n = 9)) +  
  theme_classic()
  
}

}









