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



{#### scale visits

split_MY_RawFeedingVisits_per_splitID <- split(MY_RawFeedingVisits,MY_RawFeedingVisits$splitID)

out_scaling <- list()
options(warn=2)

for (j in 1:length(unique(MY_RawFeedingVisits$splitID)))
{

x <- split_MY_RawFeedingVisits_per_splitID[[j]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[2]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[778]]
	# x <- split_MY_RawFeedingVisits_per_splitID[[432]]
	
FirstSex <- x$Sex[1]
x_FirstSex = subset(x, Sex == FirstSex)
x_OtherSex = subset(x, Sex != FirstSex)

x_FirstSex$NextTstart <- c(x_FirstSex$TstartFeedVisit[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$TstartFeedVisit[-1],NA)	

multiplicator <-  mean(x_FirstSex$Interval[x_FirstSex$Sex == FirstSex]) /x_FirstSex$Interval[x_FirstSex$Sex == FirstSex]

# remark: problem caused by two consecutive visits of the same bird at the same Tstart (10th of minutes...) : avoided by using 'median' of the scaled intervals excluding Inf
ScaledInterval <- median(x_FirstSex$Interval*multiplicator, na.rm=TRUE)
multiplicator <- !is.infinite(multiplicator)

x_FirstSex$ScaledTstart <- x_FirstSex$TstartFeedVisit[1] + c(0,cumsum(rep(ScaledInterval, nrow(x_FirstSex)-1)))


		# create vector of times on a foraging trip (the *10 and then /10 are the easiest way to construct thenths of minutes)

		FirstSex_trip = mapply(FUN = function(TstartFeedVisit, NextTstart) {  
		if (TstartFeedVisit==NextTstart) 
		{return (TstartFeedVisit)} 
		if (TstartFeedVisit!=NextTstart)	
		{return(list(((TstartFeedVisit*10) : (NextTstart*10-1))/10))}}, 
		TstartFeedVisit = x_FirstSex$TstartFeedVisit[-nrow(x_FirstSex)], 
		NextTstart = x_FirstSex$NextTstart[-nrow(x_FirstSex)])
		
		OtherSex_trip = mapply(FUN = function(TstartFeedVisit, NextTstart) {  
		if (TstartFeedVisit==NextTstart) 
		{return (TstartFeedVisit)} 
		if (TstartFeedVisit!=NextTstart)	
		{return(list(((TstartFeedVisit*10) : (NextTstart*10-1))/10))}}, 
		TstartFeedVisit = x_OtherSex$TstartFeedVisit[-nrow(x_OtherSex)], 
		NextTstart = x_OtherSex$NextTstart[-nrow(x_OtherSex)])
		

		# check for the list entry of the other sex how many of the numbers also occur for the first sex
		# this gives you the number of tenths-of-minutes that both birds were foraging at the same time
		outK <- NULL
		outKI<- list()
		
		for (i in 1:length(OtherSex_trip)){
		for (k in 1:length(FirstSex_trip)){
		outK[k] <- length(which(OtherSex_trip[[i]] %in% FirstSex_trip[[k]]))
		}
		outKI[[i]] <- sum(outK*multiplicator[-1])/10
		}
		
# add to subset other sex
x_OtherSex$ScaledInterval <- c(0,do.call(rbind,outKI))
x_OtherSex$ScaledTstart <- x_OtherSex$TstartFeedVisit[1] +cumsum(x_OtherSex$ScaledInterval)

x <-rbind(x_FirstSex, x_OtherSex[,-which(names(x_OtherSex)%in%c("ScaledInterval"))])
x <- x[,c('DVDRef','TstartFeedVisit','Sex','ScaledTstart','splitID')]
x$ScaledTstart <- round(x$ScaledTstart,1)
x <- x[order(x$TstartFeedVisit),]

out_scaling[[j]] <- x	

}	
	
MY_RawFeedingVisits_scaled <- do.call(rbind, out_scaling)

}

head(MY_RawFeedingVisits_scaled,20)

{#### plot scaled visit near raw visits

{## reshape dataframe

MY_RawFeedingVisits_scaled_raw <- MY_RawFeedingVisits_scaled[,c('splitID','DVDRef','TstartFeedVisit','Sex')]
MY_RawFeedingVisits_scaled_raw$Type <- 'Original'
colnames(MY_RawFeedingVisits_scaled_raw)[which(names(MY_RawFeedingVisits_scaled_raw) == "TstartFeedVisit")] <- "Tstart"
	
MY_RawFeedingVisits_scaled_scaled <- MY_RawFeedingVisits_scaled[,c('splitID','DVDRef','ScaledTstart','Sex')]
MY_RawFeedingVisits_scaled_scaled$Type <- 'Scaled'
colnames(MY_RawFeedingVisits_scaled_scaled)[which(names(MY_RawFeedingVisits_scaled_scaled) == "ScaledTstart")] <- "Tstart"	
MY_RawFeedingVisits_scaled_scaled$Sex[MY_RawFeedingVisits_scaled_scaled$Sex == 0 & MY_RawFeedingVisits_scaled_scaled$Type == "Scaled"] <- -1
MY_RawFeedingVisits_scaled_scaled$Sex[MY_RawFeedingVisits_scaled_scaled$Sex == 1 & MY_RawFeedingVisits_scaled_scaled$Type == "Scaled"] <- 2

MY_RawFeedingVisits_scaled_for_plotting <- rbind(MY_RawFeedingVisits_scaled_raw,MY_RawFeedingVisits_scaled_scaled)
MY_RawFeedingVisits_scaled_for_plotting <- MY_RawFeedingVisits_scaled_for_plotting[order(MY_RawFeedingVisits_scaled_for_plotting$DVDRef),]
rownames(MY_RawFeedingVisits_scaled_for_plotting) <- NULL

split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID <- split(MY_RawFeedingVisits_scaled_for_plotting,MY_RawFeedingVisits_scaled_for_plotting$splitID)

add_color_function <- function(x){
x$Colours[x$Sex == 0] <- "orange"
x$Colours[x$Sex == 1] <- "green"

if(x$Sex[1] == 0){
x$Colours[x$Sex == -1] <- "black"
x$Colours[x$Sex == 2] <- "deepskyblue"
}
else{
x$Colours[x$Sex == -1] <- "red"
x$Colours[x$Sex == 2] <- "black"
}

return(x)
}

out1_split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID <- lapply(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID,add_color_function)
MY_RawFeedingVisits_scaled_for_plotting <- do.call(rbind,out1_split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID)
rownames(MY_RawFeedingVisits_scaled_for_plotting) <- NULL


}

head(MY_RawFeedingVisits_scaled_for_plotting,50)

{## plot '4' sexes # couldn't get the rigth color to be selected
split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID <- split(MY_RawFeedingVisits_scaled_for_plotting,MY_RawFeedingVisits_scaled_for_plotting$splitID)

plot9randomgraphs_scaled <- function(){
p <- NULL

for (j in 1:9)  {
for (i in sample(1:length(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID),9))
{
# colour=split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$Colours
p[[j]] <-ggplot(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]])  + 
		geom_segment(aes(x=Tstart, xend=Tstart+0.5, y=as.factor(Sex), yend=as.factor(Sex)), size=3, colour =split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$Colours) +
		#scale_colour_manual(values = split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$Colours)+
		xlab("Duration") + ylab("Sex")+
		geom_text(x=(max(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$Tstart) + 0.5)/2, 
				y= 0.5,label=unique(split_MY_RawFeedingVisits_scaled_for_plotting_per_splitID[[i]]$DVDRef), colour='black')+
		theme(legend.position="none") 
}
}

multiplot(p[[1]],p[[2]],p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], cols=3)

}

}

set.seed(10)
dev.new()
plot9randomgraphs_scaled()

}







