#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Alternation in parental provisioning
#	 Randomization, Scaling, Simulation
#	 Start : 20/10/2016
#	 last modif : 24/10/2016
#	 commit: test all functions to correct and optimize them
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

{#### generic functions

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
 
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # same as as.numeric(as.character(x)) but more efficient

as.numeric.column.factor <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],  as.numeric.factor))

create_splitID <-function(x) {
x$splitID <- rep(1:length(rle((x$DVDRef))$lengths), times=rle((x$DVDRef))$lengths)}

} 

output_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_output"



{######## THE SPECIFIC FUNCTIONS 

Redefining_Tstarts <- function(Data){

	#Data <- RealData
	#x <- split(Data,Data$splitID)[[1]]

Redefining_Tstarts_one_split <- function(x) {

x0 <- x[x$Sex == 0,]
x1 <- x[x$Sex == 1,]

x0$NextTstart <-  c(x0$Tstart[-1],NA)	
x1$NextTstart <-  c(x1$Tstart[-1],NA)	

# modify all Tstart of visits that have the same Tstart as the previous visit (add 0.05), within individuals
for (i in 1:nrow(x0)){
if (x0$Tstart[i] == x0$NextTstart[i] & !is.na(x0$NextTstart[i]))
{
x0$Interval[i+1] <- 0.05
x0$Tstart[i+1] <- x0$Tstart[i]+0.05
}
}

for (i in 1:nrow(x1)){
if (x1$Tstart[i] == x1$NextTstart[i] & !is.na(x1$NextTstart[i]))
{
x1$Interval[i+1] <- 0.05
x1$Tstart[i+1] <- x1$Tstart[i]+0.05
}
}


x <- rbind(x0, x1)
x <- x[,-which(names(x)%in%c("NextTstart"))]
x <- x[order(as.numeric(rownames(x))),] 

return(x)
}

DataRedefined <- do.call(rbind, lapply(split(Data,Data$splitID),Redefining_Tstarts_one_split))
rownames(DataRedefined) <- NULL

return(DataRedefined)
}


## original data or scaled data

SummariseData <- function(Data, Type) {

CreateDataSummary = function(x) {

if (Type == 'Original'){x <- x[order(x$Tstart),] }
if (Type == 'Scaled'){x <- x[order(x$ScaledTstart),] }

x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex

return(c(
as.character(unique(x$DVDRef)), 
length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]),#NbAlternation
nrow(x[x$Sex == 1,]), # MVisit
nrow(x[x$Sex == 0,]), # FVisit
#abs(round(nrow(x[x$Sex == 1,]) - nrow(x[x$Sex == 0,]))), # VisitRateDifference if this was one hour video !
round(length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)])/(nrow(x) -1) *100,1)# AlternationValue
))


}

out1 <-lapply(split(Data,Data$splitID),CreateDataSummary)
SummaryData <- data.frame(rownames(do.call(rbind,out1)),do.call(rbind, out1))
rownames(SummaryData) <- NULL
colnames(SummaryData) <- c('splitID','DVDRef','NbAlternation', 'MVisit','FVisit','AlternationValue')
SummaryData <- as.numeric.column.factor(SummaryData)

return(SummaryData)

}

SummariseData_perVisitRateDifference <- function(DataSummary, Type) {

if (Type == 'Scaled')
{names(DataSummary)[names(DataSummary) == 'ScaledAvAlternationValue'] <- 'AlternationValue'}


return(data.frame(summarise (group_by(DataSummary, VisitRateDifference),
				Amean = mean(AlternationValue),
				Alower = Amean - sd(AlternationValue)/sqrt(n())*1.96,
				Aupper = Amean + sd(AlternationValue)/sqrt(n())*1.96,
				NbFiles = n())))
				

}


## randomization of original data or scaled data

Randomize_Data_WithinFile_and_Calculate_AlternationValue <- function(Data, Type) {

RandomizeData_oneSplit <-  function(x){

if (Type == 'Scaled'){
x <- x[,c('DVDRef','Sex','splitID','ScaledInterval','ScaledTstart')]
names(x)[names(x) == 'ScaledTstart'] <- 'Tstart'
names(x)[names(x) == 'ScaledInterval'] <- 'Interval'
}

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- c(0, sample_vector(x0$Interval[-1]))
x0$Tstart <- x0$Tstart[1] + cumsum(x0$Interval) 

x1$Interval <- c(0, sample_vector(x1$Interval[-1]))
x1$Tstart <- x1$Tstart[1] + cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
xsim$NextSexSame <- c(xsim$Sex[-1],NA) == xsim$Sex

return(round(length(xsim$NextSexSame[xsim$NextSexSame == FALSE & !is.na(xsim$NextSexSame)])/(nrow(xsim) -1) *100,1)) # AlternationValue

}


Asim <- data.frame(splitID = unique(Data$splitID), 
do.call(cbind, replicate(NreplicatesWithinFileRandomization,do.call(rbind,lapply(split(Data,Data$splitID),RandomizeData_oneSplit)),simplify=FALSE)))

return(Asim)

}
	
Add_MeanAsim_to_DataSummary <- function(Asim,DataSummary ){
# add result of randomisation to summary of original data
return(DataSummary <- merge(y=data.frame(splitID = unique(Asim$splitID),MeanAsim = rowMeans(Asim[-1])), 
							x= DataSummary, by='splitID', all.x =TRUE))
}

Summarise_RandomizedData_perVisitRateDifference <- function(Asim, DataSummary) {

Asim_df <- merge(x=Asim, y= DataSummary[,c('splitID','VisitRateDifference')], by='splitID', all.x =TRUE)

Summarise_AlternationValue_from_Randomized_Data_oneSplit <- function(x) {
	
	# for testing function
	# Asim <- AlternationValue_from_RealDataRandomized
	# DataSummary <- RealDataSummary
	# Asim_df <- merge(x=Asim, y= DataSummary[,c('splitID','VisitRateDifference')], by='splitID', all.x =TRUE)
	# x <- split(Asim_df,Asim_df$VisitRateDifference)[[1]]


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

out1 <- lapply(split(Asim_df,Asim_df$VisitRateDifference),Summarise_AlternationValue_from_Randomized_Data_oneSplit)
DataSummary_perVisitRateDifference <- data.frame(rownames(do.call(rbind,out1)),do.call(rbind, out1))

nrow(DataSummary_perVisitRateDifference)	# 32
rownames(DataSummary_perVisitRateDifference) <- NULL
colnames(DataSummary_perVisitRateDifference) <- c('VisitRateDifference','Amean','Alower','Aupper','NbFiles')

return(DataSummary_perVisitRateDifference)
}


## join original and randomized data for plotting

Plot_Original_vs_Randomized <- function(DataSummary_perVisitRateDifference, RandomizedDataSummary_perVisitRateDifference){

DataSummary_perVisitRateDifference$Type <- "Observed"
RandomizedDataSummary_perVisitRateDifference$Type <- "Expected"

Original_vs_Randomized <- data.frame(rbind(DataSummary_perVisitRateDifference[1:17,],RandomizedDataSummary_perVisitRateDifference[1:17,] ))
Original_vs_Randomized$VisitRateDifference <- as.numeric(Original_vs_Randomized$VisitRateDifference)

ggplot(data=Original_vs_Randomized, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper))+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  scale_colour_manual(values=c("#56B4E9", "black"), labels=c("95% Expected", "95% Observed"))+
  ylim(0,100) +
  theme_classic()

}


## scale original data

Scale_Data <- function(Data, StandardizingSex) {
	#out_x <- list()#to test each file
	#options(warn=2)#to test each file
	#Data <- RealData#to test each file
	#StandardizingSex <- 1#to test each file

	#for (j in 1:length(split(Data,Data$splitID))) {#to test each file

	#x <- split(Data,Data$splitID)[[j]]#to test each file

scaling_function <- function(x) {

x_StandardizingSex = subset(x, Sex == StandardizingSex)
x_OtherSex = subset(x, Sex != StandardizingSex)

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$Tstart[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$Tstart[-1],NA)	

{# modify all Tstart of visits that have the same Tstart as the previous visit (add 0.05), within individuals
for (i in 1:nrow(x_StandardizingSex)){
if (x_StandardizingSex$Tstart[i] == x_StandardizingSex$NextTstart[i] & !is.na(x_StandardizingSex$NextTstart[i]))
{
x_StandardizingSex$Interval[i+1] <- 0.05
x_StandardizingSex$Tstart[i+1] <- x_StandardizingSex$Tstart[i]+0.05
}
}

for (i in 1:nrow(x_OtherSex)){
if (x_OtherSex$Tstart[i] == x_OtherSex$NextTstart[i] & !is.na(x_OtherSex$NextTstart[i]))
{
x_OtherSex$Interval[i+1] <- 0.05
x_OtherSex$Tstart[i+1] <- x_OtherSex$Tstart[i]+0.05
}
}


x_StandardizingSex$Interval <- c(0,diff(x_StandardizingSex$Tstart))
x_OtherSex$Interval <- c(0,diff(x_OtherSex$Tstart))

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$Tstart[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$Tstart[-1],NA)	

}

		# for the standardizing sex, all intervals will be set to its initial mean interval
		multiplicator <-  mean(x_StandardizingSex$Interval[-1])/x_StandardizingSex$Interval[-1]


# to recalculate ScaledTstart, start from the initial first Tstart and add up scaled intervals (cumulative sum)
x_StandardizingSex$ScaledInterval <- c(0,(rep(median(x_StandardizingSex$Interval[-1]*multiplicator), nrow(x_StandardizingSex)-1))) # the scaled Interval for the standardizing sex is always the same, hence the repeat function ; the use of median instead of unique is because of rounding that make identical number tiny different
x_StandardizingSex$ScaledTstart <- x_StandardizingSex$Tstart[1] + cumsum(x_StandardizingSex$ScaledInterval) 


		# create vector of times on each foraging trip (all 10th of minute in between two Tstart from the same sex)
		StandardizingSex_trip = mapply(FUN = function(Tstart, NextTstart) {  
		if (Tstart==NextTstart) 
		{return (Tstart)} 
		if (Tstart!=NextTstart)	
		{return(list(((Tstart*10) : (NextTstart*10-1))/10))}}, # (the *10 and then /10 are the easiest way to construct thenths of minutes)
		Tstart = x_StandardizingSex$Tstart[-nrow(x_StandardizingSex)], 
		NextTstart = x_StandardizingSex$NextTstart[-nrow(x_StandardizingSex)])
		
		OtherSex_trip = mapply(FUN = function(Tstart, NextTstart) {  
		if (Tstart==NextTstart) 
		{return (Tstart)} 
		if (Tstart!=NextTstart)	
		{return(list(((Tstart*10) : (NextTstart*10-1))/10))}}, 
		Tstart = x_OtherSex$Tstart[-nrow(x_OtherSex)], 
		NextTstart = x_OtherSex$NextTstart[-nrow(x_OtherSex)])
		

		# check for the list entry of the other sex how many of the numbers also occur for the first sex (here the standadirzing sex)
		# this gives you the number of tenths-of-minutes that both birds were foraging at the same time
		outK <- NULL
		outKI<- list()
		
		for (i in 1:length(OtherSex_trip)){
		for (k in 1:length(StandardizingSex_trip)){
		outK[k] <- length(which(OtherSex_trip[[i]] %in% StandardizingSex_trip[[k]])) # stored the number of 10th of minutes from the other sex i trip that overlaps with all k trips from the standardising sex
		}
		outKI[[i]] <- sum(outK*multiplicator)/10 # there is one multiplicator per standardizing sex trip ; this is the scaled interval for the other sex for this i trip
		}
		
# recalculate ScaledTstart from those ScaledIntervals for the other sex
x_OtherSex$ScaledInterval <- c(0,do.call(rbind,outKI))
x_OtherSex$ScaledTstart <- x_OtherSex$Tstart[1] +cumsum(x_OtherSex$ScaledInterval)

# recreate x with Tstart and scaledTstart for both sexes
x <- rbind(x_StandardizingSex, x_OtherSex)
x <- x[,-which(names(x)%in%c("NextTstart"))]
x <- x[order(as.numeric(rownames(x))),] # sort as in initial x (order ScaledTstart migth slightly vary)

# to solve edges with no overlap: modify some ScaledInterval and ScaledTstart from x
FirstSex <- x$Sex[1] # who is the first sex to visit
LastSex <- x$Sex[nrow(x)] # who is the last sex to visit

if (StandardizingSex != FirstSex){ # if StandardizingSex is not the FirstSex, the first intervals of the others sex can't already be standardized, they do not fully overlap with the intervals of the standardizing sex, and are therefore left intact, unstandardized

if(sum(x_OtherSex$ScaledInterval) >0){ 
# add to the first overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==FirstSex & x$Tstart >=min(x$Tstart[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==FirstSex & x$Tstart >=min(x$Tstart[x$Sex==StandardizingSex]) ])]+
min(x$Tstart[x$Sex==StandardizingSex]) - max(x$Tstart[x$Sex==FirstSex & x$Tstart <= min(x$Tstart[x$Sex==StandardizingSex])])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$Tstart <= min(x$Tstart[x$Sex==StandardizingSex]) & x$Sex==FirstSex] <- 
x$Interval[x$Tstart <= min(x$Tstart[x$Sex==StandardizingSex]) & x$Sex==FirstSex] 

# recalculate the Tstart for the first overlapping trip of the other sex and for the exra non overlapping trips of the other sex from the beginning of the nest watch
x$ScaledTstart[x$Sex==FirstSex] <- x$Tstart[1] + cumsum(x$ScaledInterval[x$Sex==FirstSex])
}

if(sum(x_OtherSex$ScaledInterval) ==0){ # these are cases where all visits happen before the first visit of the standardizing sex

x$ScaledInterval[x$Sex == FirstSex] <- x$Interval[x$Sex == FirstSex]
x$ScaledTstart[x$Sex == FirstSex] <- x$ScaledTstart[x$Sex == FirstSex][1] +cumsum(x$ScaledInterval[x$Sex == FirstSex])

}

}

if (StandardizingSex != LastSex){ # if Standardazing is not the LastSex: there is no interval for the other sex to overlap with at the end, those ones can't be standardized, and are therefore 'left' intact unstandardized

if(sum(x_OtherSex$ScaledInterval) >0){
# add to the last overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])]+
min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ]) - max(x$Tstart[x$Sex==StandardizingSex])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$Tstart > min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])] <- 
x$Interval[x$Tstart > min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])]

# recalculate the Tstart for the last sex
x$ScaledTstart[x$Sex==LastSex] <- x$Tstart[x$Sex==LastSex][1] +cumsum(x$ScaledInterval[x$Sex==LastSex])
}

if(sum(x_OtherSex$ScaledInterval) ==0){  # these are cases where all visits happen after the last visit of the standardizing sex
x$ScaledInterval[x$Sex == LastSex] <- x$Interval[x$Sex == LastSex]
x$ScaledTstart[x$Sex == LastSex] <- x$ScaledTstart[x$Sex == LastSex][1] +cumsum(x$ScaledInterval[x$Sex == LastSex])

}
}

# round calculated numbers
x$ScaledTstart <- round(x$ScaledTstart,1)
x$ScaledInterval <- round(x$ScaledInterval,1)

	#out_x[[j]] <- x#to test each file
return(x)

}

out_scaling_list <- lapply(split(Data,Data$splitID),scaling_function)
Data_scaled <- do.call(rbind, out_scaling_list)

return(Data_scaled)

}

Plot_Timeline_Original_and_Scaled_Data <- function(Data_scaled, StandardizingSex) {

Reshape_Scaled_Data_for_plotting_oneSplit <- function(x) {

x_raw <- x[,c('splitID','DVDRef','Tstart','Sex')]
x_raw$Type <- 'Original'
	
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
 
Data_scaled_for_plotting_list <- lapply(split(Data_scaled,Data_scaled$splitID),Reshape_Scaled_Data_for_plotting_oneSplit)

plot9randomgraphs_scaled <- function(Data_scaled_for_plotting_list){
p <- NULL

for (j in 1:9)  {
for (i in sample(1:length(unique(Data_scaled$DVDRef)),9))
{

p[[j]] <-ggplot(Data_scaled_for_plotting_list[[i]])  + 
		geom_segment(aes(x=Tstart, xend=Tstart+0.5, y=as.factor(Sex), yend=as.factor(Sex)), size=3, colour =Data_scaled_for_plotting_list[[i]]$Colours) +
		xlab("Nest watch duration") + ylab("Sex")+ scale_y_discrete(breaks=c("-1","0","1","2"),
        labels=c("S0", "0", "1","S1"))+
		geom_text(x=(max(Data_scaled_for_plotting_list[[i]]$Tstart) + 0.5)/2, 
				y= 2.5,label=unique(Data_scaled_for_plotting_list[[i]]$DVDRef), colour='black')+
		theme(legend.position="none") 
}
}

multiplot(p[[1]],p[[2]],p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], p[[9]], cols=3)

}

return(plot9randomgraphs_scaled(Data_scaled_for_plotting_list))
}


## scaled and randomized scaled data

Summarise_AverageScaledData_AcrossBothStandardizingSex <- function(SummaryScaledData_0, SummaryScaledData_1, Type){

SummaryScaledData_01 <- rbind(SummaryScaledData_0,SummaryScaledData_1)

if (Type == 'ScaledRandomized'){
names(SummaryScaledData_01)[names(SummaryScaledData_01) == 'ScaledNbAlternation'] <- 'NbAlternation'
names(SummaryScaledData_01)[names(SummaryScaledData_01) == 'ScaledAlternationValue'] <- 'AlternationValue'
}


Summary_AverageScaledData <-  data.frame(summarise (group_by(SummaryScaledData_01, splitID),
					DVDRef = unique(DVDRef),
					ScaledAvNbAlternation = mean(NbAlternation),
					DiffScaledNbAlternation = max(NbAlternation)-min(NbAlternation), # to check where Alternation differ depending on which sex is the standardizing sex
					VisitRateDifference = unique(VisitRateDifference),
					ScaledAvAlternationValue = mean(AlternationValue)))

Summary_AverageScaledData <- Summary_AverageScaledData[order(Summary_AverageScaledData$splitID),]

return(Summary_AverageScaledData)

}


## join original and randomized data for plotting

Plot_Original_vs_Randomized_Scaled_vs_Non_Scaled <- function(DataSummary_perVisitRateDifference, 
															RandomizedDataSummary_perVisitRateDifference,
															ScaledDataSummary_perVisitRateDifference,
															RandomizedScaledDataSummary_perVisitRateDifference,
															Type){

DataSummary_perVisitRateDifference$Type <- "O_Observed"
RandomizedDataSummary_perVisitRateDifference$Type <- "O_Expected"
ScaledDataSummary_perVisitRateDifference$Type <- "S_Observed"
RandomizedScaledDataSummary_perVisitRateDifference$Type <- "S_Expected"

Original_vs_Randomized <- data.frame(rbind(DataSummary_perVisitRateDifference[1:17,],
											RandomizedDataSummary_perVisitRateDifference[1:17,],
											ScaledDataSummary_perVisitRateDifference[1:17,],
											RandomizedScaledDataSummary_perVisitRateDifference[1:17,]))
											
Original_vs_Randomized$VisitRateDifference <- as.numeric(Original_vs_Randomized$VisitRateDifference)

ggplot(data=Original_vs_Randomized, aes(x=VisitRateDifference, y=Amean, group=Type, colour=Type))+
  geom_point(size = 2)+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin=Alower, ymax=Aupper),width=0.9, size =0.8)+
  xlab("Visit rate difference")+
  ylab("Mean alternation")+
  ggtitle(Type) + 
  scale_colour_manual(values=c("#56B4E9", "black","#009e24", "gray50"), name= "", labels=c(
  "95% Expected Original", 
  "95% Observed Original",
  "95% Expected Scaled",
  "95% Observed Scaled"))+
  ylim(20,75)+
 theme_classic()+ 
 theme(panel.background = element_rect(colour = "black"))+
 theme(plot.title = element_text(size = rel(2)))+
 theme(axis.title.y = element_text(size = rel(1.5))) +
 theme(axis.title.x = element_text(size = rel(1.5)))+
 theme(axis.text = element_text(size = rel(1.5)))+
 theme(legend.position = c(0.1, 0.1))+
 theme(legend.text = element_text(size = rel(1.1)))

}


## Randomization among nest watch, within individual of same provisioning rate: from Real Data to Simulated Ransomized Data (Original Dataset 2)

Randomize_among_nest_watch <- function(Data, visitRATES){

Data <- merge(Data, visitRATES[,c('DVDRef','MVisit1RateH', 'FVisit1RateH')])
MData <- subset(Data[Data$Sex == 1,])
FData <- subset(Data[Data$Sex == 0,])

# remove the first line with interval (=0) from each file for each sex before shuffling interval
FData_to_Shuffle <- do.call(rbind,lapply(X=split(FData,FData$DVDRef), FUN=function(x){return(x[-1,])}))
MData_to_Shuffle <- do.call(rbind,lapply(X=split(MData,MData$DVDRef), FUN=function(x){return(x[-1,])}))

# save first Tstart of each file and each sex  (with interval = 0)
F_FirstTstart <- do.call(rbind,lapply(X=split(FData,FData$DVDRef), FUN=function(x){return(x[1,])}))
M_FirstTstart <- do.call(rbind,lapply(X=split(MData,MData$DVDRef), FUN=function(x){return(x[1,])}))

# function that simulate all nest watches once with identical first Tstarts and shuffled interval among individual of same sex and same visit rate

randomization_among_nest_watch_fun <- function(FData_to_Shuffle,M_FirstTstart){

# shuffled intervals among individuals of the same sex that have the same visit rate
FShuffled <- data.frame(FData_to_Shuffle %>% group_by(FVisit1RateH) %>% mutate(Interval=base::sample(Interval)))
MShuffled <- data.frame(MData_to_Shuffle %>% group_by(MVisit1RateH) %>% mutate(Interval=base::sample(Interval)))

# add first Tstart
SimFemale <- rbind(F_FirstTstart,FShuffled) # this set the order: the first Tstart from the original file is above the visits with shuffled intervals
SimFemale <- SimFemale[order(SimFemale$DVDRef),]

SimMale <- rbind(M_FirstTstart,MShuffled)
SimMale <- SimMale[order(SimMale$DVDRef),]

# recalculate Tstart
SimFemale <- do.call(rbind,lapply(X=split(SimFemale,SimFemale$DVDRef), FUN=function(x){x$Tstart <- x$Tstart[1] + cumsum(x$Interval)
return(x)}))
rownames(SimFemale) <- NULL

SimMale <- do.call(rbind,lapply(X=split(SimMale,SimMale$DVDRef), FUN=function(x){x$Tstart <- x$Tstart[1] + cumsum(x$Interval)
return(x)}))
rownames(SimMale) <- NULL

# bind together
SimData <- data.frame(bind_rows(SimMale, SimFemale)) # different from rbind as it binds two df with different columns, adding NAs
SimData[is.na(SimData)] <- 0
SimData <- SimData[order(SimData$DVDRef,SimData$Tstart),]
rownames(SimData) <- NULL

return(SimData[,c('DVDRef','Tstart','Sex','Interval','splitID')])

}

# replicate that function
SimData <- do.call(rbind, 
replicate(NreplicatesAmongFileRandomization, 
randomization_among_nest_watch_fun(FData_to_Shuffle, MData_to_Shuffle), 
simplify=FALSE ) )

SimData$splitID <- create_splitID(SimData)

return(SimData)

}



}



NreplicatesWithinFileRandomization <- 50


{### Original DataSet 1: Real data

{## Original Data

RealData <- read.csv(paste(output_folder,"R_RealData_to_Scale.csv", sep="/")) 
MY_tblParentalCare <- read.csv(paste(output_folder,"R_MY_tblParentalCare.csv", sep="/")) 
RealDataVisitRATES <- MY_tblParentalCare[c('DVDRef','FVisit1RateH','MVisit1RateH','DiffVisit1Rate')]

	#RealData <- Redefining_Tstarts(RealData) # does not make any difference?

RealDataSummary <- SummariseData(RealData, Type='Original')
RealDataSummary <- merge(RealDataSummary,RealDataVisitRATES[,c('DVDRef','DiffVisit1Rate')])
names(RealDataSummary)[names(RealDataSummary) == 'DiffVisit1Rate'] <- 'VisitRateDifference' # on visit rate per hour

RealDataSummary_perVisitRateDifference <- SummariseData_perVisitRateDifference(RealDataSummary, Type='Original')

## Randomization Original Data
AlternationValue_from_RealDataRandomized <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(RealData, Type='Original')
	
	# just to compare OBserved and Expected 
	RealDataSummary <- Add_MeanAsim_to_DataSummary(AlternationValue_from_RealDataRandomized, RealDataSummary)

RandomizedRealDataSummary_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(
AlternationValue_from_RealDataRandomized,RealDataSummary)

## join them to plot per Visit Rate Difference
PlotRealData_Original_vs_Randomized <- Plot_Original_vs_Randomized(
RealDataSummary_perVisitRateDifference,
RandomizedRealDataSummary_perVisitRateDifference)

}

head(RealData)

{## Scaled Original Data

ScaledRealData_0 <- Scale_Data(RealData, StandardizingSex = 0)
ScaledRealData_1 <- Scale_Data(RealData, StandardizingSex = 1)

## plot timeline with Original and scaled data
dev.new()
set.seed(10)
Plot_Timeline_Original_and_Scaled_Data(ScaledRealData_0,StandardizingSex = 0)
dev.new()
set.seed(10)
Plot_Timeline_Original_and_Scaled_Data(ScaledRealData_1,StandardizingSex = 1)

## Summarize Scaled Data
Summary_ScaledRealData_0 <- SummariseData(ScaledRealData_0,Type='Scaled')
Summary_ScaledRealData_1 <- SummariseData(ScaledRealData_1,Type='Scaled')

Summary_ScaledRealData_0 <- merge(Summary_ScaledRealData_0,RealDataVisitRATES[,c('DVDRef','DiffVisit1Rate')])
names(Summary_ScaledRealData_0)[names(Summary_ScaledRealData_0) == 'DiffVisit1Rate'] <- 'VisitRateDifference' # on visit rate per hour

Summary_ScaledRealData_1 <- merge(Summary_ScaledRealData_1,RealDataVisitRATES[,c('DVDRef','DiffVisit1Rate')])
names(Summary_ScaledRealData_1)[names(Summary_ScaledRealData_1) == 'DiffVisit1Rate'] <- 'VisitRateDifference' # on visit rate per hour


## Average summary scaled data accross both standardizing sex 
Summary_ScaledRealData_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledRealData_0,Summary_ScaledRealData_1, Type = 'Scaled')

## summarize average scaled data accross VisitRateDifference
Summary_Scaled_RealData_perVisitRateDifference <- SummariseData_perVisitRateDifference(Summary_ScaledRealData_01,Type='Scaled')

## randomize Scaled data 0 and 1
AlternationValue_from_ScaledRealDataRandomized_0 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(ScaledRealData_0, Type='Scaled')
AlternationValue_from_ScaledRealDataRandomized_1 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(ScaledRealData_1, Type='Scaled')

AlternationValue_from_ScaledRealDataRandomized_01 <- data.frame(bind_cols(AlternationValue_from_ScaledRealDataRandomized_0,AlternationValue_from_ScaledRealDataRandomized_1[-1]))

## Average summary randomized scaled data accross both standardizing sex 	
Summary_RandomizedScaledRealData_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledRealData_0,Summary_ScaledRealData_1, Type = 'ScaledRandomized')

## summarize average scaled data accross VisitRateDifference
Summary_Randomized_Scaled_RealData_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(
AlternationValue_from_ScaledRealDataRandomized_01,
Summary_ScaledRealData_01)

Plot_Scaled_RealData_Original_vs_Randomized <- Plot_Original_vs_Randomized(
Summary_Scaled_RealData_perVisitRateDifference,
Summary_Randomized_Scaled_RealData_perVisitRateDifference)
	
	
}

head(ScaledRealData_0)

}

dev.new()
Plot_Original_vs_Randomized_Scaled_vs_Non_Scaled(
RealDataSummary_perVisitRateDifference,
RandomizedRealDataSummary_perVisitRateDifference,
Summary_Scaled_RealData_perVisitRateDifference,
Summary_Randomized_Scaled_RealData_perVisitRateDifference, 
Type = 'Real Data')



	
{### Original DataSet 2: Simulated Randomized Data : shuffle interval AMONG individual same sex same visit rate

{## Original Data
NreplicatesAmongFileRandomization <- 2
OriginalSimulatedData <- Randomize_among_nest_watch(RealData,RealDataVisitRATES)

OriginalSimulatedDataSummary <- SummariseData(OriginalSimulatedData, Type='Original')
OriginalSimulatedDataSummary <- merge(OriginalSimulatedDataSummary,RealDataVisitRATES[,c('DVDRef','DiffVisit1Rate')])
names(OriginalSimulatedDataSummary)[names(OriginalSimulatedDataSummary) == 'DiffVisit1Rate'] <- 'VisitRateDifference' # on visit rate per hour

OriginalSimulatedDataSummary_perVisitRateDifference <- SummariseData_perVisitRateDifference(OriginalSimulatedDataSummary, Type='Original')

## Randomization Original Data
	AlternationValue_from_OriginalSimulatedData_Randomized <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(OriginalSimulatedData, Type='Original')
	#write.csv(AlternationValue_from_OriginalSimulatedData_Randomized,file = paste(output_folder,"R_Sim_within_on_Sim_among.csv", sep="/"), row.names = FALSE) # 20161021
	#AlternationValue_from_OriginalSimulatedData_Randomized <- read.csv(paste(output_folder,"R_Sim_within_on_Sim_among.csv", sep="/")) 
RandomizedOriginalSimulatedDataSummary_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(AlternationValue_from_OriginalSimulatedData_Randomized,OriginalSimulatedDataSummary)

Plot_OriginalSimulatedData_Original_vs_Randomized <- 
Plot_Original_vs_Randomized(OriginalSimulatedDataSummary_perVisitRateDifference,
RandomizedOriginalSimulatedDataSummary_perVisitRateDifference)


}

{## Scaled Original Data

ScaledOriginalSimulatedData_0 <- Scale_Data(OriginalSimulatedData, StandardizingSex = 0)
ScaledOriginalSimulatedData_1 <- Scale_Data(OriginalSimulatedData, StandardizingSex = 1)

## Summarize Scaled Data
Summary_ScaledOriginalSimulatedData_0 <- SummariseData(ScaledOriginalSimulatedData_0,Type='Scaled')
Summary_ScaledOriginalSimulatedData_1 <- SummariseData(ScaledOriginalSimulatedData_1,Type='Scaled')

Summary_ScaledOriginalSimulatedData_0 <- merge(Summary_ScaledOriginalSimulatedData_0,RealDataVisitRATES[,c('DVDRef','DiffVisit1Rate')])
names(Summary_ScaledOriginalSimulatedData_0)[names(Summary_ScaledOriginalSimulatedData_0) == 'DiffVisit1Rate'] <- 'VisitRateDifference' # on visit rate per hour

Summary_ScaledOriginalSimulatedData_1 <- merge(Summary_ScaledOriginalSimulatedData_1,RealDataVisitRATES[,c('DVDRef','DiffVisit1Rate')])
names(Summary_ScaledOriginalSimulatedData_1)[names(Summary_ScaledOriginalSimulatedData_1) == 'DiffVisit1Rate'] <- 'VisitRateDifference' # on visit rate per hour


## Average summary scaled data accross both standardizing sex 
Summary_ScaledOriginalSimulatedData_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledOriginalSimulatedData_0,Summary_ScaledOriginalSimulatedData_1, Type = 'Scaled')

## summarize average scaled data accross VisitRateDifference
Summary_Scaled_OriginalSimulatedData_perVisitRateDifference <- SummariseData_perVisitRateDifference(Summary_ScaledOriginalSimulatedData_01,Type='Scaled')

## randomize Scaled data 0 and 1
AlternationValue_from_ScaledOriginalSimulatedDataRandomized_0 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(ScaledOriginalSimulatedData_0, Type='Scaled')
AlternationValue_from_ScaledOriginalSimulatedDataRandomized_1 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(ScaledOriginalSimulatedData_1, Type='Scaled')

AlternationValue_from_ScaledOriginalSimulatedDataRandomized_01 <- data.frame(bind_cols(AlternationValue_from_ScaledOriginalSimulatedDataRandomized_0,AlternationValue_from_ScaledOriginalSimulatedDataRandomized_1[-1]))

## Average summary randomized scaled data accross both standardizing sex 	
Summary_RandomizedScaledOriginalSimulatedData_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledOriginalSimulatedData_0,Summary_ScaledOriginalSimulatedData_1, Type = 'ScaledRandomized')

## summarize average scaled data accross VisitRateDifference
Summary_Randomized_Scaled_OriginalSimulatedData_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(AlternationValue_from_ScaledOriginalSimulatedDataRandomized_01,Summary_ScaledOriginalSimulatedData_01)

Plot_Scaled_OriginalSimulatedData_Original_vs_Randomized <- 
Plot_Original_vs_Randomized(
Summary_Scaled_OriginalSimulatedData_perVisitRateDifference,
Summary_Randomized_Scaled_OriginalSimulatedData_perVisitRateDifference)
	

}

}

dev.new()
Plot_Original_vs_Randomized_Scaled_vs_Non_Scaled(
OriginalSimulatedDataSummary_perVisitRateDifference,
RandomizedOriginalSimulatedDataSummary_perVisitRateDifference,
Summary_Scaled_OriginalSimulatedData_perVisitRateDifference,
Summary_Randomized_Scaled_OriginalSimulatedData_perVisitRateDifference, 
Type = 'Simulated Randomized Data')




{## Original DataSet 3: Simulated Gamma Data


{## simulate nest watches of 1 hour (to have visit rate per hour directly as number of visits)

visits <- function(){
	visits <- rgamma(100, rate=0.281,shape=1.08)
	csVisits <- cumsum(visits)
	outVisits <- round(csVisits[csVisits<60],1)
	if (length(outVisits) >= 2) {return(outVisits)}
	}

males <- as.data.frame(do.call(rbind, lapply(1:1619, function(x) data.frame(splitID=x, DVDRef = x, Tstart=visits()))))
length(unique(males$splitID))

females <- as.data.frame(do.call(rbind, lapply(1:1619, function(x) data.frame(splitID=x, DVDRef = x, Tstart=visits()))))
length(unique(males$splitID))


males$Sex <- 1
females$Sex <-0

males <- as.data.frame(do.call(rbind, lapply(split(males,males$DVDRef),function(x) {x$Interval <- c(0,diff(x$Tstart))
return(x)})))

females <- as.data.frame(do.call(rbind, lapply(split(females,females$DVDRef),function(x) {x$Interval <- c(0,diff(x$Tstart))
return(x)})))

OriginalSimulatedGammaData <- rbind(males,females)
OriginalSimulatedGammaData <- OriginalSimulatedGammaData[order(OriginalSimulatedGammaData$DVD,OriginalSimulatedGammaData$Tstart),]
rownames(OriginalSimulatedGammaData) <- NULL

}

head(OriginalSimulatedGammaData,100)


{## Original Data

OriginalSimulatedGammaDataSummary <- SummariseData(OriginalSimulatedGammaData, Type='Original')
OriginalSimulatedGammaDataSummary$VisitRateDifference <- abs(OriginalSimulatedGammaDataSummary$MVisit - OriginalSimulatedGammaDataSummary$FVisit)

OriginalSimulatedGammaDataSummary_perVisitRateDifference <- SummariseData_perVisitRateDifference(OriginalSimulatedGammaDataSummary, Type='Original')

## Randomization Original Data
AlternationValue_from_OriginalSimulatedGammaData_Randomized <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(OriginalSimulatedGammaData, Type='Original')
RandomizedOriginalSimulatedGammaDataSummary_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(AlternationValue_from_OriginalSimulatedGammaData_Randomized,OriginalSimulatedGammaDataSummary)

Plot_OriginalSimulatedGammaData_Original_vs_Randomized <- 
Plot_Original_vs_Randomized(OriginalSimulatedGammaDataSummary_perVisitRateDifference,
RandomizedOriginalSimulatedGammaDataSummary_perVisitRateDifference)


}

{## Scaled Original Data

ScaledOriginalSimulatedGammaData_0 <- Scale_Data(OriginalSimulatedGammaData, StandardizingSex = 0)
ScaledOriginalSimulatedGammaData_1 <- Scale_Data(OriginalSimulatedGammaData, StandardizingSex = 1)

## Summarize Scaled Data
Summary_ScaledOriginalSimulatedGammaData_0 <- SummariseData(ScaledOriginalSimulatedGammaData_0,Type='Scaled')
Summary_ScaledOriginalSimulatedGammaData_1 <- SummariseData(ScaledOriginalSimulatedGammaData_1,Type='Scaled')

Summary_ScaledOriginalSimulatedGammaData_0$VisitRateDifference <- abs(Summary_ScaledOriginalSimulatedGammaData_0$MVisit - Summary_ScaledOriginalSimulatedGammaData_0$FVisit)
Summary_ScaledOriginalSimulatedGammaData_1$VisitRateDifference <- abs(Summary_ScaledOriginalSimulatedGammaData_1$MVisit - Summary_ScaledOriginalSimulatedGammaData_1$FVisit)

## Average summary scaled data accross both standardizing sex 
Summary_ScaledOriginalSimulatedGammaData_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledOriginalSimulatedGammaData_0,Summary_ScaledOriginalSimulatedGammaData_1, Type = 'Scaled')

## summarize average scaled data accross VisitRateDifference
Summary_Scaled_OriginalSimulatedGammaData_perVisitRateDifference <- SummariseData_perVisitRateDifference(Summary_ScaledOriginalSimulatedGammaData_01,Type='Scaled')

## randomize Scaled data 0 and 1
AlternationValue_from_ScaledOriginalSimulatedGammaDataRandomized_0 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(ScaledOriginalSimulatedGammaData_0, Type='Scaled')
AlternationValue_from_ScaledOriginalSimulatedGammaDataRandomized_1 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue(ScaledOriginalSimulatedGammaData_1, Type='Scaled')

AlternationValue_from_ScaledOriginalSimulatedGammaDataRandomized_01 <- data.frame(bind_cols(AlternationValue_from_ScaledOriginalSimulatedGammaDataRandomized_0,AlternationValue_from_ScaledOriginalSimulatedGammaDataRandomized_1[-1]))

## Average summary randomized scaled data accross both standardizing sex 	
Summary_RandomizedScaledOriginalSimulatedGammaData_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledOriginalSimulatedGammaData_0,Summary_ScaledOriginalSimulatedGammaData_1, Type = 'ScaledRandomized')

## summarize average scaled data accross VisitRateDifference
Summary_Randomized_Scaled_OriginalSimulatedGammaData_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(AlternationValue_from_ScaledOriginalSimulatedGammaDataRandomized_01,Summary_ScaledOriginalSimulatedGammaData_01)

Plot_Scaled_OriginalSimulatedGammaData_Original_vs_Randomized <- 
Plot_Original_vs_Randomized(Summary_Scaled_OriginalSimulatedGammaData_perVisitRateDifference,
Summary_Randomized_Scaled_OriginalSimulatedGammaData_perVisitRateDifference)
	

}

}

dev.new()
Plot_Original_vs_Randomized_Scaled_vs_Non_Scaled(
OriginalSimulatedGammaDataSummary_perVisitRateDifference,
RandomizedOriginalSimulatedGammaDataSummary_perVisitRateDifference,
Summary_Scaled_OriginalSimulatedGammaData_perVisitRateDifference,
Summary_Randomized_Scaled_OriginalSimulatedGammaData_perVisitRateDifference, 
Type = 'Simulated Gamma Data')



{## Original Data Set 4: Simulated Gamma Data - do not keep first Tstart for randomization nor scaling

{# slightly modify simulation above: the forst interval is not 0 but the first Tstart

males2 <- as.data.frame(do.call(rbind, lapply(split(males,males$DVDRef),function(x) {x$Interval <- c(x$Tstart[1],diff(x$Tstart))
return(x)})))

females2 <- as.data.frame(do.call(rbind, lapply(split(females,females$DVDRef),function(x) {x$Interval <- c(x$Tstart[1],diff(x$Tstart))
return(x)})))

OriginalSimulatedGammaData2 <- rbind(males2,females2)
OriginalSimulatedGammaData2 <- OriginalSimulatedGammaData2[order(OriginalSimulatedGammaData2$DVD,OriginalSimulatedGammaData2$Tstart),]
rownames(OriginalSimulatedGammaData2) <- NULL
}

head(OriginalSimulatedGammaData2)


Randomize_Data_WithinFile_and_Calculate_AlternationValue_Without_Keeping_First_Tstart <- function(Data, Type) {

RandomizeData_oneSplit <-  function(x){

if (Type == 'Scaled'){
x <- x[,c('DVDRef','Sex','splitID','ScaledInterval','ScaledTstart')]
names(x)[names(x) == 'ScaledTstart'] <- 'Tstart'
names(x)[names(x) == 'ScaledInterval'] <- 'Interval'
}

	# x <- split(OriginalSimulatedGammaData2, OriginalSimulatedGammaData2$splitID)[[1]]

x <- x[order(x$Tstart),]
x0 <- x[x$Sex==0,]
x1 <- x[x$Sex==1,]

x0$Interval <- sample_vector(x0$Interval)
x0$Tstart <- cumsum(x0$Interval) 

x1$Interval <- sample_vector(x1$Interval)
x1$Tstart <- cumsum(x1$Interval) 

xsim <- rbind(x0,x1)
xsim <- xsim[order(xsim$Tstart),] 
xsim$NextSexSame <- c(xsim$Sex[-1],NA) == xsim$Sex

return(round(length(xsim$NextSexSame[xsim$NextSexSame == FALSE & !is.na(xsim$NextSexSame)])/(nrow(xsim) -1) *100,1)) # AlternationValue

}


Asim <- data.frame(splitID = unique(Data$splitID), 
do.call(cbind, replicate(NreplicatesWithinFileRandomization,do.call(rbind,lapply(split(Data,Data$splitID),RandomizeData_oneSplit)),simplify=FALSE)))

return(Asim)

}


{## Original Data

OriginalSimulatedGammaData2Summary <- SummariseData(OriginalSimulatedGammaData2, Type='Original')
OriginalSimulatedGammaData2Summary$VisitRateDifference <- abs(OriginalSimulatedGammaData2Summary$MVisit - OriginalSimulatedGammaData2Summary$FVisit)

OriginalSimulatedGammaData2Summary_perVisitRateDifference <- SummariseData_perVisitRateDifference(OriginalSimulatedGammaData2Summary, Type='Original')

## Randomization Original Data
AlternationValue_from_OriginalSimulatedGammaData2_Randomized <- Randomize_Data_WithinFile_and_Calculate_AlternationValue_Without_Keeping_First_Tstart(OriginalSimulatedGammaData2, Type='Original')
RandomizedOriginalSimulatedGammaData2Summary_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(AlternationValue_from_OriginalSimulatedGammaData2_Randomized,OriginalSimulatedGammaData2Summary)

}


Scale_Data_Without_Keeping_First_Tstart <- function(Data, StandardizingSex) {
	#out_x <- list()#to test each file
	#options(warn=2)#to test each file
	#Data <- OriginalSimulatedGammaData2#to test each file
	#StandardizingSex <- 0#to test each file

	#for (j in 1:length(split(Data,Data$splitID))) {#to test each file

	#x <- split(Data,Data$splitID)[[j]]#to test each file

scaling_function <- function(x) {

x_StandardizingSex = subset(x, Sex == StandardizingSex)
x_OtherSex = subset(x, Sex != StandardizingSex)

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$Tstart[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$Tstart[-1],NA)	

{# modify all Tstart of visits that have the same Tstart as the previous visit (add 0.05), within individuals
for (i in 1:nrow(x_StandardizingSex)){
if (x_StandardizingSex$Tstart[i] == x_StandardizingSex$NextTstart[i] & !is.na(x_StandardizingSex$NextTstart[i]))
{
x_StandardizingSex$Interval[i+1] <- 0.05
x_StandardizingSex$Tstart[i+1] <- x_StandardizingSex$Tstart[i]+0.05
}
}

for (i in 1:nrow(x_OtherSex)){
if (x_OtherSex$Tstart[i] == x_OtherSex$NextTstart[i] & !is.na(x_OtherSex$NextTstart[i]))
{
x_OtherSex$Interval[i+1] <- 0.05
x_OtherSex$Tstart[i+1] <- x_OtherSex$Tstart[i]+0.05
}
}


x_StandardizingSex$Interval <- c(x_StandardizingSex$Tstart[1],diff(x_StandardizingSex$Tstart))
x_OtherSex$Interval <- c(x_OtherSex$Tstart[1],diff(x_OtherSex$Tstart))

x_StandardizingSex$NextTstart <- c(x_StandardizingSex$Tstart[-1],NA)
x_OtherSex$NextTstart <- c(x_OtherSex$Tstart[-1],NA)	

}

		if (x_StandardizingSex$Interval[1] != 0) {
		
		# for the standardizing sex, all intervals will be set to its initial mean interval
		multiplicator <-  mean(x_StandardizingSex$Interval)/x_StandardizingSex$Interval
		
		x_StandardizingSex$ScaledInterval <- rep(median(x_StandardizingSex$Interval*multiplicator), nrow(x_StandardizingSex)) # the scaled Interval for the standardizing sex is always the same, hence the repeat function ; the use of median instead of unique is because of rounding that make identical number tiny different
		x_StandardizingSex$ScaledTstart <- cumsum(x_StandardizingSex$ScaledInterval) 
		
		# create vector of times on each foraging trip (all 10th of minute in between two Tstart from the same sex)
		StandardizingSex_trip = mapply(FUN = function(Tstart, NextTstart) {  
		if (Tstart==NextTstart) 
		{return (Tstart)} 
		if (Tstart!=NextTstart)	
		{return(list(((Tstart*10) : (NextTstart*10-1))/10))}}, # (the *10 and then /10 are the easiest way to construct thenths of minutes)
		Tstart = c(0,x_StandardizingSex$Tstart[-nrow(x_StandardizingSex)]), # add 0 as a first Tstart since 0 to first Tstart is a real interval / foraging trip
		NextTstart = c(x_StandardizingSex$Tstart[1],x_StandardizingSex$NextTstart[-nrow(x_StandardizingSex)]))
		}
		
		if (x_StandardizingSex$Interval[1] == 0){ # when bird in the nest when starts video
		
		multiplicator <-  mean(x_StandardizingSex$Interval[-1])/x_StandardizingSex$Interval[-1]
		
		x_StandardizingSex$ScaledInterval <- rep(median(x_StandardizingSex$Interval[-1]*multiplicator), nrow(x_StandardizingSex)) # the scaled Interval for the standardizing sex is always the same, hence the repeat function ; the use of median instead of unique is because of rounding that make identical number tiny different
		x_StandardizingSex$ScaledTstart <- cumsum(x_StandardizingSex$ScaledInterval) 
		
		# create vector of times on each foraging trip (all 10th of minute in between two Tstart from the same sex)
		StandardizingSex_trip = mapply(FUN = function(Tstart, NextTstart) {  
		if (Tstart==NextTstart) 
		{return (Tstart)} 
		if (Tstart!=NextTstart)	
		{return(list(((Tstart*10) : (NextTstart*10-1))/10))}}, # (the *10 and then /10 are the easiest way to construct thenths of minutes)
		Tstart = x_StandardizingSex$Tstart[-nrow(x_StandardizingSex)],
		NextTstart = x_StandardizingSex$NextTstart[-nrow(x_StandardizingSex)])
		}

		OtherSex_trip = mapply(FUN = function(Tstart, NextTstart) {  
		if (Tstart==NextTstart) 
		{return (Tstart)} 
		if (Tstart!=NextTstart)	
		{return(list(((Tstart*10) : (NextTstart*10-1))/10))}}, 
		Tstart = c(0,x_OtherSex$Tstart[-nrow(x_OtherSex)]), 
		NextTstart = c(x_OtherSex$Tstart[1],x_OtherSex$NextTstart[-nrow(x_OtherSex)]))
		

		# check for the list entry of the other sex how many of the numbers also occur for the first sex (here the standadirzing sex)
		# this gives you the number of tenths-of-minutes that both birds were foraging at the same time
		outK <- NULL
		outKI<- list()
		
		for (i in 1:length(OtherSex_trip)){
		for (k in 1:length(StandardizingSex_trip)){
		outK[k] <- length(which(OtherSex_trip[[i]] %in% StandardizingSex_trip[[k]])) # stored the number of 10th of minutes from the other sex i trip that overlaps with all k trips from the standardising sex
		}
		outKI[[i]] <- sum(outK*multiplicator)/10 # there is one multiplicator per standardizing sex trip ; this is the scaled interval for the other sex for this i trip
		}
		
# recalculate ScaledTstart from those ScaledIntervals for the other sex
x_OtherSex$ScaledInterval <- do.call(rbind,outKI)
x_OtherSex$ScaledTstart <- cumsum(x_OtherSex$ScaledInterval)

# recreate x with Tstart and scaledTstart for both sexes
x <- rbind(x_StandardizingSex, x_OtherSex)
x <- x[,-which(names(x)%in%c("NextTstart"))]
x <- x[order(as.numeric(rownames(x))),] # sort as in initial x (order ScaledTstart migth slightly vary)

# to solve edges with no overlap: modify some ScaledInterval and ScaledTstart from x
FirstSex <- x$Sex[1] # who is the first sex to visit
LastSex <- x$Sex[nrow(x)] # who is the last sex to visit

if (StandardizingSex != FirstSex){ # if StandardizingSex is not the FirstSex, the first intervals of the others sex can't already be standardized, they do not fully overlap with the intervals of the standardizing sex, and are therefore left intact, unstandardized

if(sum(x_OtherSex$ScaledInterval) >0){ 
# add to the first overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==FirstSex & x$Tstart >=min(x$Tstart[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==FirstSex & x$Tstart >=min(x$Tstart[x$Sex==StandardizingSex]) ])]+
min(x$Tstart[x$Sex==StandardizingSex]) - max(x$Tstart[x$Sex==FirstSex & x$Tstart <= min(x$Tstart[x$Sex==StandardizingSex])])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$Tstart <= min(x$Tstart[x$Sex==StandardizingSex]) & x$Sex==FirstSex] <- 
x$Interval[x$Tstart <= min(x$Tstart[x$Sex==StandardizingSex]) & x$Sex==FirstSex] 

# recalculate the Tstart for the first overlapping trip of the other sex and for the exra non overlapping trips of the other sex from the beginning of the nest watch
x$ScaledTstart[x$Sex==FirstSex] <- x$Tstart[1] + cumsum(x$ScaledInterval[x$Sex==FirstSex])
}

if(sum(x_OtherSex$ScaledInterval) ==0){ # these are cases where all visits happen before the first visit of the standardizing sex

x$ScaledInterval[x$Sex == FirstSex] <- x$Interval[x$Sex == FirstSex]
x$ScaledTstart[x$Sex == FirstSex] <- x$ScaledTstart[x$Sex == FirstSex][1] +cumsum(x$ScaledInterval[x$Sex == FirstSex])

}

}

if (StandardizingSex != LastSex){ # if Standardazing is not the LastSex: there is no interval for the other sex to overlap with at the end, those ones can't be standardized, and are therefore 'left' intact unstandardized

if(sum(x_OtherSex$ScaledInterval) >0){
# add to the last overlapping foraging trip interval, the extra time that is not overlapping, left unstandardized
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])] <- 
x$ScaledInterval[x$Tstart == min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])]+
min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ]) - max(x$Tstart[x$Sex==StandardizingSex])

# keep the interval non standardized for the extra non overlapping trips
x$ScaledInterval[x$Tstart > min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])] <- 
x$Interval[x$Tstart > min(x$Tstart[x$Sex==LastSex & x$Tstart >=max(x$Tstart[x$Sex==StandardizingSex]) ])]

# recalculate the Tstart for the last sex
x$ScaledTstart[x$Sex==LastSex] <- x$Tstart[x$Sex==LastSex][1] +cumsum(x$ScaledInterval[x$Sex==LastSex])
}

if(sum(x_OtherSex$ScaledInterval) ==0){  # these are cases where all visits happen after the last visit of the standardizing sex
x$ScaledInterval[x$Sex == LastSex] <- x$Interval[x$Sex == LastSex]
x$ScaledTstart[x$Sex == LastSex] <- x$ScaledTstart[x$Sex == LastSex][1] +cumsum(x$ScaledInterval[x$Sex == LastSex])

}
}

# round calculated numbers
x$ScaledTstart <- round(x$ScaledTstart,1)
x$ScaledInterval <- round(x$ScaledInterval,1)

	#out_x[[j]] <- x#to test each file
return(x)

}

out_scaling_list <- lapply(split(Data,Data$splitID),scaling_function)
Data_scaled <- do.call(rbind, out_scaling_list)

return(Data_scaled)

}


{## Scaled Original Data

ScaledOriginalSimulatedGammaData2_0 <- Scale_Data_Without_Keeping_First_Tstart(OriginalSimulatedGammaData2, StandardizingSex = 0)
ScaledOriginalSimulatedGammaData2_1 <- Scale_Data_Without_Keeping_First_Tstart(OriginalSimulatedGammaData2, StandardizingSex = 1)

## Summarize Scaled Data
Summary_ScaledOriginalSimulatedGammaData2_0 <- SummariseData(ScaledOriginalSimulatedGammaData2_0,Type='Scaled')
Summary_ScaledOriginalSimulatedGammaData2_1 <- SummariseData(ScaledOriginalSimulatedGammaData2_1,Type='Scaled')

Summary_ScaledOriginalSimulatedGammaData2_0$VisitRateDifference <- abs(Summary_ScaledOriginalSimulatedGammaData2_0$MVisit - Summary_ScaledOriginalSimulatedGammaData2_0$FVisit)
Summary_ScaledOriginalSimulatedGammaData2_1$VisitRateDifference <- abs(Summary_ScaledOriginalSimulatedGammaData2_1$MVisit - Summary_ScaledOriginalSimulatedGammaData2_1$FVisit)

## Average summary scaled data accross both standardizing sex 
Summary_ScaledOriginalSimulatedGammaData2_01 <- Summarise_AverageScaledData_AcrossBothStandardizingSex(Summary_ScaledOriginalSimulatedGammaData2_0,Summary_ScaledOriginalSimulatedGammaData2_1, Type = 'Scaled')

## summarize average scaled data accross VisitRateDifference
Summary_Scaled_OriginalSimulatedGammaData2_perVisitRateDifference <- SummariseData_perVisitRateDifference(Summary_ScaledOriginalSimulatedGammaData2_01,Type='Scaled')

## randomize Scaled data 0 and 1
AlternationValue_from_ScaledOriginalSimulatedGammaData2Randomized_0 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue_Without_Keeping_First_Tstart(ScaledOriginalSimulatedGammaData2_0, Type='Scaled')
AlternationValue_from_ScaledOriginalSimulatedGammaData2Randomized_1 <- Randomize_Data_WithinFile_and_Calculate_AlternationValue_Without_Keeping_First_Tstart(ScaledOriginalSimulatedGammaData2_1, Type='Scaled')

AlternationValue_from_ScaledOriginalSimulatedGammaData2Randomized_01 <- data.frame(bind_cols(AlternationValue_from_ScaledOriginalSimulatedGammaData2Randomized_0,AlternationValue_from_ScaledOriginalSimulatedGammaData2Randomized_1[-1]))

## summarize average scaled data accross VisitRateDifference
Summary_Randomized_Scaled_OriginalSimulatedGammaData2_perVisitRateDifference <- Summarise_RandomizedData_perVisitRateDifference(AlternationValue_from_ScaledOriginalSimulatedGammaData2Randomized_01,Summary_ScaledOriginalSimulatedGammaData2_01)

}

}

dev.new()
Plot_Original_vs_Randomized_Scaled_vs_Non_Scaled(
OriginalSimulatedGammaData2Summary_perVisitRateDifference,
RandomizedOriginalSimulatedGammaData2Summary_perVisitRateDifference,
Summary_Scaled_OriginalSimulatedGammaData2_perVisitRateDifference,
Summary_Randomized_Scaled_OriginalSimulatedGammaData2_perVisitRateDifference, 
Type = 'Simulated Gamma Data without Keeping first Tstart')



## Original Data Set 5: Simulaed Gamma data from different gamma distributions to match observed variation in provisioning rates?



## 
regularvisits <- function(){
	visits <- rep(2.74,1000)
	csVisits <- cumsum(visits)
	outVisits <- round(csVisits[csVisits<60],1)
	{return(outVisits)}
	}


males <- as.data.frame(do.call(rbind, lapply(1:1619, function(x) data.frame(splitID=x, DVDRef = x, Tstart=regularvisits()))))
length(unique(males$splitID))

females <- as.data.frame(do.call(rbind, lapply(1:1619, function(x) data.frame(splitID=x, DVDRef = x, Tstart=visits()))))
length(unique(males$splitID))

males$Sex <- 1
females$Sex <-0



males <- as.data.frame(do.call(rbind, lapply(split(males,males$DVDRef),function(x) {x$Interval <- c(0,diff(x$Tstart))
return(x)})))

females <- as.data.frame(do.call(rbind, lapply(split(females,females$DVDRef),function(x) {x$Interval <- c(0,diff(x$Tstart))
return(x)})))


OriginalSimulatedGammaData3 <- rbind(males,females)
OriginalSimulatedGammaData3 <- OriginalSimulatedGammaData3[order(OriginalSimulatedGammaData3$DVD,OriginalSimulatedGammaData3$Tstart),]
rownames(OriginalSimulatedGammaData3) <- NULL


head(OriginalSimulatedGammaData3)








