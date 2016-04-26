#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Compile provisioning data sparrows
#	 Extract data from excel files and DB
#	 Start : 21/12/2015
#	 last modif :25/04/2016  
#	 commit: adjust code to new DB
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### Important remarks to read !

{### remarks about packages

## library 'xlsx' run with rjava, does not have enough memory to go through all excel files, hence the change to 'openxlsx', that can only read xlsx files (not xls)
## but to get colors out of old templates, need to have excel files as java object, hence use of package 'xlsx'. 
## In addition, this require to convert all xls files to xlsx

## within package xlsx:
## if load workbook as a java object > header is index = 1 ; while with read.xlsx function > header has no index but the 1st row of data has index = 1
# in addition: load workbook supress lines without data, which read.xlsx does not do, so index not matching if long excel files with blank lines (needs to be deleted)


}

{### overview decisions taken

## excel files considered are only the one which have:
# an entry in tblDVD_XlsFiles (rk: some files have an entry in DB but are not included here: see 'FILES THAT SHOULD BE INCLUDED') 
# OR that should have one and belong to 'missingDVDFilenames'...
# a situation = 4 in tblDVDInfo (i.e. only chicks)
# wrong = No in tblDVDInfo
# methodYN = either yes or no as no one figured out what it was about...

## nb of chicks considered in nest at time of recording
# DVDInfo nb of offspring 
# mismatches with nb of chiks alive at time of recording bigger than 1 were SOMETIMES corrected
# for 2004-2005 nest checks were actually done after the recording and reported in DVD Info as Shinishi wanted to use those data, so its correct for those years

## last seen alive is the output of the DB query 
#> needs to be updated when new pedigree imported !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## decision still to be taken:
# should excluded early age chicks because can still be brooded ??
# should exclude recordings when the next nest visit revealed every chicks were dead ?

## LHT calculation
# prior residence accross year (does not start fresh each year)

## Nb Visit1 calculation:
# if OF is followed by IN and if time end and start are similar > this is merged into one visit 
# this is the procedure for Malika's protocol, but the code has been applied to all files
# since time in decimal can be similar despite the bird leaving for 6 second, this can lead to (very few) 'mistakes'
# Nevertheless, in Shinishi's files, if a bird stay around the nest boxe for a few second before entering again, this was counted as two visits
# it still is in this code, although this is arguable.



}

{### annotations

## New and Old template mess up
# New means Issie's template
# Old means Shinichi's
# Malika means Malika's

## Mother Father Mum Dad Male Female mess up
# for me M  always stands for Male, F for Female
#'Mum' and 'Dad' are sometimes used

## Time 0 1 2
# time 0 = absent from the nest box
# time 1 = time feeding (IN or OF)
# time 2 = time around the nest box
# time 02 = time not feeding = time not 1


}

{### forseen code breaks in the future
# when metadata for excel files (Tape length, Observer, Protocol, and Notes) will be imported to tblXlsFiles rather than tblParentalCare
}

}

rm(list = ls(all = TRUE))
TimeStart <- Sys.time()


{### packages, settings, working directories, connection to Access DB, sqlFetch and sqlQuery

{# packages
library(RODBC)
# library(openxlsx) # package openxlsx will be needed later in the code (after detaching the conflicting xlsx nevertheless needed to get colors for old templates)
# library(xlsx)	# package xlsx will be needed later in the code (after detaching the conflicting openxlsx nevertheless needed at first to be faster/not crashing)
require(zoo)
}

{# input from excel files in dropbox
pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"
}

{# text files from input folder
input_folder <- "C:/Users/mihle/Documents/_Malika_Sheffield/_CURRENT BACKUP/stats&data_extraction/ProvisioningDataCombination/R_input"
sys_LastSeenAlive <- read.table(file= paste(input_folder,"sys_LastSeenAlive_20160314.txt", sep="/"), sep='\t', header=T)	## !!! to update when new pedigree !!! (and other corrections potentially)
sunrise <- read.table(file= paste(input_folder,"sunrise.txt", sep="/"), sep='\t', header=T)
}

{# input from database

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")

# SqlFetch
tblDVD_XlsFiles <- sqlFetch(conDB, "tblDVD_XlsFiles")
tblDVD_XlsFiles <- tblDVD_XlsFiles[with(tblDVD_XlsFiles, order(tblDVD_XlsFiles$Filename)),]
tblBirdID <- sqlFetch(conDB, "tblBirdID")
tblParentalCare <- sqlFetch(conDB, "zzz_OldtblParentalCare")
tblBroodEvents <- sqlFetch(conDB, "tblBroodEvents")
tblBroods <- sqlFetch(conDB, "tblBroods")
tblAllCodes <- sqlFetch(conDB, "tblAllCodes")
tblDVDInfo <- sqlFetch(conDB, "tblDVDInfo")
tblNestVisits <- sqlFetch(conDB, "tblNestVisits")

# select video made when provisioning chick (situation = 4 )			# this is the query that I used to select which excel files to be considered, but it appears that some files in 2005 and all files in 2009 dont have an entry in tblXlsFiles...
tblDVD_XlsFilesALLDBINFO <- sqlQuery(conDB, "
SELECT tblDVD_XlsFiles.DVDRef, tblDVD_XlsFiles.Filename, tblDVDInfo.BroodRef, tblDVDInfo.Situation, tblDVDInfo.Deaths, tblDVDInfo.OffspringNo, tblDVDInfo.Age, tblDVDInfo.Wrong, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.Weather, tblDVDInfo.Wind, tblDVDInfo.Notes, tblDVDInfo.TapeLength, zzz_OldtblParentalCare.EffectTime, zzz_OldtblParentalCare.Method, tblDVDInfo.Observer, zzz_OldtblParentalCare.MTime, zzz_OldtblParentalCare.FTime, zzz_OldtblParentalCare.ShareTime, zzz_OldtblParentalCare.MVisit1, zzz_OldtblParentalCare.FVisit1, zzz_OldtblParentalCare.MVisit2, zzz_OldtblParentalCare.FVisit2, zzz_OldtblParentalCare.MBout, zzz_OldtblParentalCare.FBout
FROM tblDVDInfo INNER JOIN (tblDVD_XlsFiles INNER JOIN zzz_OldtblParentalCare ON tblDVD_XlsFiles.DVDRef = zzz_OldtblParentalCare.DVDRef) ON (tblDVDInfo.DVDRef = zzz_OldtblParentalCare.DVDRef) AND (tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef)
WHERE (((tblDVDInfo.Situation)=4) AND ((tblDVDInfo.Wrong)=False));
")

# get the missing DVD Filenames (those in zzz_OldtblParentalCare but not in tblXlsFiles)
missingDVDFilenames <- sqlQuery(conDB, "
SELECT zzz_OldtblParentalCare.DVDRef, Year([DVDdate]) & '\\' & [DVDNumber] & '.xlsx' AS Filename, tblDVDInfo.BroodRef, tblDVDInfo.OffspringNo, tblDVDInfo.Age, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.Notes,tblDVDInfo.TapeLength, zzz_OldtblParentalCare.EffectTime
FROM tblDVDInfo INNER JOIN (zzz_OldtblParentalCare LEFT JOIN tblDVD_XlsFiles ON zzz_OldtblParentalCare.[DVDRef] = tblDVD_XlsFiles.[DVDRef]) ON tblDVDInfo.DVDRef = zzz_OldtblParentalCare.DVDRef
WHERE (((tblDVDInfo.TapeLength) Is Not Null) AND ((tblDVD_XlsFiles.DVDRef) Is Null) AND ((tblDVDInfo.Situation)=4) AND ((tblDVDInfo.Wrong)=No));
")

# get the rearing brood for all birds
RearingBrood_allBirds <- sqlQuery(conDB, "
SELECT tblBirdID.BirdID, tblBirdID.Cohort, IIf([FosterBrood] Is Null,[BroodRef],[FosterBrood]) AS RearingBrood, tblBirdID.DeathDate, tblBirdID.LastStage, tblBirdID.DeathStatus
FROM tblBirdID LEFT JOIN tblFosterBroods ON tblBirdID.BirdID = tblFosterBroods.BirdID
WHERE (((tblBirdID.BroodRef) Is Not Null));
")

close(conDB)
}

options(warn=2)	# when loop generate a error at one iteration, the loop stop, so one can call the filename and check what's wrong with it

}

head(tblDVD_XlsFiles)
head(tblBirdID)
head(tblParentalCare)
head(tblBroodEvents)
head(tblBroods)
head(tblAllCodes)
head(tblDVDInfo)
head(tblDVD_XlsFilesALLDBINFO)
head(missingDVDFilenames)
head(RearingBrood_allBirds)
head(sys_LastSeenAlive)
head(sunrise)


{### extract provisioning raw data from excel files

{### create list of filenames for New and Old Template: check those not included yet but that should be

{## create list of file names from files analysed from 2015 with Malika's protocol in Issie's template
	# not elegant as require that the DVDRef of the DB not to change
FilenamesMalikaTemplate <- sort(tblDVD_XlsFiles$Filename[tblDVD_XlsFiles$RowRef >= 3180 & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename ])
length(FilenamesMalikaTemplate) # 83 (20160322)

}


{## create list of file names from files analysed between 2012 and 2015 with Issie's protocol
	# not elegant as require that the DVDRef of the DB not to change
#Filenames1215 <- sort(tblDVD_XlsFiles$Filename[tblDVD_XlsFiles$DVDRef >=2933 & tblDVD_XlsFiles$DVDRef <=5146 & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename ])
Filenames1215 <- sort(tblDVD_XlsFiles$Filename[tblDVD_XlsFiles$DVDRef >=2933 & tblDVD_XlsFiles$RowRef <3180 & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename & tblDVD_XlsFiles$DVDRef != 5147]) # file 4001LM19 added as DVD5147 in march 2016

}								   
	
head(Filenames1215)

{##  create list of file names from files analysed in 2010 and 2011 with Issie's template

filename1011_oldtemplate <- c(
"2010\\VJ0039.xlsx", "2010\\VJ0040.xlsx", "2010\\VJ0041.xlsx", "2010\\VJ0044.xlsx", "2010\\VJ0050.xlsx", "2010\\VJ0052.xlsx",
"2010\\VJ0058.xlsx", "2010\\VJ0059.xlsx", "2010\\VJ0060.xlsx", "2010\\VJ0064.xlsx", "2010\\VJ0066.xlsx", "2010\\VJ0067.xlsx",
"2010\\VJ0068.xlsx", "2010\\VJ0070.xlsx", "2010\\VJ0078.xlsx", "2010\\VJ0079.xlsx", "2010\\VJ0080.xlsx", "2010\\VJ0081.xlsx",
"2011\\VK0001.xlsx", "2011\\VK0002.xlsx", "2011\\VK0003.xlsx", "2011\\VK0005.xlsx", "2011\\VK0006.xlsx", "2011\\VK0007.xlsx",
"2011\\VK0010.xlsx", "2011\\VK0011.xlsx", "2011\\VK0012.xlsx", "2011\\VK0013.xlsx", "2011\\VK0017.xlsx", "2011\\VK0019.xlsx", "2011\\VK0020.xlsx",
"2011\\VK0021.xlsx", "2011\\VK0022.xlsx", "2011\\VK0024.xlsx", "2011\\VK0025.xlsx", "2011\\VK0026.xlsx", "2011\\VK0027.xlsx", "2011\\VK0028.xlsx",
"2011\\VK0029.xlsx", "2011\\VK0031.xlsx", "2011\\VK0034.xlsx", "2011\\VK0037.xlsx", "2011\\VK0038.xlsx", "2011\\VK0039.xlsx", "2011\\VK0040.xlsx",
"2011\\VK0041.xlsx", "2011\\VK0042.xlsx", "2011\\VK0044.xlsx", "2011\\VK0045.xlsx", "2011\\VK0046.xlsx", "2011\\VK0047.xlsx", "2011\\VK0048.xlsx",
"2011\\VK0050.xlsx", "2011\\VK0051.xlsx", "2011\\VK0056.xlsx", "2011\\VK0061.xlsx", "2011\\VK0062.xlsx", "2011\\VK0063.xlsx", "2011\\VK0067.xlsx",
"2011\\VK0069.xlsx", "2011\\VK0070.xlsx", "2011\\VK0072.xlsx",
"2011\\VK0101.xlsx", "2011\\VK0102.xlsx", "2011\\VK0103.xlsx",
"2011\\VK0105.xlsx", "2011\\VK0106.xlsx",
"2011\\VK0410.xlsx", "2011\\VK0412.xlsx", "2011\\VK0413.xlsx", "2011\\VK0416.xlsx",
"2011\\VK0418.xlsx", "2011\\VK0419.xlsx", "2011\\VK0421.xlsx", "2011\\VK0422.xlsx", "2011\\VK0423.xlsx"
)


Filenames1011newtemplate <- tblDVD_XlsFiles$Filename[grepl("2010|2011", tblDVD_XlsFiles$Filename) == TRUE & !tblDVD_XlsFiles$Filename%in%filename1011_oldtemplate & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename]
													# Filenames that contained 2010 or 2011 				and that do not belong to the list above				where situation = 4 (only chicks)

}

head(Filenames1011newtemplate)													
		
{## combine all files analyzed with Issie's template (will take newly analyzed files only if those are put in the root of the year folder, with a normal file name)

FilenamesNewTemplate <- c(as.character(Filenames1011newtemplate), as.character(Filenames1215))
length(FilenamesNewTemplate)	# 915 files, situation 4, new template # 20160321 with addition AJ files with Issie's template: 935

}


{## combined all files analyzed with Shinichi's templates

{FilenamesOldTemplate <- tblDVD_XlsFiles$Filename[

# where situation = 4
tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename &

# years before 2010, or after 2010 but belonging to list created above
(tblDVD_XlsFiles$DVDRef <2016 | tblDVD_XlsFiles$DVDRef ==5147 | tblDVD_XlsFiles$Filename%in%filename1011_oldtemplate) & 

# EXCLUDED BUT WITH DATA IN DB (COULD BE INCLUDED if rewatched)
tblDVD_XlsFiles$Filename != "2004\\40055.xlsx" & # files that contain comments that are not standardized (data in DB)
tblDVD_XlsFiles$Filename != "2004\\40061.xlsx" & # files that contain comments that are not standardized (data in DB)
tblDVD_XlsFiles$Filename != "2008\\80055.xlsx" & # file empty (data in DB)
tblDVD_XlsFiles$Filename != "2005\\50268.xlsx"   # commented: too difficult to distinguish nale and female (and therefore file is empty, DB parental care = line of NA)
] 
}

which(duplicated(merge(x=data.frame(FilenamesOldTemplate), y=tblDVD_XlsFilesALLDBINFO[,c("DVDRef","Filename")], by.x= "FilenamesOldTemplate", by.y= "Filename",all.x=TRUE)[,"DVDRef"]))	# no duplicates of DVDRef

# add missing Filenames
FilenamesOldTemplate <- c(as.character(FilenamesOldTemplate),as.character(missingDVDFilenames$Filename[missingDVDFilenames$Filename != "2007\\70004.xlsx"]))  # excel file not found !

which(duplicated(missingDVDFilenames[,c("DVDRef","Filename")][,"DVDRef"]))	# no duplicates of DVDRef

length(FilenamesOldTemplate)	# 889 files, situation 4, old template # on 20160314: addition missing Filename > 1094 files


}


}

head(FilenamesNewTemplate)
head(FilenamesOldTemplate)
head(FilenamesMalikaTemplate)


require(openxlsx)
search() # make sure package 'xlsx' is not in the list

{### extraction data in Excel files analyzed with Malika's excel template and error checking

outMalika = list()
warninggz <- list()
warninggzz <- list()
	
for (j in 1:length(FilenamesMalikaTemplate)){
filenamej <- paste(pathdropboxfolder, FilenamesMalikaTemplate[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheet="DVD NO") # read.xlsx function from library 'openxlsx' (not library 'xlsx'): make sure xlsx is not in the list given by 'search()'

warninggz[[j]] <- as.character(FilenamesMalikaTemplate[j])
warninggzz[[j]] <- as.character(FilenamesMalikaTemplate[j])

{### warningz in raw data (only numbers and no blank)

# check if no missing Time or state
if ((length(b$Fend[!is.na(b$Fend)]) != length(b$Fstart[!is.na(b$Fstart)])) | (length(b$Mend[!is.na(b$Mend)]) != length(b$Mstart[!is.na(b$Mstart)])))
{warninggz[[j]] <- c(warninggz[[j]],"missing Time !")}	

# check if no comments in raw data
if (is.numeric(b$Fstart) == FALSE | is.numeric(b$Fend) == FALSE | is.numeric(b$Mstart) == FALSE | is.numeric(b$Mend) == FALSE)
{warninggz[[j]] <- c(warninggz[[j]],"character in raw data")}

# check if no state
if ((length(b$Fend[!is.na(b$Fend)]) != length(b$Fstate[!is.na(b$Fstate)])) | (length(b$Mend[!is.na(b$Mend)]) != length(b$Mstate[!is.na(b$Mstate)])))
{warninggz[[j]] <- c(warninggz[[j]],"missing state !")}	
}

{### if no warningz: extract Female and Male raw data separately
if (length(warninggz[[j]]) == 1)
{
bbF <- data.frame(b$Fstart, b$Fend, b$Fstate, 0, NA,NA)
colnames(bbF) <- c("Tstart", "Tend", "State", "Sex", "prevEnd","Diff_Tstart_prevEnd")
bbF <- bbF[!is.na(bbF$Tstart) | !is.na(bbF$Tend),]


bbM <- data.frame(b$Mstart, b$Mend, b$Mstate,1,NA,NA)
colnames(bbM) <- c("Tstart", "Tend",  "State", "Sex", "prevEnd","Diff_Tstart_prevEnd")
bbM <- bbM[!is.na(bbM$Tstart) | !is.na(bbM$Tend),]

if (nrow(bbF)>0)
{
bbF$prevEnd <- c(NA,bbF$Tend[-nrow(bbF)])
bbF$Diff_Tstart_prevEnd <- bbF$Tstart-bbF$prevEnd
}

if (nrow(bbM)>0)
{
bbM$prevEnd <- c(NA,bbM$Tend[-nrow(bbM)])
bbM$Diff_Tstart_prevEnd <- bbM$Tstart-bbM$prevEnd
}

}
}

{### warningzz in chronology



if ((nrow(bbF[bbF$Tend - bbF$Tstart <0,]) > 0) | (nrow(bbF[!is.na(bbF$Diff_Tstart_prevEnd) & bbF$Diff_Tstart_prevEnd <0,]) > 0) | 
	(nrow(bbM[bbM$Tend - bbM$Tstart <0,])) > 0 | (nrow(bbM[!is.na(bbM$Diff_Tstart_prevEnd) & bbM$Diff_Tstart_prevEnd <0,]) > 0))

	{warninggzz[[j]] <-c(warninggzz[[j]], "wrong chronology in female or male!")
	bbF <- NULL
	bbM <- NULL}



}

{## if no warningzz: create bb
if (length(warninggz[[j]])==1 & length(warninggzz[[j]])==1)
{
# when no bird ever visited: keep a line with NA
if (nrow(bbF)== 0  & nrow(bbM)== 0)	
{
bb <- data.frame(rbind(c(NA,NA,NA,NA,NA)))
colnames(bb) <- c('Tstart','Tend','State','Sex','Filename') # filename will be filled in later
}

# when only one bird  visited
if (nrow(bbF)!= 0  & nrow(bbM)== 0)
{
bb <- bbF[,c('Tstart','Tend','State','Sex')]
}

if (nrow(bbF)== 0  & nrow(bbM)!= 0)
{
bb <- bbM[,c('Tstart','Tend','State','Sex')]
}

# when both birds visited, combine both sex data and order by Tstart then Tend
if(nrow(bbF)!= 0 & nrow(bbM)!= 0)
{
bb <- rbind(bbF[,c('Tstart','Tend','State','Sex')], bbM[,c('Tstart','Tend','State','Sex')])
bb <- bb[with(bb,order(bb$Tstart, bb$Tend)),] 
 }
 

# add filename
bb$Filename <- as.character(FilenamesMalikaTemplate[j])
}

outMalika[[j]] <- bb
bb <-NULL
}

}

condout <- sapply(outMalika, function(x) length(x) > 1)
outMalika <- outMalika[condout]
length(outMalika)

combinedprovisioningMalikaTemplate = do.call(rbind, outMalika)

{# error check for Malika's Template
 
length(unique(combinedprovisioningMalikaTemplate$Filename))	# 83 files, situation 4, Malika's template

# weird comments or missing info
condwarninggz <- sapply(warninggz, function(x) length(x) > 1)
warninggz <- warninggz[condwarninggz]

warninggz	# should be empty list

# mistake in chronology
condwarninggzz <- sapply(warninggzz, function(x) length(x) > 1)
warninggzz <- warninggzz[condwarninggzz]

warninggzz	# should be empty list

}


}

head(combinedprovisioningMalikaTemplate)


{### extraction data in Excel files analyzed with Issie's excel template (after conversion all files to xlsx) and error checking

outIssie = list()
warninggz <- list()
warninggzz <- list()
	
for (j in 1:length(FilenamesNewTemplate)){
filenamej <- paste(pathdropboxfolder, FilenamesNewTemplate[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheet="DVD NO") # read.xlsx function from library 'openxlsx' (not library 'xlsx'): make sure xlsx is not in the list given by 'search()'

warninggz[[j]] <- as.character(FilenamesNewTemplate[j])
warninggzz[[j]] <- as.character(FilenamesNewTemplate[j])

{### warningz in raw data (only numbers and no blank)

# check if no missing Time
if ((length(b$F.out[!is.na(b$F.out)]) != length(b$F.in[!is.na(b$F.in)])) | (length(b$M.out[!is.na(b$M.out)]) != length(b$M.in[!is.na(b$M.in)])))
{warninggz[[j]] <- c(warninggz[[j]],"missing Time !")}	

# check if no comments in raw data
if (is.numeric(b$F.in) == FALSE | is.numeric(b$F.out) == FALSE | is.numeric(b$M.in) == FALSE | is.numeric(b$M.out) == FALSE)
{warninggz[[j]] <- c(warninggz[[j]],"character in raw data")}
}

{### if no warningz: extract Female and Male raw data separately
if (length(warninggz[[j]]) == 1)
{
bbF <- data.frame(b$F.in, b$F.out, 0, NA,NA)
colnames(bbF) <- c("Tin", "Tout", "Sex", "prevOut","Diff_Tin_prevOut")
bbF <- bbF[!is.na(bbF$Tin) | !is.na(bbF$Tout),]


bbM <- data.frame(b$M.in, b$M.out, 1,NA,NA)
colnames(bbM) <- c("Tin", "Tout", "Sex", "prevOut","Diff_Tin_prevOut")
bbM <- bbM[!is.na(bbM$Tin) | !is.na(bbM$Tout),]

if (nrow(bbF)>0)
{
bbF$prevOut <- c(NA,bbF$Tout[-nrow(bbF)])
bbF$Diff_Tin_prevOut <- bbF$Tin-bbF$prevOut
}

if (nrow(bbM)>0)
{
bbM$prevOut <- c(NA,bbM$Tout[-nrow(bbM)])
bbM$Diff_Tin_prevOut <- bbM$Tin-bbM$prevOut
}

}
}

{### warningzz in chronology



if ((nrow(bbF[bbF$Tout - bbF$Tin <0,]) > 0) | (nrow(bbF[!is.na(bbF$Diff_Tin_prevOut) & bbF$Diff_Tin_prevOut <0,]) > 0) | 
	(nrow(bbM[bbM$Tout - bbM$Tin <0,])) > 0 | (nrow(bbM[!is.na(bbM$Diff_Tin_prevOut) & bbM$Diff_Tin_prevOut <0,]) > 0))

	{warninggzz[[j]] <-c(warninggzz[[j]], "wrong chronology in female or male!")
	bbF <- NULL
	bbM <- NULL}



}

{## if no warningzz: create bb
if (length(warninggz[[j]])==1 & length(warninggzz[[j]])==1)
{
# when no bird ever visited: keep a line with NA
if (nrow(bbF)== 0  & nrow(bbM)== 0)	
{
bb <- data.frame(rbind(c(NA,NA,NA,NA)))
colnames(bb) <- c('Tin','Tout','Sex','Filename') # filename will be filled in later
}

# when only one bird  visited
if (nrow(bbF)!= 0  & nrow(bbM)== 0)
{
bb <- bbF[,c('Tin','Tout','Sex')]
}

if (nrow(bbF)== 0  & nrow(bbM)!= 0)
{
bb <- bbM[,c('Tin','Tout','Sex')]
}

# when both birds visited, combine both sex data and order by Tin then Tout
if(nrow(bbF)!= 0 & nrow(bbM)!= 0)
{
bb <- rbind(bbF[,c('Tin','Tout','Sex')], bbM[,c('Tin','Tout','Sex')])
bb <- bb[with(bb,order(bb$Tin, bb$Tout)),] 
 }
 

# add filename
bb$Filename <- as.character(FilenamesNewTemplate[j])
}

outIssie[[j]] <- bb
bb <-NULL
}

}

condout <- sapply(outIssie, function(x) length(x) > 1)
outIssie <- outIssie[condout]
length(outIssie)

combinedprovisioningNewTemplate = do.call(rbind, outIssie)

{# error check for NewTemplate
 
length(unique(combinedprovisioningNewTemplate$Filename))	# 858 files, situation 4, new template # 20162203: 935 files

# weird comments or missing info
condwarninggz <- sapply(warninggz, function(x) length(x) > 1)
warninggz <- warninggz[condwarninggz]

warninggz	# should be empty list

# mistake in chronology
condwarninggzz <- sapply(warninggzz, function(x) length(x) > 1)
warninggzz <- warninggzz[condwarninggzz]

warninggzz	# should be empty list

}


}

head(combinedprovisioningNewTemplate)


detach("package:openxlsx", unload=TRUE)
require(xlsx)
search()

{## extraction data in Excel files analyzed with Shinichi's old excel template (after conversion all files to xlsx) and creation of lists of errors

FUNcellColor <- function(x) {
	fg  <- x$getFillForegroundXSSFColor()
	rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
	rgb <- paste(rgb, collapse = "")
	return(rgb)
}	

colornames <- list(blue = "00ffff", grey = "c0c0c0") # 'O' blue = feeding from outside ; 'O' grey = hanging around the NB

outShinichi <- list()
warningz <- list()
warningzz <- list()

for (j in 1:length(FilenamesOldTemplate)){

filenamej <- paste(pathdropboxfolder, FilenamesOldTemplate[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheetIndex =2) # as data.frame

warningz[[j]] <- as.character(FilenamesOldTemplate[j])
warningzz[[j]] <- as.character(FilenamesOldTemplate[j])

{### warningz in comments

for (i in 1:nrow(b))
{
# check if bird IN at the end in TinCom
if ((!is.na(b$com[i]) & b$com[i] == "IN" & !is.na(b$F.in[i]) & b$F.in[i] != 0) | (!is.na(b$com.2[i]) & b$com.2[i] == "IN"& !is.na(b$M.in[i]) & b$M.in[i] != 0))
{warningz[[j]] <- c(warningz[[j]], "bird IN at end of video: please write Tout, move 'IN' into TouCom")}

# check if all coms are NA, S, G, O, IN
if ((!is.na(b$com[i]) & b$com[i]!= "S" & b$com[i]!= "IN")| 
	(!is.na(b$com.2[i]) & b$com.2[i]!= "S" & b$com.2[i]!= "IN"))
{warningz[[j]] <- c(warningz[[j]],"Tin has weird comments !")}

if ((!is.na(b$com.1[i]) & b$com.1[i]!= "S" & b$com.1[i]!= "O" & b$com.1[i]!= "G" & b$com.1[i]!= "IN")|
	(!is.na(b$com.3[i]) & b$com.3[i]!= "S" & b$com.3[i]!= "O" & b$com.3[i]!= "G" & b$com.3[i]!= "IN") ) 
{warningz[[j]] <- c(warningz[[j]],"Tout has weird comments !")}		

# check if no missing com in Tout
if ((!is.na(b$F.out[i]) & is.na(b$com.1[i])) | (!is.na(b$M.out[i]) & is.na(b$com.3[i])))
{warningz[[j]] <- c(warningz[[j]],"missing info in Tout com  !")}	

}

# check if starts with Tin
if (length(b$F.out[!is.na(b$F.out)]) >0)
{
	if (min(which(!is.na(b$F.out))) < min(which(!is.na(b$F.in)))) 
	{warningz[[j]] <- c(warningz[[j]],"file starts with Fout !")}
}

if (length(b$M.out[!is.na(b$M.out)]) >0)
{
	if (min(which(!is.na(b$M.out))) < min(which(!is.na(b$M.in)))) 
	{warningz[[j]] <- c(warningz[[j]],"file starts with Mout !")}
}


}

{### if no warningz: get the data for female and male separately
if (length(warningz[[j]])==1)
{
{### Females

bbF <- data.frame(b$F.in,b$com, b$F.out,b$com.1, NA, NA, 0, NA)
colnames(bbF) <- c("Tin", "TinCom", "Tout", "ToutCom", "prevOut", "Diff_Tin_prevOut", "Sex", "Com")
bbF <- bbF[!is.na(bbF$Tin) | !is.na(bbF$Tout),]
bbF$Tin <- na.locf(bbF$Tin,na.rm=FALSE)
bbF$Tout <- na.locf(bbF$Tout,na.rm=TRUE, fromLast = TRUE)

if (nrow(bbF)>0)
{
for (i in 1:nrow(bbF))
{
# accept comment O
if (!is.na(bbF$ToutCom[i]) & bbF$ToutCom[i] == "O")
{bbF$Com[i] <- "O"}

# change ToutCom from IN, S or G, into IN (some will be replaced in the next loop)
if (!is.na(bbF$ToutCom[i]) &( bbF$ToutCom[i] == "S" | bbF$ToutCom[i] == "G" | bbF$ToutCom[i] == "IN"))
{bbF$Com[i] <- "IN"}
}

}

if (nrow(bbF)>1)
{
for (i in 1:(nrow(bbF)-1))
{
#  add Tin when ToutCom==G directly following a TouCom==S
if (bbF$Tin[i] == bbF$Tin[i+1] & bbF$Tout[i] != bbF$Tout[i+1])
{bbF$Tin[i+1] <- bbF$Tout[i]
bbF$Com[i+1] <- "S"}

# add Tout when TinCom==S
if (bbF$Tin[i] != bbF$Tin[i+1] & bbF$Tout[i] == bbF$Tout[i+1])
{bbF$Tout[i] <- bbF$Tin[i+1]
bbF$Com[i] <- "S"}

# insert row when TouCom==S is followed by another Tin  rather than followed directly by a Toutcom==G)
if (!is.na(bbF$ToutCom[i]) & bbF$ToutCom[i] == "S" & bbF$Tout[i] != bbF$Tin[i+1])
{bbF <- rbind(bbF,c(bbF$Tout[i],NA,bbF$Tin[i+1],NA, NA,NA,0,"S"))}
}

}

if (nrow(bbF)>0)
{
bbF$Tin <- as.numeric(bbF$Tin)
bbF$Tout <- as.numeric(bbF$Tout)
bbF <- bbF[order(bbF$Tin, bbF$Tout),]
bbF <- bbF[!is.na(bbF$Com),]
bbF <- unique(bbF[,c("Tin","Tout", "Sex", "Com", "prevOut","Diff_Tin_prevOut")])

bbF$prevOut <- c(NA,bbF$Tout[-nrow(bbF)])
bbF$Diff_Tin_prevOut <- bbF$Tin-bbF$prevOut

}

}

{### Males

bbM <- data.frame(b$M.in,b$com.2, b$M.out,b$com.3, NA, NA, 1, NA)
colnames(bbM) <- c("Tin", "TinCom", "Tout", "ToutCom","prevOut", "Diff_Tin_prevOut", "Sex", "Com")
bbM <- bbM[!is.na(bbM$Tin) | !is.na(bbM$Tout),]
bbM$Tin <- na.locf(bbM$Tin,na.rm=FALSE)
bbM$Tout <- na.locf(bbM$Tout,na.rm=TRUE, fromLast = TRUE)


if (nrow(bbM)>0)
{
for (i in 1:nrow(bbM))
{
# accept comment O	############ AT THE MOMENT I DO NOT HAVE ITS COLOR !!!!!! ####################
if (!is.na(bbM$ToutCom[i]) & bbM$ToutCom[i] == "O")
{bbM$Com[i] <- "O"}

# change ToutCom from IN, S or G, into IN
if (!is.na(bbM$ToutCom[i]) &( bbM$ToutCom[i] == "S" | bbM$ToutCom[i] == "G" | bbM$ToutCom[i] == "IN"))
{bbM$Com[i] <- "IN"}
}
}

if (nrow(bbM)>1)
{
for (i in 1:(nrow(bbM)-1))
{
#  add Tin when ToutCom==G directly following a TouCom==S
if (bbM$Tin[i] == bbM$Tin[i+1] & bbM$Tout[i] != bbM$Tout[i+1])
{bbM$Tin[i+1] <- bbM$Tout[i]
bbM$Com[i+1] <- "S"}

# add Tout when TinCom==S
if (bbM$Tin[i] != bbM$Tin[i+1] & bbM$Tout[i] == bbM$Tout[i+1])
{bbM$Tout[i] <- bbM$Tin[i+1]
bbM$Com[i] <- "S"}

# insert row when TouCom==S is followed by another Tin  rather than followed directly by a Toutcom==G)
if (!is.na(bbM$ToutCom[i]) & bbM$ToutCom[i] == "S" & bbM$Tout[i] != bbM$Tin[i+1])
{bbM <- rbind(bbM,c(bbM$Tout[i],NA,bbM$Tin[i+1],NA,NA,NA,1,"S"))}
}
}

if (nrow(bbM)>0)
{
bbM$Tin <- as.numeric(bbM$Tin)
bbM$Tout <- as.numeric(bbM$Tout)
bbM <- bbM[order(bbM$Tin, bbM$Tout),]
bbM <- bbM[!is.na(bbM$Com),]
bbM <- unique(bbM[,c("Tin","Tout", "Sex", "Com","prevOut","Diff_Tin_prevOut")])

bbM$prevOut <- c(NA,bbM$Tout[-nrow(bbM)])
bbM$Diff_Tin_prevOut <- bbM$Tin-bbM$prevOut
}

}

{### warningzz in chronology

if ((nrow(bbF[bbF$Tout - bbF$Tin <0,]) > 0) | (nrow(bbF[!is.na(bbF$Diff_Tin_prevOut) & bbF$Diff_Tin_prevOut <0,]) > 0) | 
	(nrow(bbM[bbM$Tout - bbM$Tin <0,])) > 0 | (nrow(bbM[!is.na(bbM$Diff_Tin_prevOut) & bbM$Diff_Tin_prevOut <0,]) > 0))

	{warningzz[[j]] <-c(warningzz[[j]], "wrong chronology in female or male!")
	bbF <- NULL
	bbM <- NULL}



}

}

}

{### if no warningzz in chronology: extract color O visits and combine both female and male visits

if (length(warningz[[j]])==1 & length(warningzz[[j]])==1)
{

{## extract color for O visits 

# reload b as a workbook wb, which is a java object
wb <- loadWorkbook(filenamej)

{# Females > get OFColors if bbF not empty and with O comments 

if (nrow(bbF) != 0 & nrow(bbF[bbF$Com == "O",]) ==0)
{
bbF$Col <- NA
}

if (nrow(bbF[bbF$Com == "O",]) >0)
{
# in b, get index of cells where T.out has been commented O	
# Rk: if load workbook > header is index = 1 ; while with read.xlsx > header has no index but the 1st row of data has index = 1
# 					   > load workbook supress lines without data, which read.xlsx does not do, so index not matching if long excel files with blank lines (needs to be deleted)

FcellsToutCommentedO <- paste(which(!is.na(b$com.1) & b$com.1 == "O")+1, which(colnames(b)=="com.1")-1, sep=".")

# get cells with Tout commented O as java objects
OFcells <- getCells(getRows(getSheets(wb)[[2]]))[FcellsToutCommentedO]

# get style of these java objects
styleOFcells <-  sapply (OFcells, getCellStyle)

# get color out of the style of those java objects
RGBcolorOFcells <- sapply(styleOFcells, FUNcellColor)
matchOF <- match(sapply(styleOFcells, FUNcellColor), colornames)
namecolorOFcells  <- data.frame(names(colornames)[matchOF])

# create data.frame with list of cell indexes, values, colors
valueOFcells <- data.frame(sapply (OFcells, getCellValue))
OFColors <- cbind(FcellsToutCommentedO,valueOFcells,namecolorOFcells,0 )
colnames(OFColors) <- c("index","Tout","Col","Sex")

# merge it to bbF
bbF <- merge(x=bbF, y=OFColors[,c("Tout","Col")], by="Tout", all.x=TRUE)
}

}

{# Males > get OMColors if bbM not empty and with O comments 

if (nrow(bbM) != 0 & nrow(bbM[bbM$Com == "O",]) ==0)
{
bbM$Col <- NA
}

if (nrow(bbM[bbM$Com == "O",]) >0)
{
# in b, get index of cells where T.out has been commented O	
# Rk: if load workbook > header is index = 1 ; while with read.xlsx > header has no index but the 1st row of data has index = 1
# 					   > load workbook supress lines without data, which read.xlsx does not do, so index not matching if long excel files with blank lines (needs to be deleted)

McellsToutCommentedO <- paste(which(!is.na(b$com.3) & b$com.3 == "O")+1, which(colnames(b)=="com.3")-1, sep=".")

# get cells with Tout commented O as java objects
OMcells <- getCells(getRows(getSheets(wb)[[2]]))[McellsToutCommentedO]

# get style of these java objects
styleOMcells <-  sapply (OMcells, getCellStyle)

# get color out of the style of those java objects
RGBcolorOMcells <- sapply(styleOMcells, FUNcellColor)
matchOM <- match(sapply(styleOMcells, FUNcellColor), colornames)
namecolorOMcells  <- data.frame(names(colornames)[matchOM])

# create data.frame wtih list of cell index, values, color
valueOMcells <- data.frame(sapply (OMcells, getCellValue))
OMColors <- cbind(McellsToutCommentedO,valueOMcells,namecolorOMcells,1 )
colnames(OMColors) <- c("index","Tout","Col","Sex")

# merge it to bbM
bbM <- merge(x=bbM, y=OMColors[,c("Tout","Col")], by="Tout", all.x=TRUE)
}

}

 }
 
{## create bb

# when no bird ever visited: keep a line with NA
if (nrow(bbF)== 0  & nrow(bbM)== 0)	
{
bb <- data.frame(rbind(c(NA,NA,NA,NA,NA,NA)))
colnames(bb) <- c('Tin','Tout','Sex','Com','Col','Filename') # filename will be filled in later
}

# when only one bird  visited
if (nrow(bbF)!= 0  & nrow(bbM)== 0)
{
bb <- bbF[,c('Tin','Tout','Sex','Com','Col')]
}

if (nrow(bbF)== 0  & nrow(bbM)!= 0)
{
bb <- bbM[,c('Tin','Tout','Sex','Com','Col')]
}

# when both birds visited, combine both sex data and order by Tin then Tout
if(nrow(bbF)!= 0 & nrow(bbM)!= 0)
{
bb <- rbind(bbF[,c('Tin','Tout','Sex','Com','Col')], bbM[,c('Tin','Tout','Sex','Com','Col')])
bb <- bb[with(bb,order(bb$Tin, bb$Tout)),] 
 }
 

# add filename
bb$Filename <- as.character(FilenamesOldTemplate[j])
}
}

outShinichi[[j]] <- bb
bb <- NULL

}


}

condout <- sapply(outShinichi, function(x) length(x) > 1)
outShinichi <- outShinichi[condout]
length(outShinichi)

combinedprovisioningOldTemplate = do.call(rbind, outShinichi)


{# error check for OldTemplate
 
length(unique(combinedprovisioningOldTemplate$Filename))	# 15/02/2016: 885 files, situation 4, old template 		17/03/2016: 1094

# weird comments or missing info
condwarningz <- sapply(warningz, function(x) length(x) > 1)
warningz <- warningz[condwarningz]

warningz	# should be empty list

# mistake in chronology
condwarningzz <- sapply(warningzz, function(x) length(x) > 1)
warningzz <- warningzz[condwarningzz]

warningzz	# should be empty list

# check for unknown colors for 'O' visits (blue = feeding from the ouside - grey: hanging out around the NB)
combinedprovisioningOldTemplate[combinedprovisioningOldTemplate$Com == "O" & is.na(combinedprovisioningOldTemplate$Col),] # should be NAs
unique(combinedprovisioningOldTemplate$Filename[combinedprovisioningOldTemplate$Com == "O" & is.na(combinedprovisioningOldTemplate$Col)]) # should be NA

}


}

head(combinedprovisioningOldTemplate)


{### combine all data

head(combinedprovisioningOldTemplate)
head(combinedprovisioningNewTemplate)
head(combinedprovisioningMalikaTemplate)

## Protocol
combinedprovisioningNewTemplate$Protocol <- "Issie"
combinedprovisioningOldTemplate$Protocol <- "Shinichi"
combinedprovisioningMalikaTemplate$Protocol <- "Malika"

## States
combinedprovisioningNewTemplate$State[!is.na(combinedprovisioningNewTemplate$Tin)] <- 'INorOF'
combinedprovisioningNewTemplate[is.na(combinedprovisioningNewTemplate$State),] # empty files

for (i in 1:nrow(combinedprovisioningOldTemplate))
{
if (is.na(combinedprovisioningOldTemplate$Com[i]))
{combinedprovisioningOldTemplate$State[i] <- NA}

if (!is.na(combinedprovisioningOldTemplate$Com[i]) & combinedprovisioningOldTemplate$Com[i] == 'O' &  !is.na(combinedprovisioningOldTemplate$Col[i]) & combinedprovisioningOldTemplate$Col[i] == 'blue')
{combinedprovisioningOldTemplate$State[i] <- 'OF'}

if ((!is.na(combinedprovisioningOldTemplate$Com[i]) & combinedprovisioningOldTemplate$Com[i] == 'O' &  !is.na(combinedprovisioningOldTemplate$Col[i]) & combinedprovisioningOldTemplate$Col[i] == 'grey') 
	| (!is.na(combinedprovisioningOldTemplate$Com[i]) & combinedprovisioningOldTemplate$Com[i] == 'S'))
{combinedprovisioningOldTemplate$State[i] <- 'A'}

if (!is.na(combinedprovisioningOldTemplate$Com[i]) & combinedprovisioningOldTemplate$Com[i] == 'IN')
{combinedprovisioningOldTemplate$State[i] <- 'IN'}

}


## Times
names(combinedprovisioningNewTemplate)[names(combinedprovisioningNewTemplate) == 'Tin'] <- 'Tstart'
names(combinedprovisioningNewTemplate)[names(combinedprovisioningNewTemplate) == 'Tout'] <- 'Tend'
names(combinedprovisioningOldTemplate)[names(combinedprovisioningOldTemplate) == 'Tin'] <- 'Tstart'
names(combinedprovisioningOldTemplate)[names(combinedprovisioningOldTemplate) == 'Tout'] <- 'Tend'

## Combine ALL
combinedprovisioningALL <- rbind(combinedprovisioningOldTemplate[!names(combinedprovisioningOldTemplate) %in% c("Col", "Com")], combinedprovisioningNewTemplate)
combinedprovisioningALL <- rbind(combinedprovisioningMalikaTemplate,combinedprovisioningALL )

## Add DVDRef
combinedprovisioningALL <- merge(x=combinedprovisioningALL, y=tblDVD_XlsFilesALLDBINFO[,c('Filename','DVDRef')], all.x=TRUE, by='Filename')

## add DVD Ref for thos missing files in tblDVD_XlsFile
for (i in 1:nrow(combinedprovisioningALL))
{
if (combinedprovisioningALL$Filename[i] %in% missingDVDFilenames$Filename)
{combinedprovisioningALL$DVDRef[i] <- missingDVDFilenames$DVDRef[missingDVDFilenames$Filename ==combinedprovisioningALL$Filename[i]]}
}


combinedprovisioningALLforDB <- combinedprovisioningALL[,c('DVDRef','Tstart', 'Tend','State', 'Sex', 'Protocol')]


## write.table(combinedprovisioningALLforDB, file = "R_RawAllVisits_forDB.xls", col.names=TRUE, sep='\t') # 20160322
# after running the line above:
# I save the xls file into a xlsx file
# shift the headers one cell right
# rename the first column 'order' 
# > not elegant but write.xlsx from openxlsx isn't working for me
}

}

head(combinedprovisioningALLforDB)
head(combinedprovisioningALL)


{### recreate tblParentalCare to check for discrepancies - some variables also used for 'MY_tblParentalCare'

{## forseen discrepancies:
# minor changes we've made in chronology (MTime, FTime) and in color (#visits1 and 2, MTime, FTime) but normally not so much from changes in letters 'G', 'O', 'S'
# different calculations of MTime and sharedTime (I have seen files where MTime = sum MTime-0.5*ShareTime)
# Time 'IN' when bird 'IN' at the beginning or end of the files

# in intermediate template (Issies templae, Malika's protocol):
# MTime and FTime are extracted from Mbout and Fbout (because it was the case in Issies template, because she did not disentangle IN from OF, and all were assumed 'IN')
# but in Malika's intermediate template, bout time only consider when bird 'IN' > so MTime and FTime in DB are wrong
# the Numbers of bout Mbout and Fbout are, on the contrary, right in the DB and not in the code > only consider when 'IN' makes more sense
# in the code need to consider just one visit when succession OF-IN, and no visit2 when succession A-IN rather than A alone.
}

{## definitions columns in DB tblParentalCare from what I can get:
# MTime / FTime = duration in NB (or, for Issie's Template, feeding outside the nest box) for visits longer than 1 min. 
# > I believe this was initially to have an idea of brooding, but as Issie's template does not distinguish feeding from outside from being in the nest box, the measure does not make sense anymore
# ShareTime = duration of double attendance in the NB (or feeding from the outside of the NB in Issie's Template)
# MVisit1/FVisit1 = # feeding visits including those < 1 min
# MVisit2/FVisit2 = # non feeding visits (not reported in Issie's Template)
# MBout/FBout = # feeding visits > 1 min (for Issie's Template, include when feeding outside the nest box)

}


{## add FeedYN and duration state to combinedprovisioningALL
combinedprovisioningALL$FeedYN <- NA

for (i in 1: nrow(combinedprovisioningALL)) {
if (!is.na(combinedprovisioningALL$State[i]))
	{
	if (combinedprovisioningALL$State[i] == 'IN' | (combinedprovisioningALL$State[i] == 'OF' | combinedprovisioningALL$State[i] == 'INorOF'))
	{combinedprovisioningALL$FeedYN[i] <- 1}
	if (combinedprovisioningALL$State[i] == 'A')
	{combinedprovisioningALL$FeedYN[i] <- 0}
	}
}

combinedprovisioningALL$Duration <- combinedprovisioningALL$Tend-combinedprovisioningALL$Tstart

}

tail(combinedprovisioningALL)
head(tblParentalCare)


{# To calculate duration of feeding visits
combinedprovisioningALL_listperFilename <- split(combinedprovisioningALL,combinedprovisioningALL$Filename)

combinedprovisioningALL_listperFilename_fun = function(x)  {
x <- x[order(x$Tstart, -x$Tend),]

return(c(
sum(x$Duration[x$Sex==1 & x$FeedYN == 1 & x$Duration > 1]),  					# MTime
sum(x$Duration[x$Sex==0 & x$FeedYN == 1 & x$Duration > 1]),  					# FTime
length(x$FeedYN[x$Sex==1 & x$FeedYN == 1 & x$Duration > 1 & !is.na(x$FeedYN)]),	# MBout
length(x$FeedYN[x$Sex==0 & x$FeedYN == 1 & x$Duration > 1 & !is.na(x$FeedYN)]))	# FBout
)


}

combinedprovisioningALL_listperFilename_out1 <- lapply(combinedprovisioningALL_listperFilename, FUN=combinedprovisioningALL_listperFilename_fun)
combinedprovisioningALL_listperFilename_out2 <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilename_out1)),do.call(rbind, combinedprovisioningALL_listperFilename_out1))

nrow(combinedprovisioningALL_listperFilename_out2)
rownames(combinedprovisioningALL_listperFilename_out2) <- NULL
colnames(combinedprovisioningALL_listperFilename_out2) <- c('Filename','MTime', 'FTime', 'MBout', 'FBout')
}

head(combinedprovisioningALL_listperFilename_out2)


{# To calculate number of visits (feeding visits (1) and non feeding visits (2) )
combinedprovisioningALL_listperFilenameperSex0 <- split(combinedprovisioningALL[combinedprovisioningALL$Sex == 0,], combinedprovisioningALL$Filename[combinedprovisioningALL$Sex == 0])
combinedprovisioningALL_listperFilenameperSex1 <- split(combinedprovisioningALL[combinedprovisioningALL$Sex == 1,], combinedprovisioningALL$Filename[combinedprovisioningALL$Sex == 1])

	# x <- combinedprovisioningALL_listperFilenameperSex0[['2015\\VO0170.xlsx']]
	# x <- combinedprovisioningALL_listperFilenameperSex0[[1543]]
	# x <- combinedprovisioningALL_listperFilenameperSex0[['2013\\VM0540.xlsx']]
	
combinedprovisioningALL_listperFilenameperSex_fun = function(x)  {
x <- x[order(x$Tstart, -x$Tend),]

x$NextTime <-  c(x$Tstart[-1],NA)
x$NextTimeSame <-  x$Tend == x$NextTime

x$PrevTime <-  c(NA, x$Tend[-nrow(x)])
x$PrevTimeSame <-  x$Tstart == x$PrevTime

x$NextState <- c(as.character(x$State[-1]),NA)

x$Visit1 <- 0
x$Visit2 <- 0


x$Visit2 <- (x$State == 'A' & 
			(is.na(x$NextTimeSame) | (!is.na(x$NextTimeSame) & x$NextTimeSame == 'FALSE')) & 
			(is.na(x$PrevTimeSame) | (!is.na(x$PrevTimeSame) & x$PrevTimeSame == 'FALSE'))) == TRUE # when all conditions are met, i.e. 'A' T start and Tend are both different from state above (if exist) or below (if exist) > this remove 'S' time in shinishi's protocol for when arrive and stay before entering, or leave the NB but stick around, also remove those specific 'A' time in Malik's protocol

		
x$Visit1 <- ((x$State == 'INorOF') | (x$State == 'IN') 
				|
				((x$State == 'OF') & (is.na(x$NextTimeSame) | (!is.na(x$NextTimeSame) & x$NextTimeSame == 'FALSE' )))
				|
				((x$State == 'OF') & (!is.na(x$NextTimeSame) & x$NextTimeSame == 'TRUE') & (!is.na(x$NextState) & x$NextState != 'IN' ))) == TRUE
				

return(c(
length(x$Visit1[!is.na(x$Visit1) & x$Visit1 == TRUE]),	# Visit1
length(x$Visit1[!is.na(x$Visit2) & x$Visit2 == TRUE])	# Visit2
))

}

combinedprovisioningALL_listperFilenameperSex0_out1 <- lapply(combinedprovisioningALL_listperFilenameperSex0, FUN=combinedprovisioningALL_listperFilenameperSex_fun)
combinedprovisioningALL_listperFilenameperSex0_out2 <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilenameperSex0_out1)),do.call(rbind, combinedprovisioningALL_listperFilenameperSex0_out1))

nrow(combinedprovisioningALL_listperFilenameperSex0_out2)
rownames(combinedprovisioningALL_listperFilenameperSex0_out2) <- NULL
colnames(combinedprovisioningALL_listperFilenameperSex0_out2) <- c('Filename','FVisit1', 'FVisit2')

combinedprovisioningALL_listperFilenameperSex1_out1 <- lapply(combinedprovisioningALL_listperFilenameperSex1, FUN=combinedprovisioningALL_listperFilenameperSex_fun)
combinedprovisioningALL_listperFilenameperSex1_out2 <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilenameperSex1_out1)),do.call(rbind, combinedprovisioningALL_listperFilenameperSex1_out1))

nrow(combinedprovisioningALL_listperFilenameperSex1_out2)
rownames(combinedprovisioningALL_listperFilenameperSex1_out2) <- NULL
colnames(combinedprovisioningALL_listperFilenameperSex1_out2) <- c('Filename','MVisit1','MVisit2')

combinedprovisioningALL_listperFilenameperSex_out2 <- merge (x= combinedprovisioningALL_listperFilenameperSex0_out2, y = combinedprovisioningALL_listperFilenameperSex1_out2, all.x =TRUE, all.y = TRUE, by='Filename')
combinedprovisioningALL_listperFilenameperSex_out2[is.na(combinedprovisioningALL_listperFilenameperSex_out2)] <- 0

# write.table(combinedprovisioningALL_listperFilenameperSex_out2, file = "R_Compare_tblParentalCareNewVisitNbCalculation.xls", col.names=TRUE, sep='\t') # 20160331

}

head(combinedprovisioningALL_listperFilenameperSex_out2) # files with no visits by either sex have been removed...
 

{# To calculate ShareTime

{# calculate share time for each visit (for both sexes separately) in the raw data

combinedprovisioningALL_listperFilenameFeedY <- split(combinedprovisioningALL[combinedprovisioningALL$FeedYN == 1,],combinedprovisioningALL$Filename[combinedprovisioningALL$FeedYN == 1])
	# x <- combinedprovisioningALL_listperFilenameFeedY[['2014\\VN0585.xlsx']] # example where 1 male visit nested within female visit
	# x <- combinedprovisioningALL_listperFilenameFeedY[['2007\\70124.xlsx']]  # example where no male visit
	# x <- combinedprovisioningALL_listperFilenameFeedY[['2004\\40369.xlsx']]  # example where several nested visits
	# x <- combinedprovisioningALL_listperFilenameFeedY[['2004\\40034.xlsx']]  # example where one visit per sex
	# x <- combinedprovisioningALL_listperFilenameFeedY[['2005\\50208.xlsx']]  # example where length one vector is equal to second vector, and therefore data frame is created instead of list


out4 <- list()

for (j in 1:length(combinedprovisioningALL_listperFilenameFeedY) )		{ 
		x <-combinedprovisioningALL_listperFilenameFeedY[[j]]
		
		# Modified from Lotte Schlicht
		#1. find the times when males/females are present at the box
		#1.a subset to each sex
		x0 = subset(x, Sex == 0)
		x1 = subset(x, Sex == 1)
		
		#1.b create vector of times present (the *10 and then /10 are the easiest way to construct thenths of minutes)
			# remove the last tenth of second for each visits, unless the visit entry and exit has the same time
			# write the output in a list in case only one visit or several visits of equal length of decimals (in those cases, mapply creates data.frame instead of lists!)
		
		sex0_presence = mapply(FUN = function(Tstart, Tend) {  if (Tstart==Tend) {return (Tstart)} 
		if (Tstart!=Tend)	{return(list(((Tstart*10) : (Tend*10-1))/10))}}, Tstart = x0$Tstart, Tend = x0$Tend)
			
		sex1_presence = mapply(FUN = function(Tstart, Tend) {  if (Tstart==Tend) {return (Tstart*10)} 
		if (Tstart!=Tend)	{return(list(((Tstart*10) : (Tend*10-1))/10))}}, Tstart = x1$Tstart, Tend = x1$Tend)

		
		#2. check for each list entry of each sex (= each row in the original table) how many of the numbers also occur for the other sex
		# this gives you the number of tenths-of-minutes that both birds were inside the box.
		# for when just one visit per sex
		
		sex0 = lapply(sex0_presence, FUN = function(x1, x2) { 
					return( length(which(x1 %in% x2)) ) # seem to take each list of sex0_prsence as x1 length(which(sex0_presence[[2]] %in% unlist(sex1_presence)))
				}, 
				x2 = unlist(sex1_presence)
			)
						
		sex1 = lapply(sex1_presence, FUN = function(x1, x2) { 
					return( length(which(x1 %in% x2)) ) 

				}, 
				x2 = unlist(sex0_presence)
			)

			
		#4. attach to original data - in order to make sure that the order remains intact, add the columns to the sex-specific datasets and then rbind these together.
		x0$ShareTime = unlist(sex0)
		x1$ShareTime = unlist(sex1)
		x = as.data.frame(rbind(x0, x1))

		#5. devide by 10 to get the unit "minutes"
		x$ShareTime = x$ShareTime/10
		
		x <- x[order(x$Tstart, -x$Tend),]
		out4[[j]] <- x
		
		#6. clean up
		sex0_presence <- NULL
		sex1_presence <- NULL
		sex0 <- NULL
		sex1 <- NULL
		x0 <- NULL		
		x1 <- NULL
		x <- NULL
}

combinedprovisioningALL_FeedY <- do.call(rbind, out4)
}

{# calculate share time for each file

combinedprovisioningALL_listperFilenameFeedY_withShare <- split(combinedprovisioningALL_FeedY,combinedprovisioningALL_FeedY$Filename)

combinedprovisioningALL_listperFilenameFeedY_fun = function(x)  {
		
return(c(sum(x$ShareTime, na.rm=T)/2))					# ShareTime
		# sum(x$ShareTime[x$Sex == 0], na.rm=T),		# FShareTime is the same
		# sum(x$ShareTime[x$Sex == 1], na.rm=T)))		# MShareTime is the same
}


combinedprovisioningALL_listperFilenameFeedY_out1 <- lapply(combinedprovisioningALL_listperFilenameFeedY_withShare, FUN=combinedprovisioningALL_listperFilenameFeedY_fun)
combinedprovisioningALL_listperFilenameFeedY_out2 <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilenameFeedY_out1)),do.call(rbind, combinedprovisioningALL_listperFilenameFeedY_out1))

nrow(combinedprovisioningALL_listperFilenameFeedY_out2)	# 1734
rownames(combinedprovisioningALL_listperFilenameFeedY_out2) <- NULL
colnames(combinedprovisioningALL_listperFilenameFeedY_out2) <- c('Filename','ShareTime')
}

}

head(combinedprovisioningALL_listperFilenameFeedY_out2) # files with no feeding visits by either sex have been removed... 


{# create MY_tblParentalCareforComparison and Compare_tblParentalCare
MY_tblParentalCareforComparison <- merge(x=combinedprovisioningALL_listperFilename_out2,y=combinedprovisioningALL_listperFilenameperSex_out2,all.x=TRUE, by='Filename')# files with no visits by either sex are back !
MY_tblParentalCareforComparison <- merge(x=MY_tblParentalCareforComparison,y=combinedprovisioningALL_listperFilenameFeedY_out2,all.x=TRUE, by='Filename')# files with no feeding visits by either sex are back !
MY_tblParentalCareforComparison <- merge(x=MY_tblParentalCareforComparison,y=unique(combinedprovisioningALL[,c('Filename','DVDRef')]),all.x=TRUE, by='Filename')
MY_tblParentalCareforComparison <- MY_tblParentalCareforComparison[,c('Filename','DVDRef','MTime', 'FTime','MVisit1', 'FVisit1', 'MVisit2', 'FVisit2', 'MBout', 'FBout', 'ShareTime')] # just to reorder columns

Compare_tblParentalCare <- merge(x=MY_tblParentalCareforComparison,y=tblParentalCare[,c('DVDRef','MTime', 'FTime','MVisit1', 'FVisit1', 'MVisit2', 'FVisit2', 'MBout', 'FBout', 'ShareTime')], all.x=TRUE, by ='DVDRef')
head(Compare_tblParentalCare)

S <- Compare_tblParentalCare[,grepl("*\\.x$",names(Compare_tblParentalCare))] - Compare_tblParentalCare[,grepl("*\\.y$",names(Compare_tblParentalCare))]
Compare_tblParentalCare <- cbind(Compare_tblParentalCare[,c(1,2),drop=FALSE],S)
Compare_tblParentalCare$MTime.x <- round(Compare_tblParentalCare$MTime.x,2)
Compare_tblParentalCare$FTime.x <- round(Compare_tblParentalCare$FTime.x,2)
Compare_tblParentalCare$ShareTime.x <- round(Compare_tblParentalCare$ShareTime.x,2)

	# hist(Compare_tblParentalCare$MTime.x)
	# hist(Compare_tblParentalCare$FTime.x)
	# hist(Compare_tblParentalCare$MVisit1.x)
	# hist(Compare_tblParentalCare$FVisit1.x)
	# hist(Compare_tblParentalCare$MVisit2.x)
	# hist(Compare_tblParentalCare$FVisit2.x)
	# hist(Compare_tblParentalCare$MBout.x)
	# hist(Compare_tblParentalCare$FBout.x)
	# hist(Compare_tblParentalCare$ShareTime.x)
}

head(MY_tblParentalCareforComparison)
head(Compare_tblParentalCare)


{## checking the largest mismatches

## write.table(Compare_tblParentalCare, file = "R_Compare_tblParentalCareNewVisitNbCalculation.xls", col.names=TRUE, sep='\t') # 20160217 20160323

{# Andrews files with Malika's protocol

# 5031 > file had been corrected explaining the discrepancy, both code and excel files are correct now, onlt tblParentalCare in DB is wrong
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 5031,]
MY_tblParentalCareforComparison[!is.na(MY_tblParentalCareforComparison$DVDRef ) & MY_tblParentalCareforComparison$DVDRef == 5031,]
tblParentalCare[tblParentalCare$DVDRef == 5031,]

# 4977 > discrepancy between bout with OF (included in code) or not (not in excel file with Malika's protocol) > tblParentalCare makes more sense like this, but not consistent with Issies calculation (includes OF). Will be removed anyways from 'MY-tblParentalCare'
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 4977,]
MY_tblParentalCareforComparison[!is.na(MY_tblParentalCareforComparison$DVDRef ) & MY_tblParentalCareforComparison$DVDRef == 4977,]
tblParentalCare[tblParentalCare$DVDRef == 4977,]

# 5044 > share time in code > only if feeding, in Andrews files, also when present around "A", so he often has more shared time
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 5044,]
MY_tblParentalCareforComparison[!is.na(MY_tblParentalCareforComparison$DVDRef ) & MY_tblParentalCareforComparison$DVDRef == 5044,]
tblParentalCare[tblParentalCare$DVDRef == 5044,]

# 4847 > not from Andrew, the guy missed some shared time
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 4847,]
MY_tblParentalCareforComparison[!is.na(MY_tblParentalCareforComparison$DVDRef ) & MY_tblParentalCareforComparison$DVDRef == 4847,]
tblParentalCare[tblParentalCare$DVDRef == 4847,]
}

{# before Malika's protocol imported - 30 cases

# When Nb of visits were calculated with this code:
# length(x$FeedYN[x$Sex==1 & x$FeedYN == 1 & !is.na(x$FeedYN)]),  				# MVisit1
# length(x$FeedYN[x$Sex==0 & x$FeedYN == 1 & !is.na(x$FeedYN)]),					# FVisit1


# VK0115 > my code is correct, data in DB for a file where no bird visits
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2622,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2622,]
tblParentalCare[tblParentalCare$DVDRef == 2622,]

# VK0101 > my code is correct, Haslina exchange visit1 and 2
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2606,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2606,]
tblParentalCare[tblParentalCare$DVDRef == 2606,]

# VN0826 > my code is correct, data have change dramatically after correction of chronology
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 4786,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 4786,]
tblParentalCare[tblParentalCare$DVDRef == 4786,]

# 50412 > my code is correct, the observer did not fill in Visits2
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 973,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 973,]
tblParentalCare[tblParentalCare$DVDRef == 973,]

# 50201 > my code is correct, data in DB do not match what's written in excel for MVisits2 (which is correct)
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 759,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 759,]
tblParentalCare[tblParentalCare$DVDRef == 759,]

# VK0002 > my code is correct, summary done by hand in excel is wrong
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2346,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2346,]
tblParentalCare[tblParentalCare$DVDRef == 2346,]

# 50177 > my code is correct, colors in excel files wrong (blue instead of green for long visits) leading the observer to fail to counting manually the bouts
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 735,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 735,]
tblParentalCare[tblParentalCare$DVDRef == 735,]

# 80005 > my code is correct, formula in excel fail to include all the relevant cells to sum
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 1948,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 1948,]
tblParentalCare[tblParentalCare$DVDRef == 1948,]

# VM0628 > my code is correct, excel formula did not reach that line
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 3967,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 3967,]
tblParentalCare[tblParentalCare$DVDRef == 3967,]

# VK0410 > my code is correct, summary times were misfilled by hand by the observer, + we made a few corrections for colors
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2918,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2918,]
tblParentalCare[tblParentalCare$DVDRef == 2918,]

# 60161 > my code is correct, observer did not include short feeding visits in his count
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 1732,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 1732,]
tblParentalCare[tblParentalCare$DVDRef == 1732,]

# 50255 > my code is correct, observer forgot to color a visit and to count it as visit 1 + observer included visit 2 in the count of visit 1
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 813,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 813,]
tblParentalCare[tblParentalCare$DVDRef == 813,]

# 50580 > my code is correct, observer has a weird function to calculate Mtime (i.e. minus half the shared time), corrected chronology, Visits2 in excel files are correct, data in DB are different and incorrect
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 1137,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 1137,]
tblParentalCare[tblParentalCare$DVDRef == 1137,]

# VK0413 > my code is correct, summary in excel call wrong cell
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2921,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2921,]
tblParentalCare[tblParentalCare$DVDRef == 2921,]

# VK0412 > my code is correct, summary in excel filled in by hand wrongly
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2920,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2920,]
tblParentalCare[tblParentalCare$DVDRef == 2920,]

# VK0422 > my code is correct, summary in excel filled in by hand wrongly and calling wrong cell if function
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2930,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2930,]
tblParentalCare[tblParentalCare$DVDRef == 2930,]

# 40148 > my code is correct, summary in excel filled in by hand wrongly
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 148,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 148,]
tblParentalCare[tblParentalCare$DVDRef == 148,]

# VK0106 > my code is correct, summary in excel filled in by hand wrongly
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2613,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2613,]
tblParentalCare[tblParentalCare$DVDRef == 2613,]

# 70167 > my code is correct, shared time not filled in by observer
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 1925,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 1925,]
tblParentalCare[tblParentalCare$DVDRef == 1925,]

# VM0574 > my code is correct, shared time missed by observer, in fact, wrong time in (checked video as it looked suspicious...)
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 3913,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 3913,]
tblParentalCare[tblParentalCare$DVDRef == 3913,]

# VN0585 > my code is NOW correct for ShareTime
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 4544,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 4544,]
tblParentalCare[tblParentalCare$DVDRef == 4544,]

# VN0622 > my code is correct, observer forgot a shared time
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 4582,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 4582,]
tblParentalCare[tblParentalCare$DVDRef == 4582,]

# 50179 > my code is ~ correct: Oblue is counted as a 'bout' if longer than 1 minute so I guess this is not what this measurement was meant for (potentially to control for incuabtion/brooding??) + observer forgot to calculate any share time
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 737,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 737,]
tblParentalCare[tblParentalCare$DVDRef == 737,]

# 70114 > my code is correct, observer did not calculate shared time and missed a green bout (color is blue but should be green)
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 1872,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 1872,]
tblParentalCare[tblParentalCare$DVDRef == 1872,]

# VM0242 > my code is correct, observer wrote a typo in one shared time
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 3574,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 3574,]
tblParentalCare[tblParentalCare$DVDRef == 3574,]

# VK0229 > my code is correct, due to a correction in chronology, Ftime, and share time have changed
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 2739,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 2739,]
tblParentalCare[tblParentalCare$DVDRef == 2739,]

# VM0339 > my code is correct, mismatch due to a correction in chronology (big typo)
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 3665,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 3665,]
tblParentalCare[tblParentalCare$DVDRef == 3665,]

# 60004 > my code is correct, observer did wrong calculation share time
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 1575,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 1575,]
tblParentalCare[tblParentalCare$DVDRef == 1575,]

# 50021 > my code is correct, summary written in excel is wrong + share time not calculated
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 579,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 579,]
tblParentalCare[tblParentalCare$DVDRef == 579,]

# 50197 > my code is correct, one Oblue count as a bout (>1 min), share time not calculated
combinedprovisioningALL[combinedprovisioningALL$DVDRef == 755,]
MY_tblParentalCareforComparison[MY_tblParentalCareforComparison$DVDRef == 755,]
tblParentalCare[tblParentalCare$DVDRef == 755,]
}


}

}

head(combinedprovisioningALL_FeedY)
head(MY_tblParentalCareforComparison)
head(Compare_tblParentalCare)


{### MY_tblParentalCare

{# replace MTime FTime to MTime1 and FTime1 and add MTime2 and FTime2 
# time1: replace sum duration only of visits > 1 min to sum duration of all feeding visits (including those < 1 min), whether OF or IN
MY_tblParentalCare <- MY_tblParentalCareforComparison[,c("DVDRef","MVisit1","FVisit1","MVisit2","FVisit2","ShareTime","Filename")]

combinedprovisioningALL_listperFilename <- split(combinedprovisioningALL,combinedprovisioningALL$Filename)

combinedprovisioningALL_listperFilename_fun2 = function(x)  {
x <- x[order(x$Tstart, -x$Tend),]

return(c(
sum(x$Duration[x$Sex==1 & x$FeedYN == 1 ]),  # MTime1
sum(x$Duration[x$Sex==0 & x$FeedYN == 1 ]),  # FTime1
sum(x$Duration[x$Sex==1 & x$FeedYN == 0 ]),  # MTime2
sum(x$Duration[x$Sex==0 & x$FeedYN == 0 ])   # FTime2
))
}

combinedprovisioningALL_listperFilename_out1b <- lapply(combinedprovisioningALL_listperFilename, FUN=combinedprovisioningALL_listperFilename_fun2)
combinedprovisioningALL_listperFilename_out2b <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilename_out1b)),do.call(rbind, combinedprovisioningALL_listperFilename_out1b))

nrow(combinedprovisioningALL_listperFilename_out2b)	# 2112
rownames(combinedprovisioningALL_listperFilename_out2b) <- NULL
colnames(combinedprovisioningALL_listperFilename_out2b) <- c('Filename','MTime1', 'FTime1','MTime2', 'FTime2')

MY_tblParentalCare <- merge(x=MY_tblParentalCare,y=combinedprovisioningALL_listperFilename_out2b,all.x=TRUE, by='Filename')
nrow(MY_tblParentalCare[(MY_tblParentalCare$MTime1 == 0 | is.na(MY_tblParentalCare$MTime1) ) & (MY_tblParentalCare$FTime1 == 0 | is.na(MY_tblParentalCare$FTime1)),])


}

{# add protocol and replace zeros in Visit2 for Issie's protocol to NA because they were not recorded, it is not that they did not occur

MY_tblParentalCare <- merge(x=MY_tblParentalCare, y= unique(combinedprovisioningALLforDB[,c('DVDRef', 'Protocol')]), all.x=TRUE, by='DVDRef')
MY_tblParentalCare$MVisit2[MY_tblParentalCare$Protocol == 'Issie'] <- NA
MY_tblParentalCare$FVisit2[MY_tblParentalCare$Protocol == 'Issie'] <- NA
MY_tblParentalCare$MTime2[MY_tblParentalCare$Protocol == 'Issie'] <- NA
MY_tblParentalCare$FTime2[MY_tblParentalCare$Protocol == 'Issie'] <- NA
}

{# replace ShareTime by ShareTime1 and add ShareTime12

colnames(MY_tblParentalCare)[which(names(MY_tblParentalCare) == "ShareTime")] <- "ShareTime1"

{# calculate sharetime12 for each visit (for both sexes separately) in the raw data

combinedprovisioningALL_listperFilename <- split(combinedprovisioningALL,combinedprovisioningALL$Filename)
	# x <- combinedprovisioningALL_listperFilename[['2014\\VN0585.xlsx']] # example where 1 male visit nested within female visit
	# x <- combinedprovisioningALL_listperFilename[['2007\\70124.xlsx']]  # example where no male visit
	# x <- combinedprovisioningALL_listperFilename[['2004\\40369.xlsx']]  # example where several nested visits
	# x <- combinedprovisioningALL_listperFilename[['2004\\40034.xlsx']]  # example where one visit per sex
	# x <- combinedprovisioningALL_listperFilename[['2005\\50208.xlsx']]  # example where length one vector is equal to second vector, and therefore data frame is created instead of list


out5 <- list()

for (j in 1:length(combinedprovisioningALL_listperFilename) )		{ 
		x <-combinedprovisioningALL_listperFilename[[j]]
		
		# Modified from Lotte Schlicht
		#1. find the times when males/females are present at the box
		#1.a subset to each sex
		x0 = subset(x, Sex == 0)
		x1 = subset(x, Sex == 1)
		
		#1.b create vector of times present (the *10 and then /10 are the easiest way to construct thenths of minutes)
			# remove the last tenth of second for each visits, unless the visit entry and exit has the same time
			# write the output in a list in case only one visit or several visits of equal length of decimals (in those cases, mapply creates data.frame instead of lists!)
		
		sex0_presence = mapply(FUN = function(Tstart, Tend) {  if (Tstart==Tend) {return (Tstart)} 
		if (Tstart!=Tend)	{return(list(((Tstart*10) : (Tend*10-1))/10))}}, Tstart = x0$Tstart, Tend = x0$Tend)
			
		sex1_presence = mapply(FUN = function(Tstart, Tend) {  if (Tstart==Tend) {return (Tstart*10)} 
		if (Tstart!=Tend)	{return(list(((Tstart*10) : (Tend*10-1))/10))}}, Tstart = x1$Tstart, Tend = x1$Tend)

		
		#2. check for each list entry of each sex (= each row in the original table) how many of the numbers also occur for the other sex
		# this gives you the number of tenths-of-minutes that both birds were visiting the box.
		# for when just one visit per sex
		
		sex0 = lapply(sex0_presence, FUN = function(x1, x2) { 
					return( length(which(x1 %in% x2)) ) # seem to take each list of sex0_prsence as x1 length(which(sex0_presence[[2]] %in% unlist(sex1_presence)))
				}, 
				x2 = unlist(sex1_presence)
			)
						
		sex1 = lapply(sex1_presence, FUN = function(x1, x2) { 
					return( length(which(x1 %in% x2)) ) 

				}, 
				x2 = unlist(sex0_presence)
			)

			
		#4. attach to original data - in order to make sure that the order remains intact, add the columns to the sex-specific datasets and then rbind these together.
		x0$ShareTime12 = unlist(sex0)
		x1$ShareTime12 = unlist(sex1)
		x = as.data.frame(rbind(x0, x1))

		#5. devide by 10 to get the unit "minutes"
		x$ShareTime12 = x$ShareTime12/10
		
		x <- x[order(x$Tstart, -x$Tend),]
		out5[[j]] <- x
		
		#6. clean up
		sex0_presence <- NULL
		sex1_presence <- NULL
		sex0 <- NULL
		sex1 <- NULL
		x0 <- NULL		
		x1 <- NULL
		x <- NULL
}

combinedprovisioningALL_withShareTime12 <- do.call(rbind, out5)
}

{# calculate sharetime12 for each file

combinedprovisioningALL_listperFilename_withShare12 <- split(combinedprovisioningALL_withShareTime12,combinedprovisioningALL_withShareTime12$Filename)

combinedprovisioningALL_listperFilename_withShare12_fun = function(x)  {
		
return(c(sum(x$ShareTime12, na.rm=T)/2))					# ShareTime12
		# sum(x$ShareTime[x$Sex == 0], na.rm=T),		# FShareTime is the same
		# sum(x$ShareTime[x$Sex == 1], na.rm=T)))		# MShareTime is the same
}

combinedprovisioningALL_listperFilename_withShare12_out1 <- lapply(combinedprovisioningALL_listperFilename_withShare12, FUN=combinedprovisioningALL_listperFilename_withShare12_fun)
combinedprovisioningALL_listperFilename_withShare12_out2 <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilename_withShare12_out1)),do.call(rbind, combinedprovisioningALL_listperFilename_withShare12_out1))

nrow(combinedprovisioningALL_listperFilename_withShare12_out2)	# 2102
rownames(combinedprovisioningALL_listperFilename_withShare12_out2) <- NULL
colnames(combinedprovisioningALL_listperFilename_withShare12_out2) <- c('Filename','ShareTime12')
}

head(combinedprovisioningALL_listperFilename_withShare12_out2) # files with no visits by either sex have been removed... 
}

MY_tblParentalCare <- merge(x=MY_tblParentalCare, y =combinedprovisioningALL_listperFilename_withShare12_out2, all.x=TRUE, by='Filename')
MY_tblParentalCare$ShareTime12[MY_tblParentalCare$Protocol == 'Issie'] <- NA


{# create RawFeedingVisit ('A' bouts removed, one succession OF-IN give the Tstart of OF and the Tend of IN - split per sex and recombine)
# add interfeed intervals 20160408

combinedprovisioningALL_FeedY_listperFilenameperSex0 <- split(combinedprovisioningALL_FeedY[combinedprovisioningALL_FeedY$Sex == 0,], combinedprovisioningALL_FeedY$Filename[combinedprovisioningALL_FeedY$Sex == 0])
combinedprovisioningALL_FeedY_listperFilenameperSex1 <- split(combinedprovisioningALL_FeedY[combinedprovisioningALL_FeedY$Sex == 1,], combinedprovisioningALL_FeedY$Filename[combinedprovisioningALL_FeedY$Sex == 1])
x <- combinedprovisioningALL_FeedY_listperFilenameperSex0[['2015\\VO0170.xlsx']]
x <- combinedprovisioningALL_FeedY_listperFilenameperSex0[['2013\\VM0540.xlsx']]
x <- combinedprovisioningALL_FeedY_listperFilenameperSex0[['2013\\VM0339.xlsx']]
	
combinedprovisioningALL_FeedY_listperFilenameperSex_fun = function(x)  {
x <- x[order(x$Tstart, -x$Tend),]

x$NextTime <-  c(x$Tstart[-1],NA)
x$NextTimeSame <-  x$Tend == x$NextTime

x$PrevTime <-  c(NA, x$Tend[-nrow(x)])
x$PrevTimeSame <-  x$Tstart == x$PrevTime

x$NextState <- c(as.character(x$State[-1]),NA)
x$PrevState <- c(NA,as.character(x$State[-nrow(x)]))

x$TstartFeedVisit <- x$Tstart
x$TendFeedVisit <- x$Tend

for (i in 1: nrow(x))
{
if((x$State[i] == 'OF') & (!is.na(x$NextTimeSame[i]) & x$NextTimeSame[i] == 'TRUE') & (!is.na(x$NextState[i]) & x$NextState[i] == 'IN' )) # only merge OF followed by IN if Tend OF == Tstart IN, in Shinichi and mostly Malika's protocol 
{
x$TstartFeedVisit[i] <- x$Tstart[i]
x$TendFeedVisit[i] <- x$Tend[i+1]
}
if((x$State[i] == 'IN') & (!is.na(x$PrevTimeSame[i]) & x$PrevTimeSame[i] == 'TRUE') & (!is.na(x$PrevState[i]) & x$PrevState[i] == 'OF' )) # only merge OF followed by IN if Tend OF == Tstart IN, in Shinichi and mostly Malika's protocol 
{
x$TstartFeedVisit[i] <- x$Tstart[i-1]
x$TendFeedVisit[i] <- x$Tend[i]
}
}

x <- unique(x[,c('Filename','TstartFeedVisit','TendFeedVisit','Sex')])
x$Interval <- c(0,diff(x$TstartFeedVisit))

return(x)

}


combinedprovisioningALL_FeedY_listperFilenameperSex0_out1 <- lapply(combinedprovisioningALL_FeedY_listperFilenameperSex0, FUN=combinedprovisioningALL_FeedY_listperFilenameperSex_fun)
combinedprovisioningALL_FeedY_listperFilenameperSex0_out2 <- data.frame(do.call(rbind, combinedprovisioningALL_FeedY_listperFilenameperSex0_out1))
rownames(combinedprovisioningALL_FeedY_listperFilenameperSex0_out2) <- NULL
head(combinedprovisioningALL_FeedY_listperFilenameperSex0_out2)

combinedprovisioningALL_FeedY_listperFilenameperSex1_out1 <- lapply(combinedprovisioningALL_FeedY_listperFilenameperSex1, FUN=combinedprovisioningALL_FeedY_listperFilenameperSex_fun)
combinedprovisioningALL_FeedY_listperFilenameperSex1_out2 <- data.frame(do.call(rbind, combinedprovisioningALL_FeedY_listperFilenameperSex1_out1))
rownames(combinedprovisioningALL_FeedY_listperFilenameperSex1_out2) <- NULL
head(combinedprovisioningALL_FeedY_listperFilenameperSex1_out2)


RawFeedingVisits <- rbind(combinedprovisioningALL_FeedY_listperFilenameperSex0_out2,combinedprovisioningALL_FeedY_listperFilenameperSex1_out2)
RawFeedingVisits <- merge(x=RawFeedingVisits, y=MY_tblParentalCare[,c('Filename','DVDRef')],by='Filename', all.x=TRUE)
RawFeedingVisits <- RawFeedingVisits[order(RawFeedingVisits$DVDRef,RawFeedingVisits$Tstart, -RawFeedingVisits$Tend),]
# write.table(RawFeedingVisits, file = "R_RawFeedingVisits.xls", col.names=TRUE, sep='\t') # 20160324 20160331

}

{# calculate alternation and NbVisits per file

RawFeedingVisits_listperDVDRef <- split (RawFeedingVisits, RawFeedingVisits$Filename)
x <- RawFeedingVisits_listperDVDRef[[3]]
x <- RawFeedingVisits_listperDVDRef[[76]]

RawFeedingVisits_listperDVDRef_fun = function(x) {
x <- x[order(x$Filename, x$Tstart, -x$Tend),]

x$NextSexSame <- c(x$Sex[-1],NA) == x$Sex

return(c(
length(x$NextSexSame[x$NextSexSame == FALSE & !is.na(x$NextSexSame)]),	#NbAlternation
length(x$Sex[x$Sex == 1]),	#NbMVisit
length(x$Sex[x$Sex == 0])	#NbFVisit
))

}

RawFeedingVisits_listperDVDRef_out1 <- lapply(RawFeedingVisits_listperDVDRef, FUN=RawFeedingVisits_listperDVDRef_fun)
RawFeedingVisits_listperDVDRef_out2 <- data.frame(rownames(do.call(rbind,RawFeedingVisits_listperDVDRef_out1)),do.call(rbind, RawFeedingVisits_listperDVDRef_out1))

nrow(RawFeedingVisits_listperDVDRef_out2) # 2100 (12 files where no Feeding visits)
rownames(RawFeedingVisits_listperDVDRef_out2) <- NULL
colnames(RawFeedingVisits_listperDVDRef_out2) <- c('Filename','NbAlternation','NbMVisit','NbFVisit')
head(RawFeedingVisits_listperDVDRef_out2)

}

{# compare Visit1 and NbFeedingVisit (is equal)
MY_tblParentalCare2 <- merge(x=MY_tblParentalCare, y=RawFeedingVisits_listperDVDRef_out2, by='Filename')
head(MY_tblParentalCare2)

MY_tblParentalCare2$DiffMVisit1 <- MY_tblParentalCare2$MVisit1 - MY_tblParentalCare2$NbMVisit
MY_tblParentalCare2$DiffFVisit1 <- MY_tblParentalCare2$FVisit1 - MY_tblParentalCare2$NbFVisit
MY_tblParentalCare2[MY_tblParentalCare2$DiffFVisit1 != 0 | MY_tblParentalCare2$DiffMVisit1!= 0,]
RawFeedingVisits[RawFeedingVisits$Filename == '2013\\VM0339.xlsx',] # OK
}

MY_tblParentalCare <- merge(x=MY_tblParentalCare, y =RawFeedingVisits_listperDVDRef_out2[,c('Filename','NbAlternation')], all.x=TRUE)

}

head(RawFeedingVisits,60)
head(MY_tblParentalCare)


{### MY_tblDVDInfo
MY_tblDVDInfo  <- tblDVD_XlsFilesALLDBINFO[tblDVD_XlsFilesALLDBINFO$DVDRef %in% unique(combinedprovisioningALLforDB$DVDRef),c('DVDRef','Filename','BroodRef','OffspringNo','Age','DVDdate','DVDtime','Notes','TapeLength','EffectTime')]
MY_tblDVDInfo <- rbind(MY_tblDVDInfo, missingDVDFilenames)

{# re calculate chick age at DVDdate
MY_tblDVDInfo <- merge (x= MY_tblDVDInfo, 
						y= tblBroodEvents[tblBroodEvents$EventNumber == 1, c('BroodRef', 'EventDate')],
						all.x=TRUE, by ='BroodRef')
						
colnames(MY_tblDVDInfo) <- c('BroodRef', 'DVDRef','Filename','DVDInfoChickNb','DVDInfoAge','DVDdate','DVDtime','DVDInfoNotes','TapeLength','EffectTime','Notes','HatchingDate')

MY_tblDVDInfo$ChickAge <- as.numeric(MY_tblDVDInfo$DVDdate - MY_tblDVDInfo$HatchingDate) # chicks are aged 0 day at date of hatching

hist(MY_tblDVDInfo$DVDInfoAge)
hist(MY_tblDVDInfo$ChickAge)
hist(MY_tblDVDInfo$DVDInfoAge-MY_tblDVDInfo$ChickAge)

}

{# RelTime
sunrise$month <- sprintf("%02d",sunrise$month)
sunrise$day <- sprintf("%02d",sunrise$day)
sunrise$Date <- paste(sunrise$year, sunrise$month, sep="-")
sunrise$Date <- paste(sunrise$Date, sunrise$day, sep="-")
sunrise$SunriseDate <- as.POSIXct(sunrise$Date)
sunrise <- sunrise[,c('SunriseDate','Sunrise')]
sunrise$Sunrise <- as.POSIXct(sunrise$Sunrise, format="%H:%M")# add today's date and unit of your current system (currently BST) to the time.

MY_tblDVDInfo$DVDtime <- substr(as.character(MY_tblDVDInfo$DVDtime), 12, 16)
MY_tblDVDInfo$DVDtime <- as.POSIXct(MY_tblDVDInfo$DVDtime, format="%H:%M") # add THE SAME today's date and unit of your current system (currently BST) to the time.

MY_tblDVDInfo <- merge(x=MY_tblDVDInfo,y=sunrise, all.x=TRUE, by.x='DVDdate', by.y='SunriseDate')

MY_tblDVDInfo$RelTimeMins <- as.numeric(difftime(MY_tblDVDInfo$DVDtime,MY_tblDVDInfo$Sunrise, units='mins'), units='mins')
MY_tblDVDInfo$LogRelTimeMins <- log10(MY_tblDVDInfo$RelTimeMins+10)


}

{# check numbers of broods with certain conditions
length(unique(MY_tblDVDInfo$BroodRef)) # 933 (20160314) 1014 (20160317) 1067 (20160323)
length(unique(MY_tblDVDInfo$BroodRef[MY_tblDVDInfo$ChickAge>5])) # 891 (20160314) 972 (20160317) 1025 (20160323)
mean(table(MY_tblDVDInfo$BroodRef)) # 1.93 (20160314) 1.98 (20160317) 1.97 (20160323)
mean(table(MY_tblDVDInfo$BroodRef[MY_tblDVDInfo$ChickAge>5])) # 1.84 (20160314) 1.89 (20160317) 1.90 (20160323)

hist(MY_tblDVDInfo$DVDdate, breaks = 'years', freq=TRUE)

MY_tblDVDInfo <- merge(x= MY_tblDVDInfo, 
						y= unique(combinedprovisioningALLforDB[,c('DVDRef', 'Protocol')]),
						all.x=TRUE,
						by ='DVDRef')
						

table(MY_tblDVDInfo$Protocol[MY_tblDVDInfo$ChickAge>5]) # Issie 932  Shinichi  931  Malika 83
table(MY_tblDVDInfo$Protocol) # Issie 934  Shinichi  1089  Malika 83 # include with Nb chick = 0


}

}

head(MY_tblDVDInfo)


{### MY_tblBroods								>> update txt files when pedigree updated !!!!!!!!!!!!!!!!!!!!!!!!!!!

MY_tblBroods <- tblBroods[tblBroods$BroodRef %in% MY_tblDVDInfo$BroodRef,]
nrow(MY_tblBroods[MY_tblBroods$SocialDadCertain == 0 | MY_tblBroods$SocialMumCertain == 0,]) # 92 brood with at least one parents unknown or uncertain

{# add hatching date from tblBroodEvent
MY_tblBroods <- merge (x= MY_tblBroods, 
						y= tblBroodEvents[tblBroodEvents$EventNumber == 1, c('BroodRef', 'EventDate')],
						all.x=TRUE, by ='BroodRef')
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "EventDate")] <- "HatchingDate"

MY_tblBroods[is.na(MY_tblBroods$HatchingDate) ,] # 0 lines

}

{# add ringedYN to RearingBrood_allBirds
for (i in 1:nrow(RearingBrood_allBirds))
{
if (RearingBrood_allBirds$BirdID[i] %in% unique(tblAllCodes$BirdID))
{RearingBrood_allBirds$RingedYN[i] <- 1}
else
{RearingBrood_allBirds$RingedYN[i] <- 0}
}
}

{# add nb of chicks per rearing brood

RearingBrood_allBirds_split_per_RearingBrood <- split(RearingBrood_allBirds,RearingBrood_allBirds$RearingBrood)

RearingBrood_allBirds_split_per_RearingBrood_fun <- function(x) {
return(c(
length(x$BirdID[x$LastStage>1]),  				# NbHatched
length(x$BirdID[x$LastStage==3]), 				# Nb3
length(x$BirdID[x$RingedYN==1])))  				# NbRinged
}

RearingBrood_allBirds_split_per_RearingBrood_out1 <- lapply(RearingBrood_allBirds_split_per_RearingBrood, FUN=RearingBrood_allBirds_split_per_RearingBrood_fun)
RearingBrood_allBirds_split_per_RearingBrood_out2 <- data.frame(rownames(do.call(rbind,RearingBrood_allBirds_split_per_RearingBrood_out1)),do.call(rbind, RearingBrood_allBirds_split_per_RearingBrood_out1))

nrow(RearingBrood_allBirds_split_per_RearingBrood_out2)	# 1940
rownames(RearingBrood_allBirds_split_per_RearingBrood_out2) <- NULL
colnames(RearingBrood_allBirds_split_per_RearingBrood_out2) <- c('RearingBrood','NbHatched', 'Nb3','NbRinged')

head(RearingBrood_allBirds_split_per_RearingBrood_out2)

MY_tblBroods <- merge(x=MY_tblBroods, y=RearingBrood_allBirds_split_per_RearingBrood_out2, all.x=TRUE, by.x='BroodRef', by.y='RearingBrood')

}

{# add lastSeenAlive for social mum and dad		>> update txt files when pedigree updated !!!!!!!!!!!!!!!!!!!!!!!!!!!
head(sys_LastSeenAlive)

MY_tblBroods <- merge(x=MY_tblBroods, y=sys_LastSeenAlive, all.x = TRUE, by.x = 'SocialDadID', by.y = 'BirdID')
MY_tblBroods <- merge(x=MY_tblBroods, y=sys_LastSeenAlive, all.x = TRUE, by.x = 'SocialMumID', by.y = 'BirdID')
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "LastLiveRecord.x")] <- "LastLiveRecordSocialDad"
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "LastLiveRecord.y")] <- "LastLiveRecordSocialMum"
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "Source.x")] <- "LastLiveRecordSocialDadSource"
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "Source.y")] <- "LastLiveRecordSocialMumSource"
}

{# add cohort and age of social parents
MY_tblBroods <- merge(x=MY_tblBroods, y=tblBirdID[,c('BirdID','Cohort')], all.x=TRUE, by.x='SocialDadID', by.y='BirdID')
MY_tblBroods <- merge(x=MY_tblBroods, y=tblBirdID[,c('BirdID','Cohort')], all.x=TRUE, by.x='SocialMumID', by.y='BirdID')
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "Cohort.x")] <- "CohortDad"
colnames(MY_tblBroods)[which(names(MY_tblBroods) == "Cohort.y")] <- "CohortMum"

MY_tblBroods$BreedingYear <- as.numeric(format(MY_tblBroods$HatchingDate,'%Y'))
MY_tblBroods$DadAge <- MY_tblBroods$BreedingYear - MY_tblBroods$CohortDad
MY_tblBroods$MumAge <- MY_tblBroods$BreedingYear - MY_tblBroods$CohortMum

MY_tblBroods$ParentsAge <- (MY_tblBroods$MumAge+ MY_tblBroods$DadAge) /2

}

{# add Male divorce
MY_tblBroods_split_per_SocialDadID <- split(MY_tblBroods,MY_tblBroods$SocialDadID)
#x <- MY_tblBroods_split_per_SocialDadID[[21]]

MY_tblBroods_split_per_SocialDadID_fun = function(x)  {
x <- x[order(x$HatchingDate),]

x$MPrevNbRinged <- c(NA,x$NbRinged[-nrow(x)]) # MPrevNbRinged
x$MBroodNb <- 1:nrow(x) # MBroodNb
x$MPriorResidence <- x$NestboxRef == c(NA,x$NestboxRef[-nrow(x)]) # Prior residence does not take into account change of year here.
x$MPrevFemaleLastSeenAlive <- c(NA,as.character(x$LastLiveRecordSocialMum[-nrow(x)]))
x$MwithsameF <- x$SocialMumID == c(NA,x$SocialMumID[-nrow(x)]) # Mwith same Female does not take into account change of year here. and neither if male goes back with an example
x$MDivorce <- as.POSIXct(x$MPrevFemaleLastSeenAlive, format = "%d.%m.%Y") > x$HatchingDate & x$MwithsameF == FALSE

x$MDivorceforEx <- NA
if(nrow(x)>1) {for (i in 1: nrow(x)) {if (!is.na(x$MDivorce[i]) & x$MDivorce[i] == TRUE)
{x$MDivorceforEx[i] <- x$SocialMumID[i] %in% x$SocialMumID[1:i-1]}}}
if(nrow(x)==1)
{x$MDivorceforEx <- NA}

return(x)

}

MY_tblBroods_split_per_SocialDadID_out1 <- lapply(MY_tblBroods_split_per_SocialDadID, FUN=MY_tblBroods_split_per_SocialDadID_fun)
MY_tblBroods_split_per_SocialDadID_out2 <- data.frame(rownames(do.call(rbind,MY_tblBroods_split_per_SocialDadID_out1)),do.call(rbind, MY_tblBroods_split_per_SocialDadID_out1))

nrow(MY_tblBroods_split_per_SocialDadID_out2)	# 975
rownames(MY_tblBroods_split_per_SocialDadID_out2) <- NULL

MY_tblBroods <- MY_tblBroods_split_per_SocialDadID_out2[,-1]
}

{# add Female divorce
MY_tblBroods_split_per_SocialMumID <- split(MY_tblBroods,MY_tblBroods$SocialMumID)
x <- MY_tblBroods_split_per_SocialMumID[[5]]

MY_tblBroods_split_per_SocialMumID_fun = function(x)  {
x <- x[order(x$HatchingDate),]

x$FPrevNbRinged <- c(NA,x$NbRinged[-nrow(x)]) # MPrevNbRinged
x$FBroodNb <- 1:nrow(x) # MBroodNb
x$FPriorResidence <- x$NestboxRef == c(NA,x$NestboxRef[-nrow(x)]) # Prior residence does not take into account change of year here.
x$FPrevMaleLastSeenAlive <- c(NA,as.character(x$LastLiveRecordSocialDad[-nrow(x)]))
x$FwithsameM <- x$SocialMumID == c(NA,x$SocialDadID[-nrow(x)]) # Mwith same Female does not take into account change of year here. and neither if male goes back with an example
x$FDivorce <- as.POSIXct(x$FPrevMaleLastSeenAlive, format = "%d.%m.%Y") > x$HatchingDate & x$FwithsameM == FALSE

x$FDivorceforEx <- NA
if(nrow(x)>1) {for (i in 1: nrow(x)) {if (!is.na(x$FDivorce[i]) & x$FDivorce[i] == TRUE)
{x$FDivorceforEx[i] <- x$SocialDadID[i] %in% x$SocialDadID[1:i-1]}}}
if(nrow(x)==1)
{x$FDivorceforEx <- NA}

return(x)

}

MY_tblBroods_split_per_SocialMumID_out1 <- lapply(MY_tblBroods_split_per_SocialMumID, FUN=MY_tblBroods_split_per_SocialMumID_fun)
MY_tblBroods_split_per_SocialMumID_out2 <- data.frame(rownames(do.call(rbind,MY_tblBroods_split_per_SocialMumID_out1)),do.call(rbind, MY_tblBroods_split_per_SocialMumID_out1))

nrow(MY_tblBroods_split_per_SocialMumID_out2)	# 962
rownames(MY_tblBroods_split_per_SocialMumID_out2) <- NULL

MY_tblBroods <- MY_tblBroods_split_per_SocialMumID_out2[,-1]
}

{# add PairBroodNb
MY_tblBroods$PairID <- paste(MY_tblBroods$SocialDadID,MY_tblBroods$SocialMumID, sep="" )

MY_tblBroods_split_per_PairID <- split(MY_tblBroods, MY_tblBroods$PairID)
x <- MY_tblBroods_split_per_PairID[[2]]

MY_tblBroods_split_per_PairID_fun <- function(x){
x <- x[order(x$HatchingDate),]

x$PairBroodNb <- 1:nrow(x)

return(x)
}

MY_tblBroods_split_per_PairID_out1 <- lapply(MY_tblBroods_split_per_PairID, FUN=MY_tblBroods_split_per_PairID_fun)
MY_tblBroods_split_per_PairID_out2 <- data.frame(rownames(do.call(rbind,MY_tblBroods_split_per_PairID_out1)),do.call(rbind, MY_tblBroods_split_per_PairID_out1))

nrow(MY_tblBroods_split_per_PairID_out2)	# 1019
rownames(MY_tblBroods_split_per_PairID_out2) <- NULL

MY_tblBroods <- MY_tblBroods_split_per_PairID_out2[,-1]

}

}

head(MY_tblBroods)


{### MY_tblParentalCare with summary of rates and durations (use of effective time from MY_tblDVDInfo)

MY_tblParentalCare <- merge(x=MY_tblParentalCare,y= MY_tblDVDInfo[,c('DVDRef','TapeLength','EffectTime')], by='DVDRef', all.x=TRUE)

{# recalculate EffectiveTime 
head(combinedprovisioningALL)

outTsartMin <- do.call(rbind, by(combinedprovisioningALL, combinedprovisioningALL$DVDRef, function(x) x[which.min(x$Tstart), c('DVDRef','Tstart')] ))
MY_tblParentalCare <-  merge(x=MY_tblParentalCare,y= outTsartMin, by='DVDRef', all.x=TRUE)
MY_tblParentalCare$EffectiveTime <- MY_tblParentalCare$TapeLength - MY_tblParentalCare$Tstart
}

{# check discrepency between EffectTime and EffectiveTime

MY_tblParentalCare$DiffEffectTime <- round(MY_tblParentalCare$EffectiveTime - MY_tblParentalCare$EffectTime,0)
MY_tblParentalCare[(is.na(MY_tblParentalCare$DiffEffectTime) | MY_tblParentalCare$DiffEffectTime != 0) & !is.na(MY_tblParentalCare$DVDRef) & !is.na(MY_tblParentalCare$MVisit1),]
}

MY_tblParentalCare <- MY_tblParentalCare[, !(names(MY_tblParentalCare) %in% c('DiffEffectTime','EffectTime', 'TapeLength','Tstart'))]

{# add MFTime1, MFTime02, MFTime2, provisioning rates, AlternationValue

MY_tblParentalCare$MFTime1 <- MY_tblParentalCare$MTime1 + MY_tblParentalCare$FTime1 - MY_tblParentalCare$ShareTime1
MY_tblParentalCare$MFTime02 <- round(MY_tblParentalCare$EffectiveTime - MY_tblParentalCare$MFTime1,1)
MY_tblParentalCare$MFTime2 <- round(MY_tblParentalCare$MTime2 + MY_tblParentalCare$FTime2 - (MY_tblParentalCare$ShareTime12 - MY_tblParentalCare$ShareTime1),2)

MY_tblParentalCare$FVisit1RateH <- round(60*MY_tblParentalCare$FVisit1/MY_tblParentalCare$EffectiveTime)
MY_tblParentalCare$MVisit1RateH <- round(60*MY_tblParentalCare$MVisit1/MY_tblParentalCare$EffectiveTime)
MY_tblParentalCare$DiffVisit1Rate <- abs(round(MY_tblParentalCare$FVisit1RateH - MY_tblParentalCare$MVisit1RateH))

MY_tblParentalCare$FTime1RateH <- round(60*MY_tblParentalCare$FTime1/MY_tblParentalCare$EffectiveTime,2)
MY_tblParentalCare$MTime1RateH <- round(60*MY_tblParentalCare$MTime1/MY_tblParentalCare$EffectiveTime,2)
MY_tblParentalCare$DiffTime1Rate <- abs(round(MY_tblParentalCare$FTime1RateH - MY_tblParentalCare$MTime1RateH, 2))

MY_tblParentalCare$AlternationValue <- round(MY_tblParentalCare$NbAlternation/(MY_tblParentalCare$MVisit1 + MY_tblParentalCare$FVisit1 -1) *100,1)


}


}

head(MY_tblParentalCare)



DurationScript <- Sys.time() - TimeStart
DurationScript # ~ 14 min


#write.table(MY_tblDVDInfo,file='R_MY_tblDVDInfo.xls',  col.names=TRUE, sep='\t') # 20160415
#write.table(MY_tblBroods,file='R_MY_tblBroods.xls',  col.names=TRUE, sep='\t') # 20160415
#write.table(MY_tblParentalCare,file='R_MY_tblParentalCare.xls',  col.names=TRUE, sep='\t') # 20160415, identical with changes to call new DB, 20160425



## TO DO
# find mass (mean and small chick) and tarsus (consider difference of age, whether to include all brood or a standardized subsets)
# select valid files (nest with chicks, nest with visits from both parents to have alternation possible ?)
# simulation alternation (bootstrapping simply sampling with replacement ??)
# repeatability alternation (considering more than two measures, randomise order of measurements, or use rptR package to fit mixed effect model ?)



{# cheap version bird mass
birdmass <- read.table("BirdMass.txt", sep='\t', header=T)
nrow(birdmass) # 2444 only age11 measurements
birdmass <- birdmass[! birdmass$RearingBrood %in% c(40,116,818,1246,1382,1583), ]
nrow(birdmass) # 2433 where chicks same brood are similar age even if cross fostered (i.e. same hatching data)
birdmass <-  birdmass[birdmass$RearingBrood %in% MY_tblBroods$BroodRef , ]
nrow(birdmass) # 2169 nestlings belonging to taped broods
nrow(birdmass[is.na(birdmass$Mass),]) # an additional 6 birds without mass at age 11
nrow(birdmass[is.na(birdmass$Tarsus),]) # an additional 27 birds without tarsus at age 11
#birdmass <- birdmass[complete.cases(birdmass),]
nrow(birdmass)
nrow(MY_tblBroods[MY_tblBroods$BroodRef %in% birdmass$RearingBrood,]) # 825 broods where measurement werent taken on age11
nrow(MY_tblBroods[!MY_tblBroods$BroodRef %in% birdmass$RearingBrood,]) # 194 broods where measurement werent taken on age11

birdmass_splitperRearingBrood <- split(birdmass, birdmass$RearingBrood)
birdmass_splitperRearingBrood[[1]]

birdmass_splitperRearingBrood_fun <- function(x) {

return(c(
nrow(x), 	   #broodsizeage11
round(mean(x$Mass),2),  # AvMass
round(mean(x$Tarsus),2) # AvTarsus
))

}

birdmass_splitperRearingBrood_out1 <- lapply(birdmass_splitperRearingBrood,birdmass_splitperRearingBrood_fun)
birdmass_splitperRearingBrood_out2 <- data.frame(rownames(do.call(rbind,birdmass_splitperRearingBrood_out1)),do.call(rbind, birdmass_splitperRearingBrood_out1))

nrow(birdmass_splitperRearingBrood_out2)	# 825
rownames(birdmass_splitperRearingBrood_out2) <- NULL
colnames(birdmass_splitperRearingBrood_out2) <- c('RearingBrood','BroodSize11','AvMass11','AvTarsus11')
head(birdmass_splitperRearingBrood_out2)


MY_tblBroods <- merge(x=MY_tblBroods, y=birdmass_splitperRearingBrood_out2, all.x =TRUE, by.x = 'BroodRef', by.y = 'RearingBrood' )
head(MY_tblBroods)
sunflowerplot(MY_tblBroods$NbRinged~ MY_tblBroods$BroodSize11)
MY_tblBroods[MY_tblBroods$NbRinged- MY_tblBroods$BroodSize11 > 0 & !is.na(MY_tblBroods$BroodSize11),]
}


{# cheap version selection files

# decision still to take:
# remove when number of chicks = 0 (how to detect them ? first attempt in next big braket)
# remove when age < 6 (when brooding) or separate stages into early and late ? ('only' lose 160 files > 1946 files left)

{### refine which files are valid for analysis and figure out the number of chicks in the nest at time of recording
# situation 4 = with chicks
# if during next visit (on the day of recording or within a few days after), nunmber of chicks = 0, maybe nest was already empty... ? 
# exclude recording where one or the two bird did not visit AND where the next nest check indicates 0 chicks ??
# what if in DVDInfo, DeathYN=Yes ?? how did they know ?
# calculate nb of chicks in nest at time of recording (DVDInfo nb of chicks, Nb of chicks alive in rearing brood, nb of offsrping at visit when visit on same day)


combinedprovisioningALL$Filename
MY_tblParentalCare


{## get back to querying the DB

head(tblBroods)
head(tblBroodEvents)

MY_LARGE_tblParentalCare <- merge(x=MY_tblParentalCare, y=tblDVD_XlsFilesALLDBINFO[,c('DVDRef','BroodRef','OffspringNo', 'DVDdate')], all.x=TRUE, by='DVDRef')
colnames(MY_LARGE_tblParentalCare)[which(names(MY_LARGE_tblParentalCare) == "OffspringNo")] <- "DVDInfoChickNb"
MY_LARGE_tblParentalCare <- merge(x=MY_LARGE_tblParentalCare, y=tblBroods, all.x=TRUE, by='BroodRef')

{# Nb of hatched, FL, and chicks alive per DVDdate from tblBirdID, per rearing broods

head(RearingBrood_allBirds)

MY_LONG_tblParentalCare_withAllBirds <- merge(x=MY_LARGE_tblParentalCare, y=RearingBrood_allBirds, by.x='BroodRef', by.y='RearingBrood', all.x=TRUE)
head(MY_LONG_tblParentalCare_withAllBirds, 20)


MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename <- split(MY_LONG_tblParentalCare_withAllBirds, MY_LONG_tblParentalCare_withAllBirds$Filename)
x <-MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename[[6]]
x <- MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename[['2011\\VK0062.xlsx']]
x <- MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename[['2005\\50166.xlsx']]
x <- MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename[['2004\\40077.xlsx']]



MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_fun <- function(x) {
return(c(
length(x$BirdID[x$LastStage>1]),  				# NbHatched
length(x$BirdID[x$LastStage==3]), 				# NbFL
length(x$BirdID[(is.na(x$DeathDate) | x$DeathDate > x$DVDdate) & x$LastStage > 1]), # NbAliveAtDVDDate
length(x$BirdID[!is.na(x$DeathDat) & x$DeathDate == x$DVDdate & x$LastStage > 1]), # NbDeadAtDVDDate
length(x$BirdID[!is.na(x$DeathDat) &x$DeathDate == x$DVDdate & x$LastStage > 1 & !is.na(x$DeathStatus) & x$DeathStatus == 3]), # NbDeadAtDVDDatebyAccident
length(x$BirdID[is.na(x$DeathDate) & x$DeathStatus > 0]), # NbDeadAtUnknownDate
length(x$BirdID[is.na(x$DeathDate) & is.na(x$DeathStatus) & x$LastStage!=3]) # NbNestlingNotKilled
))
}

MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out1 <- lapply(MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename, FUN=MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_fun)
MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out2 <- data.frame(rownames(do.call(rbind,MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out1)),do.call(rbind, MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out1))

nrow(MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out2)	# 1940
rownames(MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out2) <- NULL
colnames(MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out2) <- c('Filename','NbHatched', 'NbFL','NbAliveAtDVDDate','NbDeadAtDVDDate','NbDeadAtDVDDatebyAccident','NbDeadAtUnknownDate','NbNestlingNotKilled')

head(MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out2)

MY_LARGE_tblParentalCare <- merge(x=MY_LARGE_tblParentalCare, y=MY_LONG_tblParentalCare_withAllBirds_split_per_BroodFilename_out2, all.x=TRUE, by='Filename')
}

head(MY_LARGE_tblParentalCare)

{# add nb of chicks during the previous and next visit from tblBroodEvent

head(tblBroodEvents)

MY_VERY_LONG_tblParentalCare_withAllBroodEvents <- merge(x=MY_LARGE_tblParentalCare, y=tblBroodEvents, by='BroodRef', all.x=TRUE)
head(MY_VERY_LONG_tblParentalCare_withAllBroodEvents, 20)

MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename <- split(MY_VERY_LONG_tblParentalCare_withAllBroodEvents, MY_VERY_LONG_tblParentalCare_withAllBroodEvents$Filename)
x <-MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename[[8]]
x <-MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename[[5]]

MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_fun <- function(x) {

if (is.infinite(suppressWarnings(min(x$EventDate[x$EventDate >= unique(x$DVDdate)], na.rm=TRUE))) | is.na(suppressWarnings(min(x$EventDate[x$EventDate >= unique(x$DVDdate)], na.rm=TRUE)))) # if no next visit
{
return(c(
as.character(max(x$EventDate[x$EventDate <= unique(x$DVDdate)], na.rm=TRUE)), # PrevEventDate
NA, # NextEventDate
unique(x$OffspringNest[!is.na(x$EventDate) & x$EventDate == max(x$EventDate[x$EventDate <= unique(x$DVDdate)], na.rm=TRUE)]), # PrevEventNbChicks # added unique because of duplicated of visits on tbleBroodEvents...
NA # NextEventNbChicks
))
}

if (!is.infinite(min(x$EventDate[x$EventDate >= unique(x$DVDdate)], na.rm=TRUE)) & !is.na(min(x$EventDate[x$EventDate >= unique(x$DVDdate)], na.rm=TRUE))) # if there is a dated next visit
{
return(c(
as.character(max(x$EventDate[x$EventDate <= x$DVDdate], na.rm=TRUE)), # PrevEventDate
as.character(min(x$EventDate[x$EventDate >= x$DVDdate], na.rm=TRUE)), # NextEventDate
unique(x$OffspringNest[!is.na(x$EventDate) & x$EventDate == max(x$EventDate[x$EventDate <= unique(x$DVDdate)], na.rm=TRUE)]), # PrevEventNbChicks  
unique(x$OffspringNest[!is.na(x$EventDate) & x$EventDate == min(x$EventDate[x$EventDate >= unique(x$DVDdate)], na.rm=TRUE)])  # NextEventNbChicks
))
}

}

MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1 <- lapply(MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename, FUN=MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_fun)

cond_MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1 <- sapply(MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1, function(x) length(x) > 4)
MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1[cond_MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1] # should be empty list (wasn't empty when remove unique x$OffsrpingNest in function)

MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out2 <- data.frame(rownames(do.call(rbind,MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1)),do.call(rbind, MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out1))

nrow(MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out2)	# 1804
rownames(MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out2) <- NULL
colnames(MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out2) <- c('Filename','PrevEventDate', 'NextEventDate','PrevEventNbChicks','NextEventNbChicks')

head(MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out2,50)

MY_LARGE_tblParentalCare <- merge(x=MY_LARGE_tblParentalCare, y=MY_VERY_LONG_tblParentalCare_withAllBroodEvents_split_per_Filename_out2, all.x=TRUE, by='Filename')
}

head(MY_LARGE_tblParentalCare)

MY_LARGE_tblParentalCare$DelayPrevEventRec <- difftime(MY_LARGE_tblParentalCare$DVDdate, MY_LARGE_tblParentalCare$PrevEventDate, units='days')
MY_LARGE_tblParentalCare$DelayNextEventRec <- difftime(MY_LARGE_tblParentalCare$NextEventDate, MY_LARGE_tblParentalCare$DVDdate, units='days')

MY_LARGE_tblParentalCare$DiffNbChickDVDInfoNbAliveatDVDdate <- MY_LARGE_tblParentalCare$DVDInfoChickNb - MY_LARGE_tblParentalCare$NbAliveAtDVDDate
MY_LARGE_tblParentalCare$DiffNbChickDVDInfoVisit <- NA
MY_LARGE_tblParentalCare$PrevEventNbChicks <- as.numeric(as.character(MY_LARGE_tblParentalCare$PrevEventNbChicks))
MY_LARGE_tblParentalCare$NextEventNbChicks <- as.numeric(as.character(MY_LARGE_tblParentalCare$NextEventNbChicks))
MY_LARGE_tblParentalCare$DiffNbChickDVDInfoVisit[MY_LARGE_tblParentalCare$DelayPrevEventRec == 0 ] <- MY_LARGE_tblParentalCare$DVDInfoChickNb[MY_LARGE_tblParentalCare$DelayPrevEventRec == 0 ] - 
																									  MY_LARGE_tblParentalCare$PrevEventNbChicks[MY_LARGE_tblParentalCare$DelayPrevEventRec == 0 ]
MY_LARGE_tblParentalCare$DiffNbAliveatDVDdateVisit[MY_LARGE_tblParentalCare$DelayPrevEventRec == 0 ] <- MY_LARGE_tblParentalCare$NbAliveAtDVDDate[MY_LARGE_tblParentalCare$DelayPrevEventRec == 0 ] - 
																									  MY_LARGE_tblParentalCare$PrevEventNbChicks[MY_LARGE_tblParentalCare$DelayPrevEventRec == 0 ]

																									  
head(MY_LARGE_tblParentalCare)

head(MY_LARGE_tblParentalCare[MY_LARGE_tblParentalCare$DiffNbChickDVDInfoNbAliveatDVDdate != 0 | !is.na(MY_LARGE_tblParentalCare$DiffNbChickDVDInfoVisit),])
## write.table(MY_LARGE_tblParentalCare, file = "R_Compare_NbChicksDuringRecording.xls", col.names=TRUE, sep='\t')

MY_LARGE_tblParentalCare[MY_LARGE_tblParentalCare$NbHatched - MY_LARGE_tblParentalCare$DVDInfoChickNb <0,]

}



PotentialFailedNest_Recordings_DeathYes <- sqlQuery(conDB, "
SELECT tblParentalCare.DVDRef, tblDVDInfo.DVDNumber, tblDVDInfo.DVDdate, tblParentalCare.MTime, tblParentalCare.FTime, tblDVDInfo.Deaths, tblParentalCare.Notes
FROM (tblBroods INNER JOIN tblDVDInfo ON tblBroods.BroodRef = tblDVDInfo.BroodRef) INNER JOIN tblParentalCare ON tblDVDInfo.DVDRef = tblParentalCare.DVDRef
GROUP BY tblParentalCare.DVDRef, tblDVDInfo.DVDNumber, tblDVDInfo.DVDdate, tblParentalCare.MTime, tblParentalCare.FTime, tblDVDInfo.Deaths, tblParentalCare.Notes, tblDVDInfo.Situation
HAVING (((tblDVDInfo.Deaths)=Yes) AND ((tblDVDInfo.Situation)=4));
")




BroodTaped = sqlQuery (conDB, "SELECT RearingBrood_allbirds.RearingBrood, AllRecordings.BroodName, AllRecordings.DVDdate, AllRecordings.DVDNumber, Abs(Sum([LastStage]>1)) AS Nbhatched, Abs(Sum([LastStage]=3)) AS NbFL, tblDVDInfo.OffspringNo AS DVDInfoNbChicks
FROM ((AllRecordings LEFT JOIN RearingBrood_allbirds ON AllRecordings.BroodRef = RearingBrood_allbirds.RearingBrood) INNER JOIN tblDVDInfo ON AllRecordings.DVDRef = tblDVDInfo.DVDRef) LEFT JOIN tblBroodEvents ON AllRecordings.BroodRef = tblBroodEvents.BroodRef
GROUP BY RearingBrood_allbirds.RearingBrood, AllRecordings.BroodName, AllRecordings.DVDdate, AllRecordings.DVDNumber, tblDVDInfo.OffspringNo
ORDER BY RearingBrood_allbirds.RearingBrood, AllRecordings.DVDdate;
")









}

}

