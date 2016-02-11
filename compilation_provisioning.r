#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Compile provisioning data sparrows
#	 Start : 21/12/2015
#	 last modif : 11/02/2016  
#	 check inconsistencies with DB
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
# a situation = 4 in tblDVDInfo (i.e. only chicks)

## decision still to be taken:
# should excluded early age chicks because can still be brooded ??
# should exclude recordings when the next nest visit revealed every chicks were dead ?

}


rm(list = ls(all = TRUE))

{### packages, working directories and connection to Access DB
library(RODBC)
# library(openxlsx) # package openxlsx will be needed later in the code (after detaching the conflicting xlsx nevertheless needed to get colors for old templates)
# library(xlsx)	# package xlsx will be needed later in the code (after detaching the conflicting openxlsx nevertheless needed at first to be faster/not crashing)
require(zoo)

options(warn=2)	# when loop generate a error at one iteration, the loop stop, so one can call the filename and check what's wrong with it

pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")

tblDVD_XlsFiles <- sqlFetch(conDB, "tblDVD_XlsFiles")
tblDVD_XlsFiles <- tblDVD_XlsFiles[with(tblDVD_XlsFiles, order(tblDVD_XlsFiles$Filename)),]

# select video made when provisioning chick (situation = 4 )
tblDVD_XlsFilesALLDBINFO <- sqlQuery(conDB, "
SELECT tblDVD_XlsFiles.DVDRef, tblDVD_XlsFiles.Filename, tblDVDInfo.BroodRef, tblDVDInfo.Situation, tblDVDInfo.Deaths, tblDVDInfo.OffspringNo, tblDVDInfo.Age, tblDVDInfo.Wrong, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.Weather, tblDVDInfo.Wind, tblDVDInfo.Notes, tblParentalCare.TapeTime, tblParentalCare.EffectTime, tblParentalCare.Method, tblParentalCare.Observer, tblParentalCare.Notes, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.ShareTime, tblParentalCare.MVisit1, tblParentalCare.FVisit1, tblParentalCare.MVisit2, tblParentalCare.FVisit2, tblParentalCare.MBout, tblParentalCare.FBout
FROM tblDVDInfo INNER JOIN (tblDVD_XlsFiles INNER JOIN tblParentalCare ON tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef) ON (tblDVDInfo.DVDRef = tblParentalCare.DVDRef) AND (tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef)
WHERE (((tblDVDInfo.Situation)=4) AND ((tblDVDInfo.Wrong)=False));
")	# contains duplicates (same DVD analyzed several times)

head(tblDVD_XlsFilesALLDBINFO)


close(conDB)

}

head(tblDVD_XlsFilesALLDBINFO)
tail(tblDVD_XlsFiles,30)


{### create list of filenames for New and Old Template: check those not included yet but that should be + special code to account for the fact that name extension not corrected to xlsx in DB

{## create list of file names from files analysed after 2012 included
	# not elegant but the DB will probably not change.
FilenamesAfter2012 <- sort(tblDVD_XlsFiles$Filename[tblDVD_XlsFiles$DVDRef >=2933 & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename]) 

}								   
	
head(FilenamesAfter2012)


{##  create list of file names from files analysed in 2010 and 2011 with the new template (from what I could see opening all the files)

filename1011_oldtemplate <- c(
"2010\\VJ0039.xls", "2010\\VJ0040.xls", "2010\\VJ0041.xls", "2010\\VJ0044.xls", "2010\\VJ0050.xls", "2010\\VJ0052.xls",
"2010\\VJ0058.xls", "2010\\VJ0059.xls", "2010\\VJ0060.xls", "2010\\VJ0064.xls", "2010\\VJ0066.xlsx", "2010\\VJ0067.xlsx",
"2010\\VJ0068.xlsx", "2010\\VJ0070.xls", "2010\\VJ0078.xls", "2010\\VJ0079.xls", "2010\\VJ0080.xls", "2010\\VJ0081.xls",
"2011\\VK0001.xls", "2011\\VK0002.xls", "2011\\VK0003.xls", "2011\\VK0005.xls", "2011\\VK0006.xls", "2011\\VK0007.xls",
"2011\\VK0010.xls", "2011\\VK0011.xls", "2011\\VK0012.xls", "2011\\VK0013.xls", "2011\\VK0017.xls", "2011\\VK0019.xls", "2011\\VK0020.xls",
"2011\\VK0021.xls", "2011\\VK0022.xls", "2011\\VK0024.xls", "2011\\VK0025.xls", "2011\\VK0026.xls", "2011\\VK0027.xls", "2011\\VK0028.xls",
"2011\\VK0029.xls", "2011\\VK0031.xls", "2011\\VK0034.xls", "2011\\VK0037.xls", "2011\\VK0038.xls", "2011\\VK0039.xls", "2011\\VK0040.xls",
"2011\\VK0041.xls", "2011\\VK0042.xls", "2011\\VK0044.xls", "2011\\VK0045.xls", "2011\\VK0046.xls", "2011\\VK0047.xls", "2011\\VK0048.xls",
"2011\\VK0050.xls", "2011\\VK0051.xls", "2011\\VK0056.xls", "2011\\VK0061.xls", "2011\\VK0062.xls", "2011\\VK0063.xls", "2011\\VK0067.xls",
"2011\\VK0069.xls", "2011\\VK0070.xls", "2011\\VK0072.xls",
"2011\\VK0101.xls", "2011\\VK0102.xls", "2011\\VK0103.xls",
"2011\\VK0105.xls", "2011\\VK0106.xls",
"2011\\VK0410.xls", "2011\\VK0412.xls", "2011\\VK0413.xls", "2011\\VK0416.xls",
"2011\\VK0418.xls", "2011\\VK0419.xls", "2011\\VK0421.xls", "2011\\VK0422.xls", "2011\\VK0423.xls"
)


Filenames1011newtemplate <- tblDVD_XlsFiles$Filename[grepl("2010|2011", tblDVD_XlsFiles$Filename) == TRUE & !tblDVD_XlsFiles$Filename%in%filename1011_oldtemplate & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename]
													# Filenames that contained 2010 or 2011 				and that do not belong to the list above				where situation = 4 (only chicks)

}

head(Filenames1011newtemplate)													
	
	
{## combine all files analyzed with the new template (will take newly analyzed files only if those are put in the root of the year folder, with a normal file name)

FilenamesNewTemplate <- c(as.character(Filenames1011newtemplate), as.character(FilenamesAfter2012))
length(FilenamesNewTemplate)	# 858 files, situation 4, new template

FilenamesNewTemplate <- gsub(".xlsx", ".xls",FilenamesNewTemplate) # as long as we do not have changed the names in the DB (xls to xlsx)
FilenamesNewTemplate <- gsub(".xls", ".xlsx",FilenamesNewTemplate) # as long as we do not have changed the names in the DB (xls to xlsx)

}


{## combined all files analyzed with old templates

{FilenamesOldTemplate <- tblDVD_XlsFiles$Filename[

# where situation = 4
tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename &

# years before 2010, or after 2010 but belonging to list created above
(tblDVD_XlsFiles$DVDRef <2016 | tblDVD_XlsFiles$Filename%in%filename1011_oldtemplate) & 

# exclude duplicates (take the one with data in DB tblParentalCare)
tblDVD_XlsFiles$Filename != "2004\\40001LM19.xls" & 
tblDVD_XlsFiles$Filename != "2004\\40032D.xls" &
tblDVD_XlsFiles$Filename != "2004\\40036.xls" & 
tblDVD_XlsFiles$Filename != "2004\\40039.xls" & 
tblDVD_XlsFiles$Filename != "2004\\40055S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40069S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40071S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40074S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40075S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40079S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40089S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40119S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40123S.xls" &
tblDVD_XlsFiles$Filename != "2004\\40133S.xls" &

# EXCLUDED BUT WITH DATA IN DB (COULD BE INCLUDED if rewatched)
tblDVD_XlsFiles$Filename != "2004\\40055.xls" & # files that contain comments that are not standardized (data in DB)
tblDVD_XlsFiles$Filename != "2004\\40061.xls" & # files that contain comments that are not standardized (data in DB)
tblDVD_XlsFiles$Filename != "2008\\80055.xls" & # file empty (data in DB)
tblDVD_XlsFiles$Filename != "2005\\50368-wrong.xls" & # why wrong ? (the copy '50368' file has data in DB)
tblDVD_XlsFiles$Filename != "2005\\50370-not sure.xls" &  # what is not sure ? (the copy '50370' file has data in DB)
tblDVD_XlsFiles$Filename != "2005\\50268.xls" & # commented: too difficult to distinguish nale and female (and therefore file is empty, DB parental care = line of NA)
tblDVD_XlsFiles$Filename != "2008\\SparrowData.mdb" # nonsense
] 
}

length(FilenamesOldTemplate)	# 888 files, situation 4, old template
which(duplicated(merge(x=data.frame(FilenamesOldTemplate), y=tblDVD_XlsFilesALLDBINFO[,c("DVDRef","Filename")], by.x= "FilenamesOldTemplate", by.y= "Filename",all.x=TRUE)[,"DVDRef"]))	# no duplicates of DVDRef

# as long as filenames in the DB are not updated...
FilenamesOldTemplate <- gsub(".xlsx", ".xls",FilenamesOldTemplate) # as long as we do not have changed the names in the DB (xls to xlsx)
FilenamesOldTemplate <- gsub(".xls", ".xlsx",FilenamesOldTemplate) # as long as we do not have changed the names in the DB (xls to xlsx)

}


}

head(FilenamesNewTemplate)
head(FilenamesOldTemplate)


require(openxlsx)
search() # make sure package 'xlsx' is not in the list

{### extraction data in Excel files analyzed with newest excel template (after conversion all files to xlsx) and error checking

out = list()
	
for (j in 1:length(FilenamesNewTemplate)){
filenamej <- paste(pathdropboxfolder, FilenamesNewTemplate[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheet="DVD NO") # read.xlsx function from library 'openxlsx' (not library 'xlsx'): make sure xlsx is not in the list given by 'search()'
b$Tin <- NA
b$Tout <- NA
b$Sex <- NA

for (i in 1:nrow(b)){

if (!is.na(b$F.in[i]) & is.na(b$M.in[i]))
{b$Tin[i] <- (as.numeric(as.character(b$F.in[i])))} 

if (is.na(b$F.in[i]) & !is.na(b$M.in[i]))
{b$Tin[i] <- (as.numeric(as.character(b$M.in[i])))}


if (!is.na(b$F.out[i]) & is.na(b$M.out[i]))
{b$Tout[i] <- (as.numeric(as.character(b$F.out[i])))}

if (is.na(b$F.out[i]) & !is.na(b$M.out[i]))
{b$Tout[i] <- (as.numeric(as.character(b$M.out[i])))}

if ((!is.na(b$F.in[i]) | !is.na(b$F.out[i])) & is.na(b$M.in[i]) & is.na(b$M.out[i])) # if one or the other Tin or Tout in 'female' column is not NA
{b$Sex[i] <- "0" }

if (is.na(b$F.in[i]) & is.na(b$F.out[i]) & (!is.na(b$M.in[i]) | !is.na(b$M.out[i]))) # if one or the other Tin or Tout in 'male' column is not NA
{b$Sex[i] <- "1" }

}

if(nrow(b[!is.na(b$Sex) & (!is.na(as.numeric(as.character(b$Tin))) | !is.na(as.numeric(as.character(b$Tout)))),])>0)
{b <- b[!is.na(b$Sex) & (!is.na(as.numeric(as.character(b$Tin))) | !is.na(as.numeric(as.character(b$Tout)))),c('Tin','Tout','Sex')]} # keep lines of data when sex was allocated and at least Tin or Tout (supress lines where only comments with a sex allocated)
else {b <- unique(b[,c('Tin','Tout','Sex')])} # keep one line of NAs + filename


b$Filename <- FilenamesNewTemplate[j]

out[[j]] <- b

}

combinedprovisioningNewTemplate = do.call(rbind, out)


{## error check for NewTemplate

length(unique(combinedprovisioningNewTemplate$Filename))	# 11/02/2016: 858 files, situation 4, new template

# missing Time
combinedprovisioningNewTemplate[is.na(combinedprovisioningNewTemplate$Tout) & !is.na(combinedprovisioningNewTemplate$Tin),] # should be 0 rows
combinedprovisioningNewTemplate[!is.na(combinedprovisioningNewTemplate$Tout) & is.na(combinedprovisioningNewTemplate$Tin),] # should be 0 rows

# error in chronology
combinedprovisioningNewTemplate[combinedprovisioningNewTemplate$Tout - combinedprovisioningNewTemplate$Tin < 0 ,] # should be Nas, number of lines = number of empty files (with no birds)

splitNewTemplates_byFilenames_bySex <- split(combinedprovisioningNewTemplate, paste(combinedprovisioningNewTemplate$Filename, combinedprovisioningNewTemplate$Sex))

splitNewTemplates_byFilenames_bySex_fun = function(x)  {
x$prevOut <- c(NA,x$Tout[-nrow(x)])
x$Diff_Tin_prevOut <- x$Tin-x$prevOut
return(x)
 }

splitNewTemplates_byFilenames_bySexout <- lapply(splitNewTemplates_byFilenames_bySex, FUN=splitNewTemplates_byFilenames_bySex_fun)
splitNewTemplates_byFilenames_bySext_df <- do.call(rbind, splitNewTemplates_byFilenames_bySexout)
rownames(splitNewTemplates_byFilenames_bySext_df) <- NULL
head(splitNewTemplates_byFilenames_bySext_df)
splitNewTemplates_byFilenames_bySext_df[splitNewTemplates_byFilenames_bySext_df$Diff_Tim_prevOut <0,]  # should be 0 rows

}

}

head(combinedprovisioningNewTemplate,100)


detach("package:openxlsx", unload=TRUE)
require(xlsx)
search()

{## extraction data in Excel files analyzed with newest excel template (after conversion all files to xlsx) and creation of lists of errors

FUNcellColor <- function(x) {
	fg  <- x$getFillForegroundXSSFColor()
	rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
	rgb <- paste(rgb, collapse = "")
	return(rgb)
}	

colornames <- list(blue = "00ffff", grey = "c0c0c0") # 'O' blue = feeding from outside ; 'O' grey = hanging around the NB

out3 <- list()
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

out3[[j]] <- bb
bb <- NULL

}





}

condout3 <- sapply(out3, function(x) length(x) > 1)
out3 <- out3[condout3]
length(out3)

combinedprovisioningOldTemplate = do.call(rbind, out3)


{# error check for OldTemplate 

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

head(combinedprovisioningOldTemplate, 100)


{### combine all data

combinedprovisioningNewTemplate$Template <- "New"
combinedprovisioningOldTemplate$Template <- "Old"
combinedprovisioningNewTemplate$Com <- NA
combinedprovisioningNewTemplate$Col <- NA

combinedprovisioningALL <- rbind(combinedprovisioningOldTemplate,combinedprovisioningNewTemplate)

## write.table(combinedprovisioningALL, file = "R_combinedprovisioningALL.xls", col.names=TRUE, sep='\t')
# after running the line above:
# I save the xls file into a xlsx file
# shift the headers one cell right
# rename the first column 'order' 
# > not elegant but write.xlsx from openxlsx isn't working for me
}

head(combinedprovisioningALL, 100)
tail(combinedprovisioningALL, 100)





{### recreate tblParentalCare

head(tblDVD_XlsFilesALLDBINFO)
summary(tblDVD_XlsFilesALLDBINFO)
length(unique(combinedprovisioningALL$Filename))	# 1527
length(unique(tblDVD_XlsFilesALLDBINFO$Filename))	# 1561


combinedprovisioningALL_listperFilename <- split(combinedprovisioningALL,combinedprovisioningALL$Filename)
x <- combinedprovisioningALL_listperFilename[[6]]

combinedprovisioningALL_listperFilename_fun = function(x)  {
x <- x[order(x$Tin, -x$Tout),]
x$unattended <-0
x$attended <-0
x$dblattended <-0

# for normal sequence where next Tin is after last Tout
for (i in 1:(nrow(x)-1))
{
	if (x$Tin[i+1] >= x$Tout[i])
	{x$unattended[i] <- round(x$Tin[i+1]-x$Tout[i],2)
	x$attended[i] <- x$Tout[i]-x$Tin[i]
	}

# for the last row to get unattended time: substract from the TapeTime
	if (tblDVD_XlsFilesALLDBINFO$TapeTime[as.character(tblDVD_XlsFilesALLDBINFO$Filename) == as.character(x$Filename[1])] > x$Tout[nrow(x)])  	# added 'as.character' otherwise compare 2 factors who do not have the same number of categories so R can't make it...
	x$unattended[nrow(x)] <- round(tblDVD_XlsFilesALLDBINFO$TapeTime[as.character(tblDVD_XlsFilesALLDBINFO$Filename) == as.character(x$Filename[1])] - x$Tout[nrow(x)],2)
}

# when one sex visit while the other partner is in
for (i in 2:(nrow(x)))
{
if (x$Tin[i] < x$Tout[i-1] & x$Sex[i]!=x$Sex[i-1])
{x$dblattended[i] <- x$Tout[i-1]-x$Tin[i]
}
}



return(c(
nrow(x),
sum(x$attended),
sum(x$unattended),
tblDVD_XlsFilesALLDBINFO$EffectTime[as.character(tblDVD_XlsFilesALLDBINFO$Filename) == as.character(x$Filename[1])] 
))
}

combinedprovisioningALL_listperFilename_out1 <- lapply(combinedprovisioningALL_listperFilename, FUN=combinedprovisioningALL_listperFilename_fun)
combinedprovisioningALL_listperFilename_out2 <- data.frame(rownames(do.call(rbind,combinedprovisioningALL_listperFilename_out1)),do.call(rbind, combinedprovisioningALL_listperFilename_out1))

nrow(combinedprovisioningALL_listperFilename_out2)	# 216
rownames(combinedprovisioningALL_listperFilename_out2) <- NULL
colnames(combinedprovisioningALL_listperFilename_out2) <- c('', '','')

ParentalCare <- merge(x=combinedprovisioningALL_listperFilename_out2, 
y = tblDVD_XlsFilesALLDBINFO[,c('DVDRef', 'Filename','BroodRef','OffspringNo','DVDdate','DVDtime','Weather','Wind','TapeTime','EffectTime','Method','Observer')], by = 'Filename', all.x=TRUE)

head(ParentalCare)
nrow(ParentalCare) # 216
}























