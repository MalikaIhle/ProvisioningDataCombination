#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 check provisioning data 
#   (where situation = 4 (chicks) in DVD info)
#	 Start : 20/01/2016
#	 last modif : 26/01/2016  
#    for sparrow meeting Februray 2016
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#RQ
# as we are changing the files directly on dropbox,  we need to keep the original copies somewhere on a permanent google drive
# we need to figure out what is this 'wrong Method YN' about, I create an excel files that have the list of provisioning excel files in it, with columns having 'featurex YN' to tick.


rm(list = ls(all = TRUE))

library(RODBC)

pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"
pathlocalfolder <- "C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination"

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")


###### DB Checks
{

### Wich duplicate of video analysed was entered in tbl Parental care ?
{
Duplicate_tblDVD_XlsFiles_DVDRef <- sqlQuery(conDB, "
SELECT First(tblDVD_XlsFiles.[DVDRef]) AS [DVDRef Field], Count(tblDVD_XlsFiles.[DVDRef]) AS NumberOfDups, First(tblDVD_XlsFiles.Filename) AS FirstOfFilename, Last(tblDVD_XlsFiles.Filename) AS LastOfFilename, tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.Observer, tblParentalCare.Notes
FROM tblDVD_XlsFiles INNER JOIN tblParentalCare ON tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef
GROUP BY tblDVD_XlsFiles.[DVDRef], tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.Observer, tblParentalCare.Notes
HAVING (((Count(tblDVD_XlsFiles.[DVDRef]))>1));
")

# take the excel file that is entered in parental care (and/or hopefully the most standardized version), put the other file in a 'junk' subfolder
# delete entry for the duplicate in tblDVD_XlsFiles (leave the unique entry in tblDVDInfo)

			# so far (in the script only) I excluded those duplicates > my rule was 'take the file with the normal name' assuming this would be the one in the DB. 
			# need to check that this is the case
			tblDVD_XlsFiles$Filename != "2004\\40001LM19.xls" & 
			tblDVD_XlsFiles$Filename != "2004\\40032.xls" & # select D file as the 'normally named' file was not presented in the standardized way
			tblDVD_XlsFiles$Filename != "2004\\40036.xls" & # select D file as the 'normally named' file was not presented in the standardized way
			tblDVD_XlsFiles$Filename != "2004\\40039.xls" & # select D file as the 'normally named' file was not presented in the standardized way
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
}




### Wich excel file was lifted but with no data in tbl Parental care ?
{
Unmatched_tblDVD_XlsFiles_DVDRef_in_tblParentalCare <- sqlQuery(conDB, "
SELECT tblDVD_XlsFiles.DVDRef, tblDVD_XlsFiles.Filename, tblDVDInfo.DVDNumber
FROM tblDVDInfo INNER JOIN (tblDVD_XlsFiles LEFT JOIN tblParentalCare ON tblDVD_XlsFiles.[DVDRef] = tblParentalCare.[DVDRef]) ON tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef
WHERE (((tblParentalCare.DVDRef) Is Null));
")

# decide whether to enter data in ParentalCare or not. If not, put the file in the 'junk' folder. 
# delete the entry in tblDVD_XlsFiles (leave entry in tblDVDInfo)
}




### Files with no visits because of pb with video or because of bad parental care ?
{
Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles <- sqlQuery(conDB, "
SELECT tblParentalCare.DVDRef, tblParentalCare.Notes, tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblDVDInfo.DVDNumber
FROM tblDVDInfo INNER JOIN (tblParentalCare LEFT JOIN tblDVD_XlsFiles ON tblParentalCare.[DVDRef] = tblDVD_XlsFiles.[DVDRef]) ON tblDVDInfo.DVDRef = tblParentalCare.DVDRef
WHERE (((tblDVD_XlsFiles.DVDRef) Is Null));
")


	# remove data apparently entered manually in tblParental care
	Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles_RemovingDataApparentlyEnteredManually <- Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles[
	(Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles$MTime + Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles$FTime) == 0 
	| is.na(Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles$MTime) ,]
	
	Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles_RemovingDataApparentlyEnteredManually[
	order(Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles_RemovingDataApparentlyEnteredManually$Notes,
	Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles_RemovingDataApparentlyEnteredManually$TapeTime ),]

	# check whether those with NA and comments should be with 0 (no visits but video watchable?)
	# check if those with zero but with TapeTime should be NA ? add comments if there isn't ?
	# check those with NA and without comments 

}
	
	
	
	
### Is DVD Number the real Filename ?
{
List_AlltblParentalCare_DVDRef <- sqlQuery(conDB, "
SELECT tblParentalCare.DVDRef, tblDVDInfo.DVDNumber, tblDVD_XlsFiles.Filename
FROM (tblDVDInfo INNER JOIN tblParentalCare ON tblDVDInfo.DVDRef = tblParentalCare.DVDRef) LEFT JOIN tblDVD_XlsFiles ON tblParentalCare.DVDRef = tblDVD_XlsFiles.DVDRef;
")
head(List_AlltblParentalCare_DVDRef)
List_AlltblParentalCare_DVDRef$NewFilename <- gsub(".xlsx", "", List_AlltblParentalCare_DVDRef$Filename )
List_AlltblParentalCare_DVDRef$NewFilename <- gsub(".xls", "", List_AlltblParentalCare_DVDRef$NewFilename )
List_AlltblParentalCare_DVDRef$NewFilename <- substr(List_AlltblParentalCare_DVDRef$Filename, 6, nchar(List_AlltblParentalCare_DVDRef$NewFilename) )


Unmatched_DVDNumber_NewFilename <- List_AlltblParentalCare_DVDRef[as.character(List_AlltblParentalCare_DVDRef$DVDNumber) != List_AlltblParentalCare_DVDRef$NewFilename,]
Unmatched_DVDNumber_NewFilename <- Unmatched_DVDNumber_NewFilename[complete.cases(Unmatched_DVDNumber_NewFilename),]
Unmatched_DVDNumber_NewFilename

# check with duplicates above, if all were taken care of

}




### Which Excel files not in DB ?
{

excelfilelists <- list()

for (j in 2004:2014){
pathyearfolder <- paste(pathdropboxfolder, j, sep="\\DVDs ")
excelfilelists[[j]] <- data.frame(list.files(pathyearfolder))
}

excelfilelists <- do.call(rbind, excelfilelists)
colnames(excelfilelists) <- "Filename"
head(excelfilelists)

excelfilelists$NewFilename <-gsub(".xlsx", "", excelfilelists$Filename )
excelfilelists$NewFilename <- gsub(".xls", "", excelfilelists$NewFilename )
head(excelfilelists)

excelfilelists$NewFilename[!excelfilelists$NewFilename%in%List_AlltblParentalCare_DVDRef$DVDNumber]

# check if and why those are not in DB, if should not be, move to 'junk' folder


}




}




###### Excel files Checks
{

{##### compiled list by hand

### Files to check Time Chronology (need to reopen video) > correct excel + DB summary if affected
{
## not done > see with DVD (I've got most of them)
{
40119 # should check time in and out at the end for female > does not make sense
40239 # should check time in and out at the end for female > does not make sense
50176 # should check what's supposed to be in F19
50548 # should check time in and out at the end for female > does not make sense
50598 # should check time in and out in the middle for male > does not make sense
VK0027 # should check time in and out at the end for male > does not make sense
40063 # check whether female and male stay when alternate
}


## already done by me for conveniency > check accuracy (don't have DVD yet) >remove purple colour (I will need tyo extract grey)
{
 40121 # change B5 from 22 to 21 as the bird entered 'later' than he exited
 40172 # change H22 from 51.2 to 52
 40200 # change H45 from 6.9 to 69.9
 40261 # change D36 from 59 to 49
 40269 # change D22 from 17.7 to 14.7
 40307 # change H97 from 78.6 to 77.6
 40391 # change H43 from 43.8 to 43.4
 40454 # change B79 from 74.8 to 74.4
 40512 # change F7 from 24.6 to 20.8
 40558 # change D28 from 88.1 to 88.9
 50161 # change F48 from 80.9 to 90.9 and H49 from 81.3 to 91.3
 50189 # change B59 from 83.7 to 84.7
 50191 # change F32 from 46.5 to 45.8
 50204 # change F32 from 48.7 to 45.7
 50209 # change H37 from 95.9 to 92.9
 50211 # change D12 from 61.3 to 61.1
 50232 # change B48 from 59.5 to 59.7
 50580 # change H45 from 40 to 48
 50598 # change F17 from 20.8 to 21.8
 60018 # change H28 from 48.5 to 48.3
 60063 # change B7 from 12.5 to 12.9
 60137 # change F22 from 18.7 to 18.5
 70106 # change F64 from 72.2 to 82.2, D65 from 72.4 to 82.4
 70108 # change H28 from 61.3 to 71.30
 VK0010 # change E37 and I37 from 51.2 to 51.6
 VK0041 # change G70 from 77.9 to 77.8
 VK0070 # change I45 from 84.3 to 85.3
 VK0102 # change E57 from 83.1 to 84.1, I55 from 70.2 to 80.2
}


## to be done > see with DVD (don't have DVD yet)
{

# combinedprovisioningNewTemplate[combinedprovisioningNewTemplate$Tout - combinedprovisioningNewTemplate$Tin < 0,]

VK0212 # Time Out before Time in: 65.1 64.3
VL0271 # Time Out before Time in: 31.9 31.1
VN0158 # Time Out before Time in: 46.0 43.5


}


}


### File excluded from my code for diverse reason (mainly, need to reformat)
{
40055 # comments that are not standardized 
40061 # comments that are not standardized 

VK0293 # reformat to old template
VK0296 # reformat to old template
VK0299 # reformat to old template

80055 # excel file empty (data in DB) -  corrupted ?? 

50268 # commented: too difficult to distinguish male and female (and therefore file is empty) > put in junk folder ?
}

}


{##### list or warningzz - run code

{## piece to run before running error check

{# packages + DB
library(RODBC)
library(xlsx)	
require(zoo)

pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"


conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")
tblDVD_XlsFiles <- sqlFetch(conDB, "tblDVD_XlsFiles")
tblDVD_XlsFiles <- tblDVD_XlsFiles[with(tblDVD_XlsFiles, order(tblDVD_XlsFiles$Filename)),]

# select vedo made when provisioning chick (situation = 4 )
tblDVD_XlsFilesALLDBINFO <- sqlQuery(conDB, "
SELECT tblDVD_XlsFiles.DVDRef, tblDVD_XlsFiles.Filename, tblDVDInfo.BroodRef, tblDVDInfo.Situation, tblDVDInfo.Deaths, tblDVDInfo.OffspringNo, tblDVDInfo.Age, tblDVDInfo.Wrong, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.Weather, tblDVDInfo.Wind, tblDVDInfo.Notes, tblParentalCare.TapeTime, tblParentalCare.EffectTime, tblParentalCare.Method, tblParentalCare.Observer, tblParentalCare.Notes, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.ShareTime, tblParentalCare.MVisit1, tblParentalCare.FVisit1, tblParentalCare.MVisit2, tblParentalCare.FVisit2, tblParentalCare.MBout, tblParentalCare.FBout
FROM tblDVDInfo INNER JOIN (tblDVD_XlsFiles INNER JOIN tblParentalCare ON tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef) ON (tblDVDInfo.DVDRef = tblParentalCare.DVDRef) AND (tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef)
WHERE (((tblDVDInfo.Situation)=4) AND ((tblDVDInfo.Wrong)=False));
")	# contains duplicates (same DVD analyzed several times)


close(conDB)
}

{## create list of filenames

filename1011_oldtemplate <- c(
"2010\\VJ0039.xls", "2010\\VJ0040.xls", "2010\\VJ0041.xls", "2010\\VJ0044.xls", "2010\\VJ0050.xls", "2010\\VJ0052.xls",
"2010\\VJ0058.xls", "2010\\VJ0059.xls", "2010\\VJ0060.xls", "2010\\VJ0064.xls", "2010\\VJ0066.xlsx", "2010\\VJ0067.xlsx",
"2010\\VJ0068.xlsx", "2010\\VJ0070.xls", "2010\\VJ0078.xls", "2010\\VJ0079.xls", "2010\\VJ0080.xls", "2010\\VJ0081.xls",
"2011\\VK0001.xls", "2011\\VK0002.xls", "2011\\VK0003.xls", "2011\\VK0005.xls", "2011\\VK0006.xls",
"2011\\VK0010.xls", "2011\\VK0011.xls", "2011\\VK0012.xls", "2011\\VK0013.xls", "2011\\VK0017.xls", "2011\\VK0019.xls", "2011\\VK0020.xls",
"2011\\VK0021.xls", "2011\\VK0022.xls", "2011\\VK0024.xls", "2011\\VK0025.xls", "2011\\VK0026.xls", "2011\\VK0027.xls", "2011\\VK0028.xls",
"2011\\VK0029.xls", "2011\\VK0031.xls", "2011\\VK0034.xls", "2011\\VK0037.xls", "2011\\VK0038.xls", "2011\\VK0039.xls", "2011\\VK0040.xls",
"2011\\VK0041.xls", "2011\\VK0042.xls", "2011\\VK0044.xls", "2011\\VK0045.xls", "2011\\VK0046.xls", "2011\\VK0047.xls", "2011\\VK0048.xls",
"2011\\VK0050.xls", "2011\\VK0051.xls", "2011\\VK0056.xls", "2011\\VK0061.xls", "2011\\VK0062.xls", "2011\\VK0063.xls", "2011\\VK0067.xls",
"2011\\VK0069.xls", "2011\\VK0070.xls", "2011\\VK0072.xls",
"2011\\VK0101.xls", "2011\\VK0102.xls", "2011\\VK0103.xls",
"2011\\VK0105.xls", "2011\\VK0106.xls",
"2011\\VK0410.xls", "2011\\VK0412.xls", "2011\\VK0413.xls", "2011\\VK0416.xls",
"2011\\VK0418.xls", "2011\\VK0419.xls", "2011\\VK0421.xls", "2011\\VK0422.xls", "2011\\VK0423.xls",

	# those have yet another template
"2011\\VK0293.xls",				
"2011\\VK0296.xls", "2011\\VK0299.xls"
)

{FilenamesOldTemplate <- tblDVD_XlsFiles$Filename[

# where situation = 4
tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename &

# years before 2010, or after 2010 but belonging to list created above
(tblDVD_XlsFiles$DVDRef <2016 | tblDVD_XlsFiles$Filename%in%filename1011_oldtemplate) & 

# exclude duplicates
tblDVD_XlsFiles$Filename != "2004\\40001LM19.xls" & 
tblDVD_XlsFiles$Filename != "2004\\40032.xls" & # select D file as the 'normally named' file was not presented in the standardized way
tblDVD_XlsFiles$Filename != "2004\\40036.xls" & # select D file as the 'normally named' file was not presented in the standardized way
tblDVD_XlsFiles$Filename != "2004\\40039.xls" & # select D file as the 'normally named' file was not presented in the standardized way
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

######### FILE THAT SHOULD BE INCLUDED
# excluded for the moment: files that contain comments that are not standardized 
tblDVD_XlsFiles$Filename != "2004\\40055.xls" &
tblDVD_XlsFiles$Filename != "2004\\40061.xls" &

# file with yet another template:
tblDVD_XlsFiles$Filename != "2011\\VK0293.xls" &
tblDVD_XlsFiles$Filename != "2011\\VK0296.xls" &
tblDVD_XlsFiles$Filename != "2011\\VK0299.xls" &

tblDVD_XlsFiles$Filename != "2008\\80055.xls" & # file empty or in another format ?? (data in DB)

tblDVD_XlsFiles$Filename != "2004\\40119.xls" & # should check time in and out at the end for female > does not make sense
tblDVD_XlsFiles$Filename != "2004\\40239.xls" & # should check time in and out at the end for female > does not make sense
tblDVD_XlsFiles$Filename != "2005\\50176.xls" &	# should check what's supposed to be in F19
tblDVD_XlsFiles$Filename != "2005\\50548.xls" & # should check time in and out at the end for female > does not make sense
tblDVD_XlsFiles$Filename != "2005\\50598.xls" & # should check time in and out in the middle for male > does not make sense
tblDVD_XlsFiles$Filename != "2011\\VK0027.xls" & # should check time in and out at the end for male > does not make sense

tblDVD_XlsFiles$Filename != "2005\\50268.xls" & # commented: too difficult to distinguish nale and female (and therefore file is empty)
########

tblDVD_XlsFiles$Filename != "2005\\50368-wrong.xls" & 
tblDVD_XlsFiles$Filename != "2005\\50370-not sure.xls" & 
tblDVD_XlsFiles$Filename != "2008\\SparrowData.mdb"
] 
}

length(FilenamesOldTemplate)	# 882 files, situation 4, old template
which(duplicated(merge(x=data.frame(FilenamesOldTemplate), y=tblDVD_XlsFilesALLDBINFO[,c("DVDRef","Filename")], by.x= "FilenamesOldTemplate", by.y= "Filename",all.x=TRUE)[,"DVDRef"]))	# no duplicates of DVDRef


FilenamesOldTemplateXLSX <- FilenamesOldTemplate[grepl("xlsx", FilenamesOldTemplate) == TRUE]	# only 3

FilenamesOldTemplateXLS <- FilenamesOldTemplate[grepl("xlsx", FilenamesOldTemplate) == FALSE]
length(FilenamesOldTemplateXLS) # 848
}

}


{## create for each excel file with an old template, a table bb containing: Tin, Tout, Sex and Filename and a list of warningz and warningzz

options(warn=2)	# convert warning into error and therefore stop the loop when it happen

out3 <- list()
warningz <- list()
warningzz <- list()
	
for (j in 1:length(FilenamesOldTemplateXLS)){

filenamej <- paste(pathdropboxfolder, FilenamesOldTemplateXLS[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheetIndex =2) # read.xlsx function from library 'xlsx' (not library 'openxlsx'): make sure openxlsx is not in the list given by 'search()'
warningz[[j]] <- as.character(FilenamesOldTemplateXLS[j])
warningzz[[j]] <- as.character(FilenamesOldTemplateXLS[j])

{### warningz in comments

for (i in 1:nrow(b))
{
# check if bird IN at beginning or end in TinCom
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
# accept comment O	############ AT THE MOMENT I DO NOT HAVE ITS COLOR !!!!!! ####################
if (!is.na(bbF$ToutCom[i]) & bbF$ToutCom[i] == "O")
{bbF$Com[i] <- "O"}

# change ToutCom from IN, S or G, into IN
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

{### if no warningzz in chronology: combine both female and male visits

if (length(warningz[[j]])==1 & length(warningzz[[j]])==1)
{
# when no bird ever visited, keep a line with NA
if (nrow(bbF)== 0  & nrow(bbM)== 0)	
{bb <- data.frame(rbind(c(NA,NA,NA,NA,NA)))
colnames(bb) <- c('Tin','Tout','Sex','Com','Filename')
}

# otherwise combine both sex visits and order by Tin then Tout
bbF <- bbF[,c('Tin','Tout','Sex','Com')]
bbM <- bbM[,c('Tin','Tout','Sex','Com')]

if(nrow(bbF)!= 0 | nrow(bbM)!= 0)
{
bb <- rbind(bbF, bbM)
bb <- bb[with(bb,order(bb$Tin, bb$Tout)),] 
 }

# add filename
bb$Filename <- as.character(FilenamesOldTemplateXLS[j])

}

out3[[j]] <- bb
bb <- NULL
}



}


condwarningz <- sapply(warningz, function(x) length(x) > 1)
warningz <- warningz[condwarningz]
condwarningzz <- sapply(warningzz, function(x) length(x) > 1)
warningzz <- warningzz[condwarningzz]
condout3 <- sapply(out3, function(x) length(x) > 1)
out3 <- out3[condout3]

warningz
warningzz
length(out3)

capture.output(warningz, file="warningz20160126.txt") 

condwarningzBirdIN <- sapply(warningz, function(x) x[2] == "bird IN at end of video: please write Tout, move 'IN' into TouCom" | x[3] == "bird IN at end of video: please write Tout, move 'IN' into TouCom" )
warningzBirdIN <- warningz[condwarningzBirdIN]
condwarningzBirdIN2 <- sapply(warningzBirdIN, function(x) length(x) > 1)
warningzBirdIN <- warningzBirdIN[condwarningzBirdIN2]


condwarningzMissInfo <- sapply(warningz, function(x) x[2] == "missing info in Tout com  !" | x[2] == "file starts with Fout !"| x[2] == "file starts with Mout !")
warningzMissInfo <- warningz[condwarningzMissInfo]

condwarningzOthers <- sapply(warningz, function(x) x[2] != "missing info in Tout com  !" &  x[2] != "bird IN at end of video: please write Tout, move 'IN' into TouCom")
warningzOthers <- warningz[condwarningzOthers]


combinedprovisioningOldTemplate = do.call(rbind, out3)

}

}


}













