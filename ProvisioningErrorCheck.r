#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 check provisioning data 
#   (where situation = 4 (chicks) in DVD info)
#	 Start : 20/01/2016
#	 last modif : 21/01/2016  
#    for sparrow meeting Februray 2016
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#RQ
# as we are changing the files directly on dropbox,  we need to keep the original copies somewhere on a permanent google drive



rm(list = ls(all = TRUE))

library(RODBC)

pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"
pathlocalfolder <- "C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination"

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")


##### DB Checks
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




##### Excel files Checks
{

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