#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Compile provisioning data sparrows
#	 Start : 20/01/2016
#	 last modif : 20/01/2016  
#    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))

library(RODBC)

pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"
pathlocalfolder <- "C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination"

conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")




Duplicate_tblDVD_XlsFiles_DVDRef <- sqlQuery(conDB, "
SELECT First(tblDVD_XlsFiles.[DVDRef]) AS [DVDRef Field], Count(tblDVD_XlsFiles.[DVDRef]) AS NumberOfDups, First(tblDVD_XlsFiles.Filename) AS FirstOfFilename, Last(tblDVD_XlsFiles.Filename) AS LastOfFilename, tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.Observer, tblParentalCare.Notes
FROM tblDVD_XlsFiles INNER JOIN tblParentalCare ON tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef
GROUP BY tblDVD_XlsFiles.[DVDRef], tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.Observer, tblParentalCare.Notes
HAVING (((Count(tblDVD_XlsFiles.[DVDRef]))>1));
")

# take the excel file that is entered in parental care, or the most standardized version, put the other file in a 'junk' subfolder






Unmatched_tblDVD_XlsFiles_DVDRef_in_tblParentalCare <- sqlQuery(conDB, "
SELECT tblDVD_XlsFiles.DVDRef, tblDVD_XlsFiles.Filename, tblDVDInfo.DVDNumber
FROM tblDVDInfo INNER JOIN (tblDVD_XlsFiles LEFT JOIN tblParentalCare ON tblDVD_XlsFiles.[DVDRef] = tblParentalCare.[DVDRef]) ON tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef
WHERE (((tblParentalCare.DVDRef) Is Null));
")

# decide whether to enter data in ParentalCare or not. If not, put the file in the 'junk' folder





Unmatched_tblParentalCare_DVDRef_in_tblDVD_XlsFiles <- sqlQuery(conDB, "
SELECT tblParentalCare.DVDRef, tblParentalCare.Notes, tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblDVDInfo.DVDNumber
FROM tblDVDInfo INNER JOIN (tblParentalCare LEFT JOIN tblDVD_XlsFiles ON tblParentalCare.[DVDRef] = tblDVD_XlsFiles.[DVDRef]) ON tblDVDInfo.DVDRef = tblParentalCare.DVDRef
WHERE (((tblDVD_XlsFiles.DVDRef) Is Null));
")

# what about those ? 
# data entered manually in tblParental care ?
# what about 'no visits' ?




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






require(xlsx)
excelfilelists <- read.xlsx("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination\\excelfilelists.xlsx", sheetIndex=1) # got list of filenames in each year by following DOS command: http://spreadsheetpage.com/index.php/tip/getting_a_list_of_file_names/
head(excelfilelists)

excelfilelists$NewFilename[!excelfilelists$NewFilename%in%List_AlltblParentalCare_DVDRef$DVDNumber]

# check why those are not in DB, if should not be, move to 'junk' folder



