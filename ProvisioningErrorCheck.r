#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 check provisioning data 
#   (where situation = 4 (chicks) in DVD info)
#	 Start : 20/01/2016
#	 last modif : 11/02/2016  
#    Sparrow meeting February 2016
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#RQ
# the original copies wre backedup on the HS google drive
# all xls files were converted to xlsx


rm(list = ls(all = TRUE))

library(RODBC)

pathdropboxfolder <- "C:\\Users\\mihle\\\\Dropbox\\Sparrow Lundy\\Sparrow video files"
conDB= odbcConnectAccess("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\db\\SparrowData.mdb")


###### DB Checks
{

### Wich duplicate of video analysed was entered in tbl Parental care ? DONE
{
Duplicate_tblDVD_XlsFiles_DVDRef <- sqlQuery(conDB, "
SELECT First(tblDVD_XlsFiles.[DVDRef]) AS [DVDRef Field], Count(tblDVD_XlsFiles.[DVDRef]) AS NumberOfDups, First(tblDVD_XlsFiles.Filename) AS FirstOfFilename, Last(tblDVD_XlsFiles.Filename) AS LastOfFilename, tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.Observer, tblParentalCare.Notes
FROM tblDVD_XlsFiles INNER JOIN tblParentalCare ON tblDVD_XlsFiles.DVDRef = tblParentalCare.DVDRef
GROUP BY tblDVD_XlsFiles.[DVDRef], tblParentalCare.TapeTime, tblParentalCare.MTime, tblParentalCare.FTime, tblParentalCare.Observer, tblParentalCare.Notes
HAVING (((Count(tblDVD_XlsFiles.[DVDRef]))>1));
")

# take the excel file that is entered in parental care (and/or hopefully the most standardized version), put the other file in a 'junk' subfolder

# output:
# those in the DB are:
40001LM18
40032
40036D
40039D
40055
40069
40071
40074
40075
40079
40089
40119
40123
40133
50368 (just a copy of the 'wrong' > add a comment in DB !)
50370 (just a copy of the 'not sure' > add a comment in DB !)
50408
60029D

# those move to junk folder are:
40001LM19 # this has been moved back to the root of the folder, new line created in DVD info > the code should now read it and latter the data will get into the new table parental care
40032D # deleted from tblDVDXlsFiles
40036 # deleted from tblDVDXlsFiles
40039 # deleted from tblDVDXlsFiles
40055S # deleted from tblDVDXlsFiles
40069S # deleted from tblDVDXlsFiles
40071S # deleted from tblDVDXlsFiles
40074S # deleted from tblDVDXlsFiles
40075S # deleted from tblDVDXlsFiles
40079S # deleted from tblDVDXlsFiles
40089S # deleted from tblDVDXlsFiles
40119S # deleted from tblDVDXlsFiles
40123S # deleted from tblDVDXlsFiles
40133S # deleted from tblDVDXlsFiles
50368-wrong # deleted from tblDVDXlsFiles
50370-not sure # deleted from tblDVDXlsFiles
50408-D # deleted from tblDVDXlsFiles
60029J # deleted from tblDVDXlsFiles




###### >>>> DB IMPROVEMENT POSSIBLE: delete entry for the duplicate in tblDVD_XlsFiles (leave the unique entry in tblDVDInfo)

}




### Wich excel file was lifted but with no data in tbl Parental care ? DONE
{
Unmatched_tblDVD_XlsFiles_DVDRef_in_tblParentalCare <- sqlQuery(conDB, "
SELECT tblDVD_XlsFiles.DVDRef, tblDVD_XlsFiles.Filename, tblDVDInfo.DVDNumber
FROM tblDVDInfo INNER JOIN (tblDVD_XlsFiles LEFT JOIN tblParentalCare ON tblDVD_XlsFiles.[DVDRef] = tblParentalCare.[DVDRef]) ON tblDVDInfo.DVDRef = tblDVD_XlsFiles.DVDRef
WHERE (((tblParentalCare.DVDRef) Is Null));
")

# decide whether to enter data in ParentalCare or not. If not, put the file in the 'junk' folder. 
# delete the entry in tblDVD_XlsFiles (leave entry in tblDVDInfo + comment)


# output: 
#   DVDRef              Filename DVDNumber
# 1    195       2004\\40195.xls     40195		# excel with data - but no chicks so keep data empty in DB is ok ? > NOT TO ENTER  # excel file moved to junk folder ; deleted row in tblDVD_XlsFiles ; comment in DVD info
# 2   1144       2005\\50587.xls     50587		# excel with data - no reasons not to enter them in DB ? 1 chick alive at time of video > SHOULD BE ENTERED
# 3   1582       2006\\60011.xls     60011		# excel with data - there only was eggs ever and there is an accident > nest was abandonned ? > NOT TO ENTER  # excel file moved to junk folder ; deleted row in tblDVD_XlsFiles ; comment in DVD info
# 4   1587       2006\\60016.xls     60016		# excel with data - no reasons not to enter them in DB ? very few visits, mostly hanging around > no longer incubating their eggs > NOT TO ENTER  # excel file moved to junk folder ; deleted row in tblDVD_XlsFiles ; comment in DVD info
# 5   1674       2006\\60103.xls     60103		# excel with data - but tape length = 13 min (camera fell) > SHOULD BE ENTERED
# 6   2016 2008\\SparrowData.mdb     80073		# to delete # deleted

}




### Files with no visits because of pb with video or because of bad parental care ?  >>>>>  MORE WORK TO BE DONE
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
	
	
# partial output
# 90091 and 90098 excel files do not exit > TO DELETE ? in parental care check N drive for excel files ? check if DVD exist ?

# in fact: many files from 2005, and all files from 2009 dont have entries in tblXlsFiles(and did not go through my code before mid-march 2016...) > should this be corrected ?	

}
	
	
### Files with video analyzed but with no entry in tblXlsFiles and therefore not included in compilation_provisioning until 20160314

	
	
	
	
### Is DVD Number the real Filename ? YES
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

# check with duplicates above, if all were taken care of: YES

}




### Which Excel files not in DB ? DONE
{

excelfilelists <- list()

for (j in 2004:2014){
pathyearfolder <- paste(pathdropboxfolder, j, sep="\\DVDs ")
excelfilelists[[j]] <- data.frame(list.files(pathyearfolder))
}

excelfilelists <- do.call(rbind, excelfilelists)
colnames(excelfilelists) <- "Filename"
head(excelfilelists)

excelfilelists$NewFilename <-gsub(".xlsm|.xlsx", "", excelfilelists$Filename )

head(excelfilelists)

ExcelNOTinDB <- excelfilelists$NewFilename[!excelfilelists$NewFilename%in%List_AlltblParentalCare_DVDRef$DVDNumber & excelfilelists$NewFilename!= "z_ToLeaveAside"]
data.frame(ExcelNOTinDB)

# check if and why those are not in DB, if should not be, move to 'junk' folder


1                     40001LM18	# DVD number is 40001		# ok
2                        40036D	# DVD number is 40036		# ok
3                        40039D # DVD number is 40039		# ok
8                        60029D # DVD number is 60029		# ok

4                         40195 # have entry in DVD_XlsFiles but not in paternal care (see above)	# dont'know why
5                         50587 # have entry in DVD_XlsFiles but not in paternal care (see above)	# dont'know why
6                         60011 # have entry in DVD_XlsFiles but not in paternal care (see above)	# dont'know why
7                         60016 # have entry in DVD_XlsFiles but not in paternal care (see above)	# dont'know why
9                         60103 # have entry in DVD_XlsFiles but not in paternal care (see above)	# dont'know why

10                        80000	# empty excel 				# moved to junk folder
11                       90028A # duplicate not in DB 		# moved to junk folder
12                      90052TR # duplicate with data in DB # move duplicate '90052' to junk folder
13                90056NOT90059 # duplicate with data in DB # move duplicate '90056' to junk folder

14                       VK0079 # data in file - Andy Turner - why not imported ?
15                       VK0080 # data in file - Andy Turner - why not imported ?
16                       VK0081 # data in file - Andy Turner - why not imported ?
17                       VK0082 # data in file - Andy Turner - what are those colors in there ?? > does not look different - why not imported ?
18                       VK0083 # data in file - Andy Turner - what are those colors in there ?? - why not imported ?
19                       VK0084 # data in file - Andy Turner - why not imported ?
20                       VK0085 # data in file - Andy Turner - why not imported ?
21                       VK0120 # data in file - Rob White - why not imported ?
22                       VK0121 # data in file - Rob White - why not imported ?
23                       VK0122 # data in file - Rob White - why not imported ?
24                       VK0125 # data in file - Rob White - why not imported ?
25                       VK0240 # data in file - Andy Turner - why not imported ?
26                       VK0241 # data in file - Andy Turner - why not imported ?
27                       VK0243 # data in file - Andy Turner - why not imported ?
28                       VK0244 # data in file - Andy Turner - why not imported ?
29                       VK0245 # data in file - Andy Turner - why not imported ?
30                       VK0246 # data in file - Andy Turner - why not imported ?
31                       VK0248 # data in file - Andy Turner - why not imported ?
32                       VK0249 # data in file - Andy Turner - why not imported ?
33                       VK0250 # data in file - Andy Turner - why not imported ?
34                       VK0251 # data in file - Andy Turner - why not imported ?
35                       VK0252 # data in file - Andy Turner - why not imported ?
36                       VK0254 # data in file - Andy Turner - why not imported ?
37                       VK0256 # data in file - Andy Turner - why not imported ?

38                       VK0337 # some data in file - EH - why not imported ?  - wild nest  bad angle							# moved to junk folder
39                       VK0365 # some data in file - EH - why not imported ? - wild nest  bad angle							# moved to junk folder
40                       VK0389 # empty file - EH - hasn't been watched ?? - wild nest - dark video  							# moved to junk folder
41                       VK0409 # empty file - EH -hasn't been watched ?? - wild nest  - hard to identify						# moved to junk folder
42                       VK0411 # data in file (within file, name is VK0410) - EH - why not imported ? - video is dark  		# moved to junk folder
43                       VK0420 # empty file - EH - hasn't been watched ??  - video is dark  									# moved to junk folder
44                       VK0424 # empty file - EH - hasn't been watched ?? (within file, name is VK0423) - video is dark  		# moved to junk folder
45                       VK0708 # file name within file is VK0408: just a copy to get the template? - EH - video does not exist # moved to junk folder

46       VL0331_impossiblevideo # have entry in DVD_XlsFiles but not in paternal care # moved to junk folder
47 VM0052 part-watched- blurred # have entry in DVD_XlsFiles but not in paternal care # moved to junk folder
48 VM0057 watched up to 43 mins # have entry in DVD_XlsFiles but not in paternal care # moved to junk folder

49                       VM0363 # data in file -  Adam Gosztonyi - why not imported ?
50                       VM0366 # data in file -  Adam Gosztonyi - why not imported ?
51                       VM0373 # data in file -  Adam Gosztonyi - why not imported ?
52                       VM0374 # data in file -  Adam Gosztonyi - why not imported ?
53                       VM0382 # data in file -  Adam Gosztonyi - why not imported ?
54                       VM0385 # data in file -  Adam Gosztonyi - why not imported ?
55                       VM0386 # data in file -  Adam Gosztonyi - why not imported ?
56                       VM0388 # data in file -  Adam Gosztonyi - why not imported ?
57                       VM0398 # data in file -  Adam Gosztonyi - why not imported ?
58                       VM0403 # data in file -  Adam Gosztonyi - why not imported ?
59                       VM0411 # data in file -  Adam Gosztonyi - why not imported ?
60                       VM0418 # data in file -  Adam Gosztonyi - why not imported ?
61                       VM0430 # data in file -  Adam Gosztonyi - why not imported ?
62                       VM0443 # data in file -  Adam Gosztonyi - why not imported ?
63                       VM0450 # data in file -  Adam Gosztonyi - why not imported ?
64                       VM0456 # data in file -  Adam Gosztonyi - why not imported ?

65          VM0470 - incomplete # data in parental care from duplicate 'VM0470' - should be deleted from DB ? # moved to junk folder
66          VM0470 - poor video # data in parental care from duplicate 'VM0470' - should be deleted from DB ? # moved to junk folder

67                       VM0498 # data in file -  Adam Gosztonyi - why not imported ?
68                       VM0499 # data in file -  Adam Gosztonyi - why not imported ?
69                       VM0504 # data in file -  Adam Gosztonyi - why not imported ?
70                       VM0506 # data in file -  Adam Gosztonyi - why not imported ?
71                       VM0521 # data in file -  Adam Gosztonyi - why not imported ?
72                       VM0523 # data in file -  Adam Gosztonyi - why not imported ?
73                       VM0526 # data in file -  Adam Gosztonyi - why not imported ?
74                       VM0528 # data in file -  Adam Gosztonyi - why not imported ?
75                       VM0534 # data in file -  Adam Gosztonyi - why not imported ?

76              VM0547-1minlong # move to junk folder

77                       VM0554 # data in file -  Adam Gosztonyi - why not imported ?
78                       VM0555 # data in file -  Adam Gosztonyi - why not imported ?
79                       VM0569 # data in file -  Adam Gosztonyi - why not imported ?
80                       VM0581 # data in file -  Adam Gosztonyi - why not imported ?
81                       VM0582 # data in file -  Adam Gosztonyi - why not imported ?
82                       VM0583 # data in file -  Adam Gosztonyi - why not imported ?
83                       VM0584 # data in file -  Adam Gosztonyi - why not imported ?
84                       VM0588 # data in file -  Adam Gosztonyi - why not imported ?
85                       VM0593 # data in file -  Adam Gosztonyi - why not imported ?
86                       VM0594 # data in file -  Adam Gosztonyi - why not imported ?
87                       VM0595 # data in file -  Adam Gosztonyi - why not imported ?
88                       VM0603 # data in file -  Adam Gosztonyi - why not imported ?
89                       VM0605 # data in file -  Adam Gosztonyi - why not imported ?
90                       VM0607 # data in file -  Adam Gosztonyi - why not imported ?
91                       VM0613 # data in file -  Adam Gosztonyi - why not imported ?
92                       VM0615 # data in file -  Adam Gosztonyi - why not imported ? some numbers have colors what is that ? > commented so OK

}



### Which DVDinfo situation 4, with chicks, with Wrong = 'No', not a wild nest (i.e. nest name = letter number and letter again or full word), removing personality videos, AND with no data in parental care / no excel file

DVDInfoWithoutMatchingParentalCare <- sqlQuery(conDB, "
SELECT tblDVDInfo.DVDRef, tblDVDInfo.DVDNumber, tblDVDInfo.BroodRef, tblDVDInfo.Situation, tblDVDInfo.OffspringNo, tblDVDInfo.Wrong, tblDVDInfo.DVDdate, tblDVDInfo.DVDtime, tblDVDInfo.Age, tblNestboxes.NestboxName
FROM tblNestboxes INNER JOIN (tblBroods INNER JOIN (tblDVDInfo LEFT JOIN tblParentalCare ON tblDVDInfo.[DVDRef] = tblParentalCare.[DVDRef]) ON tblBroods.BroodRef = tblDVDInfo.BroodRef) ON tblNestboxes.NestboxRef = tblBroods.NestboxRef
WHERE (((tblDVDInfo.Situation)=4) AND ((tblDVDInfo.OffspringNo)<>0) AND ((tblDVDInfo.Wrong)=False) AND ((tblDVDInfo.DVDdate)<#1/1/2015#) AND ((tblParentalCare.DVDRef) Is Null) AND ((tblDVDInfo.Notes) Is Null));
")

# among those, 2 third are wild nest, the others have no apparent reasons not to be watched


}




###### Excel files Checks > correct DB summary when affected
{

### Files to check Time Chronology (need to reopen video)

40119 # should check time in and out at the end for female > does not make sense
40239 # should check time in and out at the end for female > does not make sense				# file not found !!! > put logical time 
50176 # should check what's supposed to be in F19												# color changed to grey > DB: MVisits2 = 4 ; MVisits1 = 20
50548 # should check time in and out at the end for female > does not make sense				# FTime = 11.6 ; total 11.6 ; share time 0; percent 13.65 ; M:F 0.00 F:M 100.00 
VK0027 # should check time in and out at the end for male > does not make sense
40063 # check whether female and male stay when alternate



## already done by me for conveniency / corrected with real data by Andrew Jones

 40121 # change B5 from 22 to 21 as the bird entered 'later' than he exited
 40172 # change H22 from 51.2 to 52																# file not found !!!
 40200 # change H45 from 6.9 to 69.9															# file not found !!!
 40261 # change D36 from 59 to 49																# file not found !!!
 40269 # change D22 from 17.7 to 14.7															
 40307 # change H97 from 78.6 to 77.6
 40391 # change H43 from 43.8 to 43.4
 40454 # change B79 from 74.8 to 74.4
 40512 # change F7 from 24.6 to 20.8
 40558 # change D28 from 88.1 to 88.9
 50161 # change F48 from 80.9 to 90.9 and H49 from 81.3 to 91.3									# Nb visit 1 for male : 13 instead of 14
 50189 # change B59 from 83.7 to 84.7															# Nb visit1 for female = 28 (instead 26); for male visit1 = 16 (instead 14); FTime = 42 (instead 41.3); MTime =21 instead 23.7, total = 63; %=68.18, M:F = 33.33; F:M=66.67
 50191 # change F32 from 46.5 to 45.8
 50204 # change F32 from 48.7 to 45.7
 50209 # change H37 from 95.9 to 92.9
 50211 # change D12 from 61.3 to 61.1															# time delay from ~1.1 from B7 onward
 50232 # change B48 from 59.5 to 59.7
 50580 # change H45 from 40 to 48
 50598 # change F17 from 20.8 to 21.8															# Ftime = 44.1, MTime = 8.5, Share time = 2.8, Total = 52.6, %=53.36, M:F = 16.16; F:M=83.84
 60018 # change H28 from 48.5 to 48.3
 60063 # change B7 from 12.5 to 12.9															# FTime = 50.4, MTime = 18.7, Total = 69.1, M:F=27.06, F:M = 72.94, %=78.17
 60137 # change F22 from 18.7 to 18.5
 70106 # change F64 from 72.2 to 82.2, D65 from 72.4 to 82.4
 70108 # change H28 from 61.3 to 71.30
 VK0010 # change E37 and I37 from 51.2 to 51.6
 VK0041 # change G50 from 77.9 to 77.8
 VK0070 # change I45 from 84.3 to 85.3
 VK0102 # change E57 from 83.1 to 84.1, I55 from 70.2 to 80.2
 
# add comments in DB
VM0245(bad quality): renamed VM0245 (since already in DB) # add comment in DB: bad quality
VM0330 - not 100percent sure this one is correct! May want to re-do (sorry): renamed VM0330 (since already in DB) # add comment in DB: not 100% sure this one is correct

}
