#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Compile provisioning data sparrows
#	 Start : 21/12/2015
#	 last modif : 25/01/2015  
#    try to get colours from old templates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{### remarks

{## changes made in excel files: 
# VL0106: displace remarks from M.in
# VL0165: displace remarks from M.in
# 40109: displace remarks from F.in
# 40531: displace remarks from F.in
# 50158: change A1 from 'R' to 'DVD'
# 70078: change 'IN' in Min to 0
# VK0001: displace comment from Fin column
# VK0019: displace comment S (prenning) from F82 to M82 to just leave 'S' in com
# VK0034: added 0 in Fin C2
# VK0041: added 0 in Fin C2, with comment IN
# VK0418: displace comment from J7 to M7
# 40005: move B60 and C60 to D60 and E60
# 80014: move comments
# 40001LM18: deleted 'TOTAL'

# 40032, 40055, 40069, 40071, 40079, 40089, 40119, 40123, 40133: create an empty sheet 1 like in other file
# 40063, 40066, 40070, 40073, 40077, 40078, 40080, 40558: create an empty sheet 1 like in other file
# 50021, 50061, 50136, 50138, 50144, 50145, 50146, 50147, 50148, 50154: create an empty sheet1 like in other file
# 50157 to 50177: create an empty sheet1 like in other file
# 50180 to 50183: create an empty sheet1 like in other file
# 50187 to 50244: create an empty sheet1 like in other file
# 50246 to 50347: create an empty sheet1 like in other file
# 50363, 50366, 50375, 50376, 50377, 50378, 50379, 50393, 50401, 50414, 50415, 50427, 50434: create an empty sheet1 like in other file
# 50626-50669: create an empty sheet1 like in other file

# 40077: added 'S' in cells I7 and I24, and G5
# 40173: added 'S' in I67
# 40224: added 'S' in E44
# 40315: added 'S' in E38 and E49
# 50136: added 'S' in I4
# 50138: added 'S' in E8 and I5
# 50145: added 'S' in I35
# 50163: added 'S' in I6
# 50168: added 'S' in I28, I50 and I56
# 50169: added 'S' in E3 and E14
# 50170: added 'S' in E10
# 50180: added 'S' in I8 and G6
# 50182: added 'S' in I34, I40, and I64 ; and replace G by S in I21
# 50183: added 'S' in I27
# 50187: added 'S' in I4, I17, I29, and I38
# 50188: added 'S' in I3, replace G by S in E27, and added F in E28
# 50192: added 'S' in I8, I15, and I23
# 50193: added 'S' in I19, I26, and I44
# 50195: added 'S' in E13
# 50198: added 'S' in I16
# 50200: added 'S' in I7
# 50201: added 'S' in E17, E33, E50, E69, and G79
# 50202: added 'S' in I36 and I57; deleted duplate visit in enf Female columns
# 50204: added 'S' in I40
# 50206: added 'S' in I44
# 50208: added 'S' in E27
# 50210: added 'S' in E4, E8, and E11
# 50211: added 'S' in E10 and I22
# 50221: added 'S' in E67, I18, I21, I46 and I63
# 50222: added 'S' in I29 and I70
# 50224: added 'S' in E5, E70
# 50231: added 'S' in I7, I15, I19, I31, and E58
# 50254: added 'S' in E3 and E46
# 50255: added 'S' in I32
# 50257: added 'S' in E19
# 50259: added 'S' in I5, I12, I18, I21, and I33
# 50285: added 'S' in E39
# 50285: added 'S' in E85
# 50296: added 'S' in E29, E94, I37, and I57
# 50363: added 'S' in I3
# 50366: added 'S' in I9
# 50378: added 'S' in I17, I25, and I38
# 50379: added 'S' in I5
# 50414: added 'S' in I76 and I115
# 50415: added 'S' in I23 and I55
# 50432: added 'S' in E17, E67, E78, I29, I49, I70, and I75
# 50434: added 'S' in E4, E58, E61, E77, E84, and I38
# 50626: added 'S' in I14
# 50630: added 'S' in E34
# 50638: added 'S' in E7, E33, and E42
# 50653: added 'S' in I12, I24, I37, and I47 ; replace S by G in I38 ; replace ON by G in I48
# 50667: added 'S' in I12 and I15
# VK0050: added 'S' in J32, displace 'O, grooming' to M32
# 50021: added 'S' in G16, G25, G32

# 40121: change B5 from 22 to 21 as the bird entered 'later' than he exited
# 40172: change H22 from 51.2 to 52
# 40200: change H45 from 6.9 to 69.9
# 40261: change D36 from 59 to 49
# 40269: change D22 from 17.7 to 14.7
# 40307: change H97 from 78.6 to 77.6
# 40391: change H43 from 43.8 to 43.4
# 40454: change B79 from 74.8 to 74.4
# 40512: change F7 from 24.6 to 20.8
# 40558: change D28 from 88.1 to 88.9
# 50161: change F48 from 80.9 to 90.9 and H49 from 81.3 to 91.3
# 50189: change B59 from 83.7 to 84.7
# 50191: change F32 from 46.5 to 45.8
# 50204: change F32 from 48.7 to 45.7
# 50209: change H37 from 95.9 to 92.9
# 50211: change D12 from 61.3 to 61.1
# 50232: change B48 from 59.5 to 59.7
# 50580: change H45 from 40 to 48
# 50598: change F17 from 20.8 to 21.8
# 60018: change H28 from 48.5 to 48.3
# 60063: change B7 from 12.5 to 12.9
# 60137: change F22 from 18.7 to 18.5
# 70106: change F64 from 72.2 to 82.2, D65 from 72.4 to 82.4
# 70108: change H28 from 61.3 to 71.30
# VK0010: change E37 and I37 from 51.2 to 51.6
# VK0041: change G70 from 77.9 to 77.8
# VK0070: change I45 from 84.3 to 85.3
# VK0102: change E57 from 83.1 to 84.1, I55 from 70.2 to 80.2

# 50313: change DVD no from 50310 to 50313 as well as sheet name
# 50341: change DVD no from 50339 to 50341 as well as sheet name

# 70031: move D37 to B37; H5 to F5
# 70033: move D2 to B2, D20 to B20
# 70040: move D74 to B74
# 70041: move D8 to B8
# 70047: move H20 to F20
# 70049: move D41 to B41, H5 to F5
# 70051: move D5 to B5, H2 to F2
# 70055: move h25 to F25
# 70057: move H40 to F40, H53 to F53
# 70061: move H66 to F66, D78 to B78
# 70064: move H34 to F34
# 70072: move H47 to F47, D77 to B77, H95 to F95
# 70084: move H19 to F19
# 70089: move D9 to B9
# 70090: move D6 to B6, D10 to B10
# 70091: move H11 to F11, H16 to F16, H19 to F19, H27 to F27
# 70094: move D19 to B19, H19 to F19
# 70096: move H31 to F31, H37 to F37
# 70102: move H30 to F30
# 70103: move H6 to F6, H24 to F24, H27 to F27, H68 to F68, H72 to F72, D47 to B47
# 70108: move H15 to F15, H27 to F27, D40 to B40
# 70113: move H44 to F44, H71 to F71
# 70119: move H2 to F2
# 70121: move H18 to F18, D49 to B49
# 70122: move H14 to F14
# 70124: move H81 to F81
# 70125: move H7 to F7, H37 to F37, H61 to F61
# 70127: move H9 to F9, D70 to B70
# 70128: move H44 to F44
# 70129: move H29 to F29
# 70134: move D21 to B21
# 70138: move D43 to B43, H11 to F11, H20 to F20, H24 to F24
# 70149: move H28 to F28
# 70156: move H42 to F42, H55to F55, H92 to F92
# 70157: move D61 to B61
# 70159: move D30 to B30, H71 to F71, H74 to F74, H77 to F77
# 70162: move H5 to F5, H22 to F22, H73 to F73, H76 to F76, D86 to B86
# 70163: move D42 to B42
# 70164: move H20 to F20
# 70165: move D33 to B33
# 70169: move D28 to B28
# 70173: move H4 to F4, H7 to F7, H10 to F10
# 70179: move H5 to F5, H12 to F12, D10 to B10
# 80051: move H2 to F2

# 70083: move D2 and E2 to B2 and C2; added Tout at the end when still IN
# 70088: move D2 and E2 to B2 and C2
# 40009: move F56 and G56 to H56 and I56
# 40010: move B55 and C55 to D55 and E55, deleted 'TOTAL'
# 40011: move G32 to I32, added Tout at the end when still IN, deleted 'TOTAL'
# 40011: move B37 and C37 to D37 and E37, deleted 'TOTAL'
# 40020: move B38 and C38 to D38 and E38, deleted 'TOTAL'
# 40024: move B45 and C45 to D45 and E45, deleted 'TOTAL'
# 40025: move B61 and C61 to D61 and E61, deleted 'TOTAL'
# 40026: deleted 'TOTAL'
# 40027: move B49 and C49 to D49 and E49, deleted 'TOTAL'
# 40030: move B64 and C64 to D64 and E64, deleted 'TOTAL'
# 40030: move F23 and G23 to H23 and I23, deleted 'TOTAL'
# 40032D: deleted 'TOTAL'
# 40033: move B48 and C48 to D48 and E48, deleted 'TOTAL'
# 40034: move B14 to D14, added 'IN', move comment
# 40036D: deleted 'TOTAL'
# 40037: move F52 and G52 to H52 and I52, deleted 'TOTAL'
# 40038: deleted 'TOTAL'
# 40039D: move B44 and C44 to D44 and E44, deleted 'TOTAL'
# 40044: move B53 and C53 to D53 and E53, deleted 'TOTAL'
# 40045: move F67 and G67 to H67 and I67, deleted 'TOTAL'; move comment from I2 and I39
# 40048: deleted 'TOTAL'
# 40050: deleted 'TOTAL'
# 40054: move B51 and C51 to D51 and E51, deleted 'TOTAL'
# 40058: move B77 and C77 to D77 and E77, deleted 'TOTAL'
# 40058: move B40 and C40 to D40 and E40, deleted 'TOTAL'
# 40063: move B16 to D16 replace STIL by IN in E16
# 40066: replace comments by letters as appropriate (describe meaning of O blue and O grey !), move comment S from I20 to I21
# 40069: replace comments by letters as appropriate
# 40072: move B64 and C64 to D64 and E64, deleted 'TOTAL'
# 40083: move B56 and C56 to D56 and E56, deleted 'TOTAL'
# 40084: deleted 'TOTAL'
# 40087: move B54 and C54 to D54 and E54 deleted 'TOTAL'
# 40087: move B38 and C38 to D38 and E38 deleted 'TOTAL'
# 40088: move F42 and G42 to H42 and I42, deleted 'TOTAL'
# 40094: move B25 and C25 to D25 and E25 deleted 'TOTAL'
# 40097: move F49 and G49 to H49 and I49, deleted 'TOTAL'
# 40098: move F16 and G16 to H16 and I16, deleted 'TOTAL'
# 40103: move B81 and C81 to D81 and E81 deleted 'TOTAL'
# 40105: move F41 and G41 to H41 and I41, deleted 'TOTAL'
# 40110: move B65 and C65 to D65 and E65,F65 and G65 to H65 and I65  deleted 'TOTAL'
# 40111: move B46 and C46 to D46 and E46 deleted 'TOTAL'
# 40112: move B24 and C24 to D24 and E24 deleted 'TOTAL'
# 40113: move B77 and C77 to D77 and E77 deleted 'TOTAL'
# 50461: deleted 'TOTAL'
# VK0020: move B94 and C94 to D94 and E91 deleted random numbers at the bottom of the file
# 60084: deleted 'TOTAL'
# 60187: deleted 'TOTAL'

# 70009: added Tout for bird IN at the end
# 70025: added Tout for bird IN at the end
# 70026: added Tout for bird IN at the end
# 70036: added Tout for bird IN at the end
# 70038: added Tout for bird IN at the end
# 70040: added Tout for bird IN at the end
# 70041: added Tout for bird IN at the end
# 70043: added Tout for bird IN at the end
# 70047: added Tout for bird IN at the end
# 70051: added Tout for bird IN at the end
# 70064: added Tout for bird IN at the end
# 70066: added Tout for bird IN at the end
# 70072: added Tout for bird IN at the end
# 70074: added Tout for bird IN at the end
# 70077: added Tout for bird IN at the end
# 70085: added Tout for bird IN at the end
# 70090: added Tout for bird IN at the end
# 70093: added Tout for bird IN at the end
# 70099: added Tout for bird IN at the end
# 70102: added Tout for bird IN at the end
# 70104: added Tout for bird IN at the end
# 70106: added Tout for bird IN at the end
# 70110: added Tout for bird IN at the end
# 70112: added Tout for bird IN at the end
# 70114: added Tout for bird IN at the end
# 70119: added Tout for bird IN at the end
# 70120: added Tout for bird IN at the end
# 70121: added Tout for bird IN at the end
# 70122: added Tout for bird IN at the end
# 70125: added Tout for bird IN at the end
# 70128: added Tout for bird IN at the end
# 70138: added Tout for bird IN at the end
# 70140: added Tout for bird IN at the end
# 70149: added Tout for bird IN at the end
# 70170: added Tout for bird IN at the end
# 70175: added Tout for bird IN at the end
# 80003: added Tout for bird IN at the end
# 80005: added Tout for bird IN at the end
# 80006: added Tout for bird IN at the end
# 80006: added Tout for bird IN at the end
# 80031: added Tout for bird IN at the end
# 80040: added Tout for bird IN at the end
# 80051: added Tout for bird IN at the end
# 80054: added Tout for bird IN at the end
# 80059: added Tout for bird IN at the end
# 80061: added Tout for bird IN at the end
# 80066: added Tout for bird IN at the end
# 80067: added Tout for bird IN at the end
# 80068: added Tout for bird IN at the end
# 80069: added Tout for bird IN at the end


# VK0061: added Tout for bird IN at the end
# VK0028: added Tout for bird IN at the end. deleted random numbers at the bottom of the file
# VK0034: added Tout for bird IN at the end. deleted random numbers at the bottom of the file
# VK0040: added Tout for bird IN at the end. deleted random numbers at the bottom of the file
# VK0046: added Tout for bird IN at the end. deleted random numbers at the bottom of the file
}

## library xlsx run with rjava, does not have enough memory to go through all excel files, hence the change to openxlsx, at least to read xlsx files...

## can't make the function write.xslx from openxlsx to work despite having done all their suggestions:
# http://stackoverflow.com/questions/27952451/error-zipping-up-workbook-failed-when-try-to-write-xlsx

}


{### overview decisions taken

## excel files considered are only the one which have:
# an entry in tblDVD_XlsFiles (rk: some files have an entry in DB but are not included here: see '########## FILES THAT SHOULD BE INCLUDED')
# a situation = 4 in tblDVDInfo (i.e. only chicks)
# should excluded early age chicks because can still be brooded ??

## old templates visits include:
# entry times commented 'S' (before getting into the NB)
# exit time commented 'gone' or at least not commented 'S' (i.e. when the bird leave the NB area, sometimes after he left the NB per se)
# visits commented 'O' (mostly hanging out of the NB and some rare feeding from the outside - probably longer duration than time during which the head comes through the NB entrance which is considered with the new template)
# > consequently: we expect much higher visit duration in old files than new ones


}

rm(list = ls(all = TRUE))


{### packages, working directories and connection to Access DB
library(RODBC)
library(openxlsx)	
# library(xlsx)	# packega xlsx will be needed later in the code (after detaching the conflicting openxlsx nevertheless needed at first to be faster/not crashing)

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

head(tblDVD_XlsFilesALLDBINFO)


close(conDB)

}

head(tblDVD_XlsFilesALLDBINFO)
tail(tblDVD_XlsFiles,30)


{### extraction data in Excel files analyzed with newest excel template

{## create list of file names from files analysed after 2012 included
	# not elegant but the DB will probably not change.
FilenamesAfter2012 <- sort(tblDVD_XlsFiles$Filename[tblDVD_XlsFiles$DVDRef >=2933 & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename]) 

########## FILES THAT SHOULD BE INCLUDED
	# really not elegant... to get rid of those files that don't have proper names in the dropbox
FilenamesAfter2012 <- FilenamesAfter2012[FilenamesAfter2012!="2012\\VL0077.xlsx" &	## named 'cant see the exit" > should be considered as no visit ?
										FilenamesAfter2012!="2013\\VM0212.xlsx" &	## should be added as no visit ?
										FilenamesAfter2012!="2013\\VM0245.xlsx" &	## named 'bad quality' but has data in DB > should be considered.
										FilenamesAfter2012!="2013\\VM0330.xlsx"]	## named 'not 100% sure correct" but has data in DB > should be considered.
###########	
}								   
	
head(FilenamesAfter2012)


{##  create list of file names from files analysed in 2010 and 2011 with the new template (from what I could see opening hopefully all the files)

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


Filenames1011newtemplate <- tblDVD_XlsFiles$Filename[grepl("2010|2011", tblDVD_XlsFiles$Filename) == TRUE & !tblDVD_XlsFiles$Filename%in%filename1011_oldtemplate & tblDVD_XlsFiles$Filename%in%tblDVD_XlsFilesALLDBINFO$Filename]
													# Filenames that contained 2010 or 2011 				and that do not belong to the list above				where situation = 4 (only chicks)

}

head(Filenames1011newtemplate)													

	
{## combine all files analyzed with the new template (will take newly analyzed files IF put in the root of the year folder, with a normal file name)

FilenamesNewTemplate <- c(as.character(Filenames1011newtemplate), as.character(FilenamesAfter2012))
}

head(FilenamesNewTemplate)

	
{## create for each excel file with a new template, a table b containing: Tin, Tout, Sex and Filename

	{## for xlsx files: use openxlsx otherwise it crashes (see remarks)

FilenamesNewTemplateXLSX <- FilenamesNewTemplate[grepl("xlsx", FilenamesNewTemplate) == TRUE]

out = list()
	
for (j in 1:length(FilenamesNewTemplateXLSX)){
filenamej <- paste(pathdropboxfolder, FilenamesNewTemplateXLSX[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheet="DVD NO") # read.xlsx function from library 'openxlsx' (not library 'xlsx'): make sure xlsx is not in the list given by 'search()'
b$Tin <- NA
b$Tout <- NA
b$Sex <- NA

for (i in 1:nrow(b)){


if (!is.na(b$F.in[i]) & is.na(b$M.in[i]))
{b$Tin[i] <- suppressWarnings(as.numeric(as.character(b$F.in[i])))} # some text is sometimes written down the column of Fin in the excel files > Tin is NA by coercion

if (is.na(b$F.in[i]) & !is.na(b$M.in[i]))
{b$Tin[i] <- suppressWarnings(as.numeric(as.character(b$M.in[i])))} # some text is sometimes written down the column of Min in the excel files > Tin is NA by coercion


if (!is.na(b$F.out[i]) & is.na(b$M.out[i]))
{b$Tout[i] <- suppressWarnings(as.numeric(as.character(b$F.out[i])))}

if (is.na(b$F.out[i]) & !is.na(b$M.out[i]))
{b$Tout[i] <- suppressWarnings(as.numeric(as.character(b$M.out[i])))}


if ((!is.na(b$F.in[i]) | !is.na(b$F.out[i])) & is.na(b$M.in[i]) & is.na(b$M.out[i])) # if one or the other Tin or Tout in 'female' column is not NA
{b$Sex[i] <- "0" }

if (is.na(b$F.in[i]) & is.na(b$F.out[i]) & (!is.na(b$M.in[i]) | !is.na(b$M.out[i]))) # if one or the other Tin or Tout in 'male' column is not NA
{b$Sex[i] <- "1" }

}

if(nrow(b[!is.na(b$Sex) & (!is.na(as.numeric(as.character(b$Tin))) | !is.na(as.numeric(as.character(b$Tout)))),])>0)
{b <- b[!is.na(b$Sex) & (!is.na(as.numeric(as.character(b$Tin))) | !is.na(as.numeric(as.character(b$Tout)))),c('Tin','Tout','Sex')]} # keep lines of data when sex was allocated and at least Tin or Tout (supress lines where only comments with a sex allocated)
else {b <- unique(b[,c('Tin','Tout','Sex')])} # keep one line of NAs + filename


b$Filename <- FilenamesNewTemplateXLSX[j]

out[[j]] <- b

}

combinedprovisioningNewTemplateXLSX = do.call(rbind, out)
}

head(combinedprovisioningNewTemplateXLSX,30)



detach("package:openxlsx", unload=TRUE)
require(xlsx)
search()


	{## for xls files: use xlsx package (take longer even though just a few files...)

FilenamesNewTemplateXLS <- FilenamesNewTemplate[grepl("xlsx", FilenamesNewTemplate) == FALSE]

out2 = list()
	
for (j in 1:length(FilenamesNewTemplateXLS)){
filenamej <- paste(pathdropboxfolder, FilenamesNewTemplateXLS[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheetName="DVD NO") # read.xlsx function from package xlsx
b$Tin <- NA
b$Tout <- NA
b$Sex <- NA

for (i in 1:nrow(b)){

if (!is.na(b$F.in[i]) & is.na(b$M.in[i]))
{b$Tin[i] <- suppressWarnings(as.numeric(as.character(b$F.in[i])))} # some text is sometimes written down the column of Fin in the excel files > Tin is NA by coercion

if (is.na(b$F.in[i]) & !is.na(b$M.in[i]))
{b$Tin[i] <- suppressWarnings(as.numeric(as.character(b$M.in[i])))} # some text is sometimes written down the column of Min in the excel files > Tin is NA by coercion


if (!is.na(b$F.out[i]) & is.na(b$M.out[i]))
{b$Tout[i] <- suppressWarnings(as.numeric(as.character(b$F.out[i])))}

if (is.na(b$F.out[i]) & !is.na(b$M.out[i]))
{b$Tout[i] <- suppressWarnings(as.numeric(as.character(b$M.out[i])))}


if ((!is.na(b$F.in[i]) | !is.na(b$F.out[i])) & is.na(b$M.in[i]) & is.na(b$M.out[i])) # if one or the other Tin or Tout in 'female' column
{b$Sex[i] <- "0" }

if (is.na(b$F.in[i]) & is.na(b$F.out[i]) & (!is.na(b$M.in[i]) | !is.na(b$M.out[i]))) # if one or the other Tin or Tout in 'male' column
{b$Sex[i] <- "1" }

}

if(nrow(b[!is.na(b$Sex) & (!is.na(as.numeric(as.character(b$Tin))) | !is.na(as.numeric(as.character(b$Tout)))),])>0)
{b <- b[!is.na(b$Sex) & (!is.na(as.numeric(as.character(b$Tin))) | !is.na(as.numeric(as.character(b$Tout)))),c('Tin','Tout','Sex')]} # keep lines of data when sex was allocated and at least Tin or Tout (supress lines where only comments with a sex allocated)
else {b <- unique(b[,c('Tin','Tout','Sex')])} # keep one line of NAs + filename


b$Filename <- FilenamesNewTemplateXLS[j]

out2[[j]] <- b

}

combinedprovisioningNewTemplateXLS = do.call(rbind, out2)
}

head(combinedprovisioningNewTemplateXLS,30)


	{## combine both xlsx and xls files into one data frame
	
combinedprovisioningNewTemplate <- rbind(combinedprovisioningNewTemplateXLSX, combinedprovisioningNewTemplateXLS)
	
nrow(combinedprovisioningNewTemplate)	# 22665
}

}

head(combinedprovisioningNewTemplate)


{## create an excel file with all raw data just to keep reference of it
	
	## write.table(combinedprovisioningNewTemplate, file = "R_combinedprovisioningNewTemplate.xls", col.names=TRUE, sep='\t')

# after running the line above:
# I save the xls file into a xlsx file
# shift the headers one cell right
# rename the first column 'order' 
# > not elegant but write.xlsx from openxlsx isn't working for me
}

length(unique(combinedprovisioningNewTemplate$Filename))	# so far, 648 files with only chicks analyzed with new template
which(duplicated(unique(merge(x=combinedprovisioningNewTemplate, y=tblDVD_XlsFilesALLDBINFO[,c("DVDRef","Filename")], by= "Filename",all.x=TRUE)[,c("DVDRef","Filename")])[,"DVDRef"]))	# no duplicates of DVDRef
}

tail(combinedprovisioningNewTemplate,30)





{## check chronology in raw data with NewTemplate

combinedprovisioningNewTemplate[combinedprovisioningNewTemplate$Tout - combinedprovisioningNewTemplate$Tin < 0,]


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
splitNewTemplates_byFilenames_bySext_df[splitNewTemplates_byFilenames_bySext_df$Diff_Tim_prevOut <0,]
}





{### extraction data in Excel files analyzed with oldest template

{## create list of filenames

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

head(FilenamesOldTemplate)


{## create for each excel file XLS (+ 3 XLSX) with an old template, a table bb containing: Tin, Tout, Sex and Filename

options(warn=2)	# convert warning into error and therefore stop the loop when it happen, ask 'j' and 'ind' or 'inde' to see when the error occured

out3 = list()
	
for (j in 1:length(FilenamesOldTemplateXLS)){
filenamej <- paste(pathdropboxfolder, FilenamesOldTemplateXLS[j], sep="\\DVDs ")
b <- read.xlsx(filenamej, sheetIndex =2) # read.xlsx function from library 'xlsx' (not library 'openxlsx'): make sure openxlsx is not in the list given by 'search()'

{# add a Tout when bird still in at the end of the video
b$F.out[!is.na(b$com) & b$com == "IN" & b$F.in != 0] <- as.numeric(as.character(b$DVD[which(!is.na(b$DVD) & (b$DVD=="tap length" | b$DVD=="Tape length"))+1])) # if the female is in the next box at the end of the video, select the value below the cell where written 'tap length' to put in 'Tout'
b$M.out[!is.na(b$com.2) & b$com.2 == "IN"& b$M.in != 0] <- as.numeric(as.character(b$DVD[which(!is.na(b$DVD) & (b$DVD=="tap length" | b$DVD=="Tape length"))+1])) # uses 'which' only to get the index of the row + 1 to access the cell below the cell written 'tap length' in excel
}

{## female visits
bbF <- list()

{# if female have more than one visit
if (length(b$F.out[!is.na(b$F.out) & (is.na(b$com.1) | b$com.1 != "S") & (is.na(b$com.1)| b$com.1!= "OTHER") & (is.na(b$com.1)| b$com.1!= "COP")]) > 1) # exclude lines where Fout is na, or Fout is commented by 'S' (stay > meaning we keep time where bird only at the nest bost without feeding, time counted until 'gone') and where Fout is commented by 'other' (i.e. not a visit but another behaviour), or Fout commented 'cop' which is also not a visit.
{
for (ind in 2:length(b$F.out[!is.na(b$F.out) & (is.na(b$com.1) | b$com.1 != "S") & (is.na(b$com.1)| b$com.1!= "OTHER") & (is.na(b$com.1)| b$com.1!= "COP")]))
{
# fill first all the Tout
bbF$Tout <- b$F.out[!is.na(b$F.out) & (is.na(b$com.1) | b$com.1 != "S") & (is.na(b$com.1)| b$com.1!= "OTHER") & (is.na(b$com.1)| b$com.1!= "COP")]
# fill the first Tin
bbF$Tin[1] <- b$F.in[!is.na(b$F.in) & (is.na(b$com.1) | b$com.1!= "OTHER") & (is.na(b$com.1) | b$com.1!= "COP")][1] 	# Fin and Fout times for 'COP' and 'OTHER' ar on the same line > exclude those
# start the loop at ind = 2 to keep filling the Tin one by one
bbF$Tin[ind] <- min(b$F.in[b$F.in <= bbF$Tout[ind] & b$F.in >= bbF$Tout[ind-1] & b$F.in > bbF$Tin[ind-1]], na.rm=T) # the minimum value of Fin between the last exit and the new exit will often be the Fin commented 'S' (i.e. the birs is at the nest but not yet feeding), while later Fin will be when the bird enter the nest. This 'min' function allows to keep a length of Tin equal to the length of Tout. I added the condition Fin > previous Fin to indicate errors in files where chronology broken
}

bbF <- cbind(bbF$Tin,bbF$Tout, rep(0, length(bbF$Tin))) # fill a third column with 0 in all rows
colnames(bbF) <- c("Tin", "Tout", "Sex")
}
}

{# if female have just one visit
if (length(b$F.out[!is.na(b$F.out) & (is.na(b$com.1) | b$com.1 != "S") & (is.na(b$com.1)| b$com.1!= "OTHER") & (is.na(b$com.1)| b$com.1!= "COP")]) == 1)	
{
# fill first the only Tout
bbF$Tout <- b$F.out[!is.na(b$F.out) & (is.na(b$com.1) | b$com.1 != "S") & (is.na(b$com.1)| b$com.1!= "OTHER") & (is.na(b$com.1)| b$com.1!= "COP")]
# fill the first and only Tin
bbF$Tin[1] <- b$F.in[!is.na(b$F.in) & (is.na(b$com.1) | b$com.1!= "OTHER") & (is.na(b$com.1) | b$com.1!= "COP")][1] # still select the first in case there is one Tin commented 'S' and then a Tin for when the bird enter the NB

bbF <- cbind(bbF$Tin,bbF$Tout, 0)
colnames(bbF) <- c("Tin", "Tout", "Sex")
}

}

}

{## male visits
bbM <- list()

{# if male have more than one visit
if (length(b$M.out[!is.na(b$M.out) & (is.na(b$com.3) | b$com.3 != "S") & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")]) > 1) 
{
for (inde in 2:length(b$M.out[!is.na(b$M.out) & (is.na(b$com.3) | b$com.3 != "S") & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")]))
{bbM$Tout <- b$M.out[!is.na(b$M.out) & (is.na(b$com.3) | b$com.3 != "S") & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")]
bbM$Tin[1] <- b$M.in[!is.na(b$M.in) & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")][1]
bbM$Tin[inde] <- min(b$M.in[b$M.in <= bbM$Tout[inde] & b$M.in >= bbM$Tout[inde-1] & b$M.in > bbM$Tin[inde-1]], na.rm=T)
}

bbM <- cbind(bbM$Tin,bbM$Tout, rep(1, length(bbM$Tin)))
colnames(bbM) <- c("Tin", "Tout", "Sex")
}
}

{# if male have just one visit
if (length(b$M.out[!is.na(b$M.out) & (is.na(b$com.3) | b$com.3 != "S") & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")]) == 1)
{
bbM$Tout <- b$M.out[!is.na(b$M.out) & (is.na(b$com.3) | b$com.3 != "S") & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")]
bbM$Tin[1] <- b$M.in[!is.na(b$M.in) & (is.na(b$com.3) | b$com.3!= "OTHER") & (is.na(b$com.3) | b$com.3!= "COP")][1]

bbM <- cbind(bbM$Tin,bbM$Tout, 1)
colnames(bbM) <- c("Tin", "Tout", "Sex")
}

}
}

{## combine both female and male visits

# when no bird ever visited, keep a line with NA
if(length(bbF)== 0  & length(bbM)== 0)	
{bb <- data.frame(rbind(c(NA,NA,NA,NA)))
colnames(bb) <- c('Tin','Tout','Sex','Filename')}

# otherwise unlist and combine both sex visits
else {bb <- data.frame(rbind(unlist(bbF), unlist(bbM)))} # need to unlist in case one list is empty when no visit at all by one sex

# order by Tin then Tout
bb <- bb[order(bb$Tin),]

# add filename
bb$Filename <- FilenamesOldTemplateXLS[j]

}

out3[[j]] <- bb

}

combinedprovisioningOldTemplate = do.call(rbind, out3)

}

}




############# try to integrate Desperate excel code into the creation of a table bb for each excel file XLS (+ 3 XLSX) with an old template, table bb containing: Tin, Tout, Sex and Filename



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

capture.output(warningz, file="warningz20160125.txt") 

condwarningzBirdIN <- sapply(warningz, function(x) x[2] == "bird IN at end of video: please write Tout, move 'IN' into TouCom")
warningzBirdIN <- warningz[condwarningzBirdIN]

condwarningzMissInfo <- sapply(warningz, function(x) x[2] == "missing info in Tout com  !")
warningzMissInfo <- warningz[condwarningzMissInfo]

condwarningzOthers <- sapply(warningz, function(x) x[2] != "missing info in Tout com  !" &  x[2] != "bird IN at end of video: please write Tout, move 'IN' into TouCom")
warningzOthers <- warningz[condwarningzOthers]


combinedprovisioningOldTemplate = do.call(rbind, out3)

}








{### extract information in coloured cell

library(xlsx)
			
			# internet example
			wb     <- loadWorkbook("test.xlsx")
			sheet1 <- getSheets(wb)[[1]]
			# get all rows
			rows  <- getRows(sheet1)
			cells <- getCells(rows)
			# quick look at the values
			sapply(cells, getCellValue)
			styles <- sapply(cells, getCellStyle)

			cellColor <- function(x) {
				fg  <- x$getFillForegroundXSSFColor()
				rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
				rgb <- paste(rgb, collapse = "")
				return(rgb)
			}

			sapply(styles, cellColor)

			pheno <- list(normal = "00ff00", tumour = "ff0000")
			m     <- match(sapply(styles, cellColor), pheno)
			labs  <-names(pheno)[m]
			labs

			
			
			
			
b <- read.xlsx("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination\\example_60187modified.xlsx", sheetIndex =2)

which(b$M.out[!is.na(b$com.3) & b$com.3 == "O"])
which( !is.na(b$com.1) & b$com.1 == "O")+1	# rows F where O
which( colnames(b)=="com.1" )-1 # column F Tout where O
which(!is.na(b$com.3) & b$com.3 == "O" )+1	# rows M where O
which( colnames(b)=="com.3" )-1 # column M Tout where O
FcellsToutCommentedO <- paste(which( !is.na(b$com.1) & b$com.1 == "O")+1, which( colnames(b)=="com.1" )-1, sep=".")
McellsToutCommentedO <- paste(which( !is.na(b$com.3) & b$com.3 == "O")+1, which( colnames(b)=="com.3" )-1, sep=".")



wb     <- loadWorkbook("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination\\example_60187modified.xlsx")
sheet2 <- getSheets(wb)[[2]]

# get all cells
rows  <- getRows(sheet2)
cells <- getCells(getRows(sheet2))
cells[[2.3]]
cells[[FcellsToutCommentedO]]

	# quick look at the values
	values <- sapply(cells, getCellValue)
cellsWithO <- 	values[values=="O"]
	styles <- sapply(cells, getCellStyle)
	stylethiscell <-  getCellStyle(cells[[FcellsToutCommentedO]])

			cellColor <- function(x) {
				fg  <- x$getFillForegroundXSSFColor()
				rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
				rgb <- paste(rgb, collapse = "")
				return(rgb)
			}

	allcellcolours <- sapply(styles, cellColor)
	thiscellcellcolours <- cellColor(stylethiscell)
	
pheno <- list(blue = "00ffff", grey = "c0c0c0")
m     <- match(sapply(styles, cellColor), pheno)
mthiscell <- match(cellColor(stylethiscell), pheno)
labs  <-names(pheno)[m]
labthiscell  <-names(pheno)[mthiscell]

# grey = c0c0c0
# blue = 00ffff
}

















head(combinedprovisioningOldTemplate, 100)


{### combine all data

combinedprovisioningNewTemplate$Template <- "New"
combinedprovisioningOldTemplate$Template <- "Old"

combinedprovisioningALL <- rbind(combinedprovisioningOldTemplate,combinedprovisioningNewTemplate)

## write.table(combinedprovisioningALL, file = "R_combinedprovisioningALL.xls", col.names=TRUE, sep='\t')

}

head(combinedprovisioningALL, 100)
tail(combinedprovisioningALL, 100)






detach("package:xlsx", unload=TRUE)
require(openxlsx)
search()

detach("package:openxlsx", unload=TRUE)
require(xlsx)
search()
































{################## MESSSSSSS about different methods used for Old template

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

sample(tblDVD_XlsFilesALLDBINFO$Filename[tblDVD_XlsFilesALLDBINFO$Method == 1], 1)
sample(tblDVD_XlsFilesALLDBINFO$Filename[tblDVD_XlsFilesALLDBINFO$Method == 0 & tblDVD_XlsFilesALLDBINFO$DVDdate < as.POSIXct("2006-01-01")], 1)

visualize <- tblDVD_XlsFilesALLDBINFO[tblDVD_XlsFilesALLDBINFO$Filename %in% combinedprovisioningALL$Filename,c("DVDRef","Filename","OffspringNo", "Age","Method","Observer","FVisit2", "MVisit2")]
#write.table(visualize, file = "R_VisualizeOldMethods.xls", col.names=TRUE, sep='\t')

}




















