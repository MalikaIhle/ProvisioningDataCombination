# small example to work out - to be integrated in compilation_provisioning before getting output for provisioning error checks
# 21/01/2016
# common mistakes in file that shouldn't be overlooked by the code:
#	- weird comments in all columns (F.in, com...), that is even in com column there shouldn't be something else than NA, S, G, O, COP, OTHER
#	- missing S (should have been corrected for all missing S in Tout), potentially also missing in Tin


require(zoo)

Tin <- c(35.8,NA,NA,NA,38.4,NA,38.7,NA,41.0,NA,42.0,NA,NA,NA,NA,47.5,NA,51.7,51.9,NA,NA,NA,NA,NA)
TinCom <- c("weird",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"S",NA,NA,NA,NA,NA,NA)
Tout <- c(NA,NA,38.3,NA,NA,38.6,NA,40.4,NA,41.6,NA,42.1,NA,NA,NA,NA,47.7,NA,NA,NA,NA,56.6,NA,56.9)
ToutCom <- c(NA,NA,"S",NA,NA,"S",NA,"G",NA,NA,NA,"O",NA,NA,NA,NA,"weird",NA,NA,NA,NA,NA,NA,"G")

x <- data.frame(Tin, TinCom, Tout, ToutCom)
x <- x[!is.na(x$Tin) | !is.na(x$Tout),]
x$Tin <- na.locf(x$Tin,na.rm=FALSE)
x$Tout <- na.locf(x$Tout,na.rm=TRUE, fromLast = TRUE)

x$Com <- NA
warningz <-  list()

for (i in 1:nrow(x))
{
# check if com are NA, S, G, O, COP, or OTHER
if (!is.na(x$TinCom[i]) & x$TinCom[i]!= "S" & x$TinCom[i]!= "O" & x$TinCom[i]!= "G" & x$TinCom[i]!= "COP" & x$TinCom[i]!= "OTHER") 
{warningz[[i]] <- c("Tin has weird comments !", i)}
if (!is.na(x$ToutCom[i]) & x$ToutCom[i]!= "S" & x$ToutCom[i]!= "O" & x$ToutCom[i]!= "G" & x$ToutCom[i]!= "COP" & x$ToutCom[i]!= "OTHER") 
{warningz[[i]] <- c("Tout has weird comments !", i)}


	

# accept comment O
if (!is.na(x$ToutCom[i]) & x$ToutCom[i] == "O")
{x$Com[i] <- "O"}

# change ToutCom from S or G, into IN
if (!is.na(x$ToutCom[i]) &( x$ToutCom[i] == "S" | x$ToutCom[i] == "G"))
{
x$Com[i] <- "IN"
}

}

warningzz <- list()

for (i in 1:(nrow(x)-1))
{

#  change Tin when ToutCom==G directly following a TouCom==S
if (x$Tin[i] == x$Tin[i+1] & x$Tout[i] != x$Tout[i+1])
{x$Tin[i+1] <- x$Tout[i]
x$Com[i+1] <- "S"

	if (is.na(x$ToutCom[i]) | x$ToutCom[i] != "S")# check if no S missing before G without Tin in between
	{warningzz[[i]] <- c("missing S in Tout !", i)}
}

# change Tout for Tin with S
if (x$Tin[i] != x$Tin[i+1] & x$Tout[i] == x$Tout[i+1])
{x$Tout[i] <- x$Tin[i+1]
x$Com[i] <- "S"

	if (is.na(x$TinCom[i]) | x$TinCom[i] != "S") # check if no S missing when two Tin one after another
	{warningzz[[i]] <- c("missing S in Tin !", i)}
	
# missing info for Tout
	if (is.na(x$ToutCom[i])) 
	{warningzz[[i]] <- c("missing info in Toutcom !", i)}

}


# insert row when TouCom==S if not followed directly by a Toutcom==G
if (!is.na(x$ToutCom[i]) & x$ToutCom[i] == "S" & x$Tout[i] != x$Tin[i+1])
{x <- rbind(x,c(x$Tout[i],NA,x$Tin[i+1],NA, "S"))}



}

x <- x[!is.na(x$Com),]
x <- unique(x[,c("Tin","Tout", "Com")])
x <- x[order(x$Tin, x$Tout),]



# warningzzz <- list()

# for (i in 1:(nrow(x)-1))
# {

# }

x
do.call(rbind,warningz)
do.call(rbind,warningzz)

# check if S before G was missing
# what if S missing
# what if G missing
# what if other comments
# what if Tin written in ToutCom (for grey o or for still IN)








{### extract color information of "O" visits

# Rk :
# code inspired from: https://nsaunders.wordpress.com/2014/08/06/when-life-gives-you-coloured-cells-make-categories/
# if load workbook > hearder is index = 1 ; if read.xlsx > header has no index, 1st row of data has index = 1
# in wb:
# ALLcells[[39.3]] # this is not the correct cell - I don't get what it does
# ALLcells[["39.3"]] # this is the correct cell
# ALLcells[c("39.3", "14.7")] # takes both cells

library(xlsx)

	
### get index of cells where T.out has been commented O	
b <- read.xlsx("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination\\example_60187modified.xlsx", sheetIndex =2)

which( !is.na(b$com.1) & b$com.1 == "O")+1	# rows F where O 
which( colnames(b)=="com.1" )-1 # column F Tout commented O
which(!is.na(b$com.3) & b$com.3 == "O" )+1	# rows M where O
which( colnames(b)=="com.3" )-1 # column M Tout commented O
FcellsToutCommentedO <- paste(which( !is.na(b$com.1) & b$com.1 == "O")+1, which( colnames(b)=="com.1" )-1, sep=".")	# will have errors if no cell ToutCom == "O"
McellsToutCommentedO <- paste(which( !is.na(b$com.3) & b$com.3 == "O")+1, which( colnames(b)=="com.3" )-1, sep=".")

# reload b as a workbook wb, which is a java object
wb <- loadWorkbook("C:\\Users\\mihle\\Documents\\_Malika_Sheffield\\_CURRENT BACKUP\\stats&data_extraction\\ProvisioningDataCombination\\example_60187modified.xlsx")

# get cells with Tout commented O as java objects
OFcells <- getCells(getRows(getSheets(wb)[[2]]))[FcellsToutCommentedO]
OMcells <- getCells(getRows(getSheets(wb)[[2]]))[McellsToutCommentedO]

# get style of these java objects
styleOFcells <-  sapply (OFcells, getCellStyle)
styleOMcells <-  sapply (OMcells, getCellStyle)

# get color out of the style of those java objects
FUNcellColor <- function(x) {
	fg  <- x$getFillForegroundXSSFColor()
	rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
	rgb <- paste(rgb, collapse = "")
	return(rgb)
}	
colornames <- list(blue = "00ffff", grey = "c0c0c0")
	
RGBcolorOFcells <- sapply(styleOFcells, FUNcellColor)
RGBcolorOMcells <- sapply(styleOMcells, FUNcellColor)

matchOF <- match(sapply(styleOFcells, FUNcellColor), colornames)
matchOM <- match(sapply(styleOMcells, FUNcellColor), colornames)

namecolorOFcells  <- data.frame(names(colornames)[matchOF])
namecolorOMcells  <- data.frame(names(colornames)[matchOM])

# create data.frame wtih list of cell index, values, color
valueOFcells <- data.frame(sapply (OFcells, getCellValue))
valueOMcells <- data.frame(sapply (OMcells, getCellValue))

FcellsToutCommentedO
McellsToutCommentedO

OFColors <- cbind(FcellsToutCommentedO,valueOFcells,namecolorOFcells,0 )
colnames(OFColors) <- c("index","Tout","colorname","Sex")

OMColors <- cbind(McellsToutCommentedO,valueOMcells,namecolorOMcells,1 )
colnames(OMColors) <- c("index","Tout","colorname","Sex")

OColors <- rbind(OFColors,OMColors)
}



