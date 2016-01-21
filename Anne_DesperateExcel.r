# example for Anne - to be integrated in compilation_provisioning before getting output for provisioning error checks
# 21/01/2016
# common mistakes in file that shouldn't be overlooked by the code:
#	- weird comments in all columns (F.in, com...), that is even in com column there shouldn't be something else than NA, S, G, O, COP, OTHER
#	- missing S (should have been corrected for all missing S in Tout), potentially also missing in Tin


require(zoo)

Tin <- c(35.8,NA,NA,NA,38.4,NA,38.7,NA,41.0,NA,42.0,NA,NA,NA,NA,47.5,NA,51.7,51.9,NA,NA,NA,NA,NA)
TinCom <- c("weird",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"S",NA,NA,NA,NA,NA,NA)
Tout <- c(NA,NA,38.3,NA,NA,38.6,NA,40.4,NA,41.6,NA,42.1,NA,NA,NA,NA,47.7,NA,NA,NA,NA,56.6,NA,56.9)
ToutCom <- c(NA,NA,"S",NA,NA,"S",NA,"G",NA,"O",NA,"O",NA,NA,NA,NA,"O",NA,NA,NA,NA,"S",NA,"G")

x <- data.frame(Tin, TinCom, Tout, ToutCom)
x <- x[!is.na(x$Tin) | !is.na(x$Tout),]
x$Tin <- na.locf(x$Tin,na.rm=FALSE)
x$Tout <- na.locf(x$Tout,na.rm=TRUE, fromLast = TRUE)

x$Com <- NA

for (i in 1:nrow(x))
{
# check if com are NA, S, G, O, COP, or OTHER
if (!is.na(x$TinCom[i]) & x$TinCom[i]!= "S" & x$TinCom[i]!= "O" & x$TinCom[i]!= "G" & x$TinCom[i]!= "COP" & x$TinCom[i]!= "OTHER") 
print("Tin has weird comments !")
if (!is.na(x$ToutCom[i]) & x$ToutCom[i]!= "S" & x$ToutCom[i]!= "O" & x$ToutCom[i]!= "G" & x$ToutCom[i]!= "COP" & x$ToutCom[i]!= "OTHER") 
print("Tout has weird comments !")


	

# accept comment O
if (!is.na(x$ToutCom[i]) & x$ToutCom[i] == "O")
{x$Com[i] <- "O"}

# change ToutCom from S or G, into IN
if (!is.na(x$ToutCom[i]) &( x$ToutCom[i] == "S" | x$ToutCom[i] == "G"))
{
x$Com[i] <- "IN"
}

}


for (i in 1:(nrow(x)-1))
{

#  change Tin when ToutCom==G directly following a TouCom==S
if (x$Tin[i] == x$Tin[i+1] & x$Tout[i] != x$Tout[i+1])
{x$Tin[i+1] <- x$Tout[i]
x$Com[i+1] <- "S"

	if (is.na(x$ToutCom[i]) | x$ToutCom[i] != "S")# check if no S missing before G without Tin in between
	{print("missing S in Tout")}

}

# change Tout for Tin with S
if (x$Tin[i] != x$Tin[i+1] & x$Tout[i] == x$Tout[i+1])
{x$Tout[i] <- x$Tin[i+1]
x$Com[i] <- "S"

	if (is.na(x$TinCom[i]) | x$TinCom[i] != "S") # check if no S missing when two Tin one after another
	{print("missing S in Tin")}

}


# insert row when TouCom==S if not followed directly by a Toutcom==G
if (!is.na(x$ToutCom[i]) & x$ToutCom[i] == "S" & (x$Tin[i] != x$Tin[i+1]) & (is.na(x$ToutCom[i]) | x$ToutCom[i] != "G"))
{x <- rbind(x,c(x$Tout[i],NA,x$Tin[i+1],NA, "S"))}
}

x <- x[!is.na(x$Com),]
x <- unique(x[,c("Tin","Tout", "Com")])


x <- x[order(x$Tin, x$Tout),]
x













