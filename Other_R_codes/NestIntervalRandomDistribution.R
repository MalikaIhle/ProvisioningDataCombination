VideoLength <- 90 # DVDs of 90 min

nPR <- 1599 # nb of selected DVD (see below)
avPR <- 15  # average provisioning (in number of visits, assuming length of videos are equal) in our videos
sdPR <- 8 # sd of provisioning on the expected scale (sqrt(mean-var))
syncint <- 2



  meanlog <- log(avPR)
  sdlog <-  sqrt(log(1 + sdPR^2/avPR^2))
  
  create_DVD <- function(){
    
    MalePexp <- rlnorm(1, meanlog = meanlog, sdlog = sdlog )
    MaleP <- rpois(1, MalePexp)
    if (MaleP == 0){MaleP <- rpois(1, MalePexp)}
    
    FemalePexp <- rlnorm(1, meanlog = log(avPR), sdlog = sqrt(log(1 + sdPR^2/avPR^2)) )
    FemaleP <- rpois(1, FemalePexp)
    if (FemaleP == 0){FemaleP <- rpois(1, FemalePexp)}
    
    TotalP <- MaleP + FemaleP
    DiffP <- abs(MaleP - FemaleP)
    
    MaleVisits <- sort(runif(MaleP,0,VideoLength))
    FemaleVisits <- sort(runif(FemaleP,0,VideoLength))
    DVD <- data.frame(rbind(cbind(Visits = MaleVisits, Sex = rep(1, length(MaleVisits))),cbind(Visits = FemaleVisits,Sex = rep(0, length(FemaleVisits)))))
    DVD <- DVD[order(DVD$Visits),]
    
   NestInt <- diff(DVD$Visits)
    
   return(NestInt)
  }
  
AllNestInt <- do.call(c,replicate(nPR,create_DVD()))
hist(AllNestInt)
        