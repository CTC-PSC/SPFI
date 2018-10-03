###############
## FUNCTIONS ##
###############
#spfiINDEX
spfiINDEX <- function(x) {
  y = x
  for(i in 2:ncol(x)) {
    y[,i] = x[,i]/mean(x[1:4,i])
  }
  if(mean(x[1:4,2])>=.97) {
    cat("BPER average is near 1, likely already an index, function will return x\n")
    return(x)
  } else {
    return(y)
  }
}

#readSTK
readSTK <- function(filename, stkCharLength = 3, fisheryNames = paste("f",1:25,sep=""), outname = "STK_ReFormatted.txt") {
  #Read in the file by line
  rawSTK <- readLines(filename)
  #Find the locations where stock names are located
  stkNameLocs <- vapply(rawSTK, nchar, 1) %in% stkCharLength 
  #Cut out the stock names
  stkNames <- rawSTK[stkNameLocs]
  #Cut out the data for each stocks
  rawSTKDat <- rawSTK[!stkNameLocs]
  #nLines for each stock
  nLines <- length(rawSTKDat)/length(stkNames)
  #Create a dummy "formatted" STK file for input into R via normal means
  STKout <- rawSTKDat #e.g. a place holder dataset
  MasterCounter <- 1
  for ( i in 1:length(stkNames)) {
    for (j in 1:nLines) {
      #first line is initial cohort abundance
      if(j == 1) { STKout[MasterCounter] <- paste(stkNames[i], " ", rawSTKDat[MasterCounter], " InitCohAbun") }
      #second line is maturation rates
      else if (j == 2) { STKout[MasterCounter] <- paste(stkNames[i], " ", rawSTKDat[MasterCounter], " MatRates") }
      #third line is the AEQ factor
      else if (j == 3) {  STKout[MasterCounter] <- paste(stkNames[i], " ", rawSTKDat[MasterCounter], " AEQfactor") }
      #4th to end of fishery designation is the ER rates by fisheries
      else { STKout[MasterCounter] <- paste(stkNames[i], " ", rawSTKDat[MasterCounter], " ", fisheryNames[j-3]) }
      MasterCounter = MasterCounter + 1
    }
  }
  #Write output to a new file
  write.table(STKout, outname, quote = FALSE, row.names = FALSE, col.names = FALSE)
  #Read back in the same file via normal means
  out <- read.table(outname)
  #Add column names to data
  names(out) <- c("Stock","Age2","Age3","Age4","Age5","Value")
  return(out)
}

readSPFI <- function(filename, outname = "SPFI_ReFormatted.txt") {
  #Read in the file by line
  rawSPFI <- readLines(filename)
  #Find the row that corresponds to a blank line
  blankLOC <- vapply(rawSPFI, nchar, 1) %in% 0
  #select out everything after first blank line
  SPFIout <- rawSPFI[!cumsum(blankLOC)>0]
  #Write output to a new file
  write.table(SPFIout, outname, quote = FALSE, row.names = FALSE, col.names = FALSE)
  #Read back in the same file via normal means
  out <- read.csv(outname, row.names=NULL)
  #Drop column 3 and return data to user
  return(out[,-3])
}

#readMDL
readMDL <- function(filename, numChar = 5, escapement = TRUE) {
  #Read in the file by line
  rawMDL <- readLines(filename)
  #Line 1 contains the tag code
  modstock <- rawMDL[1]
  #Line 2 is a junk line - just has "MODEL"
  by = "bpc broods"
  #Line 3 contains the number of fish tagged
  numTagg <- as.numeric(rawMDL[3])
  #Line 4 contains the number of fish released
  numFish <- as.numeric(rawMDL[4])
  #Line 5 contains max age
  maxAge <- as.numeric(rawMDL[5])
  #Line 6 contains the number of fisheries
  #numFisheries <- as.numeric(rawMDL[6])
  numFisheries <- length(rawMDL) - 9 - 1 #b/c the 48F MDLs have the wrong numFishery (doh!)
  #Lines 7 to numFish have the fishery names
  nameFisheries <- as.vector(rawMDL[7:(6+numFisheries)])
  #Last lines of the file contain the number of recoveries by age and fishery
  numAges <- length(rawMDL)-(6+numFisheries)
  #Create a blank matrix to contain the data
  recByFish <- matrix(ncol=numAges, nrow=numFisheries)
  colnames(recByFish) <- paste("age",2:(numAges+1),sep="")
  rownames(recByFish) <- nameFisheries
  for(i in 1:numAges) {
    recov <- rawMDL[i+6+numFisheries]
    #if old format (#### or #####)
    if(grepl(",",recov)==FALSE) {
      for(j in 1:numFisheries) {
        hold <- substr(recov, start=((j-1)*numChar+1),stop=(j*numChar))
        #additional error checking...
        recByFish[j,i] <- as.numeric(hold)
      }
    }
    #if new format (csv)
    if(grepl(",",recov)==TRUE) {
      recov2 = strsplit(recov,",")
      for(j in 1:numFisheries) {
        hold <- recov2[[1]][j]
        #additional error checking...
        recByFish[j,i] <- as.numeric(hold)
      }
    }
    
  }
  #If escapement is set to be true, then the last numChar's in the row are 'escapement'
  #NOTE: I may want to test to see if numFisheries*numChar < nchar(recov), and if it is, read in the last line as escapement...
  if(escapement==TRUE) {
    escapByAge <- matrix(ncol=numAges, nrow=1)
    for(i in 1:numAges) {
      recov <- rawMDL[i+6+numFisheries]
      #if old format (#### or #####)
      if(grepl(",",recov)==FALSE) {
        hold <- substr(recov, start=nchar(recov)-numChar+1,stop=nchar(recov))
        escapByAge[1,i] <- as.numeric(hold)
      }
      #if new format (csv)
      if(grepl(",",recov)==TRUE) {
        recov2 = strsplit(recov,",")
        hold <- recov2[[1]][length(recov2[[1]])]
        escapByAge[1,i] <- as.numeric(hold)
      }
    }
    rownames(escapByAge) <- "Escapement"
    colnames(escapByAge) <- paste("age",2:(numAges+1),sep="")
  } else escapByAge = NULL
  #Return output 
  list(ModelStock=modstock, BroodYear=by, FishTagged = numTagg, FishReleased = numFish, AgeMax = maxAge, nFisheries = numFisheries, RecoveriesByFishery = recByFish, Escapement = escapByAge)
}


#SPFItoFP
SPFItoFP <- function(modfishery, spfistratvec, baseperiod, mdldat, spfidat, stkdat, npredfuture) {
  #Preliminaries
   #data user provides
    numStrata <- length(spfistratvec)
    BaseERFisheryName <- modfishery
    startYear <- max(baseperiod)
    mdlList = mdldat
    spfiDAT = spfidat
    nAhead <- npredfuture
    stkDAT <- stkdat
   #data inferred from input files
    numAges <- ncol(mdlList[[1]]$RecoveriesByFishery)
    numStocks <- length(mdlList)
    numYears <- nrow(spfiDAT)
    stockNames <- as.character(unique(stkDAT$Stock))
  #Base catch ER for the specified fishery
   BaseCatER <- subset(stkDAT, Value == BaseERFisheryName)
  #Stratified ER blank matrix
   stratifiedER <- matrix(nrow=numAges*numStocks, ncol = numStrata)
  #For each stock MDL...
  k <- 1
  for(i in 1:numStocks) {
    #Pull out base catch exploitation rates for a specific stock and age
    BaseCatERbyStock <- BaseCatER[i,2:5]
    #Pull out recoveries by stratum (by fishery and age). force results to be matrix (pertinent if the number of strata = 1)
    if(numStrata==1) RecByStra <- t(t(as.matrix(mdlList[[i]]$Recover)[spfistratvec,]))
    if(numStrata >1) RecByStra <- t(as.matrix(mdlList[[i]]$Recover)[spfistratvec,])
    #For each age...
    for(j in 1:numAges) {
      if(rowSums(RecByStra)[j]!=0) stratifiedER[k,] <- RecByStra[j,] * as.numeric(BaseCatERbyStock[j]*(1/rowSums(RecByStra)[j]))
      else stratifiedER[k,] <- 0
      k <- k + 1
    }
  }
  #SPFI index matrixfied
  spfi <- as.matrix(spfiDAT)[,-1:-2]
  #Scaled ER blank matrix
  scaledER <- matrix(nrow=numAges*numStocks*numYears,ncol=numStrata)
  #FP blank vector
  FP <- rep(NA, numAges*numStocks*numYears)
  #For each stock in the MDL
  k <- 0
  for(i in 1:numStocks) {
    #Pull out base catch exploitation rates for a specific stock and age
    BaseCatERbyStock <- BaseCatER[i,2:5]
    #For each age in stock
    for(j in 1:numAges) {
      #Calculate the scaled ER rate
      scaledERtemp <- spfi * matrix(rep(stratifiedER[k+1,],numYears),ncol=numStrata,byrow=T)
      #If the base catch ER rate for an age is greater than 0, then divide the sum scaledER by the base ER
      if(BaseCatERbyStock[j]>0) FP[((k*numYears+1):(k*numYears+numYears))] <- rowSums(scaledERtemp)/as.numeric(BaseCatERbyStock[j])
      else FP[((k*numYears+1):(k*numYears+numYears))] <- 0
      #save the scaled ER rates in case of future interest
      scaledER[((k*numYears+1):(k*numYears+numYears)),] <- scaledERtemp
      k <- k + 1
    }
  }
  #Place all the data into a single data frame
  ages <- rep(c(rep(2,numYears), rep(3,numYears), rep(4,numYears), rep(5,numYears)), numStocks)
  years <- rep(spfiDAT$YEAR,numAges*numStocks)
  stocks <- as.vector(mapply(rep, stockNames, numAges*numYears))
  stockns <- as.vector(mapply(rep, 1:numStocks, numAges*numYears))
  year2dig <- substr(years,3,4)
  FPdat <- data.frame(stock=stocks, stockn=stockns, age=ages, year=years, year2=year2dig, FP=FP)
  #fpa list object
  fpa=list(fpa=FPdat, modfishery=modfishery, startYear=startYear, numStocks=numStocks, numAges=numAges, numYears=numYears, baseperiod=baseperiod, nAhead=nAhead, spfiDAT=spfiDAT)
  #return output
  return(fpa)
}

#writeFP
writeFP <- function(fpa, fpfilename, modfisherynumber) {
  #Manipulate data into same format as the FPA file
  FPAout <- matrix(nrow=fpa$numStocks*fpa$numAges,ncol=fpa$numYears)
  colnames(FPAout) <- substr(fpa$spfiDAT$YEAR,3,4)
  rownames(FPAout) <- as.vector(mapply(rep, 1:fpa$numStocks, fpa$numAges))
  yearVector <- fpa$spfiDAT$YEAR
  for(i in 1:fpa$numYears) FPAout[,i] <- fpa$fpa[(fpa$fpa$year %in% yearVector[i]),]$FP
  #Loop through output to determine which stocks (by numAges block) are zeroed out
  ZeroOut <- rep(NA,fpa$numStocks)
  for(i in 0:(fpa$numStocks-1)) ZeroOut[i+1] <- sum(rowSums(FPAout[(i*fpa$numAges+1):(i*fpa$numAges+fpa$numAges),]))
  RemoveRows <- as.vector(mapply(rep, ZeroOut==0, fpa$numAges))
  Out <- FPAout[!RemoveRows,]  
  #"0" stock FPA
  tempMat0 <- matrix(NA, nrow=fpa$numAges, ncol=fpa$numYears)
  for(i in 1:fpa$numAges) tempMat0[i,] <- fpa$spfiDAT$SPFI
  rownames(tempMat0) <- rep(0, nrow(tempMat0))
  #row and cbind these two manipulations
  Out0 <- rbind(tempMat0, Out)
  #Calculate nAhead average
  nAheadMat <- matrix(NA, nrow=nrow(Out0), ncol=fpa$nAhead)
  for(i in 1:fpa$nAhead) nAheadMat[,i] <- rowSums(Out0[,((ncol(Out0)-fpa$nAhead+1):ncol(Out0))])/fpa$nAhead
  OutFinal <- cbind(Out0, nAheadMat)
  #1 out the base period
  OutFinal[,1:length(fpa$baseperiod)] <- 1 #base period
  #column and row names
  colnames(OutFinal) <- substr(c(fpa$spfiDAT$YEAR,(max(fpa$spfiDAT$YEAR)+1):(max(fpa$spfiDAT$YEAR)+fpa$nAhead)),3,4)
  rownames(OutFinal)[-seq(1,nrow(OutFinal),by=fpa$numAges)] <- ""
  #Write output to a tab deliminated file
  OutFinal = round(OutFinal,3)
  write.table(OutFinal, "tmp.txt", quote=FALSE, sep = "\t", row.names=TRUE, col.names=TRUE)
  #Append top lines to the file
  fileConn<-file("tmp.txt", "r+b")
  tmp=readLines(fileConn)
  close(fileConn)
  file.remove("tmp.txt")
  fpfile = paste(fpfilename,".fpa",sep="")
  file.create(fpfile)
  fileConn<-file(fpfile, "w")
  writeLines(paste(modfisherynumber, ",    1,    ", fpa$modfishery,": (", max(fpa$spfiDAT$YEAR)+1, "-", max(fpa$spfiDAT$YEAR)+fpa$nAhead, " = ", max(fpa$spfiDAT$YEAR)-2,"-",max(fpa$spfiDAT$YEAR)," average)", sep=""), fileConn)
  writeLines(paste(min(fpa$baseperiod),"",sep=""), fileConn)
  writeLines(paste(max(fpa$spfiDAT$YEAR)+fpa$nAhead,sep=""), fileConn)
  writeLines(paste((length(tmp)-1)/4-1, ",\t", tmp[1],sep=""),fileConn)
  for(i in 2:length(tmp)) writeLines(tmp[i], fileConn)
  close(fileConn)
}

