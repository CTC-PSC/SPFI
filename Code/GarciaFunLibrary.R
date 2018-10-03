
###########################################
# readHRJ
#
# Description
# ------------------
# reads in a. HRJ file and returns a list object with the n 
#
# Argument(s)
# ------------------
# filename
# nFisheries
# straysinescap
# Age6
#
# Output(s)
# ------------------
# list object 
###########################################
#readHRJ
readHRJ <- function(filename, nFisheries=69, straysinescap=TRUE, Age6=c("DNE","ignore","include")) {
  #Read in the file by line
   rawHRJ <- readLines(filename)
  #Determine if there are strays in escapement
    #best let user specify... #if((length(strsplit(rawHRJ[1]," ")[[1]][nchar(strsplit(rawHRJ[1], split=" ")[[1]])!=0])-3) %% 3 == 0) 
  #Determine the number of years of data in the HRJ
   nYears <- length(rawHRJ)/(nFisheries+1) #+1 b/c first row always contains escapement
   if(I(nYears-floor(nYears))!=0) cat("WARNNG: number of fisheries in the HRJ and what's user specified mismatch\n")
  #First, subset the data to escapRows and fishRows
   escRows <- rawHRJ[seq(1, length(rawHRJ), by=nFisheries+1)]
   fshRows <- rawHRJ[-seq(1, length(rawHRJ), by=nFisheries+1)]
  #Second, set up the vectors for the access database dependent on;
   #1)if strays are present or not
   #2)what to do with 5 age HRJs (2:6 vs. 2:5)
   hrjNamFish = c("AEQCat","AEQTot","NomCat","NomTot","Pop")
   if(straysinescap) {
     hrjNamEsc = c("All_Esc", "CA_Esc", "US_Esc")
   } else hrjNamEsc = "Esc"
   #given the straysinescap and Age6 option specified
    if(Age6=="DNE" || Age6=="ignore") {
      if(straysinescap) {
         hrjEscVec = sort(paste(hrjNamEsc, sort(rep(2:5,3)), sep=""))
      } else hrjEscVec = paste(hrjNamEsc, 2:5, sep="")
      hrjFshVec = sort(paste(hrjNamFish, sort(rep(2:5,5)), sep=""))
    } else if(Age6=="include") {
      if(straysinescap) {
        hrjEscVec = sort(paste(hrjNamEsc, sort(rep(2:6,3)), sep=""))
      } else hrjEscVec = paste(hrjNamEsc, 2:6, sep="")
      hrjFshVec = sort(paste(hrjNamFish, sort(rep(2:6,5)), sep=""))     
    }  else cat("WARNING: user-specified Age6 option not recognized\n")
  #Third, set up the number of escapement and fishery data matrices
   escMat <- as.data.frame(matrix(NA, ncol=length(hrjEscVec)+3, nrow=length(escRows)))
   names(escMat) = c("brood", "fishery", "oldestage", hrjEscVec)
   fshMat <- as.data.frame(matrix(NA, ncol=length(hrjFshVec)+3, nrow=length(fshRows)))
   names(fshMat) = c("brood", "fishery", "oldestage", hrjFshVec)
  #Fourth, load the escapement data
   for(i in 1:length(escRows)) {
    tmp = strsplit(escRows[i]," ")[[1]][nchar(strsplit(escRows[i], split=" ")[[1]])!=0]
    escMat[i,"brood"] = as.numeric(tmp[1])
    escMat[i,"fishery"] = as.numeric(tmp[2])
    escMat[i,"oldestage"] = as.numeric(tmp[3])
    #if length of the HRJ row is 3, there's no data, so do nothing
    if(length(tmp)!=3) {
      #oldestage = as.numeric(tmp[3]) (stocks that have age 5 & 6 collapsed reports the oldestage as 6, not 5, so i can't reference that value)
      oldestage = as.numeric(tmp[3]) #length(tmp[4:length(tmp)])/ifelse(straysinescap,3,1)+1
      minage = as.numeric(tmp[3]) - length(tmp[4:length(tmp)])/ifelse(straysinescap,3,1)+1
      hrjEscTmp = sort(paste(hrjNamEsc, sort(rep(minage:oldestage,ifelse(straysinescap,3,1))), sep=""))
      tmp2 = as.numeric(tmp[4:length(tmp)])
      names(tmp2) = hrjEscTmp[1:length(tmp2)]
      #IFFF YOU WANT TO IGNORE AGE6, YOU HAVE TO READ THE DATA IN, BUT IT IS THEN DROPPED HERE!!!!
      #Case only applies if age6 is present
      if(Age6=="ignore" && length(-grep(6,names(tmp2)))!=0) {
        escMat[i,names(tmp2)[-grep(6,names(tmp2))]] = tmp2[-grep(6,names(tmp2))]
      } else escMat[i,names(tmp2)] = tmp2     
    }
   }
  #Fifth, load the fishery data
   for(i in 1:length(fshRows)) {
     tmp = strsplit(fshRows[i]," ")[[1]][nchar(strsplit(fshRows[i], split=" ")[[1]])!=0]
     fshMat[i,"brood"] = as.numeric(tmp[1])
     fshMat[i,"fishery"] = as.numeric(tmp[2])
     fshMat[i,"oldestage"] = as.numeric(tmp[3])
     #if length of the HRJ row is 3, there's no data, so do nothing
     if(length(tmp)!=3) {
       #oldestage = as.numeric(tmp[3]) (stocks that have age 5 & 6 collapsed reports the oldestage as 6, not 5, so i can't reference that value)
       oldestage = as.numeric(tmp[3]) #length(tmp[4:length(tmp)])/5 + 1
       minage = as.numeric(tmp[3]) - length(tmp[4:length(tmp)])/5 + 1
       
       hrjFshTmp = sort(paste(hrjNamFish, sort(rep(minage:oldestage,5)), sep=""))
       tmp2 = as.numeric(tmp[4:length(tmp)])
       names(tmp2) = hrjFshTmp[1:length(tmp2)]
       #IFFF YOU WANT TO IGNORE AGE6, YOU HAVE TO READ THE DATA IN, BUT IT IS THEN DROPPED HERE!!!!
       #Case only applies if age6 is present
       if(Age6=="ignore" && length(-grep(6,names(tmp2)))!=0) {
         fshMat[i,names(tmp2)[-grep(6,names(tmp2))]] = tmp2[-grep(6,names(tmp2))]
       } else fshMat[i,names(tmp2)] = tmp2      
     }
   }
  #Sixth, return results to user
 return(list(ESC=escMat,HRJ=fshMat,strays=straysinescap,nFisheries=nFisheries))
}

###########################################
# .convertHRJ_BYtoCY_bytable
#
# Description
# ------------------
# Internal function that manipulates an HRJ standardized R format object
# from brood year data to calendar year data and
# determines if a brood is complete
#
# Dependent(s)
# ------------------
# .convertHRJ_BYtoCY_bytable - interal function that 
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
.convertHRJ_BYtoCY_bytable <- function(x) {
 #subset the data by age
  headings=x[,match(c("brood","fishery","oldestage","stock"), names(x), nomatch=0)]
  x.age2 = cbind(headings,x[,c(grep("2", names(x)))])
  x.age3 = cbind(headings,x[,c(grep("3", names(x)))])
  x.age4 = cbind(headings,x[,c(grep("4", names(x)))])
  x.age5 = cbind(headings,x[,c(grep("5", names(x)))])
  x.age6 = cbind(headings,x[,c(grep("6", names(x)))])
 #add cy date to each
  x.age2$cy <- x.age2$brood+2
  x.age3$cy <- x.age3$brood+3
  x.age4$cy <- x.age4$brood+4
  x.age5$cy <- x.age5$brood+5
  x.age6$cy <- x.age6$brood+6
 #create blank matrix
  minCY <- min(x.age2$cy,x.age3$cy,x.age4$cy,x.age5$cy,x.age6$cy)
  maxCY <- max(x.age2$cy,x.age3$cy,x.age4$cy,x.age5$cy,x.age6$cy)
  cy.HRJ = matrix(NA,nrow=max(x$fishery)*(maxCY-minCY+1),ncol=ncol(x)+1)
  colnames(cy.HRJ) = c(names(x),"cy")
  cy.HRJ = as.data.frame(cy.HRJ)
  cy.HRJ$cy = sort(rep(minCY:maxCY,max(x$fishery)))
 #if the max fishery number is 0, assume that we're dealing with escapement. 
  if(max(x$fishery)==0) {
   cy.HRJ = matrix(NA,nrow=1*(maxCY-minCY+1),ncol=ncol(x)+1)
   colnames(cy.HRJ) = c(names(x),"cy")
   cy.HRJ = as.data.frame(cy.HRJ)
   cy.HRJ$cy = sort(rep(minCY:maxCY,1))
  }
 #subset data to each cy and recombine
  for(i in minCY:maxCY) {
    tmp = subset(cy.HRJ,cy==i)
    age2.temp = subset(x.age2, cy==i)
    
   # subset(x.age2, brood==i&fishery==2)
   # subset(x.age3, brood==i&fishery==2)
   # subset(x.age4, brood==i&fishery==2)
   # subset(x.age5, brood==i&fishery==2)
   # i=1982
   # subset(x.age2, cy==i&fishery==2)
   # subset(x.age3, cy==i&fishery==2)
   # subset(x.age4, cy==i&fishery==2)
   # subset(x.age5, cy==i&fishery==2)
    
    if(nrow(age2.temp)!=0) tmp[,names(age2.temp)] = age2.temp
    age3.temp = subset(x.age3, cy==i)
    if(nrow(age3.temp)!=0) tmp[names(age3.temp)] = age3.temp
    age4.temp = subset(x.age4, cy==i)
    if(nrow(age4.temp)!=0) tmp[names(age4.temp)] = age4.temp
    age5.temp = subset(x.age5, cy==i)
    if(nrow(age5.temp)!=0) tmp[names(age5.temp)] = age5.temp
    age6.temp = subset(x.age6, cy==i)
    if(nrow(age6.temp)!=0) tmp[names(age6.temp)] = age6.temp
    cy.HRJ[cy.HRJ$cy %in% i,] = tmp
  }
 #cleanup and rearrange the matrix
  cy.HRJ = cy.HRJ[,c(1,ncol(cy.HRJ),2:(ncol(cy.HRJ)-1))] 
  cy.HRJ = cy.HRJ[,-match("brood",names(cy.HRJ),nomatch=0)] #drop column brood, and put the column cy in its place
 #determine if it's a 4 age or 5 age HRJ file
  if(sum(colSums(apply(cy.HRJ, 2, is.na))==nrow(cy.HRJ))>=5) {
    #if it's a 4 age HRJ, it'll have all blanks across all years in a given age
    cy.HRJ$inc = rowSums(apply(cy.HRJ, 2, is.na))/5>1
  } else {
    #if it's a 5 age HRJ
    cy.HRJ$inc = rowSums(apply(cy.HRJ, 2, is.na))>0   
  }
 #return object
  cy.HRJ
}

###########################################
# convertHRJ_BYtoCY
#
# Description
# ------------------
# Manipulates an HRJ standardized R format object  from brood year data to calendar year data
# Determine if a brood is complete
#
# Dependent(s)
# ------------------
# .convertHRJ_BYtoCY_bytable - interal function that 
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
convertHRJ_BYtoCY <- function(x) {
  if(x$HRJformat=="calendar") cat("ERROR: data is already in calendar year format\n")
  for(i in 1:x$nstocks) {
    #convert fishery data
    x[[i]]$HRJ_BY = .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_BY)
    x[[i]]$HRJ_CY = .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_CY)
    #convert escapement data
    x[[i]]$ESC_BY = .convertHRJ_BYtoCY_bytable(x[[i]]$ESC_BY)
    x[[i]]$ESC_CY = .convertHRJ_BYtoCY_bytable(x[[i]]$ESC_CY)
  }
  x$HRJformat="calendar"
  x
}

###########################################
# addPTableHRJ
#
# Description
# ------------------
# Adds the "preferred" table to any HRJ object. Use the 'BY' method data when the brood is incomplete, else use the 'CY" method
#
# Dependent(s)
# ------------------
# na
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
addPTableHRJ <- function(x, hrjclass=c("R","Access")) {
  if(hrjclass=="R") {
    for(i in 1:x$nstocks) {
      cat("not implemented\n")
      stop()
      #determine if its an incomplete brood or not
      incBroods = rowSums(apply(x[[i]]$HRJ_BY, 2, is.na))/5>1 #NOTE!!!: assumes a 4 age HRJ file

      x[[i]]$HRJ_BY[inc.broods,]
      
      ifelse(inc.broods, x[[i]]$HRJ_BY, x[[i]]$HRJ_CY)
      
      #convert fishery data
      x[[i]]$HRJ_P = x[[i]]$HRJ_CY
      
      .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_BY)
      #convert escapement data (NOTE!: there's no difference between the two methods for escap data)
      x[[i]]$ESC_P = x[[i]]$ESC_CY 
      
    } 
  }
  if(hrjclass=="Access" && !is.null(x$HRJ_BY$inc) && x$HRJformat=="calendar") {
    #use the cy method if complete,
     tmp = x$HRJ_CY
    #for rows where the brood is not complete, use the by method
     tmp[tmp$inc,] <- x$HRJ_BY[tmp$inc,]
    #in the 'p' table, update the inc field to state where the data is from
     tmp$inc <- ifelse(tmp$inc,"by","cy")
     x$HRJ_P = tmp
  }
  if(hrjclass=="Access" && x$HRJformat=="brood") {
    #use the cy method if complete,
    tmp = x$HRJ_CY
    tmp$inc = rep(NA,nrow(tmp))
    #
    for(i in 1:x$nstocks) {
      hrj_cy_tmp = subset(x$HRJ_CY, stock==i)
      #determine if it's a 4 age or 5 age HRJ file
      if(sum(colSums(apply(hrj_cy_tmp, 2, is.na))==nrow(hrj_cy_tmp))>=5) {
        #if it's a 4 age HRJ, it'll have all blanks across all years in a given age
        hrj_cy_tmp$inc = rowSums(apply(hrj_cy_tmp, 2, is.na))/5>1
      } else {
        #if it's a 5 age HRJ
        hrj_cy_tmp$inc = rowSums(apply(hrj_cy_tmp, 2, is.na))>0   
      }
      #update the "inc" field in the tmp object
      tmp[rownames(hrj_cy_tmp),grep("inc",colnames(tmp))] = hrj_cy_tmp$inc
    }
    #
    tmp2 =  x$HRJ_BY
    tmp2$inc = tmp$inc
    #for rows where the brood is not complete, use the by method
    tmp[tmp$inc,] <- tmp2[tmp$inc,]
    #in the 'p' table, update the inc field to state where the data is from
    tmp$inc <- ifelse(tmp$inc,"by","cy")
    x$HRJ_P = tmp
  }
  x
}

###########################################
# readHRJdir
#
# Description
# ------------------
# read all HRJ files in a directory. one noteworthy limitation of this function is that all HRJ's must be in the same format (#fisheries, strays)
#
# Argument(s)
# ------------------
#
#
# Output(s)
# ------------------
# an HRJ object
###########################################
readHRJdir <- function(userDir=choose.dir(), verbose=TRUE, ...) {
#Get old dir
 odir = getwd()
#Set directory
 setwd(userDir)
#Find all HRJs in the user-specified directory 
 myList <- list.files(".", pattern="1.HRJ", full.names=TRUE)
 myListNoDir <- list.files("./", pattern="1.HRJ")
#Pull out the stock names and assign them numbers
 stkList = strtrim(myListNoDir,3)
 stkNum = sort(rep(1:length(unique(stkList)),2))
#Throw an exception if there's an odd number of HRJ's in the directory
 if((length(myList) %% 2) != 0) {
   cat("ERROR: an odd number of HRJ files was found in the directory. You're probably missing a C or B HRJ\n")
   print(myList)
   stop()
 }
#Initialize a few things
 hrjList <- list()
 ii = 1
#Read in the HRJ files by stock (and put them into the R HRJ data object format)
 for(i in 1:length(unique(stkList))) {
 #Throw an exception HRJ[1] and HRJ[2] are not the same stock
  if(substr(myListNoDir[ii],1,3) != substr(myListNoDir[ii+1],1,3)) {
    cat("ERROR: I don't know how this happened, but somehow the next two HRJ's are not the same stock\n")
    print(myList[ii])
    print(myList[ii+1])
    stop()
  }
  #Throw an exception HRJ[1] and HRJ[2] are not the same stock
   if(substr(myListNoDir[ii],4,4)!="B" || substr(myListNoDir[ii+1],4,4)!="C") {
     cat("ERROR: I don't know how this happened, but the B and C method HRJ's are out of order - they're read in as B, C\n")
     print(myList[ii])
     print(myList[ii+1])
     stop()
   }
 #Read in the the B, then C HRJ from the same stock
  if(verbose) cat("Reading", ii, "HRJ File of", length(myList), ":", myListNoDir[ii], "\n")
  hrj_tmp1 = readHRJ(myList[ii], ...)
  if(verbose) cat("Reading", ii+1, "HRJ File of", length(myList), ":", myListNoDir[ii+1], "\n")
  hrj_tmp2 = readHRJ(myList[ii+1], ...)
 #Read in the data, which is organized by brood year 
  hrjList[[i]] = list(HRJ_BY=hrj_tmp1$HRJ, 
                      HRJ_CY=hrj_tmp2$HRJ,
                      ESC_BY=hrj_tmp1$ESC,
                      ESC_CY=hrj_tmp2$ESC,
                      stkAcronym=strtrim(myListNoDir[ii],3),
                      imMethod=c(substr(myListNoDir[ii],4,4),substr(myListNoDir[ii+1],4,4)) )
  #And increment
   ii=ii+2
  }
#Add stock names
 names(hrjList) = unique(stkList)
#Add the additional info for the R HRJ data object format
 hrjList$stknames = unique(stkList)
 hrjList$fshnames = as.character(1:nrow(hrj_tmp1$HRJ))
 hrjList$nstocks = length(unique(stkList))
 hrjList$nfisheries = length(hrjList$fshnames)
 hrjList$HRJformat = "brood"
#Reset directory
 setwd(odir)
#Return HRJ object
 return(hrjList)
}

###########################################
# convertHRJ_RtoAccess
#
# Description
# ------------------
# Convert a R HRJ data object format into the access database format (later will be a function) by
# collapsing the by stock HRJ's B & C method (ESC & HRJ) data into individual dataframes
#
# Argument(s)
# ------------------
#
#
# Output(s)
# ------------------
# 
###########################################
convertHRJ_RtoAccess <- function(x, writeCSV=FALSE, userDir=NULL) {
 #Confirm that the first item in the vector is B because program assumes the order of B and C
  if(!(x[[1]]$imMethod[1]=="B" && x[[1]]$imMethod[2]=="C")) {
   cat("ERROR: B and C method HRJ methods are out of order. They should be in B/C Method\n")
   print(x[[1]]$imMethod)
   stop()
  }
 #Create the base BY method table
  hrj_by <- x[[1]]$HRJ_BY
  esc_by <- x[[1]]$ESC_BY
  hrj_by$stock <- 1
  esc_by$stock <- 1
 #Create the base CY method table
  hrj_cy <- x[[1]]$HRJ_CY
  esc_cy <- x[[1]]$ESC_CY
  hrj_cy$stock <- 1
  esc_cy$stock <- 1
 #Loop through and read the next 
  for(i in 2:x$nstocks) {
   #BY
    hrj_tmp <- x[[i]]$HRJ_BY
    esc_tmp <- x[[i]]$ESC_BY
    hrj_tmp$stock <- i
    esc_tmp$stock <- i
    hrj_by <- rbind(hrj_by, hrj_tmp)
    esc_by <- rbind(esc_by, esc_tmp)
   #CY
    hrj_tmp <- x[[i]]$HRJ_CY
    esc_tmp <- x[[i]]$ESC_CY
    hrj_tmp$stock <- i
    esc_tmp$stock <- i
    hrj_cy <- rbind(hrj_cy, hrj_tmp)
    esc_cy <- rbind(esc_cy, esc_tmp)
  }
  hrj_by <- hrj_by[,c(ncol(hrj_by),1:(ncol(hrj_by)-1))]
  esc_by <- esc_by[,c(ncol(esc_by),1:(ncol(esc_by)-1))]
  hrj_cy <- hrj_cy[,c(ncol(hrj_cy),1:(ncol(hrj_cy)-1))]
  esc_cy <- esc_cy[,c(ncol(esc_cy),1:(ncol(esc_cy)-1))]
  hrj=list(
    HRJ_BY=hrj_by,
    HRJ_CY=hrj_cy,
    ESC_BY=esc_by,
    ESC_CY=esc_cy,
    stknames = x$stknames,
    fshnames = x$fshnames,
    nstocks = x$nstocks,
    nfisheries = x$nfisheries,
    HRJformat = x$HRJformat
  )
 #if write CSV
  if(writeCSV) {
   #ask user to specify the directory iif userDir is equal to ask, otherwise write files to the default directory
    if(userDir=="ask") setwd(choose.dir()) 
   #write directories
   write.table(hrj_by, "hrj_by - by layout.csv")
   write.table(hrj_cy, "hrj_cy - by layout.csv")
   write.table(esc_by, "esc_by - by layout.csv")
   write.table(esc_cy, "esc_cy - by layout.csv") 
  }
 #Return output
  return(hrj)
}

###########################################
# convertHRJtoMRE
#
# Description
# ------------------
# 
# 
#
# Argument(s)
# ------------------
# x - matrix (output from a 
#
# Output(s)
# ------------------
# 
###########################################
convertHRJtoMRE <- function(x, datatype=c("fishery","escapement")) {
 if("cy" %in% names(x)) {
  if(datatype=="fishery") {
   myvec1 = c("stock","cy","fishery","oldestage","inc","AEQCat","AEQTot","NomCat","NomTot","Pop")
   myvec2 = c("stock","cy","fishery","oldestage","IMmethod","AEQCat","AEQTot","NomCat","NomTot","Pop","by")
  } else if (datatype=="escapement") {
   myvec1 = c("stock","cy","fishery","oldestage","inc","All_Esc","CA_Esc","US_Esc")
   myvec2 = c("stock","cy","fishery","oldestage","IMmethod","All_Esc","CA_Esc","US_Esc","by")
  }
  age2 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 2, sep=""))]
  age3 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 3, sep=""))]
  age4 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 4, sep=""))]
  age5 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 5, sep=""))]
  age6 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 6, sep=""))]
  age2$by = with(age2, cy-2)
  age3$by = with(age3, cy-3)
  age4$by = with(age4, cy-4)
  age5$by = with(age5, cy-5)
  age6$by = with(age6, cy-6)
  names(age2)=names(age3)=names(age4)=names(age5)=names(age6)=myvec2
  all_ages = rbind(age2,age3,age4,age5,age6)
  all_ages$age = with(all_ages, cy-by)
  if(datatype=="fishery") out = all_ages[is.na(all_ages$Pop)==FALSE,]
  if(datatype=="escapement") out = all_ages[is.na(all_ages$All_Esc)==FALSE,]
 } else if ("brood" %in% names(x)) { 
  if(datatype=="fishery") {
   myvec1 = c("stock","brood","fishery","oldestage","inc","AEQCat","AEQTot","NomCat","NomTot","Pop")
   myvec2 = c("stock","by","fishery","oldestage","IMmethod","AEQCat","AEQTot","NomCat","NomTot","Pop","cy")
  } else if (datatype=="escapement") {
   myvec1 = c("stock","brood","fishery","oldestage","inc","All_Esc","CA_Esc","US_Esc")
   myvec2 = c("stock","by","fishery","oldestage","IMmethod","All_Esc","CA_Esc","US_Esc","cy")
   if(!("inc" %in% names(x))) x$inc = NA
  }
  age2 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 2, sep=""))]
  age3 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 3, sep=""))]
  age4 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 4, sep=""))]
  age5 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 5, sep=""))]
  age6 = x[c(myvec1[1:5],paste(myvec1[6:length(myvec1)], 6, sep=""))]
  age2$cy = with(age2, brood+2)
  age3$cy = with(age3, brood+3)
  age4$cy = with(age4, brood+4)
  age5$cy = with(age5, brood+5)
  age6$cy = with(age6, brood+6)
  names(age2)=names(age3)=names(age4)=names(age5)=names(age6)=myvec2
  all_ages = rbind(age2,age3,age4,age5,age6)
  all_ages$age = with(all_ages, cy-by)
 }
 if(datatype=="fishery") out = all_ages[is.na(all_ages$Pop)==FALSE,]
 if(datatype=="escapement") out = all_ages[is.na(all_ages$All_Esc)==FALSE,]
 return(out)
}

###########################################
# .convertAgeMatrixMRE
#
# Description
# ------------------
# Internal function
# 
#
# Argument(s)
# ------------------
# x - matrix (output from a 
#
# Output(s)
# ------------------
# 
###########################################
.convertAgeMatrixMRE <- function(x, type="by") {
 tmp = list()
 for(i in 1:ncol(x)) tmp[[i]] = cbind(as.numeric(rownames(x)), as.numeric(colnames(x)[i]),x[,i])
 tmp2 = do.call(rbind, tmp)
 rownames(tmp2) = 1:nrow(tmp2)
 tmp2 = as.data.frame(tmp2)
 if(type=="by") {
  names(tmp2) = c("by","age","value")
  tmp2$cy = tmp2$by+tmp2$age
 }
 if(type=="cy") {
  names(tmp2) = c("cy","age","value")
  tmp2$by = tmp2$cy-tmp2$age
 }
 return(tmp2)
}

###########################################
# roundUp (courtesy of Pete McHugh)
#
# Description
# ------------------
# defines  script-specific functions (this is a silly one, but it
# eliminates the need to install packages on end-user machines)
#
# Argument(s)
# ------------------
# x - matrix (output from a 
#
# Output(s)
# ------------------
# 
###########################################
roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

###########################################
# plotGarciaAll (courtesy of Pete McHugh)
#
# Description
# ------------------
# Original garcia plotting script written by Pete McHugh, converted to a function for ease of use
# 
#
# Argument(s)
# ------------------
# garcia
# outdir
# outtype - either "pdf" or "tff"
# Output(s)
# ------------------
# 
###########################################
plotGarciaAll <- function(Garcia, outdir, outtype, pdffilename = NULL) {
#get original directory
 odir <- getwd()
#
 this_is_the_place<-outdir #set output directory for specs
#---------------------------------------------------
# set some general specs relevant to all plots ##
# Periods for coloring points
per1=as.character(levels(Garcia$Period)[1])
per2=as.character(levels(Garcia$Period)[2])
per3=as.character(levels(Garcia$Period)[3])
per4=as.character(levels(Garcia$Period)[4])
# The options for x-axis labeling (MRE ER vs. CY ER) and values to display
Xnames<-c("Mature-run equivalent exploitation rate (%)","Calendar year harvest rate (%)")
xticks<-c("0%","20%","40%","60%","80%","100%")
#---------------------------------------------------
  if(is.null(pdffilename)) pdffilename = "Garcia Plot Figures.pdf"
  if(outtype == "pdf") pdf(file = paste(outdir,"\\",pdffilename,sep=""),height = 9.27, width = 12.76) # open graphics device
#---------------------------------------------------
# Now subset data, set specs, and make figures
for(Si in 1:max(Garcia$StockNum)){
  subGarc<-subset(Garcia,StockNum==Si) #get stock Si's data subset
  #recompute period range
   per1_sub = NA
   per2_sub = NA
   per3_sub = NA
   per4_sub = NA
  #
   hold_per1 = subset(subGarc, Period==per1&!is.na(Rate)&!is.na(Escapement))
   if(nrow(hold_per1)!=0) per1_sub = paste(min(hold_per1$Year),"-",right(max(hold_per1$Year),2),sep="")
   hold_per2 = subset(subGarc, Period==per2&!is.na(Rate)&!is.na(Escapement))
   if(nrow(hold_per2)!=0) per2_sub = paste(min(hold_per2$Year),"-",right(max(hold_per2$Year),2),sep="")
   hold_per3 = subset(subGarc, Period==per3&!is.na(Rate)&!is.na(Escapement))
   if(nrow(hold_per3)!=0) per3_sub = paste(min(hold_per3$Year),"-",right(max(hold_per3$Year),2),sep="")
   hold_per4 = subset(subGarc, Period==per4&!is.na(Rate)&!is.na(Escapement))
   if(nrow(hold_per4)!=0) per4_sub = paste(min(hold_per4$Year),"-",right(max(hold_per4$Year),2),sep="")

  # Set stock(plot)-specific specs (i.e., the plotting 
  # range and y tick width ); also, do data manipulations
  # idea here is to make figure scale/display vary for
  # optimal display across a wide range of escapements
  m1<-max(subGarc$Escapement,na.rm=TRUE)/0.95
  yt1<-c("Spawning escapement (thousands)","Spawning escapement") # use different title, if /1K vs. not
  ytitle<-yt1[2] #raw values as default, /1K otherwise 
  ydat<-subGarc$Escapement #raw values as default, /1K otherwise
  sname<-as.character(subGarc$EISStock[1]) #stock (from old excel worksheet name) for fig naming
  fname<-paste(as.character(subGarc$Stock[1]),sep="")
  SmsyRef<-subGarc$Smsy[1]
  S85Ref<-subGarc$S85[1]
  if(m1>=250000){
    ymax<-roundUp(m1,50000)/1000
    ystep<-50000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=100000) {
    ymax<-roundUp(m1,25000)/1000
    ystep<-25000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=50000) {
    ymax<-roundUp(m1,10000)/1000
    ystep<-10000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=10000) {
    ymax<-roundUp(m1,5000)/1000
    ystep<-5000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if(m1>=5000) {
    ymax<-roundUp(m1,1000)/1000
    ystep<-1000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if(m1>=1000) {
    ymax<-roundUp(m1,500)/1000
    ystep<-1000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else {
    ymax<-roundUp(m1,100)
    ystep<-100
  }
  # Now actually plot them (write to file)
  #if(outtype == "tiff") tiff(file = paste(outdir,"\\",Si," ",sname,".tif",sep=""),height = 9.27, width = 12.76,units='in',res=300) # open graphics device
  if(outtype == "tiff") tiff(file = paste(outdir,"\\",Si," ",sname,".tif",sep=""),height = 9.27, width = 12.76,units='in',res=300,compression="zip") # open graphics device
  if(outtype == "png") png(file = paste(outdir,"\\",Si," ",sname,".png",sep=""),height = 9.27, width = 12.76,units='in',res=300) # open graphics device

    # plot data and points 
    par(mfrow=c(1,1), mar=c(4,5,0.5,1), oma=c(8,2,1,3),cex=1) #plot region specs
    plot(subGarc$Rate,ydat, #just an empty figure region initially
         pch="",xlab=Xnames[subGarc$RateType[1]],
         ylim=c(0,ymax),
         yaxs="i",xaxs="i",
         ylab=ytitle,xlim=c(0,1),cex.axis=1.4,cex.lab=1.8,
         font.lab=2,xaxt="n",yaxt="n")
    abline(h=SmsyRef,lty=1,lwd=5) #Smsy ref line
    abline(v=subGarc$Umsy[1],lty=1,lwd=5,col="darkgray") #Umsy ref line
    dashedRefx<-seq(from=-.25,to=subGarc$Umsy[1],by=0.01) #0.85*Smsy ref part1
    dashedRefy<-rep(S85Ref,length(dashedRefx)) #0.85*Smsy ref part2
    lines(dashedRefx,dashedRefy,lty="dotted",lwd=5,col="darkgray") #Add 0.85*Smsy line
    axis(2,seq(0,ymax,ystep),cex.axis=1.4) #pretty y axis
    axis(1,seq(0,1,0.2),xticks,cex.axis=1.4) #pretty x axis
    #now add x,y points, color-type coded for each period
    points(subGarc$Rate[as.character(subGarc$Period)==per1],ydat[as.character(subGarc$Period)==per1],
           pch=21,bg="white",cex=3.2) #1975-84
    points(subGarc$Rate[as.character(subGarc$Period)==per2],ydat[as.character(subGarc$Period)==per2],
           pch=22,bg="chartreuse4",cex=3.2) #1985-1998
    points(subGarc$Rate[as.character(subGarc$Period)==per3],ydat[as.character(subGarc$Period)==per3],
           pch=24,bg="royalblue",cex=3) #1999-2008
    points(subGarc$Rate[as.character(subGarc$Period)==per4],ydat[as.character(subGarc$Period)==per4],
           pch=21,bg="orange",cex=3.2) #2009-present
    box(lwd=2) #add thick frame to plot region
    
    # make the legend (it's actually pretty complicated to get this right); the whole code chunk
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(4.5, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    leg.lab<-c(per1_sub,per2_sub,per3_sub,per4_sub)
    leg.pch<-c(ifelse(is.na(per1_sub),NA,21),ifelse(is.na(per2_sub),NA,22),ifelse(is.na(per3_sub),NA,24),ifelse(is.na(per4_sub),NA,21))
    leg.cex<-c(3.2,3.2,3,3.2)
    leg.bg<-c(ifelse(is.na(per1_sub),NA,"white"),ifelse(is.na(per2_sub),NA,"chartreuse4"),ifelse(is.na(per3_sub),NA,"royalblue"),ifelse(is.na(per3_sub),NA,"orange"))
    legend(x="bottom",leg.lab,pt.bg=leg.bg,pch=leg.pch,bty="n",cex=1.6,ncol=4)
    leg.lab<-c(expression(S~(0.85~S[MSY])),expression(U[MSY]),expression(S[MSY]))
    leg.lty<-c(3,1,1)
    leg.col<-c("darkgray","darkgray","black")
    leg.cex<-c(rep(4,3))#3))

    if(outtype == "pdf") {     
     fig_text_line1 = paste("Figure ",Si,".-",Xnames[subGarc$RateType[1]],", spawning escapement, and threshold reference lines for exploitation rate and spawning escapement by CY", sep="")
     fig_text_line2 = paste("                for the ", subGarc$EISStock[1], " stock of Chinook salmon, 1979-", max(subGarc$Year,na.rm=TRUE),". Cumulative mature-run equivalent exploitation rate calculated from the ", as.character(subGarc$ERIS[1]), " CWT",sep="")
     fig_text_line3 = paste("                exploitation rate indicator stock.",sep="")

     mtext(fig_text_line1, side = 1, adj=0, line=1.5) #work in progress
     mtext(fig_text_line2, side = 1, adj=0, line=2.5) #work in progress
     mtext(fig_text_line3, side = 1, adj=0, line=3.5) #
    }

    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.5, 17, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend("bottomleft",leg.lab[1],lty=leg.lty[1],col=leg.col[1],bty="n",xjust=0,lwd=leg.cex[1],cex=1.6,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.5, 30, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottomleft",leg.lab[2],lty=leg.lty[2],col=leg.col[2],bty="n",xjust=0,lwd=leg.cex[2],cex=1.6,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(2.5, 38, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottomleft",leg.lab[3],lty=leg.lty[3],col=leg.col[3],bty="n",xjust=0,lwd=leg.cex[3],cex=1.6,ncol=4)

    if(outtype == "tiff" || outtype=="png") dev.off() #if tiff then close graphics device FOR EACH loop
} #end loop over all data (for Si in 1:...)
#----------------------------------------------------------
    if(outtype == "pdf") dev.off() #if pdf then close graphics device at the end of ALL loops
#reset to original directory
 setwd(odir)
}


###########################################
# calcMRE
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
calcMRE <- function(HRJ, ESC, fisheryinfotable, stknum, mre_startage="guess", eris_startage="guess") {
  #-------------------------------------------------#
  #Data (extract HRJ data via the Henry block method)
  #-------------------------------------------------#
   #Escapement (note, b/c transformed by BROOD YEAR b/c that's hwo it appears in the Henry block method; however, it could be sped up by doing it by CY...)
    Escapement = with(ESC, tapply(All_Esc, list(stock, by, age), sum))[stknum,,]
   #
    termfisheries=subset(fisheryinfotable, Cohort_Type=="Term")$FisheryNumber
    pretermfisheries=subset(fisheryinfotable, Cohort_Type=="Ocean")$FisheryNumber
    oceannetfisheries=subset(fisheryinfotable, Cohort_Type=="OceanNet")$FisheryNumber
   #Preterminal fishery catch
    PreterminalCat = with(subset(HRJ,fishery%in%pretermfisheries), tapply(NomTot, list(stock, by, age), sum))[stknum,,]
   #Ocean net fishery catch
    OcneannetCat = with(subset(HRJ,fishery%in%oceannetfisheries), tapply(NomTot, list(stock, by, age), sum))[stknum,,]
   #Terminal fishery catch
    TermCat = with(subset(HRJ,fishery%in%termfisheries), tapply(NomTot, list(stock, by, age), sum))[stknum,,]
   #Ocean cohort size (after natural mortality)
    OceanCohort = with(HRJ, tapply(Pop, list(stock, by, age), max))[stknum,,]

  #------------#
  #Computations
  #------------#
   #If eris age guess
    if(eris_startage=="guess") {
     #If not all of age 2 assume a 2-5 (or 2-6) age stock
      if(!all(is.na(Escapement[,as.numeric(colnames(Escapement))==2]))) eris_startage = 2
     #If all of age 2 are na, assume a 3-6 age stock
      if(all(is.na(Escapement[,as.numeric(colnames(Escapement))==2]))) eris_startage = 3
    }
   #If mre age guess
    if(mre_startage=="guess") mre_startage = eris_startage + 1
   #determine end age
    #if all of the age 6 data is NA, then eris startage should be 5
     if(all(is.na(Escapement[,as.numeric(colnames(Escapement))==6])))  eris_endage = 5
    #and if not all of the age 6 data is NA, then eris startage should be 5
     if(!all(is.na(Escapement[,as.numeric(colnames(Escapement))==6]))) eris_endage = 6
    #use eris_startage and eris_endage to compute the number of ages
     eris_ages = eris_startage:eris_endage
     eris_nages = length(eris_ages)
   #Differentiate terminal and non-terminal fish in the ocean net catch (i.e. if it's ERIS start age = 2, it implies that age 4 and older fish are mature fish in the ocean net fishery)
    OcneannetTerm = OcneannetCat
    OcneannetPreterm = OcneannetCat
    OcneannetTerm[,as.numeric(colnames(OcneannetTerm))<(eris_startage+2)] <- 0
    OcneannetPreterm[,as.numeric(colnames(OcneannetTerm))>=(eris_startage+2)] <- 0
    if(!(eris_startage==2 || eris_startage==3)) cat("WARNING: an ERIS start age of:", eris_startage, "is not generally recognized\n")
   #Survival from pre-terminal fisheries
    PreterminalHR = (PreterminalCat+OcneannetPreterm)/OceanCohort
    PreterminalSurv = 1-PreterminalHR
   #Mature terminal run
    MatureTermRun =  TermCat + OcneannetTerm + Escapement
  #Calculate cumulative pre-terminal survival
   #If a 2-5 (or 2-6) age stock
    if(eris_startage==2) CumSurvival = t(apply(PreterminalSurv, 1, cumprod))
   #If a 3-6 age stock
    if(eris_startage==3) CumSurvival = cbind(PreterminalSurv[,1],t(apply(PreterminalSurv[,2:ncol(PreterminalSurv)], 1, cumprod)))
  #Compute potential escapement
   PotentialEscapement = MatureTermRun/CumSurvival
  #Compute cumulative potential escapement
   hold = .convertAgeMatrixMRE(PotentialEscapement)
   CumPotentialEscapement = with(subset(hold, age>=mre_startage), tapply(value, list(cy), sum, na.rm=T))
  #Compute observed escapement
   hold = .convertAgeMatrixMRE(Escapement)
   CumObsEscapement = with(subset(hold, age>=mre_startage), tapply(value, list(cy), sum, na.rm=T))
   #note the following IF escapement is done by CY: CumObsEscapement = rowSums(Escapement[,as.numeric(colnames(Escapement))>=3], na.rm=TRUE)
  #valid mre calc? adds two variables. cy_nagesMRE & valid_mre. count the number of ages 
   validmre_mat = with(hold, tapply(value, list(cy, age), sum))
   cy_nagesMRE = rowSums(!apply(validmre_mat[,as.numeric(colnames(validmre_mat))%in%mre_startage:eris_endage],2,is.na))
   valid_mre = cy_nagesMRE==length(mre_startage:eris_endage)
  #### ADD INC AGE INFO LOGIC HERE #### 
  #Compute MRE
   MRE = 1 - (CumObsEscapement/CumPotentialEscapement)
  #number of ages present in a given brood year
   by_nages = rowSums(!apply(Escapement,2,is.na))
  #number of ages present in a given calednar year
   cy_nages = rowSums(!apply(with(hold, tapply(value, list(cy, age), sum)),2,is.na))
  #Subset cy_nages & valid_mre & cy_nagesMRE
   cy_nages = cy_nages[names(cy_nages)%in%names(MRE)]
   valid_mre = valid_mre[names(valid_mre)%in%names(MRE)]
   cy_nagesMRE = cy_nagesMRE[names(cy_nagesMRE)%in%names(MRE)]
  #Combine the intermediate calcs
   henry_block1 = cbind(PreterminalSurv, MatureTermRun, Escapement, CumSurvival, PotentialEscapement, by_nages)
   eris_ages= colnames(Escapement) #override eris_ages to what's actually present for auto-naming
   colnames(henry_block1) = c(paste("PretermSurv_Age",eris_ages,sep=""), paste("TermRun_Age",eris_ages,sep=""),paste("Escap_Age",eris_ages,sep=""),paste("CumPretermSurv_Age",eris_ages,sep=""),paste("PotEscap_Age",eris_ages,sep=""),"by_nages")
  #Combine the final calcs
   henry_block2 = cbind(CumPotentialEscapement, CumObsEscapement, MRE, cy_nagesMRE, valid_mre)
  #------#
  #Output
  #------#
  #add the output into a list object
   out=list()
   out$IntermediateCalcs = henry_block1
   out$FinalCalcs = henry_block2
   out$Inputs = list(StkNum = stknum, MRECalcStartAge=mre_startage, ERISCalcStartAge=eris_startage)
  #return output
   return(out)
}

###########################################
# calcMREAll
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
calcMREAll <- function(HRJ, ESC, fisheryinfotable, mre_startage="guess", eris_startage="guess") {
 IntermediateCalcs = list()
 FinalCalcs = list()
 nstks = length(unique(HRJ$stock))
 stks = sort(unique(HRJ$stock))
 for(i in 1:nstks) {
  stkNum = stks[i]
  cat("calc",i,"of",nstks,"for hrj stock number",stkNum,"\n")
  #set defaults for mre and eris start age
   mre_startage2  = "guess"
   eris_startage2 = "guess"
  #if mre_startage is not equal to guess, should be a vector...
   if(class(mre_startage)=="data.frame") {
    mre_startage2 = subset(mre_startage, stknum==stkNum)$MRE_StartAge
    #if length 0 (as in not specified)
    if(length(mre_startage2)==0) mre_startage2="guess"
    if(length(mre_startage2)>1) mre_startage2=mre_startage2[1]
   }
   if(class(eris_startage)=="data.frame") {
    eris_startage2 = subset(eris_startage, stknum==stkNum)$ERIS_StartAge
    #if length 0 (as in not specified)
    if(length(eris_startage2)==0) eris_startage2="guess"
    if(length(eris_startage2)>1) eris_startage2=eris_startage2[1]
   }
  #mre calcs
   tmp = calcMRE(HRJ, ESC, fisheryinfotable=fisheryinfotable, stknum=stkNum, mre_startage=mre_startage2, eris_startage=eris_startage2)
  #manipulate data
   IntermediateCalcs[[i]] = cbind(stock=rep(stkNum, nrow(tmp$IntermediateCalcs)), by=as.numeric(rownames(tmp$IntermediateCalcs)), tmp$IntermediateCalcs)
   FinalCalcs[[i]] = cbind(stock=rep(stkNum, nrow(tmp$FinalCalcs)), cy=as.numeric(rownames(tmp$FinalCalcs)), mrecalcstartage=rep(tmp$Inputs$MRECalcStartAge, nrow(tmp$FinalCalcs)), tmp$FinalCalcs)
 }
 #collapse stock-specific results into a list
  out = list()
  out$IntermediateCalcs = do.call("rbind", IntermediateCalcs)
  out$FinalCalcs = do.call("rbind", FinalCalcs)
 #return output
  return(out)
}

###########################################
# right() & left()
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
#right 
right = function (string, char){
 substr(string,nchar(string)-(char-1),nchar(string))
}
#left 
left = function (string, char){
 substr(string,1,char)
}

###########################################
# pivotMatrix
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
pivotMatrix <- function(x, repeatcolumns, movecolumns) {
 out=list()
 for(i in 1:length(movecolumns)) out[[i]] = cbind(x[,repeatcolumns], ColName=names(x)[movecolumns[i]], Repeat=x[,movecolumns[i]])
 return(do.call("rbind",out))
}

###########################################
# externalHRadjustment
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
externalHRadjustment <- function(x, hrt, hrjstk, type=c("tm","lc"), newstkname=NULL) {
#if hrj data is not laid out by calendar year print an error message
 if(x$HRJformat!="calendar") cat("ERROR: HRJ data must be sorted by calendar year to apply the external HR adjustment to work correctly\n")
#determine which stock, by stock number of by name
 if(is.numeric(hrjstk)) select = hrjstk
 if(is.character(hrjstk)) select = grep(hrjstk, x$stknames)
#for each of the three files that need to be adjusted (note they may not all exist)
 unadj_hrj = subset(x$HRJ_P, stock==select)
 unadj_esc = subset(x$ESC_BY, stock==select) #note that ESC_BY and ESC_CY are the same
#create holders for the adjusted results
 adj_hrj = unadj_hrj
 adj_esc = unadj_esc
#determine hrt fisheries
 hrtfisheries = as.numeric(substr(names(hrt)[2:ncol(hrt)],2,4))
 nhrtfisheries = length(hrtfisheries)
#determine the years in both hrt and hrj files
 years_in_both = c(1900:2100)[(1900:2100%in%unadj_esc$cy)+(1900:2100%in%hrt$CY)==2]
#FOR EACH FISHERY IN THE HRT FILE
 for(i in 1:nhrtfisheries) {
  #subset hrj data to only the fishery in the hrt file
   adj_hrj_hrtfishery = subset(adj_hrj, fishery==hrtfisheries[i])
  #compute adjusted terminal cat
   Ttilda_AEQCat = (adj_hrj_hrtfishery[,paste("AEQCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_AEQTot = (adj_hrj_hrtfishery[,paste("AEQTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_NomCat = (adj_hrj_hrtfishery[,paste("NomCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_NomTot = (adj_hrj_hrtfishery[,paste("NomTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
  #update
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQCat",2:6,sep="")] = Ttilda_AEQCat
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQTot",2:6,sep="")] = Ttilda_AEQTot
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomCat",2:6,sep="")] = Ttilda_NomCat
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomTot",2:6,sep="")] = Ttilda_NomTot
  #compute adjusted escapement, noting that you must make a decision here whether or not to move the IM over to escapement (or not)
   if(type=="tm") Etilda = (adj_hrj_hrtfishery[,paste("NomTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(1-hrt[hrt$CY%in%years_in_both,i+1])
   if(type=="lc") Etilda = (adj_hrj_hrtfishery[,paste("NomCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(1-hrt[hrt$CY%in%years_in_both,i+1])
  #update
   adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")] = Etilda
  }
 #update the stock numbering with the new tables
  adj_esc$stock = x$nstocks+1
  adj_hrj$stock = x$nstocks+1
 #append the new reslts to the correc tables
  x$HRJ_P = rbind(x$HRJ_P,adj_hrj)
  x$ESC_BY = rbind(x$ESC_BY,adj_esc)
  x$ESC_CY = rbind(x$ESC_CY,adj_esc)
 #update a few more things 
  if(is.null(newstkname)) newstkname = paste(x$stknames[select],"adj",sep="")
  x$nstocks = x$nstocks+1
  x$stknames = c(x$stknames, newstkname)
 #return the updated HRJ data to users
  return(x)
}

###########################################
# msfSITadjustment
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
msfSITadjustment <- function(x, hrjstk, type=c("tm","lc"), newstkname=NULL) {

#in progress


#if hrj data is not laid out by calendar year print an error message
 if(x$HRJformat!="calendar") cat("ERROR: HRJ data must be sorted by calendar year to apply the external HR adjustment to work correctly\n")
#determine which stock, by stock number of by name
 if(is.numeric(hrjstk)) select = hrjstk
 if(is.character(hrjstk)) select = grep(hrjstk, x$stknames)
#for each of the three files that need to be adjusted (note they may not all exist)
 unadj_hrj = subset(x$HRJ_P, stock==select)
 unadj_esc = subset(x$ESC_BY, stock==select) #note that ESC_BY and ESC_CY are the same
#create holders for the adjusted results
 adj_hrj = unadj_hrj
 adj_esc = unadj_esc
#determine hrt fisheries
 hrtfisheries = as.numeric(substr(names(hrt)[2:ncol(hrt)],2,4))
 nhrtfisheries = length(hrtfisheries)
#determine the years in both hrt and hrj files
 years_in_both = c(1900:2100)[(1900:2100%in%unadj_esc$cy)+(1900:2100%in%hrt$CY)==2]
#FOR EACH FISHERY IN THE HRT FILE
 for(i in 1:nhrtfisheries) {
  #subset hrj data to only the fishery in the hrt file
   adj_hrj_hrtfishery = subset(adj_hrj, fishery==hrtfisheries[i])
  #compute adjusted terminal cat
   Ttilda_AEQCat = (adj_hrj_hrtfishery[,paste("AEQCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_AEQTot = (adj_hrj_hrtfishery[,paste("AEQTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_NomCat = (adj_hrj_hrtfishery[,paste("NomCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
   Ttilda_NomTot = (adj_hrj_hrtfishery[,paste("NomTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(hrt[hrt$CY%in%years_in_both,i+1])
  #update
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQCat",2:6,sep="")] = Ttilda_AEQCat
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("AEQTot",2:6,sep="")] = Ttilda_AEQTot
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomCat",2:6,sep="")] = Ttilda_NomCat
   adj_hrj[adj_hrj$fishery==hrtfisheries[i]&adj_hrj$cy%in%years_in_both, paste("NomTot",2:6,sep="")] = Ttilda_NomTot
  #compute adjusted escapement, noting that you must make a decision here whether or not to move the IM over to escapement (or not)
   if(type=="tm") Etilda = (adj_hrj_hrtfishery[,paste("NomTot",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(1-hrt[hrt$CY%in%years_in_both,i+1])
   if(type=="lc") Etilda = (adj_hrj_hrtfishery[,paste("NomCat",2:6,sep="")]+adj_esc[,paste("All_Esc",2:6,sep="")])[adj_esc$cy%in%years_in_both,]*(1-hrt[hrt$CY%in%years_in_both,i+1])
  #update
   adj_esc[adj_esc$cy%in%years_in_both,paste("All_Esc",2:6,sep="")] = Etilda
  }
 #update the stock numbering with the new tables
  adj_esc$stock = x$nstocks+1
  adj_hrj$stock = x$nstocks+1
 #append the new reslts to the correc tables
  x$HRJ_P = rbind(x$HRJ_P,adj_hrj)
  x$ESC_BY = rbind(x$ESC_BY,adj_esc)
  x$ESC_CY = rbind(x$ESC_CY,adj_esc)
 #update a few more things 
  if(is.null(newstkname)) newstkname = paste(x$stknames[select],"adj",sep="")
  x$nstocks = x$nstocks+1
  x$stknames = c(x$stknames, newstkname)
 #return the updated HRJ data to users
  return(x)
}

###########################################
# cyer
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
#cyer
cyer <- function(stkname, hrjobj, fmap, type=c("AEQCat","AEQTot","NomCat","NomTot"), strays=c("addtoesc","ignore","separate"), ages=2:6) {
#
 stkloc = grep(stkname, hrjobj$stknames)
 hrj=subset(hrjobj$HRJ_P,stock==stkloc)
 esc=esc=subset(hrjobj$ESC_CY,stock==stkloc)
 if(hrjobj$HRJformat=="calendar") yearformat="cy"
 if(hrjobj$HRJformat=="brood") {
    yearformat="brood"
    names(hrj)[2] = "cy"
    names(esc)[2] = "cy"
 }

#what to do with strays? note that the same information appears twice (both in the hrj table and the esc table)..
 #so to prevent double counting, this function will only reference the info in the esc table, so make sure the ESC stray line is 'blanked' out
  fmap[left(fmap$FisheryName,1)=="X",]$CYER=NA
  fmap$CYER=droplevels(fmap$CYER)
  stray_pivot = NULL
#age check
 if(any(ages<2) | any(ages>6)) ages = ages[!c(ages<2 | ages>6)]
 if(length(ages)<1) {
  cat("ERROR: age vector not valid, program will default to using all ages\n\n")
  ages = 2:6
 }
#
 if(strays=="addtoesc") {
   esctype = c(paste("All_Esc",ages,sep=""),paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
 } else if (strays=="ignore") { 
   esctype = paste("All_Esc",ages,sep="")
 } else if (strays=="separate") {
   esctype = paste("All_Esc",ages,sep="")
   straytype = c(paste("CA_Esc",ages,sep=""),paste("US_Esc",ages,sep=""))
   stray_pivot = pivotMatrix(esc, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(straytype,names(esc)))
   names(stray_pivot)[3:4] = c("type","value")
 } else { cat("ERROR: USER OPTION FOR strays NOT RECOGNIZED\n")  }
#
 esc_pivot = pivotMatrix(esc, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(esctype,names(esc)))
 names(esc_pivot)[3:4] = c("type","value")
#
 hrj_pivot = pivotMatrix(hrj, repeatcolumns=match(c("cy","fishery"),names(hrj)), movecolumns=match(c(paste(type,ages,sep="")),names(hrj)))
 names(hrj_pivot)[3:4] = c("type","value")
#
 hrj_pivot$age = as.numeric(right(as.character(hrj_pivot$type),1))
 esc_pivot$age = as.numeric(right(as.character(esc_pivot$type),1))
#
 hrj_pivot = merge(hrj_pivot, fmap[,c("FisheryNumber","CYER")], by.x="fishery", by.y="FisheryNumber")
#
 cat_byage = with(hrj_pivot, tapply(value, list(cy,age),sum))
 esc_byage = with(esc_pivot, tapply(value, list(cy,age),sum))
#determine which age classes are present
 agespresent = rep(NA, nrow(cat_byage))
 for(i in 1:nrow(cat_byage)) agespresent[i]=paste(colnames(cat_byage)[!is.na(cat_byage[i,])], collapse=",")
#if strays are present and are to have separate accounting
 if(!is.null(stray_pivot)) {
  #note that if strays are present in the hrj_pivot, then the hrj_pivot[!is.na(hrj_pivot$CYER),] needs to be further subsetted to remove said fishery
  numer = with(hrj_pivot[!is.na(hrj_pivot$CYER),], tapply(value, list(cy,CYER),sum,na.rm=TRUE))
  #numer = cbind(numer,STRAY=with(stray_pivot, tapply(value, list(cy),sum, na.rm=TRUE)),ESCAP=with(esc_pivot, tapply(value, list(cy),sum, na.rm=TRUE))) #equivlanet to...numer$ESC = denom-rowSums(numer)
  #denom = with(hrj_pivot[!is.na(hrj_pivot$CYER),], tapply(value, list(cy),sum,na.rm=TRUE))+with(esc_pivot, tapply(value, list(cy),sum, na.rm=TRUE))+with(stray_pivot, tapply(value, list(cy),sum, na.rm=TRUE))
  numer = cbind(numer,STRAY=with(stray_pivot[!is.na(stray_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE)),ESCAP=with(esc_pivot[!is.na(esc_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))) #equivlanet to...numer$ESC = denom-rowSums(numer)
  denom = with(hrj_pivot[!is.na(hrj_pivot$CYER),], tapply(value, list(cy),sum,na.rm=TRUE))+with(esc_pivot[!is.na(esc_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))+with(stray_pivot[!is.na(stray_pivot$fishery),], tapply(value, list(cy),sum, na.rm=TRUE))
#else
 } else {
  numer = with(hrj_pivot, tapply(value, list(cy,CYER),sum,na.rm=TRUE))
  numer = cbind(numer,Esc=with(esc_pivot, tapply(value, list(cy),sum, na.rm=TRUE))) #equivlanet to...numer$ESC = denom-rowSums(numer)
  denom = with(hrj_pivot, tapply(value, list(cy),sum,na.rm=TRUE))+with(esc_pivot, tapply(value, list(cy),sum, na.rm=TRUE))
 }
#
 morttab = matrix(NA,ncol=ncol(numer),nrow=nrow(numer))
 colnames(morttab) = colnames(numer)
 rownames(morttab) = rownames(numer)
 for(i in 1:ncol(numer)) morttab[,i] = numer[,i]/denom 
#
 out = list(Years = as.numeric(row.names(morttab)), Mortality = numer, PercentMortality = morttab, Recoveries = denom, AgesPresent = agespresent, YearFormat = yearformat, ShakerMethod="P Method")
 class(out) = "CYER"
 return(out)
}

###########################################
# print.CYER
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
print.CYER <- function(x, digits=1, type="percent", prop=FALSE, yearstoshow=1979:2017, criteria="levels") {
 whichyears = x$Years %in% yearstoshow
 morttoshow = NA
 if(type=="percent") { 
  morttoshow = x$PercentMortality[whichyears,]
  if(!prop) morttoshow = morttoshow * 100
 } else if (type=="number") {
  morttoshow = x$Mortality[whichyears,]
 } else { cat("DNE\n") } 
 if(!is.null(digits)) morttoshow= round(morttoshow, digits)
 morttoshow=ifelse(is.nan(morttoshow), NA, morttoshow)
 wherearetheblanks = x$AgesPresent[whichyears]==""
 morttoshow[wherearetheblanks,] = NA
 x$Recoveries[whichyears][wherearetheblanks] = NA

 criteriaout=rep(NA,length(x$Years))
 if(!is.null(criteria)) {
  if(criteria=="levels") {
   #apply criteria in the following sequence:
    criteriaout[nchar(x$AgesPresent)==3] = "shade" #always shade when 2 age classes are present
    criteriaout[x$Recoveries<105] = "shade" #
    criteriaout[nchar(x$AgesPresent)<=1] = "omit" #always print 'failed criteria' when only 1 age class is present
   #and only set criteria equal to ok if it meets the standards:
    criteriaout[x$Recoveries>=105 & nchar(x$AgesPresent)>3] = "ok"
  }
 }
 if(x$YearFormat=="cy") tmp = data.frame(CatchYear=x$Years[whichyears], "Ages"=x$AgesPresent[whichyears], "Recoveries"=x$Recoveries[whichyears], morttoshow, criteria=criteriaout[whichyears])
 if(x$YearFormat=="brood") tmp = data.frame(BroodYear=x$Years[whichyears], "Ages"=x$AgesPresent[whichyears], "Recoveries"=x$Recoveries[whichyears], morttoshow, criteria=criteriaout[whichyears])

 print(tmp,row.names=FALSE)
}

###########################################
# summary.CYER
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
summary.CYER <- function(x, yearranges=list(1979:1984,1985:1995,1996:1998,1999:2008,2009:2017)) {
#
 yearlabs = rep(NA,length(yearranges))
 for(i in 1:length(yearranges)) yearlabs[i] = paste(right(min(yearranges[[i]]),2), right(max(yearranges[[i]]),2), sep="-")
#
 mort_summary = matrix(NA, nrow=length(yearranges), ncol=ncol(x$Mortality))
 colnames(mort_summary) = colnames(x$Mortality)
 perc_summary = matrix(NA, nrow=length(yearranges), ncol=ncol(x$PercentMortality))
 colnames(perc_summary) = colnames(x$PercentMortality)
 recov_summary = rep(NA,length(yearranges))
#
 for(i in 1:length(yearranges)) {
  whichyears = x$Years %in% yearranges[[i]]
  mort_summary[i,] = colMeans(x$Mortality[whichyears,],na.rm=TRUE)
  perc_summary[i,] = colMeans(x$PercentMortality[whichyears,],na.rm=TRUE)
  #recov_summary[i] = sum(x$Recoveries[whichyears],na.rm=TRUE)
  recov_summary[i] = round(mean(x$Recoveries[whichyears],na.rm=TRUE),0)
 }
#
 out <- list(YearLabs=yearlabs,AvgMortality=mort_summary, AvgPercentMortality=perc_summary, SumRecoveries=recov_summary)
 class(out) = "summary.CYER"
 return(out)
}

###########################################
# print.summary.CYER
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
print.summary.CYER <- function(x, digits=1, type="percent", prop=FALSE) {
 if(type=="percent") { 
  morttoshow = x$AvgPercentMortality
 if(!prop) morttoshow = morttoshow * 100
 } else if (type=="number") {
  morttoshow = x$AvgMortality
 } else { cat("DNE\n") } 
 if(!is.null(digits)) morttoshow= round(morttoshow, digits)
 morttoshow=ifelse(is.nan(morttoshow), NA, morttoshow)
 tmp = data.frame("Years"=x$YearLabs, "Recoveries"=x$SumRecoveries, morttoshow)
 print(tmp,row.names=FALSE)
}

###########################################
# cyerAll
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
#cyerAll
cyerAll <- function(x, hrj, esc, fmap, type=c("AEQCat","AEQTot","NomCat","NomTot"), strays=c("addtoesc","ignore","separate")) {

}

###########################################
# MRE2Plot
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
MRE2Plot <- function(esc, mre, smap, stknames, mrecriteria, auxdata=NULL) {
 #
  Escap2 = pivotMatrix(esc, 1:3, 4:ncol(esc))
  names(Escap2)[4:5] = c("EISStock","Escapement")
 #Subset the escapement workbook to only the garcia stocks
  Escap3 = subset(Escap2, EISStock%in%unique(smap$EIS_StockName))
 #Append slookups to Escapement data frame
  Escap4 = merge(x=Escap3, y=smap, by.x="EISStock",by.y="EIS_StockName")
 #Append the mre calcs to the Escapement data frame
  mre_finalcalcs = as.data.frame(mre$FinalCalcs)
  hrjstknames = data.frame(HRJ_Number=1:length(stknames),ERIS_Name=stknames)
  mre_finalcalcs = merge(mre_finalcalcs, hrjstknames, by.x="stock", by.y="HRJ_Number")
  mre2 = mre_finalcalcs[,c("cy","MRE","ERIS_Name","valid_mre","cy_nagesMRE")]
 #IF NAN, replace with NA
  mre2$MRE=ifelse(is.nan(mre2$MRE),NA,mre2$MRE)
 #IF valid_mre is not equal to TRUE, replace with NA
  if(mrecriteria) {
  #criteria 1 - remove MRE calcs that have ANY missing data (usually 3 years of returns)
   #mre2$MRE=ifelse(mre2$valid_mre!=TRUE,NA,mre2$MRE)
  #criteria 2 - remove MRE calcs that have n ages worth of data (current criteria is set to remove calcs based on only a single age of return data)
   mre2$MRE=ifelse(mre2$cy_nagesMRE<1,NA,mre2$MRE)
  }

 #Append AUX DATA
  if(!is.null(auxdata)) mre2 = rbind(mre2,.convertAuxMREMatrix(auxdata))

 #SUBSET mre data to only the year's of interest (i.e. year's with escapement data!)
  mre2 = subset(mre2, cy%in%unique(Escap4$Year))
  mre3 = list()
  for(i in 1:nrow(smap)) {
   escaptemp=subset(Escap4, StockNum==i)
   mretemp  =subset(mre2, ERIS_Name==as.character(escaptemp$ERIS[1]))
   #re-order the columns in mretemp to match escaptemp
   mretemp  =mretemp[match(escaptemp$Year, mretemp$cy),]
   #occasionally there's less mre data than escapement data, so override the 'cy' column from the mre with the ESC 'Year' column
   mretemp$cy = escaptemp$Year
   #dunno if the following check is needed now given the line of code above: meh
   if(!all(escaptemp$Year==mretemp$cy)) cat("WARNING!: year mismatch between MRE and Escapement data for stock:", i, "\n")
   mre3[[i]] = cbind(escaptemp, Rate=mretemp$MRE)
  }
  mre4 = do.call("rbind", mre3)
#
 return(mre4)
}

###########################################
# MRE2Excel
#
# Description
# ------------------
#
# Argument(s)
# ------------------
# 
# Output(s)
# ------------------
# 
###########################################
MRE2Excel <- function(x, stknames, filename="mre calcs.xlsx") {
 require(openxlsx)
 colnames(MRE$IntermediateCalcs)
 colnames(MRE$FinalCalcs)
 nstks = length(stknames)
 wb <- createWorkbook("MRECalcs")
 #add the indiv stock sheets
 for(i in 1:nstks) {
  addWorksheet(wb=wb, sheetName=stknames[i])
  intercalcs = MRE$IntermediateCalcs[MRE$IntermediateCalcs[,1]==i,]
  finalcalcs = MRE$FinalCalcs[MRE$FinalCalcs[,1]==i,]
  out2Excel = list(Intermediate_Calculation=intercalcs, MRE_Calculation=finalcalcs)
  writeData(wb=wb, sheet=stknames[i], x=out2Excel[[1]], startCol=1 , borders="surrounding")
  writeData(wb=wb, sheet=stknames[i], x=out2Excel[[2]], startCol=30, borders="surrounding")
 }
 #add the all stock sheet
  addWorksheet(wb=wb, sheetName="all stocks")
  writeData(wb=wb, sheet="all stocks", x=MRE$IntermediateCalcs, startCol=1 , borders="surrounding")
  writeData(wb=wb, sheet="all stocks", x=MRE$FinalCalcs, startCol=30, borders="surrounding")
  writeData(wb=wb, sheet="all stocks", x=data.frame(stock=1:length(stknames),stknames=stknames), startCol=39, borders="surrounding")
 #save workbook
 saveWorkbook(wb, filename, overwrite = TRUE)
 #returns nothing
}

###########################################
# plotGarcia
#
# Description
# ------------------
# Adapted plot all stock code by Pete McHugh
# 
#
# Argument(s)
# ------------------
# garcia
# outdir
# outtype - either "pdf" or "tff"
# Output(s)
# ------------------
# 
###########################################
plotGarcia <- function(Garcia) {

# set some general specs relevant to all plots ##
# Periods for coloring points
per1=as.character(levels(Garcia$Period)[1])
per2=as.character(levels(Garcia$Period)[2])
per3=as.character(levels(Garcia$Period)[3])
per4=as.character(levels(Garcia$Period)[4])
# The options for x-axis labeling (MRE ER vs. CY ER) and values to display
Xnames<-c("Mature-run equivalent exploitation rate (%)","Calendar year harvest rate (%)")
xticks<-c("0%","20%","40%","60%","80%","100%")

# Now subset data, set specs, and make figures
  subGarc=Garcia

  # Set stock(plot)-specific specs (i.e., the plotting 
  # range and y tick width ); also, do data manipulations
  # idea here is to make figure scale/display vary for
  # optimal display across a wide range of escapements
  m1<-max(subGarc$Escapement,na.rm=TRUE)/0.95
  yt1<-c("Spawning escapement (thousands)","Spawning escapement") # use different title, if /1K vs. not
  ytitle<-yt1[2] #raw values as default, /1K otherwise 
  ydat<-subGarc$Escapement #raw values as default, /1K otherwise
  sname<-as.character(subGarc$EISStock[1]) #stock (from old excel worksheet name) for fig naming
  fname<-paste(as.character(subGarc$Stock[1]),".tif",sep="")
  SmsyRef<-subGarc$Smsy[1]
  S85Ref<-subGarc$S85[1]
  if(m1>=250000){
    ymax<-roundUp(m1,50000)/1000
    ystep<-50000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=100000) {
    ymax<-roundUp(m1,25000)/1000
    ystep<-25000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=50000) {
    ymax<-roundUp(m1,10000)/1000
    ystep<-10000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if (m1>=10000) {
    ymax<-roundUp(m1,5000)/1000
    ystep<-5000/1000
    ytitle<-yt1[1]
    ydat<-subGarc$Escapement/1000
    SmsyRef<-subGarc$Smsy[1]/1000
    S85Ref<-subGarc$S85[1]/1000
  } else if(m1>=5000) {
    ymax<-roundUp(m1,1000)
    ystep<-1000
  } else if(m1>=1000) {
    ymax<-roundUp(m1,500)
    ystep<-500
  } else {
    ymax<-roundUp(m1,100)
    ystep<-100
  }
  # Now actually plot them (write to file)
    # plot data and points 
    par(mfrow=c(1,1), mar=c(4,5,0.5,1), oma=c(8,2,1,3),cex=1) #plot region specs
    plot(subGarc$Rate,ydat, #just an empty figure region initially
         pch="",xlab=Xnames[subGarc$RateType[1]],
         ylim=c(0,ymax),
         yaxs="i",xaxs="i",
         ylab=ytitle,xlim=c(0,1),cex.axis=1.4,cex.lab=1.8,
         font.lab=2,xaxt="n",yaxt="n")
    abline(h=SmsyRef,lty=1,lwd=5) #Smsy ref line
    abline(v=subGarc$Umsy[1],lty=1,lwd=5,col="darkgray") #Umsy ref line
    dashedRefx<-seq(from=-.25,to=subGarc$Umsy[1],by=0.01) #0.85*Smsy ref part1
    dashedRefy<-rep(S85Ref,length(dashedRefx)) #0.85*Smsy ref part2
    lines(dashedRefx,dashedRefy,lty="dotted",lwd=5,col="darkgray") #Add 0.85*Smsy line
    axis(2,seq(0,ymax,ystep),cex.axis=1.4) #pretty y axis
    axis(1,seq(0,1,0.2),xticks,cex.axis=1.4) #pretty x axis
    #now add x,y points, color-type coded for each period
    points(subGarc$Rate[as.character(subGarc$Period)==per1],ydat[as.character(subGarc$Period)==per1],
           pch=21,bg="white",cex=3.2) #1975-84
    points(subGarc$Rate[as.character(subGarc$Period)==per2],ydat[as.character(subGarc$Period)==per2],
           pch=22,bg="chartreuse4",cex=3.2) #1985-1998
    points(subGarc$Rate[as.character(subGarc$Period)==per3],ydat[as.character(subGarc$Period)==per3],
           pch=24,bg="royalblue",cex=3) #1999-2008
    points(subGarc$Rate[as.character(subGarc$Period)==per4],ydat[as.character(subGarc$Period)==per4],
           pch=21,bg="orange",cex=3.2) #2009-present
    box(lwd=2) #add thick frame to plot region
    
    # make the legend (it's actually pretty complicated to get this right); the whole code chunk
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(5, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    leg.lab<-c(per1,per2,per3,per4)
    leg.pch<-c(21,22,24,21)
    leg.cex<-c(3.2,3.2,3,3.2)
    leg.bg<-c("white","chartreuse4","royalblue","orange")
    legend(x="bottom",leg.lab,pt.bg=leg.bg,pch=leg.pch,bty="n",cex=1.4,ncol=4)
    leg.lab<-c("S (0.85 Smsy)","Umsy","Smsy")
    leg.lty<-c(3,1,1)
    leg.col<-c("darkgray","darkgray","black")
    leg.cex<-c(rep(4,3))#3))

    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.5, 17, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend("bottom",leg.lab[1],lty=leg.lty[1],col=leg.col[1],bty="n",xjust=0,lwd=leg.cex[1],cex=1.25,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.5, 22, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab[2],lty=leg.lty[2],col=leg.col[2],bty="n",xjust=0,lwd=leg.cex[2],cex=1.25,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.5, 37, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab[3],lty=leg.lty[3],col=leg.col[3],bty="n",xjust=0,lwd=leg.cex[3],cex=1.25,ncol=4)

}

###########################################
# .convertAuxMREMatrix
#
# Description
# ------------------
# converts aux data into a format that can be appended to the mre final calc data frame
# 
#
# Argument(s)
# ------------------
# x - input matrix of auxiliary mre calcs in the layout of
#     * column 1 year, 
#     * column 2:n mre calcs by stock, 
#     * column name is numeric, corresponding to the model fishery number
#
# Output(s)
# ------------------
# out - a dataframe of the aux catch data in the layout used elsewhere
#
###########################################
.convertAuxMREMatrix <- function(x) {
	#note that the format of aux mre matrix is year, stock, and mre
	snames = names(x)[-1] #fishery names reformatted - R adds an "X" to numeric variables
	out = data.frame(cy = rep(matrix(as.matrix(x[,1])),ncol(x)-1),
	                 MRE = matrix(as.matrix(x[,2:ncol(x)])),
	                 ERIS_Name = matrix(sapply(snames, rep, nrow(x))))
      out$valid_mre   = NA
      out$cy_nagesMRE = NA
	return(out)
}

###########################################
# plotSynopticSynoptic
#
# Description
# ------------------
# plots the synoptic synoptic plot
# 
#
# Argument(s)
# ------------------
# garcia
# outdir
# outtype - either "pdf" or "tff" or null
# Output(s)
# ------------------
# 
###########################################
plotSynopticSynoptic <- function(garcia, year, outtype=NA, verbose=FALSE, filename=NA, addeistext=FALSE) {
#Subset garcia data
 subGarc = subset(garcia, Year==year)
#drop levels
# set some general specs relevant to all plots ##
# Periods for coloring points
 per1=as.character(levels(subGarc$EIS_Region)[1]) #ak
 per2=as.character(levels(subGarc$EIS_Region)[2]) #canada
 per3=as.character(levels(subGarc$EIS_Region)[3]) #col r
 per4=as.character(levels(subGarc$EIS_Region)[4]) #tbr
 per5=as.character(levels(subGarc$EIS_Region)[5]) #wa/or coast

#Compute the indices
 subGarc$EscIndex = with(subGarc, ifelse(is.na(LowerGoal),Escapement/Smsy, Escapement/LowerGoal))
 subGarc$MREIndex = with(subGarc, Rate/Umsy)
# Set stock(plot)-specific specs (i.e., the plotting 
# range and y tick width ); also, do data manipulations
# idea here is to make figure scale/display vary for
# optimal display across a wide range of escapements
  ymax<-max(subGarc$EscIndex,na.rm=TRUE)/0.95
  ytitle<-expression(Escapement~to~S[MSY]~Index)
  ydat<-subGarc$EscIndex #raw values as default, /1K otherwise
  ystep = 7
  SmsyRef<-1
  S85Ref<-.85
# The options for x-axis labeling (MRE ER vs. CY ER) and values to display
 xmax=max(1,max(subGarc$MREIndex,na.rm=TRUE)/.95)
 Xnames<-expression(ER~to~U[MSY]~Index)
 xticks<-paste(seq(0,xmax,by=.2)*100,"%",sep="")

 #Now actually plot them (write to file)
  if(is.na(filename)) filename=paste(year," synoptic summary",sep="")
  if(outtype == "pdf") pdf(file = paste(filename,".pdf",sep=""),height = 9.27, width = 12.76) # open graphics device
  if(outtype == "tiff") tiff(file = paste(filename,".tif",sep=""),height = 9.27, width = 12.76,units='in',res=300,compression="zip") # open graphics device
 #Plot data and points 
    par(mfrow=c(1,1), mar=c(4,5,0.5,1), oma=c(8,2,1,3),cex=1) #plot region specs
    plot(subGarc$MREIndex,ydat, #just an empty figure region initially
         pch="",xlab=Xnames,
         ylim=c(0,ymax),
         yaxs="i",xaxs="i",
         ylab=ytitle,xlim=c(0,xmax),cex.axis=1.4,cex.lab=1.8,
         font.lab=2,xaxt="n",yaxt="n")
    abline(h=SmsyRef,lty=1,lwd=5) #Smsy ref line
    abline(v=1,lty=1,lwd=5,col="darkgray") #Umsy ref line
    dashedRefx<-seq(from=-.25,to=1,by=0.01) #0.85*Smsy ref part1
    dashedRefy<-rep(S85Ref,length(dashedRefx)) #0.85*Smsy ref part2
    lines(dashedRefx,dashedRefy,lty="dotted",lwd=5,col="darkgray") #Add 0.85*Smsy line
    axis(2,cex.axis=1.4) #pretty y axis
    axis(1,seq(0,xmax,0.2),xticks,cex.axis=1.4) #pretty x axis
    #now add x,y points, color-type coded for each period
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per1],ydat[as.character(subGarc$EIS_Region)==per1],
           pch=21,bg="white",cex=3.2) #Alaska
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per2],ydat[as.character(subGarc$EIS_Region)==per2],
           pch=22,bg="chartreuse4",cex=3.2) #Canada
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per3],ydat[as.character(subGarc$EIS_Region)==per3],
           pch=24,bg="royalblue",cex=3) #Columbia
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per4],ydat[as.character(subGarc$EIS_Region)==per4],
           pch=8,col="orange",cex=3.2,lwd=2) #TBR
    points(subGarc$MREIndex[as.character(subGarc$EIS_Region)==per5],ydat[as.character(subGarc$EIS_Region)==per5],
           pch=23,bg="red",cex=3.2) #2009-present
    box(lwd=2) #add thick frame to plot region
    #add text for each stock name if asked
    if(addeistext) text(subGarc$MREIndex, ydat, subGarc$EISStock)
    # make the legend (it's actually pretty complicated to get this right); the whole code chunk
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(4, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab="", ylab="")
    leg.lab<-c(per1,per2,per3,per4,per5)
    leg.pch<-c(21,22,24,8,23)
    leg.cex<-c(3.2,3.2,3,3.2,3.2)
    leg.bg<-c("white","chartreuse4","royalblue","orange","red")
    leg.col<-c("black","black","black","orange","black")
    leg.lwd=c(1,1,1,2,1)
    legend(x="bottom",leg.lab,pt.bg=leg.bg,pch=leg.pch,col=leg.col,bty="n",cex=1.5,horiz=TRUE)
    leg.lab<-c(expression(S~(0.85~S[MSY])),expression(U[MSY]),expression(S[MSY]))
    leg.lty<-c(3,1,1)
    leg.col<-c("darkgray","darkgray","black")
    leg.cex<-c(rep(4,3))#3))
   #
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1.5, 17, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend("bottom",leg.lab[1],lty=leg.lty[1],col=leg.col[1],bty="n",xjust=0,lwd=leg.cex[1],cex=1.5,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1.5, 22, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab[2],lty=leg.lty[2],col=leg.col[2],bty="n",xjust=0,lwd=leg.cex[2],cex=1.5,ncol=4)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1.5, 37, 0, 0), new = TRUE)
    plot(2, 2, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
    legend(x="bottom",leg.lab[3],lty=leg.lty[3],col=leg.col[3],bty="n",xjust=0,lwd=leg.cex[3],cex=1.5,ncol=4)
   #
    if(outtype=="pdf" || outtype=="tiff") dev.off() #if tiff or pdf then close graphics device FOR EACH loop
    if(verbose) return(subGarc)
}


