###########################################
# HarvestRate
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
HarvestRate <- function(x) {
  for(i in 1:x$nspfifisheries) {
   for(n in 1:x$nyears) {
    for(j in 1:x$nstocks) {
     for(k in 1:x$nages) {
      if(!all(is.na(x$cwtpop[j,,k]))) {
       if(!( is.na(x$distribution[i,j,k]) || is.na(x$cwtpop[j,n,k]))) x$cwtpopdistrib[n,i] = x$cwtpopdistrib[n,i] + x$distribution[i,j,k] * x$cwtpop[j,n,k] 
      } #end of if stock/age combo exists
     } #end of age loop
    } #end of stock loop
    x$cwthr[n,i] = x$sumcwtcat2[n,i] / x$cwtpopdistrib[n,i]
   } #end of year loop
  } #end of spfi fishery loop
 return(x)
}

###########################################
# DistributionParameter
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
DistributionParameter = function(x, blnLoopFlag, blnFirstLoop) {
  #Prelims in the distribtion param sub
   #maxdifference   = rep(0, x$nspfifisheries)
   #maxdistribution = rep(0, x$nspfifisheries)
   #x$cwtpopdistrib = data.frame(matrix(0, nrow=x$nyears, ncol=x$nspfifisheries))
   #sumestcat = array(0, c(x$nstocks, x$nspfifisheries, x$nages))
   #MaxMaxDifference = 0
   olddistribution = array(0, c(x$nspfifisheries, x$nstocks, x$nages))
   difference = array(0, c(x$nspfifisheries, x$nstocks, x$nages))
  #Ze loops
  for(i in 1:x$nspfifisheries) {
   blnFirstStockAge = TRUE
   for(j in 1:x$nstocks) {
    for(k in 1:x$nages) {
     if(!all(is.na(x$cwtpop[j,,k]))) {
      for(n in 1:x$nyears) {
       if(!is.na(x$cwtpop[j,n,k])) x$sumestcat[j,i,k] = x$sumestcat[j,i,k] + x$cwthr[n,i] * x$cwtpop[j,n,k]
      } #end of year loop
      olddistribution[i,j,k] = x$distribution[i,j,k]
      if(blnLoopFlag) {
       if(blnFirstStockAge) {
        x$distribution[i,j,k] = 1
       } else { 
        x$distribution[i,j,k] = x$sumcwtcat[j,i,k] / x$sumestcat[j,i,k]
       }
       if(x$distribution[i,j,k] > x$maxdistribution[i]) x$maxdistribution[i]=x$distribution[i,j,k]
      } else {
       if(blnFirstLoop) {
        x$distribution[i,j,k] = olddistribution[i,j,k] / x$oldmaxdistribution[i]
       } else if(x$distribution[i,j,k]!=1) {
        x$distribution[i,j,k] = x$sumcwtcat[j,i,k] / x$sumestcat[j,i,k]
       }
       else { 
        "do nothing"
       }
      } #end blnLoopFlag
      difference[i,j,k] = abs(olddistribution[i,j,k] - x$distribution[i,j,k])
      if(difference[i,j,k] > x$maxdifference[i]) x$maxdifference[i] = difference[i,j,k]
      blnFirstStockAge = FALSE
     } #end of if stock/age combo exists
    } #end of age loop
   } #end of stock loop
   x$MaxMaxDifference = x$MaxMaxDifference + x$maxdifference[i]
  } #end of spfi fishery loop
 return(x)
}

###########################################
# spfi
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
spfi <- function(spfidat, hrjdat, hrjtype="BY", tolerance=.0000001) {
#helper funcitons
 right <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
 left <- function(x, n) substr(x, 1, n)
#class checks (did user provide appropriate data objects/specifications)
 #spfidat must be an 'spfi_input' object
 #hrjdat must by in cy layout
 #hrjdat must be an 'hrj' object
 nstocks = nrow(spfidat$stockage)
 nstockandages =   sum(spfidat$stockage[,2:6])
 ages = as.numeric(right(names(spfidat$stockage[,2:6])[colSums(spfidat$stockage[,2:6])!=0],1))
 nages = length(ages)
 nerafisheries = length(spfidat$erafisheries)
 nspfifisheries = length(unique(spfidat$combinefisheries$SPFIFishery))
 collapsefisheries = ifelse(nerafisheries==nspfifisheries, FALSE, TRUE)
 years  = sort(unique(spfidat$catch$Year)) #as opposed to minyear:maxyear b/c data gaps will exist
 minyear = min(years)
 maxyear = max(years)
 nyears  = length(unique(years)) #as opposed to minyear:maxyear b/c data gaps will exist
 if(length(minyear:maxyear)!=nyears) cat("catch time series is missing years in the middle - double check if this is ok\n")
#input error check (did user tell the program to do an erroneous computation?)
 #must select at least 1 stock and age
 #must select at least 1 fishery
 #era fishery must exist in the list
 #stock name must exist in the hrj data frame
 ####################
 #SPFI DATA from HRJ#
 ####################
 #subset the hrjdat
  hrjstockloc = match(spfidat$stockage$StockAcronym, hrjdat$stknames)
 #if hrjtype... (note in the future this option should go away and use only the P method
  if(hrjtype=="BY") x = subset(hrjdat$HRJ_BY, stock%in%hrjstockloc) #same approach as the spfi program... :(
  if(hrjtype=="CY") x = subset(hrjdat$HRJ_CY, stock%in%hrjstockloc) #dunno why you would do this
  if(hrjtype=="P") x = subset(hrjdat$HRJ_P, stock%in%hrjstockloc) #what should be done
 #
  out=list()
  for(i in 1:nstocks) {
  #determine what stock-specific ages, remove the ages
   whichages=ifelse(spfidat$stockage[i,2:6]==1, 2:6, NA)
   whichages=whichages[!is.na(whichages)]
  #create a vector that will be used to select the specific columns
   whichcols=c("stock","cy","fishery","inc",paste("AEQCat",whichages,sep=""),paste("AEQTot",whichages,sep=""),paste("NomTot",whichages,sep=""),paste("NomCat",whichages,sep=""),paste("Pop",whichages,sep=""))
  #
   y=subset(x,stock==hrjstockloc[i]&fishery%in%spfidat$erafisheries,select=whichcols)
  #last, in order to crunch all data into a single matrix, convert the matrix
   library(reshape)
   out[[i]]=melt(y, id=c("stock","cy","fishery","inc"))
  }
 #combine data into a single data frame
  myhrj=do.call("rbind",out)
  myhrj=merge(myhrj,data.frame(stock=1:length(hrjdat$stknames),stockacr=hrjdat$stknames),by.x="stock",by.y="stock")
  myhrj$stockacr=droplevels(myhrj$stockacr)
  myhrj$age = as.numeric(right(as.character(myhrj$variable),1))
  myhrj = subset(myhrj, cy%in%years)
 #############
 #FISHERY MAP#
 #############
 #best way to collapse fisheries: done via merging the spfi fishery number
  #hrj data
   myhrj=merge(x=myhrj,y=spfidat$combinefisheries, by.x="fishery", by.y="ERAFishery")
   myhrj$SPFIFishery = as.factor(myhrj$SPFIFishery)
  #catch data
  if(names(spfidat$catch)[2]!="SPFIFishery") {
   mycat = merge(x=spfidat$catch,y=spfidat$combinefisheries, by.x="ERAFishery", by.y="ERAFishery")
  } else {
   mycat = spfidat$catch
  }
   mycat$SPFIFishery = as.factor(mycat$SPFIFishery)
 ############################
 #DATA MANIPS FOR SPFI CALCS#
 ############################
 #catch by year and spfi fishery
  catchbystrata = with(mycat, tapply(Catch, list(Year,SPFIFishery),sum, na.rm=TRUE))
  contribbystrata = with(mycat, tapply(Addon, list(Year,SPFIFishery),sum, na.rm=TRUE))
  treatybystrata = catchbystrata - contribbystrata
 #cwt contrib by stock, spfi fishery, and age
  cwtcatchdata = subset(myhrj,left(myhrj$variable,6)=="NomCat")
  cwtcatchdata$variable = droplevels(cwtcatchdata$variable)
 #cwt contrib by stock, spfi fishery, and age
  sumcwtcat = with(cwtcatchdata, tapply(value, list(stockacr,SPFIFishery,age),sum,na.rm=TRUE))
 #cwt contrib by year and spfi fishery
  sumcwtcat2 = with(cwtcatchdata, tapply(value, list(cy,SPFIFishery),sum,na.rm=TRUE))
 #
  cwtcohortdata = subset(myhrj,variable%in%paste("Pop",2:6,sep="")&SPFIFishery==spfidat$combinefisheries$SPFIFishery[1])
  cwtcohortdata$variable = droplevels(cwtcohortdata$variable)
  cwtcohortdata$value = round(cwtcohortdata$value,0) #sigh... :(
  cwtpop = with(cwtcohortdata, tapply(value, list(stockacr,cy,age),sum, na.rm=TRUE))
 ############
 #SPFI CALCS#
 ############
  spfiloop = list() 
  #spfiloop$cwthr = data.frame(matrix(.01, nrow=nyears, ncol=nspfifisheries))
  spfiloop$cwthr = matrix(.01, nrow=nyears, ncol=nspfifisheries)
  spfiloop$nspfifisheries = nspfifisheries
  spfiloop$nstocks = nstocks
  spfiloop$nages = nages
  spfiloop$nyears = nyears 
  spfiloop$distribution = array(0, c(nspfifisheries, nstocks, nages))
  spfiloop$cwtpop = with(cwtcohortdata, tapply(value, list(stockacr,cy,age),sum, na.rm=TRUE))
  spfiloop$sumcwtcat = with(cwtcatchdata, tapply(value, list(stockacr,SPFIFishery,age),sum,na.rm=TRUE))
  spfiloop$sumcwtcat2 = with(cwtcatchdata, tapply(value, list(cy,SPFIFishery),sum,na.rm=TRUE))
  spfiloop$oldmaxdistribution = rep(0, nspfifisheries)
  spfiloop$MaxMaxDifference = 1
 #First while loop
  MaxMaxCounter1 = 0 
  while(spfiloop$MaxMaxDifference>tolerance) {
   spfiloop$maxdifference   = rep(0, nspfifisheries)
   spfiloop$maxdistribution = rep(0, nspfifisheries)
   spfiloop$cwtpopdistrib = data.frame(matrix(0, nrow=nyears, ncol=nspfifisheries))
   spfiloop$sumestcat = array(0, c(nstocks, nspfifisheries, nages))
   spfiloop$MaxMaxDifference = 0
   spfiloop=DistributionParameter(spfiloop, blnLoopFlag=TRUE, blnFirstLoop=FALSE)
   spfiloop=HarvestRate(spfiloop)
   MaxMaxCounter1 = MaxMaxCounter1 + 1
  }
 #Second while loop
  spfiloop$MaxMaxDifference = 1
  MaxMaxCounter2 = 0 
  blnFirstLoop=TRUE
  while(spfiloop$MaxMaxDifference>tolerance) {
   spfiloop$oldmaxdistribution=spfiloop$maxdistribution
   spfiloop$maxdifference   = rep(0, nspfifisheries)
   spfiloop$maxdistribution = rep(0, nspfifisheries)
   spfiloop$cwtpopdistrib = data.frame(matrix(0, nrow=nyears, ncol=nspfifisheries))
   spfiloop$sumestcat = array(0, c(nstocks, nspfifisheries, nages))
   spfiloop$MaxMaxDifference = 0
   spfiloop=DistributionParameter(spfiloop, blnLoopFlag=FALSE, blnFirstLoop=blnFirstLoop)
   spfiloop=HarvestRate(spfiloop)
   MaxMaxCounter2 = MaxMaxCounter2 + 1
   blnFirstLoop = FALSE #
  }
 #############
 #SPFI OUTPUT#
 #############
 #
  nomtotdata = subset(myhrj,left(myhrj$variable,6)=="NomTot")
  nomtotdata$variable = droplevels(nomtotdata$variable)
  aeqcatdata = subset(myhrj,left(myhrj$variable,6)=="AEQCat")
  aeqcatdata$variable = droplevels(aeqcatdata$variable)
  aeqtotdata = subset(myhrj,left(myhrj$variable,6)=="AEQTot")
  aeqtotdata$variable = droplevels(aeqtotdata$variable)
 #
  spfiout = list()
 #
  spfiout$years = years
  spfiout$nomtotal = with(nomtotdata, tapply(value, list(cy,SPFIFishery),sum,na.rm=TRUE))
  spfiout$aeqcatch = with(aeqcatdata, tapply(value, list(cy,SPFIFishery),sum,na.rm=TRUE))
  spfiout$aeqtotal = with(aeqtotdata, tapply(value, list(cy,SPFIFishery),sum,na.rm=TRUE))
  spfiout$cwtcatch = sumcwtcat2
  spfiout$cwthr = spfiloop$cwthr
  rownames(spfiout$cwthr) = years
  spfiout$treatycatch = treatybystrata
  spfiout$distribution = spfiloop$distribution
 #
  class(spfiout) = "spfi_output"
 #
  return(spfiout)
}

###########################################
# summary.spfi_output
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
summary.spfi_output <- function(x, unit=c("nom cat", "nom tot", "aeq cat", "aeq tot"), baseperiod=1979:1982) {
#
 cohortbystrata = x$treatycatch/x$cwthr
#if gauntlet
 if(!is.null(x$gauntlet)) cohortbystrata = x$estcohort
#
 totalcohort = rowSums(cohortbystrata,na.rm=TRUE)
#if
 if(unit=="aeq cat") scalar = x$aeqcatch/x$cwtcatch
 else if(unit=="aeq tot") scalar = x$aeqtotal/x$cwtcatch
 else if(unit=="nom cat") scalar = 1
 else if(unit=="nom tot") scalar = x$nomtotal/x$cwtcatch
 #NOTE that if there IS catch, but no cwtcatch (e.g. cwt sampling doesn't often occur when catch is low), we STILL need a non-zero scalar. I use the grand mean, can easily be refined further 
 if(!is.null(x$gauntlet)) {
  scalarforavg = ifelse(x$isimputed, NA, scalar)
  scalar = ifelse(!x$isimputed, scalar, colMeans(scalarforavg,na.rm=TRUE))
 }

#
 catchbystrata = x$treatycatch*scalar
 hrbystrata = catchbystrata/cohortbystrata
#
 totalcatch = rowSums(catchbystrata,na.rm=TRUE)
 totalhr = totalcatch/totalcohort
#spfis
 spfibystrata = hrbystrata
 if(ncol(hrbystrata)==1) spfibystrata = totalhr/mean(totalhr[x$years%in%baseperiod])
 else for(i in 1:ncol(hrbystrata)) spfibystrata[,i] = hrbystrata[,i]/colMeans(hrbystrata[x$years%in%baseperiod,])[i]
 spfigrand = totalhr/mean(totalhr[x$years%in%baseperiod])
#
 out=list()
 out$unit = unit
 out$bp = baseperiod
 out$years = x$years
 out$hr     = cbind(totalhr, hrbystrata)
 out$spfi   = cbind(spfigrand, spfibystrata)
#if gauntlet
 if(!is.null(x$gauntlet)) {
  out$gauntlet = x$gauntlet
  removenas = function(x) ifelse(all(x==FALSE),NA,paste(names(x[x==TRUE]),collapse=","))
  out$imputedstrata = apply(x$isimputed, 1, removenas)
 }
#
 class(out) = "summary.spfi_output"
#
 return(out)
}

###########################################
# print.summary.spfi_output
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
print.summary.spfi_output <- function(x, digits=2) {
  if(!is.null(digits)) tmp= round(x$spfi, digits)
  tmp = data.frame("Years"=x$years, tmp)
  names(tmp)[-1] = colnames(x$spfi)
  print(tmp,row.names=FALSE)
}

###########################################
# multispfi
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
multispfi <- function(spfi_input, yearvec, ...) {
  #maxyear=max(spfi_input$catch$Year)
  #if(maxyear!=max(yearvec)) cat("max of year vec doesn't match max year of catch data...?\n")
  spfi_list <- list()
  for(i in 1:length(yearvec)) {
   cat("calc",i,"of",length(yearvec),"\n")
   tmp = spfi_input
   if( is.list(yearvec)) tmp$catch = subset(tmp$catch, Year%in%yearvec[[i]])
   if(!is.list(yearvec)) tmp$catch = subset(tmp$catch, Year<=yearvec[i])
   spfi_list[[i]] <- spfi(spfidat=tmp, ...)
  }
  out <- list()
  out$results <- spfi_list
  out$yearvec <- yearvec
  class(out) = "spfi_output_list"
  return(out)
}

###########################################
# spfidistfun
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
spfidistfun <- function(y) {
 x=y$distribution
 nstrata=dim(x)[1]
 nstks=dim(x)[2]
 nages=dim(x)[3]
 out=expand.grid(strata=1:nstrata, stock=1:nstks, age=1:nages)
 out$distrib=NA
 for(i in 1:nages) {
  for(j in 1:nstks) {
   out[out$age%in%i&out$stock%in%j,]$distrib = x[,j,i]
  }
 }
 return(out)
}

###########################################
# multispfidistfun
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
multispfidistfun <- function(spfi_output_list, spfi_input) {
 for(i in 1:nrow(spfi_input$stockage)) {
  for(j in 1:nlevels(spfi_input$catch$SPFIFisheryName)) {
   tmp = do.call("rbind",lapply(spfi_output_list$results, spfidistfun))
  } #end j loop
 } #end i loop
 nages = length(names(spfi_input$stockage[,-1])[colSums(spfi_input$stockage[,-1])>0])
 nstks = nlevels(spfi_input$stockage$StockAcronym)
 nstra = nlevels(spfi_input$catch$SPFIFisheryName)
 if(!is.list(spfi_output_list$yearvec)) {
  tmp$ERAYear = sort(rep(spfi_output_list$yearvec,nages*nstks*nstra))
  tmp = tmp[order(tmp$ERAYear),]
 }
 if(is.list(spfi_output_list$yearvec)) {
  tmp$Simulation = sort(rep(1:length(spfi_output_list$yearvec),nages*nstks*nstra))
  tmp = tmp[order(tmp$Simulation),]
 }
 age_merge = data.frame(age=1:nages,Age=names(spfi_input$stockage[,-1])[colSums(spfi_input$stockage[,-1])>0])
 stock_merge = data.frame(stock=1:nstks,Stock=as.character(spfi_input$stockage$StockAcronym))
 strata_merge = data.frame(strata=1:nstra, Strata=as.character(unique(spfi_input$catch$SPFIFisheryName)))
 tmp = merge(tmp,age_merge)
 tmp = merge(tmp,stock_merge)
 tmp = merge(tmp,strata_merge)
 if(!is.list(spfi_output_list$yearvec)) {
  tmp = tmp[order(tmp$ERAYear),]
 }
 if(is.list(spfi_output_list$yearvec)) {
  tmp = tmp[order(tmp$Simulation),]
 }
 return(tmp)
}

###########################################
# spficwthrfun
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
spficwthrfun <- function(y, index=1979:1982) {
 x=y$cwthr
 nstrata=dim(x)[2]
 nyears=dim(x)[1]
 years = as.numeric(rownames(x))
 out = data.frame(strata=sort(rep(1:nstrata,nyears)),Year=rep(years,nstrata),cwthr=as.vector(x))
 if(!is.null(index) && nstrata==1) out$cwthri = as.vector(t(t(x)/mean(x[rownames(x)%in%1979:1982,])))
 if(!is.null(index) && nstrata>1)  out$cwthri = as.vector(t(t(x)/colMeans(x[rownames(x)%in%1979:1982,])))
 return(out)
}

###########################################
# multispficwthrfun
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
multispficwthrfun <- function(spfi_output_list, spfi_input) {
 tmp = lapply(spfi_output_list$results, spficwthrfun)
 if(!is.list(spfi_output_list$yearvec)) {
  for(i in 1:length(tmp)) tmp[[i]]$ERAYear = spfi_output_list$yearvec[i]
  tmp = do.call("rbind", tmp)
  tmp = tmp[order(tmp$ERAYear),]
 }
 if(is.list(spfi_output_list$yearvec)) {
  for(i in 1:length(tmp)) tmp[[i]]$Simulation = i
  tmp = do.call("rbind", tmp)
  tmp = tmp[order(tmp$Simulation),]
 }
 nstra = nlevels(spfi_input$catch$SPFIFisheryName)
 strata_merge = data.frame(strata=1:nstra, Strata=as.character(unique(spfi_input$catch$SPFIFisheryName)))
 tmp = merge(tmp,strata_merge)
 if(!is.list(spfi_output_list$yearvec)) {
  tmp = tmp[order(tmp$ERAYear),]
 }
 if(is.list(spfi_output_list$yearvec)) {
  tmp = tmp[order(tmp$Simulation),]
 }
 return(tmp)
}

###########################################
# spfisummaryfun
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
spfisummaryfun <- function(y, whichvar="hr", ...) {
 if(whichvar=="spfi") x=summary(y, unit="aeq cat", baseperiod=1979:1982)$spfi
 if(whichvar=="hr")   x=summary(y, unit="aeq cat", baseperiod=1979:1982)$hr
 nstrata=dim(x)[2]
 nyears=dim(x)[1]
 years = as.numeric(rownames(x))
 out = data.frame(strata=sort(rep(1:nstrata,nyears))-1,Year=rep(years,nstrata),spfi=as.vector(x))
 return(out)
}

###########################################
# multispfifun
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
multispfifun <- function(spfi_output_list, spfi_input) {
 tmp = lapply(spfi_output_list$results, spfisummaryfun)
 if(!is.list(spfi_output_list$yearvec)) {
  for(i in 1:length(tmp)) tmp[[i]]$ERAYear = spfi_output_list$yearvec[i]
  tmp = do.call("rbind", tmp)
  tmp = tmp[order(tmp$ERAYear),]
 }
 if(is.list(spfi_output_list$yearvec)) {
  for(i in 1:length(tmp)) tmp[[i]]$Simulation = i
  tmp = do.call("rbind", tmp)
  tmp = tmp[order(tmp$Simulation),]
 }
 nstra = nlevels(spfi_input$catch$SPFIFisheryName)
 strata_merge = data.frame(strata=1:nstra, Strata=levels(spfi_input$catch$SPFIFisheryName))
 strata_merge = rbind(data.frame(strata=0,Strata="Grand"), strata_merge)
 tmp = merge(tmp,strata_merge)
 if(!is.list(spfi_output_list$yearvec)) {
  tmp = tmp[order(tmp$ERAYear),]
 }
 if(is.list(spfi_output_list$yearvec)) {
  tmp = tmp[order(tmp$Simulation),]
 }
 return(tmp)
}

###########################################
# gauntlet
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
gauntlet <- function(x, method="glm", catchcriteria=4000) {
#
 library(reshape)
 cat=melt(x$treatycatch)
 cwthr=melt(x$cwthr)
 mydf = cbind(cat,cwthr[,3])
 names(mydf) = c("Year","Strata","TreatyCatch","CWTHR")
 mydf$COHORT = mydf$TreatyCatch/mydf$CWTHR
 mydf$CRITERIA = mydf$TreatyCatch<catchcriteria
#
 allstratamissing = rowSums(x$treatycatch<catchcriteria)==ncol(x$treatycatch)
 yearswithallstratamissing = as.numeric(names(allstratamissing)[allstratamissing])
#
 if(method=="apc") {
 #data manips
  hold = ifelse(x$treatycatch<catchcriteria,NA,x$treatycatch/x$cwthr)
  apcmat = hold[apply(hold, 1, function(x) all(!is.na(x)&x<Inf&!is.nan(x))),]
  apccorr = colSums(apcmat)/sum(apcmat)
 #####
  #note the following is only for illustrative purposes, and are not necessary for this function
   #total cohort
    totcohort = rowSums(hold,na.rm=TRUE)
    apc_scalar = 1-apply(hold, 1, function(x, apccorr) 1-sum(apccorr[is.na(x)]), apccorr=apccorr)
    totcohort = ifelse(apc_scalar!=0,totcohort+totcohort/(1-apc_scalar),totcohort)
   #by strata 
    newmat = hold[apply(hold, 1, function(x) !all(!is.na(x)&x<Inf&!is.nan(x))),]
    newmat = t(apply(newmat, 1, function(x, apccorr) ifelse(is.na(x),(apccorr[is.na(x)]/sum(apccorr[is.na(x)]))*(sum(x[!is.na(x)])/(1-sum(apccorr[is.na(x)]))),x), apccorr=apccorr))
  #end example area
 #####
 #impute missing strata
  mydf = melt(hold)
  names(mydf) = c("Year","Strata","COHORT")
  tmp = t(apply(hold, 1, function(x, apccorr) ifelse(is.na(x),(apccorr[is.na(x)]/sum(apccorr[is.na(x)]))*(sum(x[!is.na(x)])/(1-sum(apccorr[is.na(x)]))),x), apccorr=apccorr))
  tmp2 = melt(tmp)
  if(all(tmp2[,1]==mydf[,1]) && all(tmp2[,2]==mydf[,2])) mydf$predCOHORT = tmp2$value
  mydf$imputed = ifelse(is.na(mydf$COHORT), TRUE, FALSE)
 }
 if(method=="glm") {
  mymod = lm(log(COHORT)~factor(Strata) + factor(Year), data=subset(mydf,CRITERIA==FALSE))
  mydf$predCOHORT = NA
  mydf[mydf$CRITERIA==FALSE,]$predCOHORT = mydf[mydf$CRITERIA==FALSE,]$COHORT
  if(length(yearswithallstratamissing)==0) mydf[mydf$CRITERIA==TRUE,]$predCOHORT = exp(predict(mymod,subset(mydf, CRITERIA==TRUE)))
  else {
   mymod2 = mymod
   mymod2$coefficients[4:length(coef(mymod))] =  mean(coef(mymod)[4:length(coef(mymod))])
   mydf2 = subset(mydf, CRITERIA==TRUE&Year%in%yearswithallstratamissing)
   mydf2$Year = 1982
   mydf[mydf$CRITERIA==TRUE&mydf$Year%in%yearswithallstratamissing,]$predCOHORT = exp(predict(mymod2, mydf2))
   mydf[mydf$CRITERIA==TRUE&!mydf$Year%in%yearswithallstratamissing,]$predCOHORT = exp(predict(mymod,subset(mydf, CRITERIA==TRUE&!Year%in%yearswithallstratamissing)))
  #TEST = mydf
  #TEST$COHORT2 = ifelse(mydf$COHORT<=4000|is.nan(mydf$COHORT)|mydf$COHORT>Inf,NA,mydf$COHORT) #Inf was 9999999999
  #mymod = lm(log(COHORT2)~factor(Strata) + factor(Year), data=TEST)
  }
 }
#
 x$gauntlet = TRUE
 x$isimputed = ifelse(x$treatycatch<=4000,TRUE,FALSE)
 x$estcohort = with(mydf, tapply(predCOHORT, list(Year,Strata),sum))
 #
 return(x)
}




