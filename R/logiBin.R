#' Simulated default data of 100 customers
#'
#' A dataset containing simulated data about the characteristic of a customer applying for a loan. The dependent variable is "bad_flag" which indicates whether the customer defaults or not.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{bad_flag}{Indicates whether a customer has defaulted or not}
#'   \item{age}{Age of the customer}
#'   \item{LTV}{Ratio of amount of loan to amount of collateral}
#'   \item{location}{Indicates the location of the customer}
#'   \item{balance}{Account balance}
#'   \item{score}{credit score of the customer}
#' }
"loanData"



#' @title Bins variables to be used in logistic regression
#' @description This function uses parallel processing to compute bins for continuous and categorical variables. The splits are computed using the partykit package which uses conditional inferencing trees. Refer to the package documentation for more details. A separate bin is created for NA values. This can be combined using naCombine function. Categorical variables with a maximum of 10 distinct values are supported.
#'
#' @param df - A data frame
#' @param y - The name of the dependent variable
#' @param xVars - A vector names of variables
#' @param minProp - The minimum proportion of observations that must be exceeded in order to implement a split. Default value is 0.03
#' @param minCr - The value of test statistic that must be exceeded in order to implement a split. Increasing this value will decrease the number of splits. Refer to the partykit package documentation for more details. Default value is 0.9
#' @param nCores - The number of cores used for parallel processing. The default value is 1
#'
#' @return Returns a list containing 3 elements.
#' The first is a data frame called varSummary which contains a summary of all the variables along with their IV value, entropy, p value from ctree function in partykit package, flag which indicates if bad rate increases/decreases with variable value, flag to indicate if a monotonic trend is present, number of bins which flip (i.e. do not follow a monotonic trend), number of bins of the variable and a flag to indicate whether it includes pure nodes (node which do not have any defaults).
#' The second element is a data frame called bin which contains details of all the bins of the variables. The third element is a dataframe called err which contains details of all the variables that could not be split and the reason for the same.
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('age', 'score', 'balance'))
#'
#' @export
getBins <- function(df,y,xVars, minProp = 0.03, minCr = 0.9, nCores=1)
{
  res <- getBin(df,y,xVars, minProp = minProp, minCr = minCr, nCores = nCores)
  err <- res[!(is.na(res$error)), c("var","error")]
  if(sum(is.na(res$error)) > 0)
  {
    varSummary <- res[is.na(res$error) & res$bin == 'Total', c("var","iv", "pVal", "stat", "ent", "trend", "monTrend", "flipRatio", "numBins", "purNode", "varType")]
    varSummary <- varSummary[order(-varSummary$iv),]
    bin <- res[is.na(res$error) , c(1,3:8,13:14)]
    return(list(varSummary= varSummary, err=err, bin=bin))
  }
  return(err)
}


getBin <- function (df,y,x, minProp = 0.03, minCr = 0.9, nCores=1)
{
  if (!is.numeric(df[[y]]))
    return ("y should be numeric")
  else if (length(unique(df[[y]])) != 2)
    return ("y has only 1 or more than 2 distinct values")
  else if (max(df[[y]]) != 1 | min(df[[y]]) != 0)
    return ("y has values other than 0 and 1")

  if(length(x) == 1)
  {
    result <- getSplit(as.data.table(df),y,x, minProp = minProp, minCr = minCr)
  }
  else{
    cbads<- df[[y]]  #this is required because in foreach do not want to copy entire data set but only vector of bads

    if(is.data.table(df))
      va= iter(df[,x, with = F])
    else
      va= iter(df[,x])

    i <- NULL
    cl <- parallel::makeCluster(nCores)
    doParallel::registerDoParallel(cl)
    result <- foreach(va= va, i=icount(), .combine = rbind, .export = 'getSplit', .packages = c('partykit', 'data.table')) %dopar% {
      d<- data.table(v = va, cbad = cbads)
      setnames(d,"v",x[i])
      iv <- getSplit(d,"cbad",x[i], minProp = minProp, minCr = minCr)
    }

    parallel::stopCluster(cl)
  }
  return(result)
}

getSplit <- function (d,y,x, minProp = 0.03, minCr = 0.9)
{
  iv <- data.frame(var = character(0), error = character(0), bin = character(0), count = numeric(0),  bads = numeric(0), goods = numeric(0),  propn = numeric(0),
                   bad_rate = numeric(0), pVal = numeric(0), stat = numeric(0), goodCap = numeric(0), badCap = numeric(0), iv = numeric(0),
                   ent = numeric(0), trend = character(0), monTrend = character(0), flipRatio = numeric(0), numBins = numeric(0), purNode= character(0), varType = character(0), stringsAsFactors = F)

  if(length(unique(d[[x]])) < 2)
  {
    iv[nrow(iv)+1, "var"] <- x
    iv[nrow(iv), "error"] <- "Only 1 distinct value of x"
  }
  else if(!is.numeric(d[[x]]) & !is.character(d[[x]]) & !is.factor(d[[x]]))
  {
    iv[nrow(iv)+1, "var"] <- x
    iv[nrow(iv), "error"] <- "x should be numeric or character or factor"
  }
  else if(!is.numeric(d[[x]]) & length(unique(d[[x]])) > 10)
  {
    iv[nrow(iv)+1, "var"] <- x
    iv[nrow(iv), "error"] <- "x has too many levels (> 10)"
  }
  else {
    d <- as.data.table(d)
    if (is.character(d[[x]]))
    {d[,x] <- d[,as.factor(get(x))]}

    charFlag = 'N'  # this is required as there is need to simply rules for character inputs
    if(is.factor(d[[x]]))
      charFlag = 'Y'

    fit<-try(ctree(formula(paste(y, "~", x)), data = d, na.action = na.exclude,
                   control = ctree_control(minbucket = minProp*nrow(d), mincriterion = minCr)), silent=T)

    if(class(fit)[1] == "try-error")
    {
      brks <- stats::quantile(d[[x]],na.rm = T,probs = seq(0,1,0.1))
      if(brks[2]==brks[1]| brks[2] == brks[11])
        brks = unique(brks)
      else
        brks = unique(brks[2:10])
      rules <- getBinRules(sort(brks), x)

      pVal <- 0
      stat <- 0
    }
    else
    {
      if (width(fit) >= 2) {

        if(charFlag == 'Y')
        {
          nodes <- nodeids(fit, terminal = T)
          rules <- character()
          for(m in 1:length(nodes))
          {
            lvls <- unique(na.omit(data_party(fit, nodes[m])[, x]))
            rules[length(rules)+1] <- paste0(x, " %in% c(",paste0('\"', unique(na.omit(data_party(fit, nodes[m])[,x])), '\"', collapse = ","), ")")
          }
        }
        else
        {
          cuts <- numeric()
          for(m in 1:length(nodeids(fit)))
            cuts <- rbind(cuts, fit[m]$node$split$breaks)
          rules <- getBinRules(sort(cuts), x)
        }


        pVal <- fit[1]$node$info$p.value
        stat <- fit[1]$node$info$criterion[1,1]
      }

      else
      {
        iv[nrow(iv)+1, "var"] <- x
        iv[nrow(iv), "error"] <- "No significant splits"
      }
    }

    if(nrow(iv) == 0)
    {
      for (j in 1:length(rules)){
        iv[nrow(iv)+1, "var"] <- x
        iv[nrow(iv),"bin"] <- rules[j]

        d1<-subset(d,eval(parse(text=rules[[j]])))
        iv[nrow(iv),"count"]<- nrow(d1)
        iv[nrow(iv),"bads"]<- sum(d1[[y]])

        if (j>1)
        {
          if(iv[nrow(iv), "bads"]/iv[nrow(iv), "count"] >= iv[nrow(iv)-1, "bads"]/iv[nrow(iv)-1, "count"])
            iv[nrow(iv), "trend"] <- "I"
          else
            iv[nrow(iv), "trend"] <- "D"
        }
      }

      if (sum(is.na(d[[x]])) > 0){
        iv[nrow(iv)+1, "var"]<- x
        iv[nrow(iv), "bin"]<- paste0("is.na(", x,")")
        iv[nrow(iv), "count"]<- sum(is.na(d[, x, with = F]))
        iv[nrow(iv), "bads"]<- sum(d[is.na(get(x)), get(y)])
      }

      iv$goods <- iv$count - iv$bads
      iv$propn <- round(iv$count/nrow(d)*100,2)
      iv$goodCap <- iv$goods/sum(iv$goods)
      iv$badCap <- iv$bads/sum(iv$bads)
      iv$iv <- round((iv$goodCap - iv$badCap)*(log(iv$goodCap/iv$badCap)),4)
      iv$ent <- ifelse(iv$bads == 0 | iv$goods == 0, 0, round(-1*((iv$bads/iv$count*log2(iv$bads/iv$count))+(iv$goods/iv$count*log2(iv$goods/iv$count))), 4))
      iv$purNode <- ifelse(iv$bads == 0 | iv$goods == 0, "Y", "N")

      iv[nrow(iv)+1, "var"]<- x
      iv[nrow(iv),"bin"] <- "Total"
      iv[nrow(iv), "pVal"] <- pVal
      iv[nrow(iv), "stat"] <- stat
      iv[nrow(iv),"count"] <- sum(iv$count, na.rm = T)
      iv[nrow(iv),"goods"] <- sum(iv$goods, na.rm = T)
      iv[nrow(iv),"bads"] <- sum(iv$bads, na.rm = T)
      iv[nrow(iv),"iv"] <- round(sum(iv$iv*is.finite(iv$iv), na.rm = T),4)  #Multiply by isFinite to remove Inf from sum as Inf*0 = NaN whih is excluded using na.rm = T
      iv[nrow(iv),"ent"] <- round(sum(iv$ent*iv$count/iv[nrow(iv),"count"], na.rm = T),4)
      iv$bad_rate <- round(iv$bads/iv$count*100,2)
      iv[nrow(iv), "monTrend"] <- ifelse(length(unique(iv[!is.na(iv$trend), "trend"])) > 1 , "N", "Y")
      iv[nrow(iv), "flipRatio"] <- min(nrow(iv[!(is.na(iv$trend)) & iv$trend == 'I',]), nrow(iv[!(is.na(iv$trend)) & iv$trend == 'D',]))/(nrow(iv)-2)
      iv[nrow(iv), "numBins"] <- nrow(iv)-1
      iv[nrow(iv), "trend"] <-  ifelse(nrow(iv[iv$trend == 'I',]) >= nrow(iv[iv$trend== 'D',]) , "I", "D")
      iv[nrow(iv), "purNode"] <- ifelse(nrow(iv[!(is.na(iv$purNode)) & iv$purNode == 'Y',])> 0, "Y", "N")
      iv[nrow(iv),c("propn","goodCap","badCap")] <- 1
      iv[nrow(iv), "varType"] <- class(d[[x]])
    }
  }
  return (iv)
}


#' @title Combine NA bins
#'
#' @description This function combines the NA bin with either the bin having the closest bad rate or the average bad rate if the count of observations in NA bin is low
#'
#' @param binObj - An object returned by getBins or other functions (except  createBins) in this package
#' @param xVars - A vector of names of variables for which NA bins have to be combined
#' @param cutoffPropn - The minimum proportion of observations that must be present in the NA bin for it to be combined with the bin with closest bad rate. If the proportion s below this, the NA bin will be combined with bin having average bad rate
#'
#' @return Returns a list containing 3 objects. Similar to the getBins function
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('age', 'score', 'LTV'))
#' b1 <- naCombine(b1, c('LTV'))
#'
#' @export
naCombine <- function(binObj, xVars, cutoffPropn = 0.01)
{
  for(i in 1:length(xVars))
  {
    bins <- binObj$bin[binObj$bin$var == xVars[i] & binObj$bin$bin != 'Total',]
    naRow <- bins[grep("^is.na{1}", bins$bin),]
    if(nrow(naRow)!=1)
      print(paste(xVars[i],":Either no NA or NA bin has already been combined"))
    else
    {
      bins <-  bins[-grep("is.na{1}", bins$bin),]
      if(naRow[, "propn"] <= cutoffPropn*100)
        binRt <- binObj$bin[binObj$bin$var == xVars[i] & binObj$bin$bin == 'Total', "bad_rate"]
      else
        binRt <- naRow[,"bad_rate"]

      bins$diff <- abs(bins$bad_rate - binRt)
      k <- which(bins$diff == min(bins$diff))
      bins[k, "bin"] <- paste(bins[k, "bin"], "|", naRow[,"bin"])
      bins[k,"goods"] <- bins[k,"goods"] + naRow[,"goods"]
      bins[k,"bads"] <- bins[k,"bads"] + naRow[,"bads"]

      iv <- updateBins(xVars[i], bins$bin, bins$bads, bins$goods)
      binObj$varSummary[binObj$varSummary$var==xVars[i], "iv"] <- iv[nrow(iv), "iv"]
      binObj$varSummary[binObj$varSummary$var==xVars[i], "ent"] <- iv[nrow(iv), "ent"]
      binObj$varSummary[binObj$varSummary$var==xVars[i], "trend"] <- iv[nrow(iv), "trend"]
      binObj$varSummary[binObj$varSummary$var==xVars[i], "monTrend"] <- iv[nrow(iv), "monTrend"]
      binObj$varSummary[binObj$varSummary$var==xVars[i], "flipRatio"] <- iv[nrow(iv), "flipRatio"]
      binObj$varSummary[binObj$varSummary$var==xVars[i], "numBins"] <- iv[nrow(iv), "numBins"]
      binObj$varSummary[binObj$varSummary$var==xVars[i], "purNode"] <- iv[nrow(iv), "purNode"]

      binObj$bin <- binObj$bin[binObj$bin$var != xVars[i],]
      binObj$bin <- rbind(binObj$bin, iv[,1:9])
    }
  }
  return(binObj)
}


updateBins <- function(var, rules, bads, goods)
{
  iv<- data.frame(cbind(var=var, bin=rules, count=NA, bads=bads, goods=goods, propn=NA, bad_rate=NA, iv=NA, ent=NA), stringsAsFactors = F)

  iv$bads <- as.numeric(as.character(iv$bads))
  iv$goods <- as.numeric(as.character(iv$goods))
  iv$count <- iv$goods + iv$bads
  iv$propn <- round(iv$count/sum(iv$count)*100, 2)
  iv$goodCap <- iv$goods/sum(iv$goods)
  iv$badCap <- iv$bads/sum(iv$bads)
  iv$iv <- round((iv$goodCap - iv$badCap)*(log(iv$goodCap/iv$badCap)),4)
  iv$ent <- ifelse(iv$bads == 0 | iv$goods == 0, 0, round(-1*((iv$bads/iv$count*log2(iv$bads/iv$count))+(iv$goods/iv$count*log2(iv$goods/iv$count))), 4))
  iv$purNode <- ifelse(iv$bads == 0 | iv$goods == 0, "Y", "N")



  for(i in 2:length(rules))
  {
    if(!grepl("^is.na{1}",iv[i, "bin"]))
      if(iv[i, "bads"]/iv[i, "count"] >= iv[i-1, "bads"]/iv[i-1, "count"])
        iv[i, "trend"] <- "I"
      else
        iv[i, "trend"] <- "D"
  }

  iv[nrow(iv)+1, "var"]<- var
  iv[nrow(iv),"bin"] <- "Total"
  iv[nrow(iv),"count"] <- sum(iv$count, na.rm = T)
  iv[nrow(iv),"goods"] <- sum(iv$goods, na.rm = T)
  iv[nrow(iv),"bads"] <- sum(iv$bads, na.rm = T)
  iv[nrow(iv),"iv"] <- round(sum(iv$iv*is.finite(iv$iv), na.rm = T),4)  #Multiply by isFinite to remove Inf from sum as Inf*0 = NaN whih is excluded using na.rm = T
  iv[nrow(iv),"ent"] <- round(sum(iv$ent*iv$count/iv[nrow(iv),"count"], na.rm = T),4)
  iv$bad_rate <- round(iv$bads/iv$count*100,2)
  iv[nrow(iv), "monTrend"] <- ifelse(length(unique(iv[!is.na(iv$trend), "trend"])) > 1 , "N", "Y")
  iv[nrow(iv), "flipRatio"] <- min(nrow(iv[!(is.na(iv$trend)) & iv$trend == 'I',]), nrow(iv[!(is.na(iv$trend)) & iv$trend == 'D',]))/(nrow(iv)-2)
  iv[nrow(iv), "numBins"] <- nrow(iv)-1
  iv[nrow(iv), "trend"] <-  ifelse(nrow(iv[iv$trend == 'I',]) >= nrow(iv[iv$trend== 'D',]) , "I", "D")
  iv[nrow(iv), "purNode"] <- ifelse(nrow(iv[!(is.na(iv$purNode)) & iv$purNode == 'Y',])> 0, "Y", "N")
  iv[nrow(iv),c("propn","goodCap","badCap")] <- 1

  return(iv)
}

#' @title Split a variable based on specified cuts
#'
#' @description This function splits variables based on cuts that have been input manually
#'
#' @param binObj - An object returned by getBins or any other function (except createBins) in this package
#' @param splitVar - The name of the variable that has to be split
#' @param y - The dependent variable
#' @param splits - The splits for the variable
#' @param df - A data frame
#'
#' @return Returns a list containing 3 objects. Similar to the getBins function
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('age', 'score', 'balance'), minCr=0.8)
#' b1 <- manualSplit(b1, 'age', 'bad_flag', c(25,40,55), loanData)
#'
#' @export

manualSplit <- function(binObj, splitVar, y, splits, df)
{
  if(length(splits) < 2)
  {
    print("Atleast 2 splits should be specified")
    return(binObj)
  }

  if(!is.numeric(splits))
  {
    print("Splits must be numeric")
    return(binObj)
  }

  if(binObj$varSummary[binObj$varSummary$var == splitVar, "varType"]== "factor")
  {
    print("This function can only be used for numerical variables")
    return(binObj)
  }

  splits <- sort(splits)
  rules <- numeric()
  cGoods <- numeric()
  cBads <- numeric()
  df <- as.data.table(df)
  counter <- 0

  for(i in 1:length(splits))
  {
    if(i==1 | counter == 0)
    {
      rules[1] <- paste(splitVar,"<=", splits[i])
      d1<-subset(df,eval(parse(text=rules[1])))
      if(nrow(d1) > 0)
      {
        counter <- counter + nrow(d1)
        cBads[1]<- sum(d1[[y]])
        cGoods[1]<- nrow(d1) - cBads[1]
      }
    }
    else
    {
      rules[length(rules)+1] <- paste(splitVar,">", splits[i-1], "&", splitVar, "<=", splits[i])
      d1<-subset(df,eval(parse(text=rules[[length(rules)]])))
      cBads[length(cBads)+1]<- sum(d1[[y]])
      cGoods[length(cGoods)+1]<- nrow(d1) - cBads[length(cBads)]
      counter <- counter + nrow(d1)

      if(counter == (nrow(df) - sum(is.na(df[[splitVar]]))))
      {
        rules[length(rules)] <- paste(splitVar,">", splits[i-1])
        break
      }
    }
  }

  if(counter < (nrow(df) - sum(is.na(df[[splitVar]]))))
  {
    rules[length(rules)+1] <- paste(splitVar,">", splits[length(splits)])
    d1<-subset(df,eval(parse(text=rules[[length(rules)]])))
    cBads[length(cBads)+1]<- sum(d1[[y]])
    cGoods[length(cGoods)+1]<- nrow(d1) - cBads[length(cBads)]
  }

  if (sum(is.na(df[[splitVar]])) > 0){
    rules[length(rules)+1]<- paste0("is.na(", splitVar,")")
    cBads[length(cBads)+1]<- sum(df[is.na(get(splitVar)), get(y)])
    cGoods[length(cGoods)+1] <- sum(is.na(df[, splitVar, with = F])) - cBads[length(cBads)]
  }

  iv<- updateBins(splitVar, rules, cBads, cGoods)
  binObj$varSummary[binObj$varSummary$var==splitVar, "iv"] <- iv[nrow(iv), "iv"]
  binObj$varSummary[binObj$varSummary$var==splitVar, "ent"] <- iv[nrow(iv), "ent"]
  binObj$varSummary[binObj$varSummary$var==splitVar, "trend"] <- iv[nrow(iv), "trend"]
  binObj$varSummary[binObj$varSummary$var==splitVar, "monTrend"] <- iv[nrow(iv), "monTrend"]
  binObj$varSummary[binObj$varSummary$var==splitVar, "flipRatio"] <- iv[nrow(iv), "flipRatio"]
  binObj$varSummary[binObj$varSummary$var==splitVar, "numBins"] <- iv[nrow(iv), "numBins"]
  binObj$varSummary[binObj$varSummary$var==splitVar, "purNode"] <- iv[nrow(iv), "purNode"]

  binObj$bin <- binObj$bin[binObj$bin$var != splitVar,]
  binObj$bin <- rbind(binObj$bin, iv[,1:9])
  return(binObj)
}


getNumSplits <- function(rules)
{
  cuts<- numeric()
  for(i in 1:length(rules))
  {
    l <-lapply(strsplit(rules[i], " & "), function(x) {strsplit(x, " <= ")})
    min <- NULL
    for (k in 1:length(l[[1]]))
    {
      if(length(l[[1]][[k]]) == 2)
        if(is.null(min))
          min <- as.numeric(l[[1]][[k]][2])
        else if(min > as.numeric(l[[1]][[k]][2]))
          min <- as.numeric(l[[1]][[k]][2])
    }

    if(!is.null(min))
    {
      cuts[length(cuts)+1] <- min
    }
  }
  return(cuts)
}

getBinRules <- function(cuts, var)
{
  bins <- character(0)
  for(i in 1:length(cuts))
  {
    if(i==1)
      bins[i] <- paste(var,"<=", cuts[1])
    else
      bins[i] <- paste(var,">", cuts[i-1], "&", var, "<=", cuts[i])
  }
  bins[length(bins)+1] <- paste(var, ">", cuts[length(cuts)])
  return(bins)
}


#' @title Force a numerical variable to follow a monotonically decreasing trend
#'
#' @description This function forces a variable to follow a monotonically decreasing trend by grouping bins. In case such a trend can not be forced a message is printed to the console
#'
#' @param binObj - An object returned by getBins or any other function (except createBins) in this package
#' @param xVars - A vector of the name of variables
#'
#' @return Returns a list containing 3 objects. Similar to the getBins function
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('age', 'score'), minCr=0.6, minProp = 0.01)
#' b1 <- forceDecrTrend(b1, c('score','age'))
#'
#' @export

forceDecrTrend <- function(binObj, xVars)
{
  revBin <- data.frame(var = character(0), bin = character(0), count = numeric(0),  bads = numeric(0), goods = numeric(0),  propn = numeric(0),
                       bad_rate = numeric(0), goodCap = numeric(0), badCap = numeric(0), iv = numeric(0),
                       ent = numeric(0),  numBins = numeric(0), purNode= character(0), stringsAsFactors = F)
  res<- data.frame()

  for(i in 1:length(xVars))
  {
    if(binObj$varSummary[binObj$varSummary$var == xVars[i], "varType"] == "factor")
      print(paste(xVars[i], " : This function can only be applied to numerical variables"))
    else
    {
      bins <- binObj$bin[binObj$bin$var == xVars[i] & binObj$bin$bin != 'Total',]
      naBin <- 0
      naRow <- data.frame()

      if(nrow(bins[grep("is.na{1}", bins$bin),])>0)
      {
        if(nrow(bins[grep("^is.na{1}", bins$bin),])>0)
        {
          naRow <- bins[grep("is.na{1}", bins$bin),]
          bins <-  bins[-grep("is.na{1}", bins$bin),]
        }
        else
        {
          naBin <- grep("is.na{1}", bins$bin)
          bins[grep("is.na{1}", bins$bin),"bin"] <- strsplit(bins[grep("is.na{1}", bins$bin),"bin"], "\\|")[[1]][1]
        }
      }

      cuts <- getNumSplits(bins$bin)
      badRt <- bins$bad_rate
      bads <- bins$bads
      goods <- bins$goods

      trend <- 'N'
      while(trend == 'N' & length(cuts) > 1)
      {
        for(j in 1:(length(cuts)))
        {
          if(badRt[j] <= badRt[j+1])
          {
            bads[j] <- bads[j]+bads[j+1]
            goods[j] <- goods[j]+goods[j+1]
            badRt[j] <- bads[j]/(bads[j]+goods[j])*100
            bads <- bads[-(j+1)]
            goods <- goods[-(j+1)]
            badRt <- badRt[-(j+1)]
            cuts <- cuts[-(j)]

            if (j < naBin)
              naBin <- naBin -1
          }

          if (length(badRt) <= j+1)
            break
        }
        trend <- 'Y'

        for(j in 1:(length(cuts)))
        {
          if(badRt[j] < badRt[j+1])
            trend <- 'N'
        }
      }

      if(length(cuts)>0)
      {
        rules <- getBinRules(cuts, xVars[i])
        if(naBin > 0)
        {
          rules[naBin] <- paste0(rules[naBin], " | is.na(" , xVars[i], ")")
        }
        if(nrow(naRow) > 0)
        {
          rules <- c(rules, naRow$bin)
          bads <- c(bads, naRow$bads)
          goods <- c(goods, naRow$goods)
        }

        iv <- updateBins(xVars[i], rules, bads, goods)

	if( iv[nrow(iv), "trend"] == 'D')
	{
           binObj$varSummary[binObj$varSummary$var==xVars[i], "iv"] <- iv[nrow(iv), "iv"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "ent"] <- iv[nrow(iv), "ent"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "trend"] <- iv[nrow(iv), "trend"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "monTrend"] <- iv[nrow(iv), "monTrend"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "flipRatio"] <- iv[nrow(iv), "flipRatio"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "numBins"] <- iv[nrow(iv), "numBins"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "purNode"] <- iv[nrow(iv), "purNode"]
           binObj$bin <- binObj$bin[binObj$bin$var != xVars[i], ]
           binObj$bin <- rbind(binObj$bin, iv[,1:9])
	}
	else print(paste(xVars[i], ": Decreasing trend could not be forced"))
      }
      else
        print(paste(xVars[i], ": Decreasing trend could not be forced"))
    }
  }
  return(binObj)
}


#' @title Force a numerical variable to follow a monotonically increasing trend
#'
#' @description This function forces a variable to follow a monotonically increasing trend by grouping bins. In case such a trend can not be forced a message is printed to the console
#'
#' @param binObj - An object returned by getBins or any other function (except createBins) in this package
#' @param xVars - A vector of the name of variables
#'
#' @return Returns a list containing 3 objects. Similar to the getBins function
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('age', 'score'), minCr=0.6, minProp = 0.01)
#' b1 <- forceIncrTrend(b1, c('score','age'))
#'
#' @export
forceIncrTrend <- function(binObj, xVars)
{
  revBin <- data.frame(var = character(0), bin = character(0), count = numeric(0),  bads = numeric(0), goods = numeric(0),  propn = numeric(0),
                       bad_rate = numeric(0), goodCap = numeric(0), badCap = numeric(0), iv = numeric(0),
                       ent = numeric(0),  numBins = numeric(0), purNode= character(0), stringsAsFactors = F)
  res<- data.frame()

  for(i in 1:length(xVars))
  {
    if(binObj$varSummary[binObj$varSummary$var == xVars[i], "varType"] == "factor")
      print(paste(xVars[i], " : This function can only be applied to numerical variables"))
    else
    {
      bins <- binObj$bin[binObj$bin$var == xVars[i] & binObj$bin$bin != 'Total',]
      naBin <- 0
      naRow <- data.frame()

      if(nrow(bins[grep("is.na{1}", bins$bin),])>0)
      {
        if(nrow(bins[grep("^is.na{1}", bins$bin),])>0)
        {
          naRow <- bins[grep("is.na{1}", bins$bin),]
          bins <-  bins[-grep("is.na{1}", bins$bin),]
        }
        else
        {
          naBin <- grep("is.na{1}", bins$bin)
          bins[grep("is.na{1}", bins$bin),"bin"] <- strsplit(bins[grep("is.na{1}", bins$bin),"bin"], "\\|")[[1]][1]
        }
      }

      cuts <- getNumSplits(bins$bin)
      badRt <- bins$bad_rate
      bads <- bins$bads
      goods <- bins$goods

      trend <- 'N'
      while(trend == 'N' & length(cuts) > 1)
      {
        for(j in 1:(length(cuts)))
        {
          if(badRt[j] >= badRt[j+1])
          {
            bads[j] <- bads[j]+bads[j+1]
            goods[j] <- goods[j]+goods[j+1]
            badRt[j] <- bads[j]/(bads[j]+goods[j])*100
            bads <- bads[-(j+1)]
            goods <- goods[-(j+1)]
            badRt <- badRt[-(j+1)]
            cuts <- cuts[-(j)]

            if (j < naBin)
              naBin <- naBin -1
          }

          if (length(badRt) <= j+1)
            break
        }
        trend <- 'Y'

        for(j in 1:(length(cuts)))
        {
          if(badRt[j] > badRt[j+1])
            trend <- 'N'
        }
      }

      if(length(cuts)>0)
      {
        rules <- getBinRules(cuts, xVars[i])
        if(naBin > 0)
        {
          rules[naBin] <- paste0(rules[naBin], " | is.na(" , xVars[i], ")")
        }
        if(nrow(naRow) > 0)
        {
          rules <- c(rules, naRow$bin)
          bads <- c(bads, naRow$bads)
          goods <- c(goods, naRow$goods)
        }

        iv <- updateBins(xVars[i], rules, bads, goods)

	if( iv[nrow(iv), "trend"] == 'I')
	{
           binObj$varSummary[binObj$varSummary$var==xVars[i], "iv"] <- iv[nrow(iv), "iv"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "ent"] <- iv[nrow(iv), "ent"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "trend"] <- iv[nrow(iv), "trend"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "monTrend"] <- iv[nrow(iv), "monTrend"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "flipRatio"] <- iv[nrow(iv), "flipRatio"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "numBins"] <- iv[nrow(iv), "numBins"]
           binObj$varSummary[binObj$varSummary$var==xVars[i], "purNode"] <- iv[nrow(iv), "purNode"]
           binObj$bin <- binObj$bin[binObj$bin$var != xVars[i], ]
           binObj$bin <- rbind(binObj$bin, iv[,1:9])
	}
	else print(paste(xVars[i], ": Increasing trend could not be forced"))
      }
      else
        print(paste(xVars[i], ": Increasing trend could not be forced"))
    }
  }
  return(binObj)
}



#' @title Checking the performance of the bins created on test data
#'
#' @description This function uses parallel processing to replicate the bins on test data. This can be used to check the stability of the variable.
#'
#' @param binObj - An object returned by getBins or any other function (except createBins) in this package
#' @param testDf - A data frame containing the test data
#' @param y - The name of the dependent variable
#' @param xVars - A vector names of variables which are to be tested
#' @param nCores - The number of cores used for parallel processing. The default value is 1
#'
#' @return Returns a list containing 2 elements.
#' The first is a data frame called varSummary which contains a summary of the performance of the variables on the test data including their IV value, entropy, flag which indicates if bad rate increases/decreases with variable value, flag to indicate if a monotonic trend is present, number of bins which flip (i.e. do not follow a monotonic trend), number of bins of the variable and a flag to indicate whether it includes pure nodes (node which do not have any defaults).
#' The second element is a data frame called bin which contains details of all the bins of the variables.
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('LTV', 'balance'))
#' b2 <- binTest(b1, loanData[1:50,], "bad_flag", c('LTV', 'balance'))
#'
#' @export
binTest<- function(binObj, testDf, y, xVars, nCores=1)
{
  df <- as.data.table(testDf)
  res <- data.frame()
  i <- NULL

  if(length(xVars) == 1)
    res <- testBin(binObj, df, y, xVars)
  else
  {
    badData <- df[[y]]
    va= iterators::iter(df[,xVars, with = F])

    cl <- parallel::makeCluster(nCores)
    doParallel::registerDoParallel(cl)
    res <- foreach::foreach(va= va, i=iterators::icount(), .combine = rbind, .export = c('testBin', 'updateBins'), .packages = c('data.table')) %dopar% {
      d<- data.table::data.table(v = va, bad = badData)
      data.table::setnames(d,"v",xVars[i])
      iv <- testBin(binObj, d, "bad", xVars[i])
    }
    parallel::stopCluster(cl)
  }
  varsummary <- res[res$bin == 'Total', c("var","iv", "ent", "trend", "monTrend", "flipRatio", "numBins", "purNode", "varType")]
  varsummary <- varsummary[order(-varsummary$iv),]
  bin <- res[,1:9]
  return(list(varSummary= varsummary, bin=bin))
}


testBin <- function(binObj, df, y, x)
{
  goods <- numeric()
  bads <- numeric()
  bins <- binObj$bin[binObj$bin$var == x,]
  rules <- bins$bin[-nrow(bins)] #removing total row
  for(i in 1:length(rules))
  {
    d1<-subset(df,eval(parse(text=rules[[i]])))
    bads[i]<- sum(d1[[y]])
    goods[i]<- nrow(d1) - bads[i]
  }
  iv<- updateBins(x, rules, bads, goods)
  iv <- cbind(iv, varType = class(df[[x]]))
  return(iv)
}

#' @title Add binned variables to data
#'
#' @description This function creates a data frame with binned variables
#'
#' @param binObj - An object returned by getBins or any other function in this package
#' @param df - A data frame
#' @param xVars - A vector of names of variables for which bins have to be created
#' @param prefix - The prefix to be added to the variable name to create the new variable. Default value is b_
#'
#' @return Returns a dataframe which adds the binned variables to the original data frame
#'
#' @examples b1 <- getBins(loanData, "bad_flag", c('age', 'score', 'balance'), minCr=0.8)
#' loanData <- createBins(b1, loanData, c('age', 'balance'))
#'
#' @export
createBins <- function(binObj, df, xVars, prefix= "b_")
{
  df <- as.data.table(df)
  for(i in 1:length(xVars))
  {
    rules <- binObj$bin[binObj$bin$var == xVars[i] & binObj$bin$bin != 'Total',"bin"]
    df[,paste0(prefix, xVars[i])] <- NA
    naRule <- NULL
    for(j in 1:length(rules))
    {
      df[,paste0(prefix, xVars[i])] <- with(df, ifelse(eval(parse(text=rules[j])), gsub(" %in% c", "", gsub(xVars[i], "", rules[j])), get(df[,paste0(prefix, xVars[i])])))
      if(grepl("\\| is.na", rules[j]))
        naRule <- gsub(xVars[i], "", rules[j])
    }
    if(!is.null(naRule))
      df[,paste0(prefix, xVars[i])] <- with(df, ifelse(is.na(df[[xVars[i]]]), naRule, get(df[,paste0(prefix, xVars[i])])))
  }
  return(as.data.frame(df))
}


