#' Fill the rest of reservoir states water values
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}.
#' @param max_cost maximal accepted water value (Replace by 'NA' crossed values)
#' @param min_cost minimal accepted water value (Replace by 'NA' crossed values)
#' @param full_imputation boolean. use 'max_cost' and 'min_cost' to impute the
#' missing values
#' @param impute_method impute method. check methods in \code{help(mice)}
#' @param fix boolean. Fill missing values with constant values
#' @param min_vu minimal water value to use in fix strategy
#' @param max_vu maximal water value to use in fix strategy
#' @import data.table
#' @importFrom mice mice complete
#' @importFrom dplyr select
#' @importFrom stats lm predict quantile
#' @return a \code{data.table}
#' @export

post_process <- function(results_dt,max_cost=3000,min_cost=0,full_imputation=FALSE,
                         impute_method='pmm',fix=F,min_vu=0,max_vu=1000){

  results <- copy(results_dt)
  results[vu==Inf|is.na(vu),vu:=NA]
  maxid <- max(results$statesid)
  q3 <- stats::quantile(results$statesid,0.75)

  results[vu>max_cost,vu:=NA]
  results[vu<min_cost,vu:=NA]

  # results[vu<-down_cost,vu:=NA]

  if(fix){

    results[states>level_high,vu:=min_vu]
    results[states<level_low,vu:=max_vu]
    results$nvu <- NA_real_
    for (i in 1:52){
      temp <- results[weeks==i]
      temp <- dplyr::select(temp,statesid,vu)
      reg <- stats::lm(vu ~ statesid, temp)
      temp$vu <- stats::predict(reg,temp)
      results[weeks==i]$nvu <-temp$vu
    }
    results[is.na(vu),vu:=nvu]
    results$nvu <- NULL
    return(results)

  }

  cat("Prepare Water Values:\n")

  pb <- txtProgressBar(min = 0, max = 51, style = 3)

  for (i in 1:52){
    maxi <- max(results[weeks==i]$vu,na.rm = TRUE)
    mini <- min(results[weeks==i]$vu,na.rm = TRUE)

    if(!full_imputation){
      results[weeks==i&!is.finite(vu)&states>=level_high,vu:=0]

      if (max_cost>0){
        results[weeks==i&!is.finite(vu)&states<=level_low,vu:=max_cost]
        results[weeks==i&states<=level_low,vu:=max_cost]
      }
      results[weeks==i&!is.finite(vu)&states<=level_low,vu:=interp_down(maxi,statesid,q3)]
    }else{
      results[weeks==i&states>level_high,vu:=NA]
      results[weeks==i&states<level_low,vu:=NA]
      results[weeks==i&statesid==maxid,vu:=max_cost]
      results[weeks==i&statesid==1,vu:=min_cost]
    }






    temp <- results[weeks==i]
    temp <- dplyr::select(temp,statesid,vu)
    if (any(is.na(temp))){

      imputed_Data <- mice::mice(temp, m=1, maxit = 50, method = impute_method,
                           seed = 500,print = F,remove.collinear=FALSE)
      completeData <- mice::complete(imputed_Data,1)


      results[weeks==i]$vu <- sort(completeData$vu,decreasing = T)


    }
    setTxtProgressBar(pb = pb, value = i)
  }
  close(pb)
  return(results)

}

#' Force monotonic water values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param noise_ratio ratio to remove values that overcome the 95\% quantile.
#' @import data.table
#' @importFrom stats quantile
#' @return a \code{data.table}
#' @export

monotonic_VU <- function(results_dt,noise_ratio=1)

{
  results <- copy(results_dt)
  results[vu==Inf|is.na(vu),vu:=NaN]
  qm <- stats::quantile(results$vu,0.95,na.rm = TRUE)
  temp <- results[vu>1.5*qm]

  if (dim(temp)[1]>noise_ratio*nrow(results)){
    results[vu>1.5*qm,vu:=NaN]
  }
  temp <- NULL

  for (i in 1:52){

    temp <- results[weeks==i]
    maxi <- max(temp$vu,na.rm = TRUE)
    meanw <- mean(temp$vu,na.rm = TRUE)
    q9 <- stats::quantile(temp$vu,0.95,na.rm = TRUE)
    counter <- 0

    m <- 0
    M <- 0

    for (j in 1:nrow(temp)){

      if (is.na(temp$vu[j])) next

      if(m==0)m <- j

      M <- j

    }


      temp$vu[m:M]<- temp$vu[m:M][order(temp$vu[m:M],decreasing = TRUE)]
      results[weeks==i,vu :=temp$vu]
  }

  return(results)
  }


#' Remove outliers water Values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param min minimal accepted water value
#' @param max maximal accepted water value
#' @param NAN boolean. True to replace outlier values by 'NaN' and False to
#' replace them by 'min' and 'max' values
#' @import data.table
#' @return a \code{data.table}
#' @export

remove_out <- function(results_dt,min=NULL,max=NULL,NAN=T){

  results <- copy(results_dt)
  if(is.numeric(max)){
    if (NAN){
      results[vu>max,vu:=NaN]
    }else{
      results[vu>max,vu:=max]

    }
  }

  if(is.numeric(min)){
    if (NAN){
      results[vu<min,vu:=NaN]
    }else{
      results[vu<min,vu:=min]

    }
  }
  return(results)
}



#' Adjust  water Values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param value value to add to water values
#' @import data.table
#' @return a \code{data.table}
#' @export

adjust_wv <- function(results_dt,value=0){
  if(value==0){
    return(results_dt)
  }
  results <- copy(results_dt)

    results[,vu:=vu+value]


  return(results)
}



#' function used to replace the values under the level_low of reservoir
#' @param maxi the maximuum of water values
#' @param statesid the state Id of the the to replace
#' @param q3 a quantile used in interpolation
#' @export
interp_down <- function(maxi,statesid,q3){

  maxi*(exp((statesid)/q3))

}
