#' @export

monotonic_VU <- function(results_dt,noise_ratio=10)

{
  results <- copy(results_dt)
  results[vu==Inf|is.na(vu),vu:=NaN]
  qm <- quantile(results$vu,0.95,na.rm = TRUE)
  temp <- results[vu>1.5*qm]

  if (dim(temp)[1]>noise_ratio*nrow(results)){
    results[vu>1.5*qm,vu:=NaN]
  }
  temp <- NULL

  for (i in 1:52){

    temp <- results[weeks==i]
    maxi <- max(temp$vu,na.rm = TRUE)
    meanw <- mean(temp$vu,na.rm = TRUE)
    q9 <- quantile(temp$vu,0.95,na.rm = TRUE)
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






#' @export

post_process <- function(results_dt,max_cost=3000,min_cost=0,full_imputation=FALSE,impute_method='pmm'){

  results <- copy(results_dt)
  results[vu==Inf|is.na(vu),vu:=NA]
  maxid <- max(results$statesid)
  q3 <- quantile(results$statesid,0.75)

  results[vu>max_cost,vu:=NA]
  results[vu<min_cost,vu:=NA]

  # results[vu<-down_cost,vu:=NA]


  cat("Prepare Water Values:\n")

  pb <- txtProgressBar(min = 0, max = 51, style = 3)

   for (i in 1:52){
    maxi <- max(results[weeks==i]$vu,na.rm = TRUE)
    mini <- min(results[weeks==i]$vu,na.rm = TRUE)

    if(!full_imputation){
      # results[weeks==i&!is.finite(vu)&states>=level_high,vu:=interp_up(mini,statesid)]
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
      results[weeks==i&statesid==1,vu:=-min_cost]



    }


    temp <- results[weeks==i]
    temp <- dplyr::select(temp,statesid,vu)
        if (any(is.na(temp))){

      imputed_Data <- mice(temp, m=1, maxit = 50, method = impute_method,
                           seed = 500,print = F)
      completeData <- complete(imputed_Data,1)


      results[weeks==i]$vu <- sort(completeData$vu,decreasing = T)


    }
    setTxtProgressBar(pb = pb, value = i)
   }
  close(pb)
return(results)

}


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

interp_up <- function(mini,statesid){

  mini*(1+(exp(-statesid)))*-1

}


interp_down <- function(maxi,statesid,q3){

  maxi*(exp((statesid)/q3))

}
