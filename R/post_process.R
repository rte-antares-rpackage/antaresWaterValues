monotonic_VU <- function(results,noise_ratio=0.001)

{
  results[vu==Inf|is.na(vu),vu:=NaN]
  qm <- quantile(results$vu,0.95,na.rm = TRUE)
  temp <- results[vu>1.5*qm]

  if (dim(temp)[1]>noise_ratio*nrow(results)){
    results[vu>1.5*qm,vu:=NaN]
  }
  temp <- NULL

  for (i in 1:53){

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







post_process <- function(results,down_cost=3000,full_imputation=FALSE,impute_method='pmm'){

  results[vu==Inf|is.na(vu),vu:=NA]
  maxid <- max(results$statesid)
  q3 <- quantile(results$statesid,0.75)

  results[abs(vu)>down_cost,vu:=NA]
  # results[vu<-down_cost,vu:=NA]


  cat("Prepare Water Values:\n")

  pb <- txtProgressBar(min = 0, max = 51, style = 3)

   for (i in 1:52){
    if (i==7) browser()
    maxi <- max(results[weeks==i]$vu,na.rm = TRUE)
    mini <- min(results[weeks==i]$vu,na.rm = TRUE)

    if(!full_imputation){
      # results[weeks==i&!is.finite(vu)&states>=level_high,vu:=interp_up(mini,statesid)]
      results[weeks==i&!is.finite(vu)&states>=level_high,vu:=0]

      if (down_cost>0){
        results[weeks==i&!is.finite(vu)&states<=level_low,vu:=down_cost]
        results[weeks==i&states<=level_low,vu:=down_cost]
      }
      results[weeks==i&!is.finite(vu)&states<=level_low,vu:=interp_down(maxi,statesid,q3)]
    }else{
      results[weeks==i&states>level_high,vu:=NA]
      results[weeks==i&states<level_low,vu:=NA]
      results[weeks==i&statesid==maxid,vu:=down_cost]
      results[weeks==i&statesid==1,vu:=-down_cost]



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

interp_up <- function(mini,statesid){

  mini*(1+(exp(-statesid)))*-1

}


interp_down <- function(maxi,statesid,q3){

  maxi*(exp((statesid)/q3))

}
