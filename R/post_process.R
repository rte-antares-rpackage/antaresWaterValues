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







post_process <- function(results){

  results[vu==Inf|is.na(vu),vu:=NaN]
  maxid <- max(results$statesid)
  q3 <- quantile(results$statesid,0.75)
  for (i in 1:53){


    maxi <- max(results[weeks==i]$vu,na.rm = TRUE)
    mini <- min(results[weeks==i]$vu,na.rm = TRUE)

    results[weeks==i&!is.finite(vu)&states>=level_high,vu:=interp_up(mini,statesid)]
    results[weeks==i&!is.finite(vu)&states<=level_low,vu:=interp_down(maxi,statesid,q3)]
    results[weeks==i&!is.finite(vu),vu:=mini]

  }

return(results)

}

interp_up <- function(mini,statesid){

  mini*(1+(exp(-statesid)))*-1

}


interp_down <- function(maxi,statesid,q3){

  maxi*(exp((statesid)/q3))

}
