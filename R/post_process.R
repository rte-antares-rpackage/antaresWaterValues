post_process <- function(results){

  results[vu==Inf|is.na(vu),vu:=NaN]
  maxid <- max(results$statesid)
  q3 <- quantile(results$statesid,0.75)
  for (i in 1:52){


    maxi <- max(results[weeks==i]$vu,na.rm = TRUE)
    mini <- min(results[weeks==i]$vu,na.rm = TRUE)

    results[weeks==i&!is.finite(vu)&states>=level_high,vu:=interp_up(mini,statesid)]
    results[weeks==i&!is.finite(vu)&states<=level_low,vu:=interp_down(mini,statesid,q3)]
    results[weeks==i&!is.finite(vu),vu:=mini]

  }

return(results)

}

interp_up <- function(mini,statesid){

  mini*(1-(exp(-statesid)))

}


interp_down <- function(maxi,statesid,q3){

  maxi*(exp((statesid)/q3))

}
