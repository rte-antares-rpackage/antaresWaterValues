#' Fill constant water values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param min_wv minimal water value to use
#' @param max_wv maximal water value to use
#' @return a \code{data.table}

constant_fill <- function(results_dt,max_wv,min_wv)

{
  results <- copy(results_dt)


  for (i in 1:52){

    temp <- results[results$weeks==i]


    m <- 0
    M <- 0

    for (j in 1:nrow(temp)){

      if (!is.finite(temp$vu[j])) next

      if(m==0)m <- temp[j,]$statesid

      M <- j

    }
    M <- temp[M,]$statesid

    temp[temp$statesid>m,"vu":=max_wv]
    temp[temp$statesid<M,"vu":=min_wv]
    for (j in 2:nrow(temp)){
      if(is.na(temp$vu[j])) temp$vu[j] <-  temp$vu[j-1]
    }
    results[results$weeks==i,"vu" :=temp$vu]
  }

  return(results)
}











#' Fill the rest of reservoir states water values
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}.
#' @param max_cost maximal accepted water value (Replace by 'NA' crossed values)
#' @param min_cost minimal accepted water value (Replace by 'NA' crossed values)
#' @param full_imputation boolean. use 'max_cost' and 'min_cost' to impute the
#' missing values
#' @param impute_method impute method. check methods in \code{help(mice)}
#' @param fix Boolean. Fill missing values with constant values
#' @param min_vu minimal water value to use in fix strategy
#' @param max_vu maximal water value to use in fix strategy
#' @param blocker Boolean False to launch two times the post_process function recursively.
#' @return a \code{data.table}
#' @export

post_process <- function(results_dt,max_cost=3000,min_cost=0,full_imputation=FALSE,
                         impute_method='pmm',fix=F,min_vu=0.5,max_vu=1000,blocker=F){

  if (!requireNamespace("mice", quietly = TRUE)) {
    stop(
      "Package \"mice\" must be installed to use this function.",
      call. = FALSE
    )
  }
  results <- copy(results_dt)
  results[results$vu==Inf|is.na(results$vu),"vu":=NA]
  maxid <- max(results$statesid)
  q3 <- stats::quantile(results$statesid,0.75)
  if(!fix){
  results[results$vu>max_cost,"vu":=NA]
  results[results$vu<min_cost,"vu":=NA]}

  # results[vu<-down_cost,vu:=NA]

  if(fix){
    if(T){
      results <- constant_fill(results,max_vu,min_vu)
      return(results)
    }

    if(!is.numeric(min_vu))min_vu <- min(results$vu,na.rm = T)
    if(!is.numeric(max_vu))max_vu <- min(results$vu,na.rm = T)
    results[results$states>results$level_high,"vu":=min_vu]
    results[results$states<results$level_low,"vu":=max_vu]
    results$nvu <- NA_real_
    for (i in 1:52){
      temp <- results[results$weeks==i]
      temp <- dplyr::select(temp,"statesid","vu")
      reg <- stats::lm(vu ~ statesid, temp)
      temp$vu <- stats::predict(reg,temp)
      results[results$weeks==i]$nvu <-temp$vu
    }
    results[is.na(results$vu),"vu":=results$nvu]
    results$nvu <- NULL
    results[results$vu<0.5,"vu":=0.5]
    if(!blocker){
    results <- post_process(results,max_cost,min_cost,full_imputation,
                             impute_method,fix,min_vu,max_vu,blocker=T)}
    return(results)

  }

  cat("Prepare Water Values:\n")

  pb <- utils::txtProgressBar(min = 0, max = 51, style = 3)

  for (i in 1:52){
    maxi <- max(results[results$weeks==i]$vu,na.rm = TRUE)
    mini <- min(results[results$weeks==i]$vu,na.rm = TRUE)

    if(!full_imputation){
      results[results$weeks==i&!is.finite(results$vu)&results$states>=results$level_high,"vu":=0]

      if (max_cost>0){
        results[results$weeks==i&!is.finite(results$vu)&results$states<=results$level_low,"vu":=max_cost]
        results[results$weeks==i&results$states<=results$level_low,"vu":=max_cost]
      }
      results[results$weeks==i&!is.finite(results$vu)&results$states<=results$level_low,"vu":=interp_down(maxi,results$statesid,q3)]
    }else{
      results[results$weeks==i&results$states>results$level_high,"vu":=NA]
      results[results$weeks==i&results$states<results$level_low,"vu":=NA]
      results[results$weeks==i&results$statesid==maxid,"vu":=max_cost]
      results[results$weeks==i&results$statesid==1,"vu":=min_cost]
    }






    temp <- results[results$weeks==i]
    temp <- dplyr::select(temp,"statesid","vu")
    if (any(is.na(temp))){

      imputed_Data <- mice::mice(temp, m=1, maxit = 50, method = impute_method,
                           seed = 500,print = F,remove.collinear=FALSE)
      completeData <- mice::complete(imputed_Data,1)


      results[results$weeks==i]$vu <- sort(completeData$vu,decreasing = T)


    }
    utils::setTxtProgressBar(pb = pb, value = i)
  }
  close(pb)
  results[results$vu<0.5,"vu":=0.5]
  if(!blocker){
  results <- post_process(results,max_cost=,min_cost,full_imputation,
                          impute_method,fix,min_vu,max_vu,blocker=T)
  }
  return(results)

}

#' Force monotonic water values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param noise_ratio ratio to remove values that overcome the 95\% quantile.
#' @return a \code{data.table}
#' @export

monotonic_VU <- function(results_dt,noise_ratio=1)

{
  results <- copy(results_dt)
  results[results$vu==Inf|is.na(results$vu),"vu":=NaN]
  qm <- stats::quantile(results$vu,0.95,na.rm = TRUE)
  temp <- results[results$vu>1.5*qm]

  if (dim(temp)[1]>noise_ratio*nrow(results)){
    results[results$vu>1.5*qm,"vu":=NaN]
  }
  temp <- NULL

  for (i in 1:52){

    temp <- results[results$weeks==i]
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
      results[results$weeks==i,"vu" :=temp$vu]
  }

  return(results)
  }





#' Force monotonic water values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @return a \code{data.table}
#' @export

monotonic_JM <- function(results_dt)

{
  results <- copy(results_dt)
  results[results$vu==Inf|is.na(results$vu),"vu":=NaN]


  for (i in 1:52){

    temp <- results[results$weeks==i]


    m <- 0
    M <- 0

    for (j in 1:nrow(temp)){

      if (is.na(temp$vu[j])) next

      if(m==0)m <- j

      M <- j

    }


    for (t in m:(M-1)){
      if(temp$vu[t+1]>temp$vu[t])temp$vu[t+1] <-temp$vu[t]
    }
    results[results$weeks==i,"vu" :=temp$vu]
  }

  return(results)
}


#' Force monotonic water values Algorithm 2
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @return a \code{data.table}
#' @export

monotonic_JM2 <- function(results_dt)

{
  results <- copy(results_dt)
  results[results$vu==Inf|is.na(results$vu),"vu":=NaN]


  for (i in 1:52){
    temp <- results[results$weeks==i]
    m <- 0
    M <- 0

    for (j in 1:nrow(temp)){

      if (is.na(temp$vu[j])) next

      if(m==0)m <- j

      M <- j

    }
    vector <- temp$vu[m:M]

    vector <- tryCatch(monotonic_JM2_fun(vector),error = function(e) e)
    temp$vu[m:M] <- vector
  #inversed
   # tryCatch({ for (t in (M):m+1){
   #    low <- 0
   #    high <- 0
   #    done <- F
   #
   #
   #    if(temp$vu[t-1]<temp$vu[t]){
   #      max_exp <- min(M-t,t-m)
   #      if(max_exp!=0){
   #        for(k in 1:max_exp){
   #          if(temp$vu[t+k]<=temp$vu[t]) low <- 1
   #          if(temp$vu[t-k]>=temp$vu[t]) high <- 1
   #          if(high+low>0){
   #            temp$vu[t-1] <- (low *temp$vu[t+1+k] +high*temp$vu[t-k])/(low+high)
   #            done <- T
   #            break
   #          }
   #        }
   #        if(done==T) next
   #        if(M-t+1>t-m){
   #          for (p in max_exp:M-t+1){
   #            if(temp$vu[t+p]<=temp$vu[t]){
   #              temp$vu[t-1] <-temp$vu[t+1+p]
   #              done <- T
   #              break
   #            }
   #          }
   #        }
   #        if(done==T) next
   #        if(M-t+1<t-m){
   #          for (p in max_exp:t-m){
   #            if(temp$vu[t-p]>=temp$vu[t]){
   #              temp$vu[t-1] <-temp$vu[t-p]
   #              done <- T
   #              break
   #            }
   #          }
   #        }
   #
   #
   #
   #      }
   #    }
   #
   #
   #  }},error = function(e) e)

    results[results$weeks==i,"vu" :=temp$vu]
  }

  return(results)
}



monotonic_JM2_fun <- function(vector){

  m <- 1
  M <- length(vector)
  if(vector[1]<vector[2]) vector[1] <- max(vector,na.rm = T)
  for (t in m:(M-1)){
    low <- 0
    high <- 0
    done <- F
    if(vector[t+1]>vector[t]){
      max_exp <- min(M-t-1,t-m+1)
      if(((M-t+1)!=0)|((t-m)!=0)){
        if(max_exp!=0){
        for(k in 1:max_exp){
          if(vector[t+1+k]<=vector[t]) low <- 1
          if(vector[t-k]>=vector[t]) high <- 1
          if(high+low>0){
            if(t>1){
            vector[t+1] <- (low *vector[t+1+k] +high*vector[t-k])/(low+high)
            done <- T
            break
            }          }
        }}
        if(done==T) next
        if(M-t-1>t-m+1){
          for (p in max_exp:M-t-1){
            if(vector[t+1+p]<=vector[t]){
              vector[t+1] <-vector[t+1+p]
              done <- T
              break
            }
          }
        }
        if(done==T) next
        if(M-t-1<t-m+1){
          for (p in max_exp:t-m+1){
            if(vector[t-p]>=vector[t]){
              vector[t+1] <-vector[t-p]
              done <- T
              break
            }
          }
        }



      }
    }
    }

    for (t in m:(M-2)){
      if(vector[t+1]>vector[t]|vector[t+1]<vector[t+2]){
        if(vector[t+2]<=vector[t])
          vector[t+1] <-(vector[t]+vector[t+2])/2
    }
      }

  return(vector)

}




#' Remove outliers water Values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param min minimal accepted water value
#' @param max maximal accepted water value
#' @param NAN boolean. True to replace outlier values by 'NaN' and False to
#' replace them by 'min' and 'max' values
#' @return a \code{data.table}
#' @export

remove_out <- function(results_dt,min=NULL,max=NULL,NAN=T){

  results <- copy(results_dt)
  if(is.numeric(max)){
    if (NAN){
      results[results$vu>max,"vu":=NaN]
    }else{
      results[results$vu>max,"vu":=max]

    }
  }

  if(is.numeric(min)){
    if (NAN){
      results[results$vu<min,"vu":=NaN]
    }else{
      results[results$vu<min,"vu":=min]

    }
  }
  return(results)
}



#' Adjust  water Values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param value value to add to water values
#' @return a \code{data.table}
#' @export

adjust_wv <- function(results_dt,value=0){
  if(value==0){
    return(results_dt)
  }
  results <- copy(results_dt)

    results[,"vu":=results$vu+value]


  return(results)
}



#' function used to replace the values under the level_low of reservoir
#' @param maxi the maximuum of water values
#' @param statesid the state Id of the the to replace
#' @param q3 a quantile used in interpolation
interp_down <- function(maxi,statesid,q3){

  maxi*(exp((statesid)/q3))

}
