#' Remove outliers water Values
#'
#' @param results_dt Output from \code{watervalues} or \code{Grid_Matrix}
#' @param min minimal accepted water value
#' @param max_vu value to replace values higher than max
#' @param min_vu value to replace values lower than min
#' @param replace_na_method Method to replace extreme values, either "constant values" to replace
#' by max_vu and min_vu or "extreme values" to replace by the extreme values of the current week
#' @param max maximal accepted water value
#'
#' @return a \code{data.table}
#' @export

remove_out <- function(results_dt,min=NULL,max=NULL,max_vu,min_vu,replace_na_method){

  results <- copy(results_dt)

  if(is.numeric(max)){
    if (replace_na_method=="Constant values"){
      results[results$vu>max,"vu":=max_vu]
    } else {
      results <- results %>%
        dplyr::filter(.data$vu<max) %>%
        dplyr::group_by(.data$weeks) %>%
        dplyr::summarise(max_weekly_vu=max(.data$vu,na.rm = T),.groups = "drop") %>%
        dplyr::right_join(results,by=c("weeks")) %>%
        dplyr::mutate(vu=dplyr::if_else(.data$vu>max,.data$max_weekly_vu,.data$vu)) %>%
        as.data.table()
    }
  }

  if(is.numeric(min)){
    if (replace_na_method=="Constant values"){
      results[results$vu<min,"vu":=min_vu]
    } else {
      results <- results %>%
        dplyr::filter(.data$vu>min) %>%
        dplyr::group_by(.data$weeks) %>%
        dplyr::summarise(min_weekly_vu=min(.data$vu,na.rm = T),.groups = "drop") %>%
        dplyr::right_join(results,by=c("weeks")) %>%
        dplyr::mutate(vu=dplyr::if_else(.data$vu<min,.data$min_weekly_vu,.data$vu)) %>%
        as.data.table()
    }
  }

  results <- results %>%
    dplyr::mutate(vu_pen=dplyr::case_when(.data$states>.data$level_high ~ .data$vu + .data$penalty_high,
                                            .data$states<.data$level_low ~ .data$vu - .data$penalty_low,
                                            TRUE ~ .data$vu))

  return(results)
}
