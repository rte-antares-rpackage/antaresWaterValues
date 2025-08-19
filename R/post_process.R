#' Post process water values
#'
#' Replace extreme water values by chosen values.
#'
#' @param results_dt A \code{dplyr::tibble()} containing water values. Output \code{watervalues} from \code{Grid_Matrix()}.
#' @param min Double. Minimal accepted water value.
#' @param max_vu Double. Value to use for water values higher than \code{max}.
#' @param min_vu Double. Value to use for water values lower than \code{min}.
#' @param replace_na_method Character. Method to replace extreme values, either \code{"constant values"} to replace
#' by \code{max_vu} and \code{min_vu} or \code{"extreme values"} to replace by the extreme values of the current week.
#' @param max Double. Maximal accepted water value.
#'
#' @returns A \code{dplyr::tibble()} with same format than \code{results_dt}.
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
        dplyr::summarise(max_weekly_vu=max(.data$vu,na.rm = TRUE),.groups = "drop") %>%
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
        dplyr::summarise(min_weekly_vu=min(.data$vu,na.rm = TRUE),.groups = "drop") %>%
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
