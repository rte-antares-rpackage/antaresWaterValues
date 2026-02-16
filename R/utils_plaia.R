validate_api_study_for_plaia <- function(opts) {

  assertthat::assert_that(
    is_api_study(opts),
    msg = "Study must be in API mode."
  )
  assertthat::assert_that(opts$parameters$`adequacy patch`$`include-adq-patch` == FALSE,
                          msg = "Adequacy Patch can only be used with Economy mode.")

  try(
    antaresRead::api_post(
      opts = opts,
      endpoint = paste0(opts$study_id, "/extensions/xpansion")
    ),
    silent = TRUE
  )
}
