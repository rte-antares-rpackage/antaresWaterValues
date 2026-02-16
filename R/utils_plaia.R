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

run_plaia_simulation <- function(opts, name_sim, other_options) {

  res <- antaresEditObject::runSimulation(
    name = name_sim,
    opts = opts,
    xpansion = list(enabled = TRUE),
    other_options = other_options
  )

  assertthat::assert_that(res$status == "success",msg = "Simulation failed.")

  res$output_id
}

download_output_zip <- function(opts, output_id, max_try = 5) {

  export_res <- antaresRead::api_get(
    opts = opts,
    endpoint = paste0(opts$study_id, "/outputs/", output_id, "/export")
  )

  i <- 0
  repeat {
    i <- i + 1
    res <- try(
      antaresRead::api_get(
        opts = opts,
        endpoint = export_res$file$id,
        default_endpoint = "v1/downloads"
      ),
      silent = TRUE
    )
    if (!inherits(res, "try-error") || i >= max_try) break
    Sys.sleep(10)
  }

  assertthat::assert_that(i < max_try, msg = "Too much attempts to download results.")

  res
}

