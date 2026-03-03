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

run_plaia_simulation <- function(opts, name_sim, other_options, cluster) {

  assertthat::assert_that(inherits(opts, "simOptions"))

  assertthat::assert_that(is_api_study(opts))

  antaresEditObject::updateGeneralSettings(mode = "economy", opts = opts)
  run <- antaresRead::api_post(
    opts = opts,
    endpoint = paste0("launcher/run/", opts$study_id,"?launcher=",cluster),
    default_endpoint = "v1",
    body = jsonlite::toJSON(list(output_suffix = name_sim,
                                 other_options = other_options,
                                 xpansion = list(enabled = TRUE)), auto_unbox = TRUE),
    encode = "raw"
  )
  assertthat::assert_that(!is.null(run$job_id), msg = "No job id returned by API, something went wrong.")

  message(paste("Job launched with ID:", run$job_id))

  status <- antaresEditObject::getJobs(run$job_id, opts = opts)
  while (is.null(status$completion_date) || is.na(status$completion_date)) {
    Sys.sleep(1)
    status <- antaresEditObject::getJobs(run$job_id, opts = opts)
  }


  assertthat::assert_that(status$status == "success",msg = "Simulation failed.")

  status$output_id
}

download_output_zip <- function(opts, output_id, max_try = 20) {

  export_res <- antaresRead::api_get(
    opts = opts,
    endpoint = paste0(opts$study_id, "/outputs/", output_id, "/export")
  )

  i <- 0
  repeat {
    i <- i + 1
    status  = get_task_status(opts, output_id, "EXPORT")
    if (status==0 || i >= max_try) break
    Sys.sleep(30)
  }

  assertthat::assert_that(i < max_try, msg = "Results export takes too much time.")

  download_res <- antaresRead::api_get(
      opts = opts,
      endpoint = export_res$file$id,
      default_endpoint = "v1/downloads"
    )

  assertthat::assert_that(!inherits(download_res, "try-error"), msg = "Results download failed.")

  tmpdir  <- tempfile("plaia_zip_")
  dir.create(tmpdir)

  zipfile <- tempfile(fileext = ".zip", tmpdir = tmpdir)
  writeBin(download_res, zipfile)

  zipfile
}

extract_from_zip <- function(zip_path, filename,sep = ",", header = TRUE) {
  utils::unzip(zip_path, files = filename, exdir = dirname(zip_path))
  utils::read.csv(file.path(dirname(zip_path), filename),sep = sep, header =  header)
}

prepare_and_launch_plaia <- function(list_areas,
                                     opts,
                                     mcyears,
                                     grid,
                                     params = list(),
                                     name_sim,
                                     other_options,
                                     cluster) {
  validate_and_normalize_areas(list_areas, opts)

  list_backup = list()
  for (j in seq_along(list_areas)) {
    area = list_areas[[j]]
    list_backup[[j]] = getBackupData(area, mcyears, opts)
    assertthat::assert_that(
      ncol(list_backup[[j]]$hydro_storage) >= max(mcyears),
      msg = paste0("There is no enough columns for data inflow for ", area)
    )
  }

  year_by_year = opts$parameters$general$`year-by-year`
  synthesis = opts$parameters$output$synthesis
  storenewset = opts$parameters$output$storenewset

  validate_api_study_for_plaia(opts)

  tryCatch({
    # Prepare study
    antaresEditObject::setPlaylist(playlist = mcyears, opts = opts)
    antaresEditObject::updateGeneralSettings(year.by.year = FALSE, opts = opts)
    antaresEditObject::updateOutputSettings(synthesis = FALSE,
                                            storenewset = FALSE,
                                            opts = opts)

    prepare_areas_for_simulation(list_areas, list_backup, opts)

    csv_content <- paste(utils::capture.output(utils::write.csv(
      grid, row.names = FALSE, quote = FALSE
    )),
    collapse = "\n")

    write_api_file(opts, "user/water_values/grid.csv", csv_content)

    write_api_file(opts,
                   "user/water_values/penalties.yaml",
                   yaml::as.yaml(params,
                                 handlers = list(logical = yaml::verbatim_logical)))

    # Start the simulations
    output_id = run_plaia_simulation(opts, name_sim, other_options, cluster)

    # Extract results
    zip_path = download_output_zip(opts, output_id)
  },
  error = function(e) {
    stop(e)
  },
  finally = {
    for (j in seq_along(list_areas)) {
      area = list_areas[[j]]
      restore_fictive_fatal_prod_demand(
        area = area,
        opts = opts,
        load = list_backup[[j]]$load,
        misc_gen = list_backup[[j]]$misc_gen
      )
    }
    antaresEditObject::updateGeneralSettings(year.by.year = year_by_year, opts =
                                               opts)
    antaresEditObject::updateOutputSettings(synthesis = synthesis,
                                            storenewset = storenewset,
                                            opts = opts)
  })

  return(zip_path)
}
