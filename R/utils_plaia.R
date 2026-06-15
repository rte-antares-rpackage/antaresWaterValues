validate_study_for_plaia <- function(opts, plaia_path = NULL) {

  assertthat::assert_that(opts$parameters$`adequacy patch`$`include-adq-patch` == FALSE,
                          msg = "Adequacy Patch can only be used with Economy mode.")

  if (is_api_study(opts)) {
    try(
      antaresRead::api_post(
        opts = opts,
        endpoint = paste0(opts$study_id, "/extensions/xpansion")
      ),
      silent = TRUE
    )
  } else {
    assertthat::assert_that(
      !is.null(plaia_path),
      msg = "plaia_path must be provided for local studies."
    )
    assertthat::assert_that(
      dir.exists(plaia_path),
      msg = paste0("plaia installation folder not found at: ", plaia_path)
    )
  }
}

run_plaia_simulation <- function(opts, name_sim, other_options, cluster, plaia_path = NULL, threads = 1) {

  assertthat::assert_that(inherits(opts, "simOptions"))

  antaresEditObject::updateGeneralSettings(mode = "economy", opts = opts)

  if (is_api_study(opts)) {
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

    assertthat::assert_that(status$status == "success", msg = "Simulation failed.")

    return(status$output_id)

  } else {
    study_path <- opts$studyPath
    output_dir <- file.path(study_path, "output")
    outputs_before <- list.dirs(output_dir, recursive = FALSE)

    exe_path <- file.path(plaia_path, "bin", paste0(other_options, ".exe"))
    assertthat::assert_that(
      file.exists(exe_path),
      msg = paste0("Plaia executable not found at: ", exe_path)
    )
    cmd <- paste0(
      shQuote(exe_path),
      " --study ", shQuote(study_path),
      " --threads ", threads
    )
    ret <- system(cmd, wait = TRUE)
    assertthat::assert_that(ret == 0, msg = paste0("Plaia simulation failed (exit code ", ret, ")."))

    outputs_after <- list.dirs(output_dir, recursive = FALSE)
    new_outputs <- setdiff(outputs_after, outputs_before)
    assertthat::assert_that(length(new_outputs) == 1, msg = paste0(
      length(new_outputs), " new output folders found after plaia run - expected exactly 1."
    ))

    return(new_outputs[[1]])
  }
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

cleanup_plaia_output <- function(result_path) {
  if (grepl("\\.zip$", result_path)) {
    unlink(dirname(result_path), recursive = TRUE)
  }
}

# Returns a zip path (API) or an output folder path (local).
# Use extract_plaia_result() to read files from either.
extract_plaia_result <- function(result_path, filename, sep = ",", header = TRUE) {
  if (grepl("\\.zip$", result_path)) {
    utils::unzip(result_path, files = filename, exdir = dirname(result_path))
    utils::read.csv(file.path(dirname(result_path), filename), sep = sep, header = header)
  } else {
    csv_path <- file.path(result_path, filename)
    assertthat::assert_that(file.exists(csv_path),
                            msg = paste0("Result file not found: ", csv_path))
    utils::read.csv(csv_path, sep = sep, header = header)
  }
}


prepare_and_launch_plaia <- function(list_areas,
                                     opts,
                                     mcyears,
                                     grid,
                                     params = list(),
                                     name_sim,
                                     other_options,
                                     cluster,
                                     plaia_path = NULL,
                                     settings = list(),
                                     threads = 1L) {
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

  validate_study_for_plaia(opts, plaia_path)

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

    penalties_yaml <- yaml::as.yaml(params,   handlers = list(logical = yaml::verbatim_logical))
    settings_yaml  <- yaml::as.yaml(settings, handlers = list(logical = yaml::verbatim_logical))

    if (is_api_study(opts)) {
      write_api_file(opts, "user/water_values/grid.csv",      csv_content)
      write_api_file(opts, "user/water_values/dynamic_programming.yaml", penalties_yaml)
      write_api_file(opts, "user/water_values/settings.yaml",  settings_yaml)
    } else {
      local_user_dir <- file.path(opts$studyPath, "user", "water_values")
      dir.create(local_user_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines(csv_content,    file.path(local_user_dir, "grid.csv"))
      writeLines(penalties_yaml, file.path(local_user_dir, "dynamic_programming.yaml"))
      writeLines(settings_yaml,  file.path(local_user_dir, "settings.yaml"))
    }

    # Start the simulations
    result_id = run_plaia_simulation(opts, name_sim, other_options, cluster, plaia_path, threads)

    # Extract results
    if (is_api_study(opts)) {
      result_path = download_output_zip(opts, result_id)
    } else {
      result_path = result_id
    }
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

  return(result_path)
}
