is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

check_area_name <- function(area, opts) {
  areaList <- antaresRead::getAreas(opts = opts)
  if (!tolower(area) %in% areaList)
    stop("'", area, "' is not a valid area name, possible names are: ", paste(areaList, collapse = ", "), call. = FALSE)
}

#' @importFrom utils URLencode
#' @importFrom shiny isRunning
fread_antares <- function(opts, file, ...) {
  if (identical(opts$typeLoad, "api")) {
    file <- gsub("\\.txt$", "", file)
    response <- antaresRead::api_get(
      opts = opts,
      endpoint = I(file),
      query = list(formatted = FALSE),
      parse_result = "text"
    )
    suppressWarnings(
      tryCatch(fread(response, ...), error = function(e){
        if(isRunning())
          e <- as.character(e)
        message(file)
        message(e)
      }))
  } else {
    suppressWarnings(
      fread(file, ...))
  }
}

createDistrict <- function(name,
                           caption = NULL,
                           comments = NULL,
                           apply_filter = c("none", "add-all", "remove-all"),
                           add_area = NULL,
                           remove_area = NULL,
                           output = FALSE,
                           overwrite = FALSE,
                           opts) {

  apply_filter <- match.arg(arg = apply_filter)

  if (tolower(name) %in% antaresRead::getDistricts(opts = opts) & !overwrite) {
    stop(paste("District", name, "already exists!"))
  }

  with_add_area <- !is.null(add_area)
  with_remove_area <- !is.null(remove_area)
  all_areas <- antaresRead::getAreas(opts = opts)

  if (with_add_area & with_remove_area) {
    stop("You can not use 'add_area' and 'remove_area' at the same time")
  }
  if (with_add_area) {
    assertthat::assert_that(length(setdiff(add_area, all_areas)) == 0, msg = "Invalid area in 'add_area'")
    assertthat::assert_that(apply_filter %in% c("remove-all", "none"), msg = "You have to use 'add_area' with 'apply_filter' set to remove-all")
    if (identical(apply_filter, "none")) {
      apply_filter <- "remove-all"
    }
  }
  if (with_remove_area) {
    assertthat::assert_that(length(setdiff(remove_area, all_areas)) == 0, msg = "Invalid area in 'remove_area'")
    assertthat::assert_that(apply_filter %in% c("add-all", "none"), msg = "You have to use 'remove_area' with 'apply_filter' set to add-all")
    if (identical(apply_filter, "none")) {
      apply_filter <- "add-all"
    }
  }

  new_district <- list(
    caption = caption,
    comments = comments,
    `apply-filter` = apply_filter,
    output = output
  )

  # API block
  if (is_api_study(opts = opts)) {
    new_district[["name"]] <- name

    if (with_add_area) {
      new_district[["areas"]] <- add_area
    }

    if (with_remove_area) {
      new_district[["areas"]] <- remove_area
    }

    body <- transform_list_to_json_for_district_parameters(district_parameters = new_district)

    result <- antaresRead::api_post(opts = opts,
                                    endpoint = file.path(opts[["study_id"]], "districts"),
                                    default_endpoint = "v1/studies",
                                    body = body
    )

    cli::cli_alert_success("Endpoint Create {.emph {.strong district}} {.emph {.strong {name}}} success")

    return(update_api_opts(opts = opts))
  } else {
    antaresEditObject::createDistrict(name,
                                      caption,
                                      comments,
                                      apply_filter,
                                      add_area,
                                      remove_area,
                                      output,
                                      overwrite,
                                      opts)
  }
}

transform_list_to_json_for_district_parameters <- function(district_parameters) {

  assertthat::assert_that(inherits(x = district_parameters, what = "list"))
  # caption is an extra input and not permitted by the endpoint
  district_parameters[["caption"]] <- NULL
  district_parameters <- dropNulls(district_parameters)
  names(district_parameters) <- sapply(names(district_parameters), rename_district_parameters_for_endpoint, USE.NAMES = FALSE)
  # If areas is a single value, auto_unbox = TRUE can lead to error. So we unbox each value except areas.
  district_parameters <- sapply(names(district_parameters),
                                FUN = function(el) {
                                  if (el != "areas") {
                                    return(jsonlite::unbox(district_parameters[[el]]))
                                  } else {
                                    return(district_parameters[[el]])
                                  }
                                },
                                USE.NAMES = TRUE,
                                simplify = FALSE
  )

  return(jsonlite::toJSON(district_parameters))
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

rename_district_parameters_for_endpoint <- function(arg) {

  if (length(arg) > 1) {
    stop("'arg' must be length one")
  }

  antares_params <- as.list(c("name", "caption", "comments", "output", "apply_filter", "areas"))

  names(antares_params) <- c("name", "caption", "comments", "output", "apply-filter", "areas")

  antares_params[[arg]]
}

update_api_opts <- function(opts) {
  modeAPI <- opts$modeAPI
  if (identical(modeAPI, "async"))
    return(invisible(opts))
  host <- opts$host
  study_id <- opts$study_id
  suppressWarnings({
    opts <- antaresRead::setSimulationPathAPI(
      host = host,
      study_id = study_id,
      token = opts$token,
      simulation = "input"
    )
  })
  opts$host <- host
  opts$study_id <- study_id
  opts$modeAPI <- modeAPI
  options(antares = opts)
  return(invisible(opts))
}
