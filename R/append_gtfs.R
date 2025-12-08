#' Append GTFS Files From NetPlan
#'
#' @param baseline_filepath Character string in quotes. The location of the folder containing the GTFS files of the baseline network.
#' @param proposed_filepath Character string in quotes. The location of the folder containing the GTFS files of the proposed network.
#' @param deleted_routes Character. List routes to exclude from the final output.
#' @param save_csv T/F Do you want to save csv files of the appended GTFS tables?
#' @param save_RDS T/F Do you want to save an RDS object of the appended GTFS list object?
#' @param output_folder File path of the place to export file exports
#'
#' @return A list object with appended GTFS information. Optionally, exports to csv and RDS.
#'
#' @export

append_gtfs <- function(
  baseline_filepath,
  proposed_filepath,
  deleted_routes,
  save_csv = FALSE,
  save_RDS = FALSE,
  output_folder = NULL
) {
  if (!dir.exists(baseline_filepath) || !dir.exists(proposed_filepath)) {
    cli::cli_abort(
      "Folder does not exist. Make sure the folder path is correct and points to the parent directory of the GTFS you are combining."
    )
  }

  # Proposed
  proposed_gtfs <- list.files(
    path = here::here(proposed_filepath),
    pattern = "*.txt"
  )
  proposed_gtfs <- proposed_gtfs[
    !proposed_gtfs %in%
      c("stops.txt", "stop_times.txt", "error_warning.txt", "routes.txt")
  ]

  proposed <- lapply(proposed_gtfs, function(x) {
    out <- readr::read_csv(
      here::here(proposed_filepath, x),
      col_names = T,
      show_col_types = F,
      progress = F
    )

    return(out)
  })

  proposed[[6]] <- readr::read_csv(
    here::here(proposed_filepath, "routes.txt"),
    col_types = list(
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c"
    ),
    col_names = T,
    show_col_types = F,
    progress = F
  )

  proposed[[7]] <- readr::read_csv(
    here::here(proposed_filepath, "stops.txt"),
    col_types = list(
      "c",
      "c",
      "c",
      "c",
      "n",
      "n",
      "n",
      "n",
      "n",
      "c",
      "c",
      "c"
    ),
    col_names = T,
    show_col_types = F,
    progress = F
  )

  proposed[[8]] <- readr::read_csv(
    here::here(proposed_filepath, "stop_times.txt"),
    col_types = list("c", "c", "c", "c", "n", "n", "n", "n", "n", "n"),
    col_names = T,
    show_col_types = F,
    progress = F
  )

  names(proposed) <- c(
    "agency",
    "calendar",
    "calendar_dates",
    "shapes",
    "trips",
    "routes",
    "stops",
    "stop_times"
  )

  # Baseline
  baseline_gtfs <- list.files(
    path = here::here(baseline_filepath),
    pattern = "*.txt"
  )
  baseline_gtfs <- baseline_gtfs[
    !baseline_gtfs %in%
      c("stops.txt", "stop_times.txt", "error_warning.txt", "routes.txt")
  ]

  baseline <- lapply(baseline_gtfs, function(x) {
    out <- readr::read_csv(
      here::here(baseline_filepath, x),
      col_names = T,
      show_col_types = F,
      progress = F
    )

    return(out)
  })

  baseline[[6]] <- readr::read_csv(
    here::here(baseline_filepath, "routes.txt"),
    col_types = list(
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c",
      "c"
    ),
    col_names = T,
    show_col_types = F,
    progress = F
  )

  baseline[[7]] <- readr::read_csv(
    here::here(baseline_filepath, "stops.txt"),
    col_types = list(
      "c",
      "c",
      "c",
      "c",
      "n",
      "n",
      "n",
      "n",
      "n",
      "c",
      "c",
      "c"
    ),
    col_names = T,
    show_col_types = F,
    progress = F
  )

  baseline[[8]] <- readr::read_csv(
    here::here(baseline_filepath, "stop_times.txt"),
    col_types = list("c", "c", "c", "c", "n", "n", "n", "n", "n", "n"),
    col_names = T,
    show_col_types = F,
    progress = F
  )

  names(baseline) <- c(
    "agency",
    "calendar",
    "calendar_dates",
    "shapes",
    "trips",
    "routes",
    "stops",
    "stop_times"
  )

  # Remove Project Routes from Baseline
  project_routes <- c(proposed$route$route_id, deleted_routes)

  baseline$routes <- dplyr::filter(
    baseline$routes,
    !route_id %in% project_routes
  )

  # Assume the baseline network for missing day types for each route in the proposed network.
  # (i.e. Proposed changes made to weekday service only for a route)
  # Currently does not handle proposed network deleting service entirely for a specific day type

  # Create a field "route_day_key" that identifies unique route and day type combinations
  proposed$trips <- dplyr::mutate(
    proposed$trips,
    route_day_key = stringr::str_c(
      route_id,
      stringr::str_extract(service_id, '(Weekday|Saturday|Sunday)'),
      sep = "-"
    )
  )

  baseline$trips <- dplyr::mutate(
    baseline$trips,
    route_day_key = stringr::str_c(
      route_id,
      stringr::str_extract(service_id, '(Weekday|Saturday|Sunday)'),
      sep = "-"
    )
  )

  # Filter Baseline Trips table to later append to proposed network
  # Removes deleted routes
  # Removes route and day type combinations already found in proposed network

  # List of routes that will not be appended
  deleted_from_baseline <- sort(unique(
    baseline$trips[
      baseline$trips$route_id %in% deleted_routes,
    ]$route_id
  ))

  baseline$trips <- dplyr::filter(
    baseline$trips,
    !route_id %in% deleted_routes &
      !route_day_key %in% proposed$trips$route_day_key
  )

  # List of routes that will be appended for missing day types
  # (Assumes trips for specific day types were not in the proposed network because there is no change proposed)
  append_missing_day_type <- sort(unique(
    baseline$trips[
      baseline$trips$route_id %in% proposed$route$route_id,
    ]$route_day_key
  ))

  # List of routes that will be appended for all day types
  append_missing_routes <- sort(unique(
    baseline$trips[
      !baseline$trips$route_day_key %in% append_missing_day_type,
    ]$route_id
  ))

  if (length(append_missing_routes) > 0) {
    cli::cli_inform(paste0(
      "The following routes have been appended to the proposed network: ",
      toString(sort(as.numeric(append_missing_routes[str_detect(
        append_missing_routes,
        "\\b\\d+\\b"
      )]))),
      ", ",
      toString(append_missing_routes[
        !str_detect(append_missing_routes, "\\b\\d+\\b")
      ])
    ))
  }

  if (length(append_missing_day_type) > 0) {
    cli::cli_inform(paste0(
      "The following routes have been appended with missing day types: ",
      toString(append_missing_day_type)
    ))
  }

  if (length(deleted_from_baseline) > 0) {
    cli::cli_inform(paste0(
      "The following routes are not appended since they are deleted: ",
      toString(deleted_from_baseline)
    ))
  }

  # Remove records from trips that will not be appended
  baseline$calendar <- dplyr::filter(
    baseline$calendar,
    service_id %in% baseline$trips$service_id
  )

  baseline$stop_times <- dplyr::filter(
    baseline$stop_times,
    trip_id %in% baseline$trips$trip_id
  )

  baseline$shapes <- dplyr::filter(
    baseline$shapes,
    shape_id %in% baseline$trips$shape_id
  )

  baseline$stops <- dplyr::filter(
    baseline$stops,
    stop_id %in% baseline$stop_times$stop_id
  )

  appended_gtfs <- flatten(list(baseline, proposed))

  agency <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "agency"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  calendar <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "\\bcalendar\\b"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      start_date = as.Date(as.character(start_date), format = "%Y%m%d"),
      end_date = as.Date(as.character(end_date), format = "%Y%m%d")
    )

  calendar_csv <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "\\bcalendar\\b"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  calendar_dates <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "calendar_dates"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  routes <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "routes"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::distinct(route_id, .keep_all = T)

  trips <- appended_gtfs[stringr::str_detect(names(appended_gtfs), "trips")] %>%
    dplyr::bind_rows() %>%
    dplyr::select(-route_day_key) %>%
    dplyr::distinct()

  stops <- appended_gtfs[stringr::str_detect(names(appended_gtfs), "stops")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::distinct(stop_id, .keep_all = T) %>%
    dplyr::filter(!(is.na(stop_lat)))

  stop_times <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "stop_times"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  shapes <- appended_gtfs[stringr::str_detect(
    names(appended_gtfs),
    "shapes"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  shapes <- dplyr::select(shapes, shape_id:shape_dist_traveled)

  combined_gtfs <- list()

  combined_gtfs[[1]] <- agency
  combined_gtfs[[2]] <- calendar
  combined_gtfs[[3]] <- calendar_dates
  combined_gtfs[[4]] <- shapes
  combined_gtfs[[5]] <- trips
  combined_gtfs[[6]] <- routes
  combined_gtfs[[7]] <- stops
  combined_gtfs[[8]] <- stop_times

  names(combined_gtfs) <- c(
    "agency",
    "calendar",
    "calendar_dates",
    "shapes",
    "trips",
    "routes",
    "stops",
    "stop_times"
  )

  if (save_csv == TRUE) {
    readr::write_csv(
      agency,
      na = "",
      file = paste0(output_folder, "/", "agency.txt")
    )
    readr::write_csv(
      calendar_csv,
      na = "",
      file = paste0(output_folder, "/", "calendar.txt")
    )
    readr::write_csv(
      calendar_dates,
      na = "",
      file = paste0(output_folder, "/", "calendar_dates.txt")
    )
    readr::write_csv(
      routes,
      na = "",
      file = paste0(output_folder, "/", "routes.txt")
    )
    readr::write_csv(
      trips,
      na = "",
      file = paste0(output_folder, "/", "trips.txt")
    )
    readr::write_csv(
      stops,
      na = "",
      file = paste0(output_folder, "/", "stops.txt")
    )
    readr::write_csv(
      stop_times,
      na = "",
      file = paste0(output_folder, "/", "stop_times.txt")
    )
    readr::write_csv(
      shapes,
      na = "",
      file = paste0(output_folder, "/", "shapes.txt")
    )
    cli::cli_inform("CSV exports at {output_folder}")
  } else {
    cli::cli_inform("No CSVs saved")
  }

  if (save_RDS == TRUE) {
    saveRDS(combined_gtfs, file = paste0(output_folder, "/", "GTFS.RDS"))
    cli::cli_inform("RDS export at {output_folder}")
  } else {
    cli::cli_inform("No RDS saved")
  }

  combined_gtfs
}
