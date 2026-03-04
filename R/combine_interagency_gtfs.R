#' Combine GTFS Files From Interagencies
#'
#' This function combines Metro's GTFS with other agency's GTFS (i.e. Community Transit, Pierce Transit, Sound Transit). This function is similar in function with @seealso \code{\link[=combine_gtfs]{combine_gtfs()}}.
#'
#' @param gtfs_filepath Character string in quotes. The location of the folders containing the GTFS files. This should be the parent directory, not the sub-directory.
#' @param save_csv T/F Do you want to save csv files of the combined GTFS tables?
#' @param save_RDS T/F Do you want to save an RDS object of the combined GTFS list object?
#' @param output_folder File path of the place to export file exports
#'
#' @return a list object with combined GTFS information. Optionally, exports to csv and RDS.
#'
#' @export

combine_interagency_gtfs <- function(
  gtfs_filepath,
  save_csv = FALSE,
  save_RDS = FALSE,
  output_folder = NULL
) {
  if (!dir.exists(gtfs_filepath)) {
    cli::cli_abort(
      "Folder does not exist. Make sure the folder path is correct and points to the parent directory of the GTFS you are combining."
    )
  } else {
    gtfs_folders <- list.files(gtfs_filepath)

    if (length(stringr::str_subset(gtfs_folders, pattern = "txt")) != 0) {
      cli::cli_abort(
        "Filepath should be for the location of the folders to be combined, not a sub-folder."
      )
    } else {
      gtfs_list <- list()

      for (i in 1:length(gtfs_folders)) {
        weekday <- list()

        weekday[[1]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "agency.txt"),
          col_types = list("c", "c", "c", "c", "c", "c", "c", "c"),
          col_names = T,
          show_col_types = F,
          progress = F
        )

        weekday[[2]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "trips.txt"),
          col_select = list(
            "route_id",
            "service_id",
            "trip_id",
            "trip_headsign",
            "direction_id",
            "block_id",
            "shape_id"
          ),
          col_types = list("c", "c", "c", "c", "c", "c", "c"),
          col_names = T,
          show_col_types = F,
          progress = F
        )

        weekday[[3]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "shapes.txt"),
          col_types = list("c", "n", "n", "n", "n"),
          col_names = T,
          show_col_types = F,
          progress = F
        )

        weekday[[4]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "calendar.txt"),
          col_types = list("c", "n", "n", "n", "n", "n", "n", "n", "c", "c"),
          col_names = T,
          show_col_types = F,
          progress = F
        )

        weekday[[5]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "calendar_dates.txt"),
          col_types = list("c", "n", "n"),
          col_names = T,
          show_col_types = F,
          progress = F
        )

        weekday[[6]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "routes.txt"),
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

        weekday[[7]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "stops.txt"),
          col_select = list(
            "stop_id",
            "stop_code",
            "stop_name",
            "stop_lat",
            "stop_lon"
          ),
          col_names = T,
          show_col_types = F,
          progress = F
        ) %>%
          dplyr::mutate(stop_id = as.character(stop_id))

        weekday[[8]] <- readr::read_csv(
          here::here(gtfs_filepath, gtfs_folders[i], "stop_times.txt"),
          col_types = list("c", "c", "c", "c", "n", "n", "n", "n", "n", "n"),
          col_names = T,
          show_col_types = F,
          progress = F
        )
        names(weekday) <- c(
          "agency",
          "trips",
          "shapes",
          "calendar",
          "calendar_dates",
          "routes",
          "stops",
          "stop_times"
        )

        # Add agency id to trips and calendar table
        weekday$trips <- weekday$trips %>%
          dplyr::left_join(
            select(weekday$routes, agency_id, route_id),
            by = "route_id"
          )

        weekday$calendar <- weekday$calendar %>%
          dplyr::left_join(
            dplyr::distinct(dplyr::select(
              weekday$trips,
              service_id,
              agency_id
            )),
            by = "service_id"
          )

        gtfs_list[[i]] <- weekday
      }

      baseline_gtfs <- flatten(gtfs_list)
    }
  }

  agency <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
    "agency"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::distinct(agency_id, .keep_all = T)

  calendar <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
    "calendar$"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(service_id, .keep_all = T)

  # Check if all service ids overlap each other
  # Get service date range for King County Metro (It should be the same across all service changes if generated with combine_gtfs/append_gtfs)
  kcm_date_range <- calendar %>%
    dplyr::filter(agency_id == 'King County  Metro Transit') %>%
    dplyr::mutate(date_range = interval(start_date, end_date)) %>%
    dplyr::distinct(date_range)

  calendar_check <- calendar %>%
    dplyr::mutate(
      overlap_flag = int_overlaps(
        interval(ymd(start_date), ymd(end_date)),
        kcm_date_range$date_range
      )
    )

  if (all(calendar_check$overlap_flag)) {
    calendar <- calendar %>%
      dplyr::select(-agency_id)
  } else {
    cli::cli_abort(
      "Service Date Range from other agency's GTFS does not overlap with King County Metro's Service Date Range. Check to see if all GTFS files are from the same service change."
    )
  }

  calendar_csv <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
    "calendar$"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::select(-agency_id) %>%
    dplyr::distinct()

  calendar_dates <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
    "calendar_dates"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  routes <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
    "routes"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::distinct(agency_id, route_id, .keep_all = T) %>%
    dplyr::mutate(
      route_id = case_match(
        agency_id,
        '29' ~ str_c('C', route_id),
        '3' ~ str_c('P', route_id),
        '40' ~ str_c('S', route_id),
        .default = route_id
      )
    ) %>%
    dplyr::group_by(route_id) %>%
    dplyr::mutate(agency_ct = length(agency_id)) %>%
    # If a route exists in both Metro's GTFS and another agency's GTFS, drop the records associated with Metro's GTFS.
    # Future considerations to make this an option
    dplyr::filter(
      !(agency_ct == 2 & agency_id %in% c('King County  Metro Transit'))
    ) %>%
    dplyr::select(-agency_ct)

  trips <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "trips")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      route_id = case_match(
        agency_id,
        '29' ~ str_c('C', route_id),
        '3' ~ str_c('P', route_id),
        '40' ~ str_c('S', route_id),
        .default = route_id
      )
    ) %>%
    dplyr::group_by(route_id) %>%
    dplyr::mutate(agency_ct = length(unique(agency_id))) %>%
    # If a route exists in both Metro's GTFS and another agency's GTFS, drop the records associated with Metro's GTFS.
    # Future considerations to make this an option
    dplyr::filter(
      !(agency_ct == 2 & agency_id %in% c('King County  Metro Transit'))
    ) %>%
    dplyr::select(-c(agency_id, agency_ct))

  stops <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "stops")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::distinct(stop_id, stop_code, .keep_all = T) %>%
    dplyr::filter(!(is.na(stop_lat)))

  stop_times <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
    "stop_times"
  )] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    # Remove trips that were dropped from the route table
    dplyr::filter(trip_id %in% trips$trip_id)

  shapes <- baseline_gtfs[stringr::str_detect(
    names(baseline_gtfs),
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
