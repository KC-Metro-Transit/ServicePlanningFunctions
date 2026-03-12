#' Clean Service Route Numbers
#'
#' @param route_table The dataframe representation of routes.txt from your GTFS file
#' @param netplan_gtfs T/F Is this GTFS from NetPlan? This impacts how route names are stored and generated
#'
#' @return A route dataframe with a field service_rte_num that representing the route number of Metro routes
#' @export
#'
#' @examples
#' spring_24_gtfs <- tidytransit::read_gtfs(path = fs::path_package(
#' "extdata", "gtfs",
#' "241_gtfs.zip", package = "ServicePlanningFunctions"))
#'
#' spring_24_routes <- clean_service_rte_num(spring_24_gtfs$routes,
#' netplan_gtfs = FALSE)
#'
#' netplan_gtfs <- tidytransit::read_gtfs(path = fs::path_package(
#' "extdata", "gtfs",
#' "SEPT24_TRIP_GTFS.zip", package = "ServicePlanningFunctions"))
#'
#' netplan_routes <- clean_service_rte_num(spring_24_gtfs$routes,
#'  netplan_gtfs = TRUE)

clean_service_rte_num <- function(route_table, netplan_gtfs = FALSE) {
  if (netplan_gtfs == TRUE) {
    routes <- route_table %>%
      # tidyr::separate(route_id, into = c("route_id", "schedule"), sep = "-",  extra = "merge") %>%
      tidyr::unite(
        service_rte_num,
        route_short_name,
        route_long_name,
        remove = FALSE,
        sep = " "
      ) %>% #deal with cases where names get split in two fields
      #dplyr::select(-schedule) %>%
      dplyr::relocate(service_rte_num, .after = route_text_color) %>%
      dplyr::relocate(agency_id, .before = route_short_name)
  } else if (netplan_gtfs == FALSE) {
    routes <- route_table %>% #create blank field for non-NetPlan GTFS
      dplyr::mutate(service_rte_num = NA)
  }

  routes_na <- routes %>%
    dplyr::mutate(
      service_rte_num = dplyr::case_when(
        route_short_name == "A" ~ "671",
        route_short_name == "B" ~ "672",
        route_short_name == "C" ~ "673",
        route_short_name == "D" ~ "674",
        route_short_name == "E" ~ "675",
        route_short_name == "F" ~ "676",
        route_short_name == "G" ~ "677",
        route_short_name == "H" ~ "678",
        route_short_name == "A Line" ~ "671",
        route_short_name == "B Line" ~ "672",
        route_short_name == "C Line" ~ "673",
        route_short_name == "D Line" ~ "674",
        route_short_name == "E Line" ~ "675",
        route_short_name == "F Line" ~ "676",
        route_short_name == "G Line" ~ "677",
        route_short_name == "H Line" ~ "678",
        route_short_name == "Link light rail" ~ "599",
        route_short_name == "First Hill Streetcar" ~ "96",
        route_short_name == "South Lake Union Streetcar" ~ "98",
        route_short_name == "SVT" ~ "629",
        route_short_name == "Valley Shuttle" ~ "629",
        route_short_name == "Duvall-Monroe Shuttle" ~ "627",
        route_short_name == "Trailhead Direct Mt. Si" ~ "636",
        route_short_name == "Trailhead Direct Mailbox Peak" ~ "637",
        route_short_name == "Trailhead Direct Issaquah Alps" ~ "634",
        route_short_name == "Trailhead Direct Cougar Mt." ~ "640",
        route_short_name == "Blue" ~ "999",
        route_short_name == "Swift Blue" ~ "701",
        route_short_name == "Swift Green" ~ "702",
        route_short_name == "Swift Orange" ~ "703",
        route_short_name == "STCL" ~ "999",
        .default = dplyr::coalesce(route_short_name, route_long_name)
      )
    )

  clean_routes <- routes_na %>%
    dplyr::mutate(
      service_rte_num = stringr::str_remove(service_rte_num, "[A-z]+")
    ) %>%
    dplyr::distinct(agency_id, service_rte_num, .keep_all = TRUE)

  na_routes <- clean_routes %>%
    dplyr::mutate(service_rte_num = as.numeric(service_rte_num)) %>%
    dplyr::filter(is.na(service_rte_num) | is.null(service_rte_num))

  if (nrow(na_routes) > 0) {
    cli::cli_alert_warning(
      paste0(
        nrow(na_routes),
        " route names need to be fixed. Please provide valid service route numbers for the following:"
      )
    )
    for (i in 1:nrow(na_routes)) {
      # Prompt user to input invalid service route numbers
      repeat {
        temp_service_rte_num <- readline(
          prompt = paste0(
            "Enter service route number for ",
            dplyr::coalesce(
              na_routes$route_short_name[i],
              na_routes$route_long_name[i]
            ),
            " (or type 'q' to quit): "
          )
        )

        # Check if user input for service route number already exists and only accept unique values
        # Exit if user enters quit command
        if (temp_service_rte_num == 'q') {
          break
        } else if (!(temp_service_rte_num %in% clean_routes$service_rte_num)) {
          na_routes$service_rte_num[i] <- temp_service_rte_num
          break
        } else {
          cli::cli_alert_warning(paste(
            "Service route number",
            temp_service_rte_num,
            "already exists. Please enter a unique value."
          ))
        }
      }

      if (temp_service_rte_num == 'q') break
    }

    # Append user updated route records back to clean_routes
    clean_routes <- filter(
      clean_routes,
      !is.na(as.numeric(service_rte_num)) &
        !is.null(as.numeric(service_rte_num))
    ) %>%
      dplyr::bind_rows(na_routes)
  } else {
    cli::cli_inform("All routes have valid service route numbers.")
    cli::cli_text(paste(clean_routes$service_rte_num, collapse = " "))
  }
  return(clean_routes)
}
