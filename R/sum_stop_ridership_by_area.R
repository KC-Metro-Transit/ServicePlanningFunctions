#' Summarize Stop Ridership by Area
#'
#' @description This function returns either an interactive map of summarized weekday stop-level ridership for the selected area or a dataframe showing ridership data for stops in the selected area.
#'
#' @param area Character. The name of the LOCUS or KC Council district of interest. Can accept multiples. Not required for block group or tract options.
#' @param gtfs_date 'YYYY-MM-DD' Date of GTFS file to use for stop locations. Defaults to nearest dataset released before or on specified date.
#' @param tbird_connection The connection object created by connect_to_tbird()
#' @param return_type Character. Specify what you want to see. Options are "table" and "interactive_map".
#' @param service_change_num Numeric. The three-digit identifier of the service change.
#'  Can accept multiple values as a vector if you are returning a table. For maps, select one service change at a time.
#' @param route Numeric.  The route identifiers of interest. Optional.
#' @param time_period Character. AM, PM, MID, XEV. XNT.
#' @param activity_type Character. ons - Average Daily Boarding, offs - Average Daily Alightings, avg_lod - Average Max Load.
#' @param data_source Character. Specify which areas to show. Options are 'block_group', 'tract', 'LOCUS' or 'King County Council Districts'
#' @returns Interactive map object or dataframe of stop ridership summed by selected periods and geometries.
#'
#' @export
#'
sum_stop_ridership_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  route = "All",
  tbird_connection,
  return_type = 'interactive_map',
  time_period = c("AM", "PM", "MID", "XEV", "XNT"),
  activity_type = 'ons',
  data_source = 'LOCUS'
) {
  stop_data <- get_stop_ridership_by_area(
    area = area,
    gtfs_date = gtfs_date,
    service_change_num = service_change_num,
    route = route,
    tbird_connection = tbird_connection,
    return_type = "table",
    time_period = time_period,
    activity_type = activity_type,
    data_source = data_source
  )

  summed_stop_ridership <- stop_data |>
    dplyr::group_by(service, service_change_num, name, variable) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(sum_value = sum(value, na.rm = T)) |>
    tidyr::drop_na(sum_value)

  if (return_type == "table") {
    summed_stop_ridership
  } else if (return_type == "interactive_map") {
    if (length(service_change_num) != 1) {
      cli::cli_abort(message = "Choose only 1 service change for each map.")
    } else {
      map_type_label <- stringr::str_flatten_comma(unique(
        summed_stop_ridership$variable
      ))

      time_period_data <- factor(
        unique(time_period),
        levels = c("AM Peak", "Midday", "PM Peak", "Evening", "Night"),
        labels = c("AM Peak", "Midday", "PM Peak", "Evening", "Night"),
        ordered = TRUE
      )

      time_period_label <- stringr::str_flatten_comma(sort(time_period_data))
    }

    if (data_source == "LOCUS") {
      geography <- sf::read_sf(fs::path_package(
        'extdata',
        'SASR_LocusZones.shp',
        package = "ServicePlanningFunctions"
      )) |>
        janitor::clean_names() |>
        dplyr::filter(name %in% area) |>
        sf::st_transform(crs = 4326)
    } else if (data_source == "King County Council Districts") {
      geography <- sf::read_sf(fs::path_package(
        'extdata',
        'king_county_council_districts.shp',
        package = "ServicePlanningFunctions"
      )) |>
        dplyr::rename(name = area) |>
        dplyr::filter(name %in% area) |>
        sf::st_transform(4326)
    } else if (data_source == "block_group") {
      geography <- sf::read_sf(fs::path_package(
        'extdata',
        'blkgrp20_shore.shp',
        package = "ServicePlanningFunctions"
      )) |>
        dplyr::rename(name = GEO_ID_GRP) |>
        sf::st_transform(4326)
    } else if (data_source == "tract") {
      geography <- sf::read_sf(fs::path_package(
        'extdata',
        '2020_Census_Tracts_for_King_County___tracts20_area.shp',
        package = "ServicePlanningFunctions"
      )) |>
        dplyr::rename(name = GEO_ID_TRT) |>
        sf::st_transform(4326)
    } else {
      cli::cli_abort(
        message = "Incorrect data_source parameter. Options are 'block_group', 'LOCUS' or 'King County Council Districts'."
      )
    }

    plot_data <- geography |>
      dplyr::left_join(summed_stop_ridership, by = "name") |>
      tidyr::drop_na(sum_value)

    pal <- leaflet::colorBin(
      palette = c(
        "#FDB71A",
        "#FF7B21",
        "#31859F",
        "#006633",
        "#390854"
      ),
      domain = plot_data$sum_value,
      reverse = FALSE,
      pretty = FALSE,
      bins = 5
    )

    ons_map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = "Stadia.AlidadeSmooth") %>%
      leaflet::addPolygons(
        data = plot_data,
        fillColor = ~ pal(sum_value),
        fillOpacity = 0.8,
        color = "black",
        stroke = TRUE,
        weight = 1,
        dashArray = c("6"),
        popup = paste(
          "<b>",
          plot_data$name,
          "<br>",
          plot_data$service,
          "</b>",
          "<br>",
          "Sum:",
          plot_data$sum_value
        )
      ) |>

      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = plot_data$sum_value,
        opacity = .75,
        title = paste(
          map_type_label,
          "<br>",
          time_period_label,
          "<br>"
        )
      )

    ons_map
  } else {
    cli::cli_alert_info(
      text = "Incorrect return_type parameter. Options are 'interactive_map' or 'table'."
    )
  }
}
