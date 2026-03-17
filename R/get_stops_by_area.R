get_stops_by_area <- function(
  area,
  gtfs_date,
  service_change_num,
  tbird_connection,
  return_type
) {
  #get gtfs data that best matches gtfs_date from tbird

  stops <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT stop_id, 
      stop_lat, 
      stop_lon, 
      capture_date
      from gtfs.stops
      where capture_date = (SELECT top 1 capture_date
      from gtfs.stops
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  ) |>
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
    sf::st_transform(2926)

  stop_times <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT trip_id, 
      arrival_time, 
      departure_time, 
      stop_id,
      capture_date
      from gtfs.stop_times
      where capture_date = (SELECT top 1 capture_date
      from gtfs.stop_times
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  )
  trips <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT trip_id, 
      route_id, 
      service_id, 
      block_id,
      shape_id,
      capture_date
      from gtfs.trips
      where capture_date = (SELECT top 1 capture_date
      from gtfs.trips
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  )
  routes <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT *
      from gtfs.routes
      where capture_date = (SELECT top 1 capture_date
      from gtfs.routes
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  )

  shapes <- DBI::dbGetQuery(
    tbird_connection,
    glue::glue_sql(
      "SELECT *
      from gtfs.shapes
      where capture_date = (SELECT top 1 capture_date
      from gtfs.shapes
      where capture_date <= cast({(vals1)} as date)
      order by capture_date desc)",
      vals1 = gtfs_date,
      .con = tbird_connection
    )
  )

  #get area boundary from raw_data folder
  #filter to polygon(s) identified

  geography <- sf::read_sf(here::here('data_raw', 'SASR_LocusZones.shp')) |>
    dplyr::filter(NAME %in% area) |>
    sf::st_transform(2926)

  #id stops & routes within boundary

  filtered_stops <- sf::st_filter(
    x = stops,
    y = geography,
    join = sf::st_intersects
  )

  stop_ids <- as.vector(filtered_stops$stop_id)

  #use get_stop_ridership to get boardings/alightings/totals by stop

  stop_ridership <- ServicePlanningFunctions::get_stop_ridership(
    service_change_num = service_change_num,
    stop_id = stop_ids,
    tbird_connection = tbird_connection
  )

  #return map of rides by stop if asked for

  #return table of rides by stop if asked for
}
