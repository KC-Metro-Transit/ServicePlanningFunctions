#' Clean Service Route Names
#'
#' @param The dataframe that contains Service Route Numbers to be converted to Service Route Names
#' @param route_col
#'
#' @returns
#'
#' @export
#' @examples
clean_service_rte_name <- function(df, route_col) {
  route_col = enquo(route_col)
  dplyr::mutate(
    df,
    clean_route = case_match(
      !!route_col,
      '671' ~ 'A Line',
      '672' ~ 'B Line',
      '673' ~ 'C Line',
      '674' ~ 'D Line',
      '675' ~ 'E Line',
      '676' ~ 'F Line',
      '677' ~ 'G Line',
      '678' ~ 'H Line',
      '96' ~ 'First Hill Streetcar',
      '98' ~ 'South Lake Union Streetcar',
      '629' ~ 'SVT',
      '628' ~ 'Duvall-Monroe Shuttle',
      '634' ~ 'Trailhead Direct Mt. Si',
      '636' ~ 'Trailhead Direct Mailbox Peak',
      '637' ~ 'Trailhead Direct Issaquah Alps',
      '639' ~ 'Trailhead Direct Cougar Mt.',
      '701' ~ 'Swift Blue',
      '702' ~ 'Swift Green',
      '703' ~ 'Swift Orange',
      .default = !!route_col
    )
  )
}
