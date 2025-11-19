#' Connect to T-BIRD Data Warehouse
#'
#' @returns A connection object
#'
#' @export
#' @examples
#' #con <- connect_to_tbird()
#' # Must be on KCM VPN for this to work
#'
#'
connect_to_tbird <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = "kcitazrsqlprp01.database.windows.net",
    Database = "tbird_dw",
    Authentication = "ActiveDirectoryIntegrated"
  )

  if (DBI::dbIsValid(con) == FALSE) {
    cli::cli_alert_warning(
      text = "Connection to T-BIRD NOT successful. Are you on the VPN?"
    )
  } else {
    cli::cli_alert_success(
      text = "Connection to T-BIRD successful"
    )
    con
  }
}
