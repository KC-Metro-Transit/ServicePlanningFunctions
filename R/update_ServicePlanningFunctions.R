#' Install/Update ServicePlanningFunctions Package
#'
#' @returns updates ServicePlanningFunctions package. Returns error if not successful.
#'
#' @export

update_ServicePlanningFunctions <- function() {
  if (curl::has_internet()) {
    # Check latest commit of ServicePlanningFunctions
    latest_commit <- remotes::remote_sha(remotes::github_remote(
      "KC-Metro-Transit/ServicePlanningFunctions"
    ))

    # Install if ServicePlanningFunctions is not yet installed
    # or Update ServicePlanningFunctions if the installed version is not the latest
    if (!('ServicePlanningFunctions' %in% installed.packages()[, "Package"])) {
      devtools::install_github(
        "KC-Metro-Transit/ServicePlanningFunctions",
        upgrade = "never"
      )

      cli::cli_alert_success(
        text = "Installing ServicePlanningFunctions for the first time."
      )
    } else if (
      utils::packageDescription("ServicePlanningFunctions")$RemoteSha !=
        latest_commit
    ) {
      # Package needs to be detached before uninstalling
      if ("ServicePlanningFunctions" %in% .packages()) {
        detach("package:ServicePlanningFunctions", unload = TRUE)
      }

      # Uninstall and reinstall the package (to ensure the latest version is used)
      remove.packages("ServicePlanningFunctions")
      devtools::install_github(
        "KC-Metro-Transit/ServicePlanningFunctions",
        upgrade = "never"
      )

      cli::cli_alert_success(
        text = "New version of ServicePlanningFunctions is available. Updating to latest version."
      )
    } else {
      cli::cli_alert_success(
        text = "Already using latest version of ServicePlanningFunctions"
      )
    }
  } else {
    cli::cli_alert_warning(
      text = "Update unsuccessful. Need internet connection to update."
    )
  }
}
