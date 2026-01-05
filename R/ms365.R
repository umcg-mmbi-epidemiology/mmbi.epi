# ===================================================================== #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at University Medical Center Groningen (UMCG),             #
#  department of Medical Microbiology & Infection Prevention (MMBI),    #
#  unit Epidemiology & Data Science:                                    #
#  https://github.com/umcg-mmbi-epidemiology                            #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Microsoft 365 Group and SharePoint Utilities
#'
#' @description
#' A collection of helper functions for interacting with Microsoft 365 groups, including
#' Teams, Planner, and SharePoint, via the `AzureGraph` package.
#'
#' These functions simplify retrieval of group information, access to associated
#' resources (Planner, SharePoint, Teams), and file transfer operations
#' between local storage and SharePoint, including RDS import/export utilities.
#'
#' @param group_name Character scalar. Name of the Microsoft 365 group to connect to. Defaults to `"MMBI Unit Epidemiologie & Data Science"`.
#' @param plan_id Identifier of the Planner.
#' @param ms365_group An `az_group` object obtained from `get_ms365_group()`. Defaults to calling `get_ms365_group()` automatically.
#' @param remote_path Character scalar. Path to a file or folder on SharePoint (relative to the group drive root). For `save_sharepoint_analysis_file()` this will at default be taken from the first file line.
#' @param destination Character scalar. Local path where a SharePoint file should be downloaded.
#' @param overwrite Logical. Whether to overwrite an existing local or remote file. Default is `FALSE`.
#' @param local_path Character scalar. Path to a local file to upload to SharePoint.
#' @param object An R object to be saved as an `.rds` file and uploaded to SharePoint.
#'
#' @details
#' These functions use the `AzureGraph` package to authenticate and connect
#' to Microsoft 365 resources within the `umcgonline` tenant. Authentication is
#' attempted using an existing cached login; if unavailable, the user is prompted
#' to log in interactively.
#' @rdname ms365
#'
#' @importFrom AzureGraph get_graph_login create_graph_login
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' # Get from / Save to SharePoint ----------------------------------------
#'
#' # Export data set to remote
#' mtcars |> export_sharepoint_rds("Projecten/Overig/mtcars.rds")
#'
#' # Import data set from remote
#' df <- import_sharepoint_rds("Projecten/Overig/iris.rds")
#'
#' # Open analysis file in RStudio from remote
#' open_sharepoint_analysis_file("Projecten/Overig/my_file.R")
#'
#' # Save current analysis file to remote
#' save_sharepoint_analysis_file()
#' # or press the button in the Addins menu in RStudio
#'
#'
#' # Other Helper Functions -----------------------------------------------
#'
#' # Download a file from SharePoint
#' download_sharepoint_file("Projecten/Overig/my_remote_file.csv", "my_local_file.csv")
#'
#' # Upload a local file to SharePoint
#' upload_sharepoint_file("my_local_file.csv", "Projecten/Overig/my_remote_file.csv")
#'
#' # Access Teams
#' teams <- get_teams()
#' # Send a message
#' teams$get_channel("General")$send_message("This is message from R")
#'
#' # Access Planner
#' planner <- get_planner()
#' planner$list_tasks()
#' }
get_ms365_group <- function(group_name = "MMBI Unit Epidemiologie & Data Science") {
  login <- tryCatch(
    suppressMessages(get_graph_login("umcgonline", selection = 1, refresh = TRUE)),
    error = function(e) NULL
  )
  if (is.null(login)) {
    login <- create_graph_login(tenant = "umcgonline", auth_type = "device_code")
  }
  login$get_group(name = group_name)
}

#' @rdname ms365
#' @importFrom AzureGraph call_graph_endpoint
#' @importFrom Microsoft365R ms_plan
#' @export
get_planner <- function(ms365_group = get_ms365_group(), plan_id = "xHLwHUqf_UKoTxaPT6gh5pYAHsuB") {
  res <- call_graph_endpoint(ms365_group$token, file.path("planner/plans", plan_id))
  ms_plan$new(ms365_group$token, "umcgonline", res)
}

#' @rdname ms365
#' @importFrom Microsoft365R ms_drive
#' @export
get_drive <- function(ms365_group = get_ms365_group()) {
  ms_drive$new(ms365_group$token, "umcgonline", ms365_group$do_operation("drive"))
}

#' @rdname ms365
#' @importFrom Microsoft365R ms_site
#' @export
get_sharepoint_site <- function(ms365_group = get_ms365_group()) {
  ms_site$new(ms365_group$token, "umcgonline", ms365_group$do_operation("sites/root"))
}

#' @rdname ms365
#' @importFrom AzureGraph call_graph_endpoint
#' @importFrom Microsoft365R ms_team
#' @export
get_teams <- function(ms365_group = get_ms365_group()) {
  res <- call_graph_endpoint(ms365_group$token, file.path("teams", ms365_group$properties$id))
  ms_team$new(ms365_group$token, "umcgonline", res)
}

#' @rdname ms365
#' @export
get_drive_file <- function(remote_path, ms365_group = get_ms365_group()) {
  get_drive(ms365_group)$get_item(remote_path)
}

#' @rdname ms365
#' @export
download_sharepoint_file <- function(remote_path, destination, overwrite = FALSE, ms365_group = get_ms365_group()) {
  sharepoint_file <- get_drive_file(remote_path, ms365_group = ms365_group)
  sharepoint_file$download(destination, overwrite = overwrite)
}

#' @rdname ms365
#' @export
upload_sharepoint_file <- function(local_path, remote_path, ms365_group = get_ms365_group()) {
  get_drive(ms365_group)$upload_file(local_path, remote_path)
}

#' @rdname ms365
#' @export
import_sharepoint_rds <- function(remote_path, ms365_group = get_ms365_group()) {
  if (!grepl("[.]rds$", remote_path, ignore.case = TRUE)) {
    stop("import_sharepoint_rds() only supports RDS files")
  }
  local_path <- file.path(tempdir(), basename(remote_path))
  download_sharepoint_file(remote_path, overwrite = TRUE, destination = local_path, ms365_group = ms365_group)
  df <- readRDS(local_path)
  unlink(local_path)
  df
}

#' @rdname ms365
#' @export
export_sharepoint_rds <- function(object, remote_path, ms365_group = get_ms365_group()) {
  if (!grepl("[.]rds$", remote_path, ignore.case = TRUE)) {
    stop("export_sharepoint_rds() only supports RDS files")
  }
  local_path <- file.path(tempdir(), basename(remote_path))
  saveRDS(object, local_path)
  upload_sharepoint_file(local_path, remote_path, ms365_group = ms365_group)
  message("Object exported to remote as ", remote_path)
  unlink(local_path)
}


#' @rdname ms365
#' @export
open_sharepoint_analysis_file <- function(remote_path, ms365_group = get_ms365_group()) {
  if (!grepl("[.](r|rmd|qmd)$", remote_path, ignore.case = TRUE)) {
    stop("open_sharepoint_analysis_file() only supports 'R', 'Rmd', and 'qmd' files")
  }
  local_path <- file.path(tempdir(), basename(remote_path))
  download_sharepoint_file(remote_path, overwrite = TRUE, destination = local_path, ms365_group = ms365_group)
  file_contents <- suppressWarnings(readLines(local_path))
  if (!grepl("# remote: ", file_contents[1])) {
    file_contents <- c(paste0("# remote: ", remote_path), "", file_contents)
    writeLines(file_contents, local_path)
  }
  tryCatch(invisible(rstudioapi::navigateToFile(local_path)),
           error = function(e) message("Could not open file in RStudio. Local file location: ", local_path))
}

#' @rdname ms365
#' @export
save_sharepoint_analysis_file <- function(local_path = get_current_file_path(), remote_path = NULL, ms365_group = get_ms365_group()) {
  message("Saving...")
  if (!is.null(remote_path) && !grepl("[.](r|rmd|qmd)$", remote_path, ignore.case = TRUE)) {
    stop("save_sharepoint_analysis_file() only supports 'R', 'Rmd', and 'qmd' files")
  }
  file_contents <- suppressWarnings(readLines(local_path))
  if (grepl("# remote: ", file_contents[1])) {
    remote_path <- trimws(gsub("# remote: ", "", file_contents[1]))
  }
  if (!is.null(names(local_path))) {
    rstudioapi::documentSave(id = names(local_path))
  }
  upload_sharepoint_file(local_path, remote_path, ms365_group = ms365_group)
  message("File saved to remote as '", remote_path, "'")
}

#' @rdname ms365
#' @export
get_current_file_path <- function() {
  if (Sys.getenv("RSTUDIO") != 1) {
    stop("This function only works in RStudio.", call. = FALSE)
  }
  current_file <- rstudioapi::getSourceEditorContext()
  stats::setNames(tools::file_path_as_absolute(current_file$path), current_file$id)
}
