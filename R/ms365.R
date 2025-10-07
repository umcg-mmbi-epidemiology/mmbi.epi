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
#' @param remote_path Character scalar. Path to a file or folder on SharePoint (relative to the group drive root).
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
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve Microsoft 365 group
#' grp <- get_ms365_group()
#'
#' # Access SharePoint drive
#' sp <- get_sharepoint(grp)
#'
#' # Download a file from SharePoint
#' download_sharepoint_file("Projecten/Overig/MS Graph API.R", "my_local_file.R")
#'
#' # Upload a local file to SharePoint
#' upload_sharepoint_file("my_local_file.R", "Projecten/Overig/MS Graph API.R")
#'
#' # Import an RDS file directly from SharePoint
#' df <- import_sharepoint_rds("Projecten/Overig/iris.rds")
#'
#' # Export an R object to SharePoint as an RDS file
#' mtcars |> export_sharepoint_rds("Projecten/Overig/mtcars.rds")
#' }
get_ms365_group <- function(group_name = "MMBI Unit Epidemiologie & Data Science") {
  login <- tryCatch(
    suppressMessages(AzureGraph::get_graph_login("umcgonline", selection = 1, refresh = TRUE)),
    error = function(e) NULL
  )
  if (is.null(login)) {
    login <- AzureGraph::create_graph_login(tenant = "umcgonline")
  }
  login$get_group(name = group_name)
}

#' @rdname ms365
#' @export
get_planner <- function(ms365_group = get_ms365_group(), plan_id = "xHLwHUqf_UKoTxaPT6gh5pYAHsuB") {
  res <- AzureGraph::call_graph_endpoint(ms365_group$token, file.path("planner/plans", plan_id))
  Microsoft365R::ms_plan$new(ms365_group$token, "umcgonline", res)
}

#' @rdname ms365
#' @export
get_sharepoint <- function(ms365_group = get_ms365_group()) {
  Microsoft365R::ms_drive$new(ms365_group$token, "umcgonline", ms365_group$do_operation("drive"))
}

#' @rdname ms365
#' @export
get_teams <- function(ms365_group = get_ms365_group()) {
  res <- AzureGraph::call_graph_endpoint(ms365_group$token, file.path("teams", ms365_group$properties$id))
  Microsoft365R::ms_team$new(ms365_group$token, "umcgonline", res)
}

#' @rdname ms365
#' @export
get_sharepoint_file <- function(remote_path, ms365_group = get_ms365_group()) {
  get_sharepoint(ms365_group)$get_item(remote_path)
}

#' @rdname ms365
#' @export
download_sharepoint_file <- function(remote_path, destination, overwrite = FALSE, ms365_group = get_ms365_group()) {
  # e.g. download_sharepoint_file("Projecten/Overig/MS Graph API.R", "my_local_file.R")
  sharepoint_file <- get_sharepoint_file(remote_path, ms365_group = ms365_group)
  sharepoint_file$download(destination, overwrite = overwrite)
}

#' @rdname ms365
#' @export
upload_sharepoint_file <- function(local_path, remote_path, ms365_group = get_ms365_group()) {
  # e.g. upload_sharepoint_file("my_local_file.R", "Projecten/Overig/MS Graph API.R")
  get_sharepoint(ms365_group)$upload_file(local_path, remote_path)
}

#' @rdname ms365
#' @export
import_sharepoint_rds <- function(remote_path, overwrite = FALSE, ms365_group = get_ms365_group()) {
  # e.g. df <- import_sharepoint_rds("Projecten/Overig/iris.rds")
  if (!grepl("[.]rds$", remote_path, ignore.case = TRUE)) {
    stop("import_sharepoint_rds() only supports RDS files")
  }
  local_file <- file.path(tempdir(), basename(remote_path))
  download_sharepoint_file(remote_path, overwrite = TRUE, destination = local_file, ms365_group = ms365_group)
  df <- readRDS(local_file)
  unlink(local_file)
  df
}

#' @rdname ms365
#' @export
export_sharepoint_rds <- function(object, remote_path, overwrite = FALSE, ms365_group = get_ms365_group()) {
  # e.g. mtcars |> export_sharepoint_rds("Projecten/Overig/mtcars.rds")
  if (!grepl("[.]rds$", remote_path, ignore.case = TRUE)) {
    stop("export_sharepoint_rds() only supports RDS files")
  }
  local_file <- file.path(tempdir(), basename(remote_path))
  saveRDS(object, local_file)
  upload_sharepoint_file(local_file, remote_path, ms365_group = ms365_group)
  unlink(local_file)
}
