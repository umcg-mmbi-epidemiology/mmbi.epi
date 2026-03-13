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

# COPIED FROM:
# https://github.com/AMRZNN/dashboard_data/blob/9c7b65857f5467713fc3e3b01a5c7d4f1144b961/upload_script.R

#' Upload a File to the AMRZNN Dashboard Data Repository
#'
#' Uploads a local file to the [AMRZNN dashboard data repository](https://github.com/AMRZNN/dashboard_data) via the GitHub Contents API. Creates the file if it does not yet exist, or overwrites it if it does. No local git installation or repository clone is required.
#' @param local_file `character` - Path to the local file to upload.
#' @param repo_path `character` - Target path within the repository, e.g. `"umcg/weekly_resistance.csv"`. Forward slashes only.
#' @param pat `character` - A GitHub Personal Access Token (PAT) with `Contents: Write` permission on the `AMRZNN/dashboard_data` repository. Defaults to the token returned by [gh::gh_token()].
#' @param commit_message `character` - Commit message to use. Defaults to `"Automated upload: <Sys.Date()>"`.
#' @param repo `character` - Target repository in `owner/repo` format. Defaults to `"AMRZNN/dashboard_data"` and should not normally be changed.
#' @param verbose `logical` - If `TRUE`, prints a confirmation message on success. Defaults to `TRUE`.
#' @return Invisibly returns the HTTP status code of the `PUT` response (`200L` for update, `201L` for new file).
#' @importFrom httr GET PUT add_headers content status_code content_type_json
#' @importFrom jsonlite base64_enc
#' @export
#' @examples
#' \dontrun{
#' amrznn_upload(
#'   local_file = "output/weekly_resistance.csv",
#'   repo_path  = "umcg/weekly_resistance.csv"
#' )
#' }
amrznn_upload <- function(local_file,
                          repo_path,
                          pat = gh::gh_token(),
                          commit_message = paste("Automated upload:", Sys.Date()),
                          repo = "AMRZNN/dashboard_data",
                          verbose = TRUE) {

  if (!file.exists(local_file)) {
    stop("File not found: ", local_file, call. = FALSE)
  }

  if (nchar(pat) == 0L) {
    stop("No PAT provided and gh::gh_token() returned an empty string. ",
         "Supply `pat` directly or configure a token via gitcreds or GITHUB_PAT.",
         call. = FALSE)
  }

  base_url <- paste0("https://api.github.com/repos/", repo, "/contents/", repo_path)

  headers <- add_headers(Authorization = paste("Bearer", pat),
                         Accept = "application/vnd.github+json")

  # Retrieve current sha if the file already exists (required by the API to overwrite)
  sha <- tryCatch({
    response <- GET(base_url, headers)
    content(response, as = "parsed")$sha
  }, error = function(e) NULL)

  encoded <- base64_enc(readBin(local_file, what = "raw", n = file.size(local_file)))

  body <- list(
    message = commit_message,
    content = encoded
  )
  if (!is.null(sha)) {
    body$sha <- sha
  }

  response <- PUT(
    url = base_url,
    config = headers,
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json())

  status <- status_code(response)

  if (verbose) {
    action <- if (status == 201L) "created" else "updated"
    message("File ", action, " successfully: ", repo_path, " (HTTP ", status, ")")
  }

  invisible(status)
}
