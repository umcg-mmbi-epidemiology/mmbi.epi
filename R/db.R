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

#' GLIMS Database Access
#'
#' Establishes a connection to the GLIMS Oracle database using environment variables for all configuration parameters. This function is designed for use in secure environments such as Posit Workbench, where credentials and driver paths are stored outside the source code.
#' @section Environment Variables:
#'
#' The following environment variables must be defined before using this function:
#'
#' - **`MMBI_EPIDS_DRIVER`** - Full path to, or name of, the Oracle ODBC driver shared library
#' - **`MMBI_EPIDS_HOST`** - Hostname or IP address of the Oracle database server
#' - **`MMBI_EPIDS_PORT`** - Port number for the Oracle service (typically `1521`)
#' - **`MMBI_EPIDS_SVC`** - Oracle Service Name (SVC) or SID identifying the database instance
#' - **`MMBI_EPIDS_USER`** - Oracle database username
#' - **`MMBI_EPIDS_PASS`** - Oracle database password
#'
#' The environment variables are read at runtime using `Sys.getenv()` and passed to [DBI::dbConnect()]. This approach ensures credentials are never exposed in the source code or logs.
#' @return
#' Returns a live `DBIConnection` object that can be used with `DBI`, `dplyr`, or `dbplyr` for querying and manipulating data.
#' @seealso [DBI::dbConnect()], [odbc::odbc()]
#' @importFrom odbc dbConnect odbc
#' @rdname db
#' @export
#' @examples
#' \dontrun{
#'
#' # Opening Connection ---------------------------------------------------
#'
#' # open connection and save as object
#' conn <- connect_db()
#'
#' # connect to a table
#' conn |>
#'   glims_tbl("ANTIBIOTICRESULT")
#'
#'
#' # Running Queries ------------------------------------------------------
#'
#' library(dplyr)
#'
#' # download first 500 rows
#' first_500 <- conn |>
#'   glims_tbl("ANTIBIOTICRESULT") |>
#'   head(500) |>
#'   collect()
#'
#' # dplyr functions are automatically translated into SQL
#' conn |>
#'   glims_tbl("ANTIBIOTICRESULT") |>
#'   filter(ABRS_RISRAWVALUE == 3) |>
#'   show_query() # use collect() instead to download
#'
#' # more advanced queries can still run server-side
#' my_count <- conn |>
#'   glims_tbl("ANTIBIOTICRESULT") |>
#'   filter(ABRS_RISRAWVALUE == 3) |>
#'   count(ABRS_RISREPORTVALUECHANGED) |>
#'   show_query() |> # first show query
#'   collect()       # then immediately run and download data
#'
#' # join to other columns server-side
#' joined_tbl <- conn |>
#'   glims_tbl("ANTIBIOTICRESULT") |>
#'   glims_join_tbl("ANOTHERTABLE", by = "some_column") |>
#'   show_query()
#'
#'
#' # Closing Connection ---------------------------------------------------
#'
#' disconnect_db(conn)
#'
#'
#' }
connect_db <- function() {
  dbConnect(
    odbc::odbc(),
    Driver   = Sys.getenv("MMBI_EPIDS_DRIVER"),
    Host     = Sys.getenv("MMBI_EPIDS_HOST"),
    Port     = Sys.getenv("MMBI_EPIDS_PORT"),
    SVC      = Sys.getenv("MMBI_EPIDS_SVC"),
    UID      = Sys.getenv("MMBI_EPIDS_USER"),
    PWD      = Sys.getenv("MMBI_EPIDS_PASS"))
}

#' @rdname db
#' @importFrom odbc dbDisconnect
#' @export
disconnect_db <- function(con) {
  dbDisconnect(con)
}

#' @param con Connection object.
#' @param table_name Name of the table to query.
#' @param schema Database schema; the upper structure name of `table_name`.
#' @rdname db
#' @importFrom dplyr tbl
#' @export
glims_tbl <- function(con, table_name, schema = "ORAGLIMS") {
  con |> tbl(I(paste0(schema, ".", table_name)))
}

#' @param .data SQL data object.
#' @param by Column name(s) to join by. Can be a named vector to allow different names:\cr  `by = c("col_A" = "col_B")`.
#' @param ... Arguments passed on the join functions.
#' @param cols_to_keep Columns to keep of the joined column. Columns used in `by` will always be included. Supports [tidyselect language][tidyselect::starts_with()].
#' @param type Direction of the join, defaults to `"left"`.
#' * [`"inner"`][dplyr::inner_join()]: returns matched x rows.
#' * [`"left"`][dplyr::left_join()]: returns all x rows.
#' * [`"right"`][dplyr::right_join()]: returns matched of x rows, followed by unmatched y rows.
#' * [`"full"`][dplyr::full_join()]: returns all x rows, followed by unmatched y rows.
#' @rdname db
#' @importFrom dplyr left_join right_join full_join inner_join select any_of collect
#' @export
glims_join_tbl <- function(.data,
                           table_name,
                           by = NULL,
                           ...,
                           cols_to_keep = NULL,
                           type = "left",
                           schema = "ORAGLIMS") {
  con <- .data[["src"]]$con

  if (interactive()) {
    message("Joining table '", table_name, "'...", appendLF = FALSE)
  }

  table_y <- con |> glims_tbl(table_name, schema)

  if (tryCatch(!is.null(cols_to_keep), error = function(e) TRUE)) {
    keep_cols <- table_y |> utils::head(0) |> collect() |> select({{ cols_to_keep }}) |> colnames()
    table_y <- table_y |> select(any_of(c(names(by), unname(by), keep_cols)))
  }

  if (type == "left") {
    out <- .data |> left_join(table_y, by = by, ...)
  } else if (type == "right") {
    out <- .data |> right_join(table_y, by = by, ...)
  } else if (type == "full") {
    out <- .data |> full_join(table_y, by = by, ...)
  } else if (type == "inner") {
    out <- .data |> inner_join(table_y, by = by, ...)
  } else {
    stop("Only supported joins: left, right, full, inner")
  }

  if (interactive()) {
    message("OK")
  }

  out
}


# get_glims_results <- function(con, ...) {
#   con |>
#
# }
