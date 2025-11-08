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
#' @param db Database to connect to.
#' @section Environment Variables:
#'
#' When `db` is set to `"Oracle"`, the following environment variables must be defined before using this function:
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
#'   retrieve(500)
#'
#' # dplyr functions are automatically translated into SQL
#' conn |>
#'   glims_tbl("ANTIBIOTICRESULT") |>
#'   filter(ABRS_RISRAWVALUE == 3) |>
#'   show_query() # use collect() or retrieve() instead to download
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
#' conn |>
#'   glims_tbl("ANTIBIOTICRESULT") |>
#'   glims_join_tbl("ANOTHERTABLE", by = "some_column") |>
#'   show_query()
#'
#'
#' # Closing Connection ---------------------------------------------------
#'
#' disconnect_db(conn)
#'
#' }
#'
#'
#' # Other ----------------------------------------------------------------
#'
#' datetime_to_oracle_julian(Sys.Date())
#'
#' Sys.Date()
#' Sys.Date() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime()
#'
#' Sys.time()
#' Sys.time() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime()
#' Sys.time() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime(tz = "UTC")
connect_db <- function(db = "Oracle") {
  if (db == "Oracle") {
    dbConnect(
      odbc::odbc(),
      Driver   = Sys.getenv("MMBI_EPIDS_DRIVER"),
      Host     = Sys.getenv("MMBI_EPIDS_HOST"),
      Port     = Sys.getenv("MMBI_EPIDS_PORT"),
      SVC      = Sys.getenv("MMBI_EPIDS_SVC"),
      UID      = Sys.getenv("MMBI_EPIDS_USER"),
      PWD      = Sys.getenv("MMBI_EPIDS_PASS")
    )
    # } else if (db == "...") {
    #
  } else {
    stop("Database currently not supported: ", db)
  }
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

#' @param qry A `tbl_dbi` object representing the database query.
#' @param by Column name(s) to join by. Can be a named vector to allow different names:\cr  `by = c("col_A" = "col_B")`.
#' @param ...
#'.  * [get_glims_data()]: Arguments passed on the `WHERE` clause in the query. Supports `dplyr` language.
#'   * [glims_join_tbl()]: Arguments passed on the join functions.
#' @param type Direction of the join, defaults to `"left"`.
#' * [`"inner"`][dplyr::inner_join()]: returns matched x rows.
#' * [`"left"`][dplyr::left_join()]: returns all x rows.
#' * [`"right"`][dplyr::right_join()]: returns matched of x rows, followed by unmatched y rows.
#' * [`"full"`][dplyr::full_join()]: returns all x rows, followed by unmatched y rows.
#' @rdname db
#' @importFrom dplyr left_join right_join full_join inner_join
#' @export
glims_join_tbl <- function(qry,
                           table_name,
                           by = NULL,
                           ...,
                           type = "left",
                           schema = "ORAGLIMS") {
  con <- qry[["src"]]$con

  if (interactive()) {
    message("Joining table '", table_name, "'...", appendLF = FALSE)
  }

  table_y <- con |> glims_tbl(table_name, schema)

  if (type == "left") {
    out <- qry |> left_join(table_y, by = by, ...)
  } else if (type == "right") {
    out <- qry |> right_join(table_y, by = by, ...)
  } else if (type == "full") {
    out <- qry |> full_join(table_y, by = by, ...)
  } else if (type == "inner") {
    out <- qry |> inner_join(table_y, by = by, ...)
  } else {
    stop("Only supported joins: left, right, full, inner")
  }

  if (interactive()) {
    message("OK")
  }

  out
}

#' @param n Number of rows to collect.
#' @rdname db
#' @importFrom dplyr collect
#' @export
preview <- function(qry, n = 100) {
  qry |>
    utils::head(n) |>
    collect()
}

#' @rdname db
#' @param x Numeric vector or data.frame, to convert Oracle Julian days to common dates. In case of data frames, only columns with numeric values that have Julian days between 1900-01-01 and 2100-01-01 will be converted.
#' @param tz Target time zone.
#' @export
oracle_julian_to_datetime <- function(x, tz = "Europe/Amsterdam") {
  if (is.list(x)) {
    # list and thus also data.frames
    for (i in seq_along(x)) {
      col <- x[[i]]
      if (is.numeric(col)) {
        # Limit check to first non-NA values
        values <- col[!is.na(col)]
        if (length(values) > 0) {
          sample_vals <- utils::head(values, 1e5)
          # detect likely Julian values, these cover dates between 1900-01-01 and 2100-01-01
          if (min(sample_vals) >= 2415021 && max(sample_vals) <= 2488070) {
            x[[i]] <- oracle_julian_to_datetime(col, tz = tz)
          }
        }
      }
    }
    return(x)
  }

  # actual function
  x <- as.numeric(x)
  if (all(x == floor(x), na.rm = TRUE)) {
    out <- as.Date(x - 2440588, origin = "1970-01-01")
  } else {
    out <- as.POSIXct((x - 2440588) * 60 * 60 * 24, origin = "1970-01-01", tz = tz)
  }
  out
}

#' @rdname db
#' @export
datetime_to_oracle_julian <- function(x) {
  if (inherits(x, "Date")) {
    x_num <- as.numeric(x)
  } else if (inherits(x, "POSIXct")) {
    x_num <- as.numeric(x) / 60 / 60 / 24
  } else {
    stop("Only supports Date and POSIXct")
  }
  x_num + 2440588
}

#' @param qry_type Type of query, see `Query Types` below.
#' @section Query Types:
#' Various query types have been defined:
#'
#' * `"orders"`: 1 row per order, info about request (department, ward, room, etc.)
#' * `"results"`: 1 row per result, can be multiple in an order
#' * `"isolates"`: 1 row per isolate, can be multiple in a result
#'
#' These types can be combined.
#' @rdname db
#' @export
build_query <- function(con, qry_type) {
  message("[", format(Sys.time()), "] Building query...", appendLF = FALSE)

  if ("order" %in% qry_type) {
    out <- con |>
      glims_tbl("REQUEST")
  } else {
    stop("Invalid query type")
  }

  message("OK")
  out
}

#' @param limit Maximum number of rows to return. The end result will probably have fewer results due to post-process data cleaning; the `limit` setting only applies to the original database query.
#' @param convert_julian Logical, whether to convert Oracle Julian date fields to Date or POSIXct.
#' @param convert_logicals Logical, whether to convert binary numeric fields (0/1) to logicals.
#' @param convert_column_names Logical, whether to rename column names using a lookup table from the package.
#' @importFrom dplyr collect mutate case_when select everything
#' @rdname db
#' @export
retrieve <- function(qry,
                     limit = Inf,
                     convert_julian = TRUE,
                     convert_logicals = TRUE,
                     convert_column_names = TRUE) {
  start_the_clock <- Sys.time()

  message("[", format(Sys.time()), "] Running query...", appendLF = FALSE)
  if (is.infinite(limit)) {
    out <- qry |> collect()
  } else {
    out <- qry |> utils::head(limit) |> collect()
  }
  message("OK")

  if (convert_julian == TRUE) {
    message("[", format(Sys.time()), "] Converting Oracle Julian days...", appendLF = FALSE)
    out <- out |> oracle_julian_to_datetime()
    message("OK")
  }

  if (convert_logicals == TRUE) {
    message("[", format(Sys.time()), "] Converting 1/0 to TRUE/FALSE...", appendLF = FALSE)
    for (i in seq_along(out)) {
      col <- out[[i]]
      if (is.numeric(col)) {
        # Limit check to first non-NA values
        values <- col[!is.na(col)]
        if (length(values) > 0) {
          sample_vals <- utils::head(values, 1e5)
          # detect likely Julian values, these cover dates between 1900-01-01 and 2100-01-01
          if (all(sample_vals %in% c(0, 1))) {
            out[[i]] <- as.logical(out[[i]])
          }
        }
      }
    }
    message("OK")
  }

  if (convert_column_names == TRUE) {
    message("[", format(Sys.time()), "] Converting columns names...", appendLF = FALSE)
    f <- system.file("readable_column_names.csv", package = "mmbi.epi")
    new_names <- utils::read.csv(f)
    matched <- match(names(out), new_names$original)
    names(out)[!is.na(matched)] <- new_names$new[matched[!is.na(matched)]]
    message("OK")
  }

  message("\nDone in ", format(round(difftime(Sys.time(), start_the_clock), 2)),
          ", returning ", format(NROW(out), big.mark = ","), " x ", format(NCOL(out), big.mark = ","), " observations")
  out
}

#' @param date_range Date range, can be length 1 or 2 (or more to use the min/max) to filter the `ORD_RECEIPTTIME` column. Supports date/time, date, and years. Use `NULL` to set no date filter.
#' @importFrom dplyr filter between
#' @importFrom dbplyr remote_query
#' @rdname db
#' @export
get_glims_data <- function(date_range,
                           ...,
                           convert_julian = TRUE,
                           convert_logicals = TRUE,
                           convert_column_names = TRUE,
                           limit = Inf,
                           qry_type = c("results", "orders"),
                           db = "Oracle") {
  message("[", format(Sys.time()), "] Connecting to database...", appendLF = FALSE)
  con <- connect_db(db = db)
  on.exit(disconnect_db(con))
  message("OK")

  # format date range
  if (length(date_range) == 1) {
    date_range <- rep(date_range, 2)
  } else if (length(date_range) > 2) {
    date_range <- c(min(date_range, na.rm = TRUE), max(date_range, na.rm = TRUE))
  }
  if (date_range[1] %in% 2010:2050) {
    date_range[1] <- as.Date(date_range[1], "-01-01")
  }
  if (date_range[2] %in% 2010:2050) {
    date_range[2] <- as.Date(date_range[2], "-12-31")
  }
  date_range <- datetime_to_oracle_julian(date_range)

  qry <- build_query(qry_type = qry_type) |>
    filter(ORD_RECEIPTTIME |>
             between(!!(date_range[1]),
                     # include full end date
                     !!((date_range[2] + 1 - 1e-10)))) |>
    filter(...)

  qry_string <- as.character(remote_query(qry))

  out <- qry |>
    retrieve(limit = limit,
             convert_julian = convert_julian,
             convert_logicals = convert_logicals,
             convert_column_names = convert_column_names)

  structure(out,
            class = c("mmbi_database_download", out),
            dims = dim(out),
            db = db,
            db_user = tryCatch(paste0(con@info$username, "@", con@info$servername), error = function(e) NULL),
            user = Sys.info()["user"],
            datetime = Sys.time(),
            qry = qry_string)
}

#' @rdname db
#' @export
original_query <- function(x) {
  if (is.null(attr(x, "qry", exact = TRUE))) {
    stop("No query found in object.", call. = FALSE)
  }
  cat(attributes(x)$qry)
  cat("\n")
  invisible(attributes(x)$qry)
}

#' @importFrom pillar tbl_sum dim_desc
#' @noRd
#' @export
tbl_sum.mmbi_database_download <- function(x, ...) {
  out <- dim_desc(x)
  names(out) <- paste0("A tibble from database '", attributes(x)$db, "'")
  if (identical(attributes(x)$dims, dim(x))) {
    # dimensions are identical to originally downloaded data, so print the details
    if (!is.null(attributes(x)$db_user)) {
      out <- c(out, `Database user` = attributes(x)$db_user)
    }
    if (!is.null(attributes(x)$datetime)) {
      out <- c(out, `Retrieved on` = format(attributes(x)$datetime,
                                            "%e %b %Y %H:%M"))
    }
    if (!is.null(attributes(x)$user)) {
      out <- c(out, `Retrieved by` = attributes(x)$user)
    }
  }
  out
}

#' @importFrom pillar tbl_format_footer style_subtle
#' @noRd
#' @export
tbl_format_footer.mmbi_database_download <- function(x, setup, ...) {
  footer <- NextMethod()

  if (is.null(attributes(x)$qry)) {
    return(footer)
  } else {
    old_dims <- attributes(x)$dims
    if (identical(old_dims, dim(x))) {
      c(footer,
        style_subtle(paste0("# ", cli::symbol$info,
                            " Use `original_query()` to get the query of this tibble")))
    } else {
      c(footer,
        style_subtle(paste0("# ", cli::symbol$info,
                            " Use `original_query()` to get the query of the original ",
                            format_dimensions(old_dims), " tibble")))
    }
  }
}

format_dimensions <- function(dims) {
  dims <- c(dims[1], dims[2])
  dims <- formatC(dims,
                  big.mark = ifelse(identical(getOption("OutDec"), ","),
                                    ".",
                                    ","),
                  format = "d",
                  preserve.width = "individual")
  dims <- trimws(paste0(" ", dims, " ", collapse = cli::symbol$times))
}
