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
    with_cli_status(
      time = FALSE,
      msg = paste("Connecting to", db, "database"),
      expr = {
        conn <- dbConnect(
          odbc::odbc(),
          Driver   = Sys.getenv("MMBI_EPIDS_DRIVER"),
          Host     = Sys.getenv("MMBI_EPIDS_HOST"),
          Port     = Sys.getenv("MMBI_EPIDS_PORT"),
          SVC      = Sys.getenv("MMBI_EPIDS_SVC"),
          UID      = Sys.getenv("MMBI_EPIDS_USER"),
          PWD      = Sys.getenv("MMBI_EPIDS_PASS")
        )
      }
    )
    # } else if (db == "...") {
    #
  } else {
    stop("Database currently not supported: ", db)
  }
  conn
}

#' @rdname db
#' @importFrom odbc dbDisconnect dbGetInfo
#' @export
disconnect_db <- function(con) {
  con_name <- tryCatch(dbGetInfo(con)$dbms.name, error = function(e) NULL)
  with_cli_status(
    time = FALSE,
    msg = sprintf("Disconnecting from%s database", if (!is.null(con_name)) paste0(" ", con_name) else ""),
    expr = DBI::dbDisconnect(con)
  )
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

#' @param date_range Date range, can be length 1 or 2 (or more to use the min/max) to filter the `ORD_RECEIPTTIME` column. Supports date/time, date, and years. Use `NULL` to set no date filter.
#' @importFrom dplyr filter between
#' @rdname db
#' @export
get_glims_data <- function(date_range = NULL,
                           ...,
                           convert_julian = TRUE,
                           convert_logicals = TRUE,
                           convert_column_names = TRUE,
                           limit = Inf,
                           qry_type = c("results", "orders"),
                           db = "Oracle",
                           only_include_labs = "Medische Microbiologie") {
  con <- connect_db(db = db)
  on.exit(disconnect_db(con))

  qry <- con |> build_query(..., qry_type = qry_type, only_include_labs = only_include_labs)

  if (!is.null(date_range)) {
    # format date range
    if (length(date_range) == 1) {
      date_range <- rep(date_range, 2)
    } else if (length(date_range) > 2) {
      date_range <- c(min(date_range, na.rm = TRUE), max(date_range, na.rm = TRUE))
    }
    if (date_range[1] %in% 2010:2050) {
      date_range[1] <- as.character(as.Date(paste0(date_range[1], "-01-01")))
    }
    if (date_range[2] %in% 2010:2050) {
      date_range[2] <- as.character(as.Date(paste0(date_range[2], "-12-31")))
    }
    date_range <- datetime_to_oracle_julian(as.Date(date_range))
    date_range[2] <- date_range[2] + 1 - 1e-10 # include full end date
    qry <- qry |>
      filter(ORD_RECEIPTTIME |>
               between(!!(date_range[1]),
                       !!(date_range[2])))
  }

  out <- qry |>
    retrieve(limit = limit,
             convert_julian = convert_julian,
             convert_logicals = convert_logicals,
             convert_column_names = convert_column_names)
}


#' @section Picking Columns with `db$`:
#' Always use the `db` object (a [list]) to pick database columns for filtering. It contains all GLIMS column names. They must **always** be preceded by the injection operator [`!!`][rlang::injection-operator] (pronounced "bang-bang"). For example:
#'
#' ```r
#' get_glims_data(2025, !!db$specimen_name == "Sputum")   # writing `db$` will bring up all columns
#'
#' get_glims_data(2025, ORD_ID == 461098)                 # Does not work
#' get_glims_data(2025, !!str2lang("ORD_ID") == 461098)   # Works
#' get_glims_data(2025, !!db$ORDER_.ORD_ID == 461098)     # Works and easier to write
#'
#' con |> build_query(!!db$specimen_name == "Sputum") |> retrieve()
#' ```
#' @rdname db
#' @export
db <- readRDS(system.file("db.rds", package = "mmbi.epi"))


#' @param qry A `tbl_dbi` object representing the database query.
#' @param by Column name(s) to join by. Can be a named vector to allow different names:\cr  `by = c("col_A" = "col_B")`.
#' @param ...
#'   * [glims_join_tbl()]: Arguments passed on the join functions.
#'.  * Otherwise: Arguments passed on the `WHERE` clause in the query. Supports `dplyr` language.
#' @param type Direction of the join, defaults to `"left"`.
#' * [`"inner"`][dplyr::inner_join()]: returns matched x rows.
#' * [`"left"`][dplyr::left_join()]: returns all x rows.
#' * [`"right"`][dplyr::right_join()]: returns matched of x rows, followed by unmatched y rows.
#' * [`"full"`][dplyr::full_join()]: returns all x rows, followed by unmatched y rows.
#' @rdname db
#' @importFrom dbplyr remote_con
#' @importFrom dplyr left_join right_join full_join inner_join
#' @export
glims_join_tbl <- function(qry,
                           table_name,
                           by = NULL,
                           ...,
                           type = "left",
                           schema = "ORAGLIMS") {
  con <- remote_con(qry)

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

  out
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
#' @param only_include_labs Laboratories to include, defaults to only `"Medische Microbiologie"`. This sets a `WHERE` on `DEPARTMENT.DEPTNAME`.
#' @section Query Types:
#' Various query types have been defined:
#'
#' * `"orders"` (always included): 1 row per order (which can contain multiple specimens): info about request (department, ward, room, etc.)
#'   * `"stays"`: 1 row per patient movement with admissions dates: info about every ward admission and room, can be multiple in an order
#'   * `"results"`: 1 row per result, can be multiple in an order
#'     * `"isolates"`: 1 row per isolate, can be multiple in a result
#'       * `"cultures"`: 1 row per carrier (such as an agar plate), can be multiple of an isolate
#'     * `"microscopy"`: 1 row per microscopy result, can be multiple in a result
#'
#' These types can be combined.
#' @importFrom dplyr select filter everything any_of
#' @rdname db
#' @export
build_query <- function(con, ..., qry_type = "results", only_include_labs = "Medische Microbiologie") {
  with_cli_status(
    msg = "Building query",
    expr = {

      # supported_qry_types <- c("orders", "stays", "results", "isolates", "cultures", "microscopy")
      supported_qry_types <- c("orders", "stays", "results")
      if (!all(qry_type %in% supported_qry_types)) {
        stop("Unspported query type, allowed are: ", toString(supported_qry_types), call. = FALSE)
      }

      # JOIN ----
      if ("results" %in% qry_type) {
        # Start from RESULT, and join REQUEST and ORDER_ from there
        full_qry <- con |>
          glims_tbl("RESULT") |>
          glims_join_tbl("REQUEST",   by = c("RSLT_ORDER"     = "RQST_ORDER")) |>
          glims_join_tbl("ORDER_",    by = c("RSLT_ORDER"     = "ORD_ID"))
      } else {
        full_qry <- con |>
          # Start from REQUEST, the ERD shows REQUEST is the hub that ties to SPECIMEN and ORDER_
          # NOTE! Starting from ORDER_ will also be very slow
          glims_tbl("REQUEST") |>
          glims_join_tbl("ORDER_",    by = c("RQST_ORDER"      = "ORD_ID"))
      }

      # always add these
      full_qry <- full_qry |>
        glims_join_tbl("ENCOUNTER",   by = c("ORD_ENCOUNTER"   = "ENCT_ID")) |>
        glims_join_tbl("DEPARTMENT",  by = c("ORD_DEPARTMENT"  = "DEPT_ID")) |>
        glims_join_tbl("PERSON",      by = c("ENCT_PERSON"     = "PRSN_ID")) |>
        glims_join_tbl("SPECIMEN",    by = c("RQST_SPECIMEN"   = "SPMN_ID")) |>
        glims_join_tbl("MATERIAL",    by = c("SPMN_MATERIAL"   = "MAT_ID")) |>
        glims_join_tbl("STAY",        by = c("ORD_ENCOUNTER"   = "STAY_ENCOUNTER")) |>
        glims_join_tbl("WARD",        by = c("STAY_WARD"       = "WARD_ID")) |>
        glims_join_tbl("SPECIALISM",  by = c("STAY_SPECIALISM" = "SPEC_ID")) |>
        glims_join_tbl("HCPROVIDER",  by = c("ENCT_PHYSICIAN"  = "HCPR_ID"))

      if ("results" %in% qry_type) {
        full_qry <- full_qry |>
          glims_join_tbl("RESULTOUTPUT",    by = c("RSLT_ID"               = "RSTO_RESULT")) |>
          glims_join_tbl("SC_USER",         by = c("RSLT_CONFIRMATIONUSER" = "USR_ID")) |>
          glims_join_tbl("SC_USER",         by = c("RSLT_VALIDATIONUSER"   = "USR_ID"), suffix = c("_CONFIRMATION", "_VALIDATION")) |>
          glims_join_tbl("PROPERTYOUTPUT",  by = c("RSTO_PROPERTYOUTPUT"   = "PRPO_ID")) |>
          glims_join_tbl("PROCEDUREOUTPUT", by = c("PRPO_PROCEDUREOUTPUT"  = "PRCO_ID")) |>
          glims_join_tbl("PROPERTY",        by = c("RSLT_PROPERTY"         = "PROP_ID")) |>
          glims_join_tbl("PROCEDURE_",      by = c("PRCO_PROCEDURE"        = "PROC_ID")) |>
          glims_join_tbl("STATION",         by = c("PROC_STATION"          = "STN_ID")) |>
          glims_join_tbl("WORKPLACE",       by = c("STN_WORKPLACE"         = "WRKP_ID")) |>
          glims_join_tbl("CHOICE",          by = c("RSLT_CHOICE"           = "CHC_ID"))
      }

      # FILTER ----
      full_qry <- full_qry |>
        filter(
          # only real patients
          !is.na(ENCT_PERSON),
          # only where specimen is associated
          !is.na(SPMN_INTERNALID),
          # only from the lab(s) that we select
          DEPT_NAME %in% !!only_include_labs)

      if (!"stay" %in% qry_type) {
        # only keep the STAY records of the moment of specimen sampling
        full_qry <- full_qry |>
          filter(
            STAY_STARTTIME <= SPMN_SAMPLINGTIME,
            is.na(STAY_ENDTIME) | STAY_ENDTIME >= SPMN_SAMPLINGTIME)
      }

      if ("results" %in% qry_type) {
        full_qry <- full_qry |>
          filter(
            # only true results, no logbook items or microscopy
            is.na(STN_MNEMONIC) | !STN_MNEMONIC %in% c("MB_Diversen", "MB_Microscopie"),
            !RSLT_RAWVALUE %in% c("Klaar", ".")
          )
      }

      # SELECT ----
      order_cols <- c("ORD_ID" = ifelse("results" %in% qry_type, "RSLT_ORDER", "RQST_ORDER"),
                      "ORD_INTERNALID",
                      "ORD_RECEIPTTIME",
                      "SPMN_SAMPLINGTIME",
                      "MAT_SHORTNAME",
                      "MAT_MNEMONIC",
                      "ENCT_PERSON",
                      "PRSN_LASTNAME",
                      "PRSN_BIRTHDATE",
                      "PRSN_SEX",
                      "HCPR_LASTNAME",
                      "HCPR_TITLE",
                      "WARD_MNEMONIC",
                      "WARD_NAME",
                      "WARD_ISICU",
                      "STAY_STARTTIME",
                      "STAY_ENDTIME",
                      "STAY_ROOM",
                      "SPEC_MNEMONIC",
                      "SPEC_NAME")

      if (!"stay" %in% qry_type) {
        order_cols <- order_cols[!order_cols %in% c("STAY_STARTTIME", "STAY_ENDTIME")]
      }

      result_cols <- c("RSLT_RAWVALUE",
                       "RSLT_EXPONENT",
                       "RSLT_NORM",
                       "CHC_NAME",
                       "PROP_UNIT",
                       "RSLT_STATUS",
                       "PROP_MNEMONIC",
                       "PROP_SHORTNAME",
                       "PROC_CODE",
                       "PROC_NAME",
                       "STN_MNEMONIC",
                       "STN_NAME",
                       "WRKP_MNEMONIC",
                       "WRKP_NAME",
                       "RSLT_FIRSTREPORTTIME",
                       "RSLT_LASTREPORTTIME",
                       "RSLT_CONFIRMATIONTIME",
                       "USR_EMAIL_CONFIRMATION",
                       "RSLT_VALIDATIONTIME",
                       "USR_EMAIL_VALIDATION",
                       "RSLT_LASTUPDATETIME",
                       "RSLT_LOG")

      select_cols <- order_cols
      if ("results" %in% qry_type) {
        select_cols <- c(select_cols, result_cols)
      }

      full_qry <- full_qry |>
        select(select_cols)

      # FILTER USER-DEFINED
      filters <- rlang::enquos(...)
      if (length(filters) > 0) {
        str <- paste0(as.character(unlist(filters)), collapse = "")
        if (grepl("db$", str, fixed = TRUE)) {
          stop("Did you forget `!!` to use with `db$`? Use `!!db$...` instead.", call. = FALSE)
        }
        full_qry <- full_qry |> filter(filters)
      }

    })
  full_qry
}

#' @param n Number of rows to collect.
#' @rdname db
#' @importFrom dplyr collect
#' @export
preview <- function(qry, n = 100) {
  if (!is.infinite(n)) {
    qry <- qry |> utils::head(n)
  }
  qry |> collect()
}

#' @param limit Maximum number of rows to return. The end result will probably have fewer results due to post-process data cleaning; the `limit` setting only applies to the original database query.
#' @param convert_julian Logical, whether to convert Oracle Julian date fields to Date or POSIXct.
#' @param convert_logicals Logical, whether to convert binary numeric fields (0/1) to logicals.
#' @param convert_column_names Logical, whether to rename column names using a lookup table from the package.
#' @importFrom dplyr collect mutate case_when select everything
#' @importFrom dbplyr remote_query remote_con
#' @importFrom odbc dbGetInfo
#' @rdname db
#' @export
retrieve <- function(qry,
                     limit = Inf,
                     convert_julian = TRUE,
                     convert_logicals = TRUE,
                     convert_column_names = TRUE) {
  start_the_clock <- Sys.time()

  with_cli_status(
    msg = "Running query",
    expr = {
      if (is.infinite(limit)) {
        out <- qry |> collect()
      } else {
        out <- qry |> utils::head(limit) |> collect()
      }
    }
  )

  if (convert_julian == TRUE) {
    with_cli_status(
      msg = "Converting Oracle Julian days",
      expr = {
        out <- out |> oracle_julian_to_datetime()
      }
    )
  }

  if (convert_logicals == TRUE) {
    with_cli_status(
      msg = "Converting 1/0 to TRUE/FALSE",
      expr = {
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
      }
    )
  }

  if (convert_column_names == TRUE) {
    with_cli_status(
      msg = "Converting columns names",
      expr = {
        f <- system.file("readable_column_names.csv", package = "mmbi.epi")
        new_names <- utils::read.csv(f, strip.white = TRUE)
        matched <- match(names(out), new_names$original)
        names(out)[!is.na(matched)] <- new_names$new[matched[!is.na(matched)]]
      })
  }

  cli::cli_alert_info(paste0("Done in ", format(round(difftime(Sys.time(), start_the_clock), 2)),
                             ", returning ", format(NROW(out), big.mark = ","), " x ", format(NCOL(out), big.mark = ","), " observations"))
  # message(" -> Done in ", format(round(difftime(Sys.time(), start_the_clock), 2)),
  #         ", returning ", format(NROW(out), big.mark = ","), " x ", format(NCOL(out), big.mark = ","), " observations")

  structure(out,
            class = c("mmbi_database_download", class(out)),
            dims = dim(out),
            db = tryCatch(dbGetInfo(remote_con(qry))$dbms.name, error = function(e) "??"),
            db_user = tryCatch(remote_con(qry) |> dbGetInfo() |> with(paste0(username, "@", dbname, servername)), error = function(e) "??"),
            user = Sys.info()["user"],
            datetime = Sys.time(),
            qry = as.character(remote_query(qry)))
}

#' @importFrom dbplyr remote_query
#' @rdname db
#' @export
retrieve_query <- function(x) {
  if (!is.null(attr(x, "qry", exact = TRUE))) {
    qry <- attributes(x)$qry
  } else if (inherits(x, c("tbl_dbi", "tbl_lazy"))) {
    qry <- as.character(remote_query(x))
  } else {
    stop("No query found in object.", call. = FALSE)
  }
  cat(qry)
  cat("\n")
  invisible(qry)
}

#' @rdname db
#' @export
search_for_test <- function(db = "Oracle") {
  search_for(db, "PROPERTY")
}

#' @rdname db
#' @export
search_for_specimen <- function(db = "Oracle") {
  search_for(db, "MATERIAL")
}

#' @rdname db
#' @export
search_for_ward <- function(db = "Oracle") {
  search_for(db, "WARD")
}

#' @rdname db
#' @export
search_for_specialism <- function(db = "Oracle") {
  search_for(db, "SPECIALISM")
}

#' @rdname db
#' @export
search_for_physician <- function(db = "Oracle") {
  search_for(db, "HCPROVIDER")
}


# OTHER FUNCTIONS ---------------------------------------------------------------------------------

#' @importFrom dplyr select collect
search_for <- function(db, type) {
  rlang::check_installed("shiny")
  rlang::check_installed("DT")

  con <- connect_db(db = db)
  on.exit(disconnect_db(con))

  if (type == "PROPERTY") {
    data <- con |>
      glims_tbl("PROPERTY") |>
      glims_join_tbl("CHOICELIST", by = c("PROP_CHOICELIST" = "CHCL_ID")) |>
      select(PROP_ID, PROP_MNEMONIC, PROP_SHORTNAME, output_type = CHCL_NAME, free_text = CHCL_FREETEXTALLOWED) |>
      collect() |>
      mutate(free_text = as.logical(free_text))
  } else if (type == "MATERIAL") {
    data <- con |>
      glims_tbl("MATERIAL") |>
      glims_join_tbl("UNIT", by = c("MAT_SIZEUNIT" = "UNIT_ID")) |>
      glims_join_tbl("DIMENSION", by = c("UNIT_DIMENSION" = "DIM_ID")) |>
      select(MAT_MNEMONIC, MAT_SHORTNAME, MAT_COMMENT, unit = UNIT_NAME, dimension = DIM_NAME) |>
      collect()
  } else if (type == "WARD") {
    data <- con |>
      glims_tbl("WARD") |>
      select(WARD_MNEMONIC, WARD_NAME) |>
      collect()
  } else if (type == "SPECIALISM") {
    data <- con |>
      glims_tbl("SPECIALISM") |>
      select(SPEC_MNEMONIC, SPEC_NAME) |>
      collect()
  } else if (type == "HCPROVIDER") {
    data <- con |>
      glims_tbl("HCPROVIDER") |>
      select(HCPR_MNEMONIC, HCPR_FIRSTNAME, HCPR_LASTNAME, HCPR_TITLE, HCPR_SEX, age_group = HCPR_BIRTHDATE) |>
      collect() |>
      mutate(HCPR_SEX = ifelse(HCPR_SEX == 1, "M", ifelse(HCPR_SEX == 2, "V", "?")),
             age_group  = ifelse(is.na(as.Date(age_group)),
                                 NA_real_,
                                 floor(as.numeric(Sys.Date() - as.Date(age_group)) / 365.25)),
             age_group  = ifelse(is.na(age_group),
                                 NA_real_,
                                 floor(age_group / 10) * 10),
             age_group  = ifelse(is.na(age_group),
                                 NA_character_,
                                 sprintf("%d-%d", age_group, age_group + 9)))
  }

  suppressMessages(
    shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::titlePanel(paste("Search GLIMS", type, "table")),
        DT::dataTableOutput("shiny_table")
      ),
      server = function(input, output, session) {
        output$shiny_table <- DT::renderDataTable({
          DT::datatable(
            data,
            filter = "top",
            rownames = FALSE,
            options = list(pageLength = 50, autoWidth = TRUE)
          )
        })
      }
    )
  )
}

#' @importFrom pillar tbl_sum dim_desc
#' @noRd
#' @export
tbl_sum.mmbi_database_download <- function(x, ...) {
  out <- NextMethod()
  if (identical(attributes(x)$dims, dim(x))) {
    # dimensions are identical to originally downloaded data, so print the details
    if (!is.null(attributes(x)$db)) {
      if (!is.null(attributes(x)$db_user)) {
        out <- c(out, `Retrieved from` = paste0(attributes(x)$db, " database by ", attributes(x)$db_user))
      } else {
        out <- c(out, `Retrieved from` = attributes(x)$db)
      }
    }
    if (!is.null(attributes(x)$datetime)) {
      out <- c(out, `Retrieved on` = format(attributes(x)$datetime,
                                            "%e %b %Y %H:%M"))
    }
    if (!is.null(attributes(x)$user)) {
      out <- c(out, `Retrieved by` = unname(attributes(x)$user))
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
                            " Use `retrieve_query()` to retrieve the query")))
    } else {
      c(footer,
        style_subtle(paste0("# ", cli::symbol$info,
                            " Use `retrieve_query()` to retrieve the query of the original ",
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

with_cli_status <- function(msg, expr, time = TRUE) {
  start_time <- Sys.time()

  cli::cli_process_start(msg = paste0(ifelse(time == TRUE, paste0("[", format(start_time), "] "), ""),
                                      cli::style_bold(msg)),
                         msg_failed = paste0(ifelse(time == TRUE, paste0("[", format(start_time), "] "), ""),
                                             cli::col_red(paste0(cli::style_bold(msg), " !ERROR"))))
  result <- tryCatch({
    val <- force(expr)
    cli::cli_process_done(msg_done = paste0(ifelse(time == TRUE, paste0("[", format(start_time), "] "), ""),
                                            paste0(cli::col_green(cli::style_bold(msg)), " (", format(round(difftime(Sys.time(), start_time), 2)), ")")))
    val
  }, error = function(e) {
    cli::cli_process_failed(e$message)
    stop(e)
  })
  invisible(result)
}
