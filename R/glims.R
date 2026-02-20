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
#' @section Picking Columns with `db$`:
#' Preferably use the `db` object (a [list]) to pick database columns for filtering. It contains all GLIMS column names. For example:
#'
#' ```r
#' get_glims_data(db$specimen_name == "Sputum")   # writing `db$` will bring up all columns
#'
#' get_glims_data(db$ORDER_.ORD_ID == 461098)
#' get_glims_data(ORD_ID == 461098)               # Also works
#' ```
#'
#' To use precalculated values, such as using [convert_glims_ordernumber()] to convert order numbers from the GLIMS UI, use `!!` (called 'bang-bang'):
#'
#' ```r
#' get_glims_data(db$order_number == !!convert_glims_ordernumber(25461123456))
#' ```
#' @return
#' Returns a live `DBIConnection` object that can be used with `DBI`, `dplyr`, or `dbplyr` for querying and manipulating data.
#' @seealso [DBI::dbConnect()], [odbc::odbc()]
#' @importFrom odbc dbConnect odbc
#' @rdname glims
#' @name glims
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
#'   glims_join_tbl("ANOTHER_TABLE", by = "SOME_COLUMN") |>
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

#' @rdname glims
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
#' @rdname glims
#' @importFrom dplyr tbl
#' @export
glims_tbl <- function(con, table_name, schema = "ORAGLIMS") {
  con |> tbl(I(paste0(schema, ".", table_name)))
}

#' @param date_range Date range, can be length 1 or 2 (or more to use the min/max) to filter the `ORD_RECEIPTTIME` column. Supports date/time, date, and years. Use `NULL` to set no date filter.
#' @param review_qry Logical to indicate whether  the query must be reviewed before running.
#' @param ab_type Type of AMR results to return. Defaults to `"reported_sir"` to reflect the final lab result. Can also be `"raw_sir"`, `"raw_mic"`, `"raw_disk"`, or `"raw_etest"`.
#' @importFrom dplyr filter between show_query left_join starts_with
#' @importFrom dbplyr sql_build
#' @importFrom tidyr pivot_wider
#' @rdname glims
#' @export
get_glims_data <- function(...,
                           date_range = NULL,
                           additional_columns = character(0),
                           convert_julian = TRUE,
                           convert_logicals = TRUE,
                           convert_column_names = TRUE,
                           limit = Inf,
                           qry_type = c("results", "orders"),
                           db = "Oracle",
                           only_include_labs = "Medische Microbiologie",
                           ab_type = c("reported_sir", "raw_sir", "raw_mic", "raw_disk", "raw_etest"),
                           review_qry = interactive()) {

  if (!any(c("orders", "results", "stays") %in% qry_type)) {
    stop("qry_type must contain orders, results, or stays")
  }

  con <- connect_db(db = db)
  on.exit(disconnect_db(con))

  qry_type.bak <- qry_type
  if (any(c("isolates", "carriers") %in% qry_type)) {
    ab_type <- ab_type[1]
    qry_type <- qry_type[!qry_type %in% c("isolates", "carriers")]
  }

  qry <- con |>
    build_query(...,
                qry_type = qry_type,
                additional_columns = additional_columns,
                only_include_labs = only_include_labs)

  if (isFALSE(date_range)) {
    date_range <- NULL
  }
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
    date_range.bak <- date_range
    date_range <- datetime_to_oracle_julian(as.Date(date_range))
    date_range[2] <- date_range[2] + 1 - 1e-10 # include full end date
    qry <- qry |>
      filter(ORD_RECEIPTTIME |>
               between(!!(date_range[1]),
                       !!(date_range[2])))
  }

  if (review_qry == TRUE) {
    where_txt <- as.character(sql_build(qry)$where)
    cat("Collect the data using this WHERE clause?\n\n")
    cat(format_sql(paste0(where_txt, collapse = "\n")), "\n\n")
    if (!is.null(date_range)) {
      cat(paste0("This corresponds to date range: ", format(date_range.bak[1]), " to ", format(date_range.bak[2])), "\n\n")
    }
    choice <- utils::menu(
      choices = c("Collect data", "Show full SQL", "Cancel"),
      title = "Select an option:")
  } else {
    choice <- 1
  }

  if (choice == 1) {
    out <- qry |>
      retrieve(limit = limit,
               convert_julian = convert_julian,
               convert_logicals = convert_logicals,
               convert_column_names = convert_column_names,
               message_text = "Collecting results data")
  } else if (choice == 2) {
    cat("\nFull SQL:\n\n")
    show_query(qry)
    out <- NULL
  } else {
    out <- NULL
  }

  # get isolates/carriers if requested
  if (any(c("isolates", "carriers") %in% qry_type.bak)) {
    if (NROW(out) == 0) {
      with_cli_status("Skipping isolates data, since row count = 0", "")
    } else {
      qry_ab <- con |>
        build_query(
          db$ISOLATION.ISOL_SPECIMEN %in% !!out$ISOL_SPECIMEN,
          qry_type = intersect(qry_type.bak, c("isolates", "carriers")),
          additional_columns = character(0))
      out_ab <- qry_ab |>
        retrieve(limit = Inf,
                 convert_julian = FALSE,
                 convert_logicals = FALSE,
                 convert_column_names = convert_column_names,
                 message_text = "Collecting isolates data")

      with_cli_status(msg = "Pivoting isolates data",
                      expr = {
                        if (ab_type == "reported_sir") {
                          out_ab <- out_ab |> mutate(ab_col = AMR::as.sir(c("R", "I", "S")[ABRS_RISREPORTVALUE]))
                        } else if (ab_type == "raw_sir") {
                          out_ab <- out_ab |> mutate(ab_col = AMR::as.sir(c("R", "I", "S")[ABRS_RISRAWVALUE]))
                        } else if (ab_type == "raw_mic") {
                          out_ab <- out_ab |> mutate(ab_col = AMR::as.mic(ABRS_MICRAWVALUE))
                        } else if (ab_type == "raw_etest") {
                          out_ab <- out_ab |> mutate(ab_col = AMR::as.mic(ABRS_ETESTRAWVALUE))
                        } else if (ab_type == "raw_disk") {
                          out_ab <- out_ab |> mutate(ab_col = AMR::as.disk(ABRS_AGARDIFFUSIONRAWVALUE))
                        } else {
                          warning("invalid ab_type, ignoring")
                        }

                        out_ab <- out_ab |>
                          pivot_wider(names_from = AB_NAME,
                                      values_from = ab_col,
                                      id_cols = -c(starts_with("ABRS_"), starts_with("AB_")))
                        out <- out |>
                          left_join(out_ab, by = "ISOL_SPECIMEN")
                      }
      )
    }
  }

  cli::cli_alert_success(
    cli::style_bold(
      cli::col_blue(
        paste0("Returning ", format_dimensions(dim(out)), " observations"))))

  out
}

#' @rdname glims
#' @export
db <- readRDS(system.file("db.rds", package = "mmbi.epi"))


#' @param qry A `tbl_dbi` object representing the database query.
#' @param by Column name(s) to join by. Can be a named vector to allow different names:\cr  `by = c("col_A" = "col_B")`.
#' @param ...
#' * `glims_join_tbl()`: Arguments passed on the join functions.
#' * Otherwise: Arguments passed on the `WHERE` clause in the query. Supports `dplyr` language.
#' @param type Direction of the join, defaults to `"left"`.
#' * [`"inner"`][dplyr::inner_join()]: returns matched x rows.
#' * [`"left"`][dplyr::left_join()]: returns all x rows.
#' * [`"right"`][dplyr::right_join()]: returns matched of x rows, followed by unmatched y rows.
#' * [`"full"`][dplyr::full_join()]: returns all x rows, followed by unmatched y rows.
#' @rdname glims
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

#' @rdname glims
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

#' @rdname glims
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
#' @param additional_columns Character vector of additional column names to return.
#' @param only_include_labs Laboratories to include, defaults to only `"Medische Microbiologie"`. This sets a `WHERE` on `DEPARTMENT.DEPTNAME`.
#' @section Query Types:
#' Various query types have been defined:
#'
#' * `"orders"` (always included): 1 row per order (which can contain multiple specimens): info about request (department, ward, room, etc.)
#'   * `"stays"`: 1 row per patient movement with admissions dates: info about every ward admission and room, can be multiple in an order
#'   * `"results"`: 1 row per result, can be multiple in an order
#'     * `"isolates"`: 1 row per isolate, can be multiple in a result
#'       * `"carriers"`: 1 row per carrier (such as an agar plate), can be multiple of an isolate
#'     * `"microscopy"`: 1 row per microscopy result, can be multiple in a result
#'
#' These types can be combined.
#' @importFrom dplyr select filter everything any_of
#' @rdname glims
#' @export
build_query <- function(con, ..., qry_type = "results", additional_columns = character(0), only_include_labs = "Medische Microbiologie") {
  with_cli_status(
    msg = paste0("Building ", paste0(qry_type, collapse = "/"), " query"),
    expr = {

      # supported_qry_types <- c("orders", "stays", "results", "isolates", "carriers", "microscopy")
      supported_qry_types <- c("orders", "stays", "results", "isolates", "carriers")
      if (!all(qry_type %in% supported_qry_types)) {
        stop("Unsupported query type, allowed are: ", toString(supported_qry_types), call. = FALSE)
      }

      if ("carriers" %in% qry_type) {
        qry_type <- c("isolates", "carriers")
      } else if ("isolates" %in% qry_type) {
        # remove orders, results, etc
        qry_type <- "isolates"
      }
      if ("results" %in% qry_type) {
        qry_type <- c(qry_type, "orders")
      }
      if ("microscopy" %in% qry_type) {
        qry_type <- c(qry_type, "results")
      }


      # JOIN ----
      if ("results" %in% qry_type) {
        # Start from RESULT, and join REQUEST and ORDER_ from there
        full_qry <- con |>
          glims_tbl("RESULT") |>
          glims_join_tbl("REQUEST",   by = c("RSLT_ORDER"     = "RQST_ORDER"), keep = TRUE) |>
          glims_join_tbl("ORDER_",    by = c("RSLT_ORDER"     = "ORD_ID"),     keep = TRUE)
      } else if ("isolates" %in% qry_type) {
        # Do not include REQUEST or RESULT data - the number of joined columns would be too large (>1000)
        # Isolates must be retrieved using a separate query
        full_qry <- con |>
          glims_tbl("ISOLATION") |>
          glims_join_tbl("MICROORGANISM",    by = c("ISOL_MICROORGANISM" = "MORG_ID"), keep = TRUE) |>
          glims_join_tbl("ANTIBIOTICRESULT", by = c("ISOL_ID" = "ABRS_ISOLATION"),     keep = TRUE) |>
          glims_join_tbl("ANTIBIOTIC",       by = c("ABRS_ANTIBIOTIC" = "AB_ID"),      keep = TRUE)
      } else {
        full_qry <- con |>
          # Start from REQUEST, the ERD shows REQUEST is the hub that ties to SPECIMEN and ORDER_
          # NOTE! Starting from ORDER_ will also be very slow
          glims_tbl("REQUEST") |>
          glims_join_tbl("ORDER_",    by = c("RQST_ORDER"      = "ORD_ID"),    keep = TRUE)
      }

      # always add these if not looking for isolates
      if (!"isolates" %in% qry_type) {
        full_qry <- full_qry |>
          # glims_join_tbl("SPECIMEN", by = c("ORD_OBJECT" = "SPMN_OBJECT"), keep = TRUE) |>
          glims_join_tbl("SPECIMEN",    by = c("RQST_SPECIMEN"   = "SPMN_ID"),        keep = TRUE) |>
          glims_join_tbl("ENCOUNTER",   by = c("ORD_ENCOUNTER"   = "ENCT_ID"),        keep = TRUE) |>
          glims_join_tbl("DEPARTMENT",  by = c("ORD_DEPARTMENT"  = "DEPT_ID"),        keep = TRUE) |>
          glims_join_tbl("PERSON",      by = c("ENCT_PERSON"     = "PRSN_ID"),        keep = TRUE) |>
          glims_join_tbl("MATERIAL",    by = c("SPMN_MATERIAL"   = "MAT_ID"),         keep = TRUE) |>
          glims_join_tbl("STAY",        by = c("ORD_ENCOUNTER"   = "STAY_ENCOUNTER"), keep = TRUE) |>
          glims_join_tbl("WARD",        by = c("STAY_WARD"       = "WARD_ID"),        keep = TRUE) |>
          glims_join_tbl("SPECIALISM",  by = c("STAY_SPECIALISM" = "SPEC_ID"),        keep = TRUE) |>
          glims_join_tbl("HCPROVIDER",  by = c("ENCT_PHYSICIAN"  = "HCPR_ID"),        keep = TRUE)
      }

      if ("results" %in% qry_type) {
        full_qry <- full_qry |>
          glims_join_tbl("RESULTOUTPUT",    by = c("RSLT_ID"               = "RSTO_RESULT"),   keep = TRUE) |>
          glims_join_tbl("SC_USER",         by = c("RSLT_CONFIRMATIONUSER" = "USR_ID"),        keep = TRUE) |>
          glims_join_tbl("SC_USER",         by = c("RSLT_VALIDATIONUSER"   = "USR_ID"),        keep = TRUE, suffix = c("_CONFIRMATION", "_VALIDATION")) |>
          glims_join_tbl("PROPERTYOUTPUT",  by = c("RSTO_PROPERTYOUTPUT"   = "PRPO_ID"),       keep = TRUE) |>
          glims_join_tbl("PROCEDUREOUTPUT", by = c("PRPO_PROCEDUREOUTPUT"  = "PRCO_ID"),       keep = TRUE) |>
          glims_join_tbl("PROPERTY",        by = c("RSLT_PROPERTY"         = "PROP_ID"),       keep = TRUE) |>
          glims_join_tbl("PROCEDURE_",      by = c("PRCO_PROCEDURE"        = "PROC_ID"),       keep = TRUE) |>
          glims_join_tbl("STATION",         by = c("PROC_STATION"          = "STN_ID"),        keep = TRUE) |>
          glims_join_tbl("WORKPLACE",       by = c("STN_WORKPLACE"         = "WRKP_ID"),       keep = TRUE) |>
          glims_join_tbl("CHOICE",          by = c("RSLT_CHOICE"           = "CHC_ID"),        keep = TRUE) |>
          # these two are only added to allow filtering on MOs - they are not returned in the select
          glims_join_tbl("ISOLATION",       by = c("SPMN_ID"               = "ISOL_SPECIMEN"), keep = TRUE) |>
          glims_join_tbl("MICROORGANISM",   by = c("ISOL_MICROORGANISM"    = "MORG_ID"),       keep = TRUE)
      }

      if ("carriers" %in% qry_type) {
        full_qry <- full_qry |>
          glims_join_tbl("CARRIER",          by = c("ISOL_CARRIER" = "CARR_ID"),       keep = TRUE) |>
          glims_join_tbl("MEDIUM",           by = c("CARR_MEDIUM"  = "MDM_ID"),        keep = TRUE)
      }

      # FILTER ----
      if (!"isolates" %in% qry_type) {
        full_qry <- full_qry |>
          filter(
            # only real patients
            !is.na(ENCT_PERSON),
            # only where specimen is associated
            !is.na(SPMN_INTERNALID),
            # only from the lab(s) that we select
            DEPT_NAME %in% !!only_include_labs)
      }


      if (!"stays" %in% qry_type && !"isolates" %in% qry_type) {
        # only keep the STAY records of the moment of specimen sampling
        full_qry <- full_qry |>
          filter(
            is.na(STAY_STARTTIME) | STAY_STARTTIME <= SPMN_SAMPLINGTIME,
            is.na(STAY_ENDTIME) | STAY_ENDTIME >= SPMN_SAMPLINGTIME)
      }

      if ("results" %in% qry_type) {
        full_qry <- full_qry |>
          filter(
            # only true results, no logbook items or microscopy
            is.na(STN_MNEMONIC) | !STN_MNEMONIC %in% c("MB_Diversen", "MB_Microscopie"),
            !PROP_MNEMONIC %in% c("MB_ANTIBIOGRAM"),
            is.na(RSLT_RAWVALUE) | !RSLT_RAWVALUE %in% c("Klaar", ".")
          )
      }

      # user-defined
      filters <- rlang::enquos(...)
      if (length(filters) > 0) {
        filters <- lapply(filters, function(q) {
          expr <- rlang::get_expr(q)
          expr_resolved <- resolve_db_names(expr, db = db)
          expr_casted <- auto_cast_filter(expr_resolved, con = con, qry = full_qry)
          rlang::new_quosure(expr_casted, rlang::get_env(q))
        })
        full_qry <- full_qry |> filter(!!!filters)
      }

      # SELECT ----
      order_cols <- c("ORD_ID" = ifelse("results" %in% qry_type, "RSLT_ORDER", "RQST_ORDER"),
                      "ORD_INTERNALID",
                      "ORD_RECEIPTTIME",
                      "ISOL_SPECIMEN" = "SPMN_ID",
                      "SPMN_SAMPLINGTIME",
                      "SPMN_SCOPE",
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

      if (!"stays" %in% qry_type) {
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
                       "PROP_CODE",
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

      isolate_cols <- c("ISOL_SPECIMEN",
                        "ISOL_INTERNALSEQUENCER",
                        "ISOL_INTERNALCOMMENT",
                        "ISOL_EXTERNALCOMMENT",
                        "ISOL_CONFIRMATIONUSER",
                        "ISOL_AUDITTRAIL",
                        "MORG_MNEMONIC",
                        "MORG_SHORTNAME",
                        "MORG_NAME",
                        "ABRS_RISRAWVALUE",
                        "ABRS_RISREPORTVALUE",
                        "ABRS_MICRAWVALUE",
                        "ABRS_AGARDIFFUSIONRAWVALUE",
                        "ABRS_ETESTRAWVALUE",
                        "AB_MNEMONIC",
                        "AB_NAME")
      carrier_cols <- c("CARR_COMMENT",
                        "MDM_MNEMONIC",
                        "MDM_DESCRIPTION")

      select_cols <- character()

      if ("orders" %in% qry_type) {
        select_cols <- order_cols
        if ("results" %in% qry_type) {
          select_cols <- c(select_cols, result_cols)
        }
      }
      if ("isolates" %in% qry_type) {
        select_cols <- isolate_cols
        if ("carriers" %in% qry_type) {
          select_cols <- c(select_cols, carrier_cols)
        }
      }

      if (!identical(additional_columns, "all")) {
        full_qry <- full_qry |>
          select(select_cols, as.character(additional_columns))
      }

    })
  full_qry
}

#' @param n Number of rows to collect.
#' @rdname glims
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
#' @param message_text Text to show in the console while retrieving data.
#' @importFrom dplyr collect mutate case_when select everything
#' @importFrom dbplyr remote_query remote_con
#' @importFrom odbc dbGetInfo
#' @rdname glims
#' @export
retrieve <- function(qry,
                     limit = Inf,
                     convert_julian = TRUE,
                     convert_logicals = TRUE,
                     convert_column_names = TRUE,
                     message_text = "Collecting data") {
  start_the_clock <- Sys.time()

  with_cli_status(
    msg = message_text,
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
          if (grepl("SEX", colnames(out)[i])) {
            next
          }
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
      msg = "Converting column names",
      expr = {
        f <- system.file("readable_column_names.csv", package = "mmbi.epi")
        new_names <- utils::read.csv(f, strip.white = TRUE)
        matched <- match(names(out), new_names$COLUMN_NAME)
        names(out)[!is.na(matched)] <- new_names$NEW_COLUMN_NAME[matched[!is.na(matched)]]
      })
  }

  cli::cli_alert_success(
    cli::style_bold(
      cli::col_blue(
        paste0("Done in ", format(round(difftime(Sys.time(), start_the_clock), 2)),
               ", collected ", format_dimensions(dim(out)), " observations"))))

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
#' @rdname glims
#' @export
retrieve_query <- function(x) {
  if (!is.null(attr(x, "qry", exact = TRUE))) {
    qry <- attributes(x)$qry
  } else if (inherits(x, c("tbl_dbi", "tbl_lazy"))) {
    qry <- as.character(remote_query(x))
  } else {
    stop("No query found in object.", call. = FALSE)
  }
  cat(format_sql(qry))
  cat("\n")
  invisible(qry)
}

format_sql <- function(qry) {
  gsub("( AND | OR |NOT| IN |EVAL| BETWEEN | ON | IS | AS )", cli::col_blue("\\1"),
       gsub("(SELECT|FROM|WHERE|LIMIT|LEFT JOIN|RIGHT JOIN|INNER JOIN)", cli::style_bold(cli::col_blue("\\1")),
            gsub("}\n  (AND|OR) ", "} \\1 ",
                 gsub(" (AND|OR) ", "\n  \\1 ",
                      gsub(" (AND|OR)\n", cli::col_blue(" \\1\n"),
                           gsub(" NULL", paste0(" ", cli::col_green("NULL")),
                                gsub(" IS ", paste0(" ", cli::col_green("IS"), " "),
                                     gsub('"(.*?)"', cli::col_yellow("\"\\1\""),
                                          gsub("'(.*?)'", cli::col_magenta("'\\1'"), qry)))))))))
}

#' @param glims_order_number A GLIMS order number. The `convert_glims_ordernumber()` function will append this with "961" and keeps a total of 12 numbers.
#' @rdname glims
#' @export
convert_glims_ordernumber <- function(glims_order_number) {
  paste0("961", substr(glims_order_number, 1, 9))
}

#' @param convert A logical to use `convert_glims_ordernumber()` before using `person_id_from_ordernumber()`
#' @importFrom dplyr filter collect pull
#' @rdname glims
#' @export
person_id_from_ordernumber <- function(glims_order_number, convert = TRUE, db = "Oracle") {
  if (convert == TRUE) {
    glims_order_number <- convert_glims_ordernumber(glims_order_number)
  }
  glims_order_number <- as.character(glims_order_number)
  con <- connect_db(db = db)
  on.exit(disconnect_db(con))
  out <- con |>
    glims_tbl("ORDER_") |>
    glims_join_tbl("ENCOUNTER", by = c("ORD_ENCOUNTER" = "ENCT_ID")) |>
    filter(`U##ORD_INTERNALID` == glims_order_number) |>
    utils::head(1) |>
    collect() |>
    pull("ENCT_PERSON")
  if (length(out) > 0) {
    c(PRSN_ID = out)
  } else {
    NA_character_
  }
}

auto_cast_filter <- function(expr, con, qry) {
  if (!rlang::is_call(expr)) {
    return(expr)
  }
  # Only handle binary comparisons
  if (rlang::call_name(expr) %in% c("==", "!=", ">", "<", ">=", "<=")) {
    lhs <- expr[[2]]
    rhs <- expr[[3]]
    if (rlang::is_symbol(lhs) && !rlang::is_symbol(rhs)) {
      colname <- rlang::as_string(lhs)
      res <- DBI::dbSendQuery(con, paste0("SELECT ", colname, " FROM (", dbplyr::remote_query(qry), ") WHERE 1=0"))
      colinfo <- DBI::dbColumnInfo(res)
      DBI::dbClearResult(res)
      if (nrow(colinfo) == 1) {
        expr[[3]] <- alter_from_odbc_type(rhs, colinfo$type, colname)
      }
    }
  }
  expr[] <- lapply(expr, auto_cast_filter, con = con, qry = qry)
  expr
}

odbc_type_to_string <- function(type_code) {
  lookup <- c(
    `1`   = "CHAR",
    `12`  = "VARCHAR",
    `-9`  = "NVARCHAR",
    `-1`  = "LONGVARCHAR",
    `4`   = "INTEGER",
    `5`   = "SMALLINT",
    `-5`  = "BIGINT",
    `6`   = "FLOAT",
    `8`   = "DOUBLE",
    `2`   = "NUMERIC",
    `3`   = "DECIMAL",
    `91`  = "DATE",
    `93`  = "TIMESTAMP"
  )
  out <- unname(lookup[as.character(type_code)] %||% paste0("UNKNOWN(", type_code, ")"))
  out[is.na(out)] <- type_code[is.na(out)]
  out
}

alter_from_odbc_type <- function(val, type_code, col) {
  type_name <- odbc_type_to_string(type_code)
  warn <- function(val, type) {
    cli::cli_alert_warning("Value in RHS {.val {val}} converted from {.var {paste(class(val), collapse = \"/\")}} to {.var character} to comply with column {.val {col}}")
  }
  if (!is.character(val) &&type_name %in% c("CHAR", "VARCHAR", "NVARCHAR", "LONGVARCHAR")) {
    warn(val, "character")
    as.character(val)
  } else if (!is.integer(val) && type_name %in% c("INTEGER", "SMALLINT", "BIGINT")) {
    warn(val, "integer")
    as.integer(val)
  } else if (!is.double(val) && type_name %in% c("DOUBLE", "FLOAT", "NUMERIC", "DECIMAL")) {
    warn(val, "numeric")
    as.double(val)
  } else if (!inherits(val, "Date") && type_name == "DATE") {
    warn(val, "Date")
    as.Date(val)
  } else if (!inherits(val, "POSIXct") && type_name == "TIMESTAMP") {
    warn(val, "POSIXct")
    as.POSIXct(val)
  } else {
    val
  }
}

# OTHER FUNCTIONS ---------------------------------------------------------------------------------

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
  dims
}

with_cli_status <- function(msg, expr, time = TRUE) {
  start_time <- Sys.time()

  geruntive <- function(x) {
    x <- gsub("Building", "Built", x)
    x <- gsub("ing", "ed", x)
    x
  }

  cli::cli_process_start(msg = paste0(ifelse(time == TRUE, paste0("[", format(start_time), "] "), ""),
                                      cli::style_bold(msg), "..."),
                         msg_failed = paste0(ifelse(time == TRUE, paste0("[", format(start_time), "] "), ""),
                                             cli::col_red(paste0(cli::style_bold(msg), " [ERROR]"))))
  result <- tryCatch({
    val <- force(expr)
    cli::cli_process_done(msg_done = paste0(ifelse(time == TRUE, paste0("[", format(start_time), "] "), ""),
                                            paste0(cli::col_green(cli::style_bold(geruntive(msg))), " (", format(round(difftime(Sys.time(), start_time), 2)), ")")))
    val
  }, error = function(e) {
    cli::cli_process_failed(e$message)
    stop(e)
  })
  invisible(result)
}

resolve_db_names <- function(expr, db) {
  if (rlang::is_call(expr, "$") && identical(expr[[2]], quote(db))) {
    resolved <- db[[rlang::as_string(expr[[3]])]]
    if (!is.name(resolved)) {
      stop(rlang::as_string(expr[[3]]), " is not a valid element of `db$`.")
    }
    return(resolved)
  }

  # Recursive walk
  if (rlang::is_call(expr)) {
    expr[] <- lapply(expr, resolve_db_names, db = db)
  }
  expr
}

glims_shiny_picker <- function() {
  rlang::check_installed("shiny")
  rlang::check_installed("DT")
  rlang::check_installed("dplyr")

  shiny::addResourcePath(
    prefix = "mmbi_epi",
    directoryPath = system.file("", package = "mmbi.epi")
  )

  suppressMessages(
    shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::tags$head(
          shiny::tags$link(
            href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;600;700&display=swap",
            rel  = "stylesheet"
          ),
          shiny::tags$style(
            shiny::HTML(sprintf("
              body {
                background-color: %s;
                font-family: 'Outfit', sans-serif;
              }

              h2, h3 {
                color: %s;
                font-weight: 700;
              }

              h3 {
                margin-top: 0;
                font-weight: normal;
              }

              .well {
                background-color: white;
                border-radius: 8px;
                border: 1px solid %s;
              }

              .shiny-input-container label {
                color: %s;
              }

              table.dataTable thead th {
                background-color: %s;
                color: white;
              }

              .sidebar-footer {
                margin-top: 30px;
                padding-top: 10px;
                border-top: 1px solid %s;
                text-align: center;
                font-size: 0.9em;
                color: %s;
              }

              .sidebar-footer img {
                margin-top: 8px;
                max-width: 200px;
                height: auto;
              }

              .server {
                color: lightgrey;
                font-size: 10px;
                margin-top: 10px;
                margin-bottom: 0;
              }
            ",
                                get_colour("umcglichtblauw"),
                                get_colour("umcgblauw"),
                                get_colour("umcgblauw"),
                                get_colour("umcgblauw"),
                                get_colour("umcgblauw"),
                                get_colour("umcgblauw"),
                                get_colour("umcgblauw")
            ))
          )
        ),

        shiny::titlePanel("ShinyGLIMS"),
        shiny::h3("Referentietabellen van GLIMS"),

        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            shiny::p("Selecteer een GLIMS-tabel om een code of beschrijving te zoeken."),
            shiny::br(),
            shiny::radioButtons(
              inputId = "type",
              label   = "GLIMS-tabel:",
              choices = c(
                "Afdelingen"           = "WARD",
                "Bepalingen"           = "PROPERTY",
                "Materiaaltypes"       = "MATERIAL",
                "Micro-organismen"     = "MICROORGANISM",
                "Specialismen"         = "SPECIALISM",
                "Zorgverleners"        = "HCPROVIDER"
              )
            ),

            shiny::hr(),

            shiny::checkboxInput(
              inputId = "glims_colnames",
              label   = "Gebruik interne GLIMS-kolomnamen",
              value   = FALSE
            ),

            shiny::div(
              class = "sidebar-footer",
              shiny::div("Ontwikkeld door:"),
              shiny::tags$img(src = "mmbi_epi/unit_logo.jpeg"),
              shiny::p(class = "server",
                       paste0("GLIMS-server: ",
                              Sys.getenv("MMBI_EPIDS_HOST"), ":",
                              Sys.getenv("MMBI_EPIDS_PORT"),
                              " [SVC ", Sys.getenv("MMBI_EPIDS_SVC"), "]"))
            )
          ),

          shiny::mainPanel(
            width = 9,
            DT::dataTableOutput("shiny_table")
          )
        )
      ),

      server = function(input, output, session) {

        data_reactive <- shiny::reactive({
          req(input$type)
          con <- connect_db(db = "Oracle")
          on.exit(disconnect_db(con))
          type <- input$type
          if (type == "PROPERTY") {
            con |>
              glims_tbl("PROPERTY") |>
              glims_join_tbl("CHOICELIST", by = c("PROP_CHOICELIST" = "CHCL_ID")) |>
              dplyr::select(
                PROP_ID,
                PROP_MNEMONIC,
                PROP_SHORTNAME,
                CHCL_NAME,
                CHCL_FREETEXTALLOWED
              ) |>
              dplyr::collect() |>
              dplyr::mutate(CHCL_FREETEXTALLOWED = ifelse(as.logical(CHCL_FREETEXTALLOWED), "Ja", "Nee"))

          } else if (type == "MATERIAL") {
            con |>
              glims_tbl("MATERIAL") |>
              glims_join_tbl("UNIT", by = c("MAT_SIZEUNIT" = "UNIT_ID")) |>
              glims_join_tbl("DIMENSION", by = c("UNIT_DIMENSION" = "DIM_ID")) |>
              dplyr::select(
                MAT_MNEMONIC,
                MAT_SHORTNAME,
                MAT_SAMPLINGCODE,
                MAT_COMMENT,
                unit      = UNIT_NAME,
                dimension = DIM_NAME
              ) |>
              dplyr::collect()

          } else if (type == "WARD") {
            con |>
              glims_tbl("WARD") |>
              dplyr::select(WARD_MNEMONIC, WARD_NAME) |>
              dplyr::collect()

          } else if (type == "SPECIALISM") {
            con |>
              glims_tbl("SPECIALISM") |>
              dplyr::select(SPEC_MNEMONIC, SPEC_NAME) |>
              dplyr::collect()

          } else if (type == "MICROORGANISM") {
            con |>
              glims_tbl("MICROORGANISM") |>
              dplyr::select(MORG_MNEMONIC, MORG_NAME, MORG_SHORTNAME) |>
              dplyr::collect()

          } else if (type == "HCPROVIDER") {
            con |>
              glims_tbl("HCPROVIDER") |>
              dplyr::select(
                HCPR_MNEMONIC,
                HCPR_FIRSTNAME,
                HCPR_LASTNAME,
                HCPR_TITLE,
                HCPR_SEX
              ) |>
              dplyr::collect() |>
              dplyr::mutate(
                HCPR_SEX = ifelse(HCPR_SEX == 1, "M",
                                  ifelse(HCPR_SEX == 2, "V", "?"))
              )
          }
        })

        output$shiny_table <- DT::renderDataTable({

          data <- data_reactive()

          if (isFALSE(input$glims_colnames)) {
            data <- translate_glims_columns(data)
          }

          DT::datatable(
            data,
            filter   = "top",
            rownames = FALSE,
            options  = list(
              pageLength  = 15,
              autoWidth   = TRUE,
              deferRender = TRUE,
              order       = list(list(0, "asc")),
              language    = list(
                url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/nl-NL.json"
              )
            )
          )
        })
      }
    )
  )
}

translate_glims_columns <- function(x) {
  translation <- c(
    PROP_ID           = "Code/mnemonic",
    PROP_MNEMONIC     = "Bepalingscode",
    PROP_SHORTNAME    = "Bepalingsnaam",
    CHCL_NAME         = "Uitvoertype",
    CHCL_FREETEXTALLOWED = "Vrije tekst toegestaan",

    MAT_MNEMONIC      = "Code/mnemonic",
    MAT_SHORTNAME     = "Materiaalnaam",
    MAT_SAMPLINGCODE  = "Afnamecode",
    MAT_COMMENT       = "Opmerking",
    unit              = "Eenheid",
    dimension         = "Dimensie",

    WARD_MNEMONIC     = "Code/mnemonic",
    WARD_NAME         = "Afdelingsnaam",

    SPEC_MNEMONIC     = "Code/mnemonic",
    SPEC_NAME         = "Specialismenaam",

    HCPR_MNEMONIC     = "Code/mnemonic",
    HCPR_FIRSTNAME    = "Voornaam",
    HCPR_LASTNAME     = "Achternaam",
    HCPR_TITLE        = "Functie",
    HCPR_SEX          = "Geslacht",

    MORG_MNEMONIC     = "Code/mnemonic",
    MORG_NAME         = "Naam",
    MORG_SHORTNAME    = "Korte naam"
  )

  names(x) <- ifelse(
    names(x) %in% names(translation),
    translation[names(x)],
    names(x)
  )

  x
}

