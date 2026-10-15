#' Convert OpenEdge column names to their database spelling
#'
#' Turns upper-case names such as `"ORD_ID"` into the spelling used in the
#' database: the first word in lower case, every following word with an
#' initial capital (`"ord_Id"`). Underscores, including leading or trailing
#' ones, are kept as they are.
#'
#' Not used by the join helpers, which look up the actual spelling with
#' `oe_match_names()`; kept for converting names that are not tied to a
#' table at hand.
#'
#' @param x A character vector of column names.
#' @return A character vector of the same length. `NA` stays `NA`.
#' @examples
#' oe_column_name(c("ORD_ID", "RQST_ORDER", "CODE"))
#' #> "ord_Id" "rqst_Order" "code"
#' @noRd
oe_column_name <- function(x) {
  stopifnot(is.character(x))
  gsub("_([a-z])", "_\\U\\1", tolower(x), perl = TRUE)
}

#' Look up the actual spelling of column names
#'
#' Matches `x` case-insensitively against `available` and returns the
#' spelling from `available`. An exact match always wins. Names without any
#' match are returned unchanged, so a typo still produces dplyr's usual
#' "column not found" error.
#'
#' @param x A character vector of column names as written in the code.
#' @param available A character vector of the columns that actually exist.
#' @return A character vector of the same length as `x`.
#' @examples
#' oe_match_names(c("ORD_ID", "MyKey", "typo"), c("ord_Id", "MyKey", "code"))
#' #> "ord_Id" "MyKey" "typo"
#' @noRd
oe_match_names <- function(x, available) {
  stopifnot(is.character(x), is.character(available))
  idx <- match(toupper(x), toupper(available))
  replace <- !is.na(idx) & !(x %in% available)
  x[replace] <- available[idx[replace]]
  x
}

#' Look up the actual spelling of the columns in a join `by`
#'
#' For a character `by`, matches the left-hand names against the columns of
#' the left table and the right-hand names against the columns of the right
#' table, using `oe_match_names()`. An unnamed element such as `"ORD_ID"`
#' refers to a column with that name on both sides; it is returned as a named
#' element, because the two tables may spell it differently.
#'
#' `NULL` (natural join) and `dplyr::join_by()` objects are returned
#' unchanged.
#'
#' @param by `NULL`, a character vector, or a `dplyr::join_by()` object.
#' @param x_cols Column names of the left table.
#' @param y_cols Column names of the right table.
#' @return `by`, with the actual column spellings where applicable.
#' @examples
#' oe_match_by(c("RQST_ORDER" = "ORD_ID"), c("rqst_Order"), c("ord_Id"))
#' #> c(rqst_Order = "ord_Id")
#' @noRd
oe_match_by <- function(by, x_cols, y_cols) {
  if (!is.character(by)) {
    return(by)
  }
  x_names <- names(by)
  if (is.null(x_names)) x_names <- rep("", length(by))
  x_names[x_names == ""] <- by[x_names == ""]

  out <- oe_match_names(unname(by), y_cols)
  names(out) <- oe_match_names(x_names, x_cols)
  out
}

#' Is this a connection made with `connect_db("ProgressJ")`?
#'
#' Recognises the connection by the OpenEdge dialect that `connect_db()`
#' attaches with `dbplyr::with_dialect()`.
#'
#' @param con A database connection.
#' @return `TRUE` or `FALSE`.
#' @noRd
is_openedge_con <- function(con) {
  inherits(dbplyr::sql_dialect(con), "sql_dialect_openedge")
}

# Table names are not converted: they are in PascalCase without delimiters
# (e.g. "RequestCode"), which can't be derived from "REQUESTCODE". OpenEdge
# accepts the all-caps spelling for table names, so none is needed.