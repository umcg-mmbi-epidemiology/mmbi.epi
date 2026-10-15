#' SQL-dialect voor Progress OpenEdge
#'
#' Bouwt voort op het generieke ODBC-dialect van dbplyr, maar gebruikt
#' `SELECT TOP n` in plaats van `LIMIT n`.
#' @noRd
dialect_openedge <- function() {
  d <- dbplyr::dialect_odbc()
  class(d) <- c("sql_dialect_openedge", class(d))
  d
}

# Interne dbplyr-functies; zie de toelichting bij R CMD check
dbplyr_internal <- function(name) utils::getFromNamespace(name, "dbplyr")

#' @exportS3Method dbplyr::sql_query_select
sql_query_select.sql_dialect_openedge <- function(con, select, from, where = NULL,
                                                  group_by = NULL, having = NULL,
                                                  window = NULL, order_by = NULL,
                                                  limit = NULL, distinct = FALSE,
                                                  ..., subquery = FALSE, lvl = 0) {
  clauses <- dbplyr_internal("sql_select_clauses")
  clause_select <- dbplyr_internal("sql_clause_select")
  clause_from <- dbplyr_internal("sql_clause_from")
  clause_where <- dbplyr_internal("sql_clause_where")
  clause_group_by <- dbplyr_internal("sql_clause_group_by")
  clause_having <- dbplyr_internal("sql_clause_having")
  clause_window <- dbplyr_internal("sql_clause_window")
  clause_order_by <- dbplyr_internal("sql_clause_order_by")

  clauses(
    select   = clause_select(select, distinct, top = limit),
    from     = clause_from(from),
    where    = clause_where(where),
    group_by = clause_group_by(group_by),
    having   = clause_having(having),
    window   = clause_window(window),
    order_by = clause_order_by(order_by, subquery, limit),
    lvl      = lvl
  )
}