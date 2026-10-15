test_that("head() gets translated to TOP instead of LIMIT", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  con <- dbplyr::with_dialect(con, dialect_openedge())
  lf <- dbplyr::lazy_frame(x = 1, con = con)
  sql <- dbplyr::remote_query(head(lf, 5))

  expect_match(sql, "SELECT TOP 5", fixed = TRUE)
  expect_no_match(sql, "LIMIT", fixed = TRUE)
})