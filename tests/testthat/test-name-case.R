# RSQLite is listed in Suggests: it only serves as a carrier for the
# OpenEdge dialect here; no query is executed.

test_that("oe_column_name() converts to lower first word, capitalised rest", {
  expect_equal(oe_column_name("ORD_ID"), "ord_Id")
  expect_equal(oe_column_name("RQST_ORDER"), "rqst_Order")
  expect_equal(oe_column_name("CODE"), "code")
  expect_equal(oe_column_name("ORD_ID_"), "ord_Id_")
  expect_equal(oe_column_name("ORD_2"), "ord_2")
  expect_equal(oe_column_name(c("A_B", NA)), c("a_B", NA))
})

test_that("oe_match_names() returns the actual spelling", {
  cols <- c("ord_Id", "MyKey", "code")
  expect_equal(oe_match_names("ORD_ID", cols), "ord_Id")
  expect_equal(oe_match_names("MyKey", cols), "MyKey")
  expect_equal(oe_match_names("mykey", cols), "MyKey")
  expect_equal(oe_match_names("typo", cols), "typo")
  expect_equal(oe_match_names(character(), cols), character())
})

test_that("oe_match_names() prefers an exact match", {
  expect_equal(oe_match_names("CODE", c("code", "CODE")), "CODE")
  expect_equal(oe_match_names("code", c("code", "CODE")), "code")
})

test_that("oe_match_by() matches each side against its own table", {
  expect_equal(
    oe_match_by(c("RQST_ORDER" = "ORD_ID"), "rqst_Order", "ord_Id"),
    c("rqst_Order" = "ord_Id")
  )
  # Unnamed key, spelled differently in the two tables
  expect_equal(
    oe_match_by("ORD_ID", "ORD_ID", "ord_Id"),
    c("ORD_ID" = "ord_Id")
  )
  # Mixed named and unnamed keys
  expect_equal(
    oe_match_by(c("RQST_ORDER" = "ORD_ID", "CODE"),
                c("rqst_Order", "code"),
                c("ord_Id", "code")),
    c("rqst_Order" = "ord_Id", "code" = "code")
  )
})

test_that("oe_match_by() leaves NULL and join_by() alone", {
  expect_null(oe_match_by(NULL, "a", "a"))
  jb <- dplyr::join_by(RQST_ORDER == ORD_ID)
  expect_identical(oe_match_by(jb, "a", "a"), jb)
})

test_that("is_openedge_con() recognises the OpenEdge dialect", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  expect_false(is_openedge_con(con))
  expect_true(is_openedge_con(dbplyr::with_dialect(con, dialect_openedge())))
})

test_that("non-character input is rejected", {
  expect_error(oe_column_name(1))
  expect_error(oe_match_names(1, "a"))
})