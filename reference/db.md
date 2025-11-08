# GLIMS Database Access

Establishes a connection to the GLIMS Oracle database using environment
variables for all configuration parameters. This function is designed
for use in secure environments such as Posit Workbench, where
credentials and driver paths are stored outside the source code.

## Usage

``` r
connect_db(db = "Oracle")

disconnect_db(con)

glims_tbl(con, table_name, schema = "ORAGLIMS")

glims_join_tbl(
  qry,
  table_name,
  by = NULL,
  ...,
  type = "left",
  schema = "ORAGLIMS"
)

preview(qry, n = 100)

oracle_julian_to_datetime(x, tz = "Europe/Amsterdam")

datetime_to_oracle_julian(x)

build_query(con, qry_type)

retrieve(
  qry,
  limit = Inf,
  convert_julian = TRUE,
  convert_logicals = TRUE,
  convert_column_names = TRUE
)

get_glims_data(
  date_range,
  ...,
  convert_julian = TRUE,
  convert_logicals = TRUE,
  convert_column_names = TRUE,
  limit = Inf,
  qry_type = c("results", "orders"),
  db = "Oracle"
)

original_query(x)
```

## Arguments

- db:

  Database to connect to.

- con:

  Connection object.

- table_name:

  Name of the table to query.

- schema:

  Database schema; the upper structure name of `table_name`.

- qry:

  A `tbl_dbi` object representing the database query.

- by:

  Column name(s) to join by. Can be a named vector to allow different
  names:  
  `by = c("col_A" = "col_B")`.

- ...:

  . \* `get_glims_data()`: Arguments passed on the `WHERE` clause in the
  query. Supports `dplyr` language.

  - `glims_join_tbl()`: Arguments passed on the join functions.

- type:

  Direction of the join, defaults to `"left"`.

  - [`"inner"`](https://dplyr.tidyverse.org/reference/mutate-joins.html):
    returns matched x rows.

  - [`"left"`](https://dplyr.tidyverse.org/reference/mutate-joins.html):
    returns all x rows.

  - [`"right"`](https://dplyr.tidyverse.org/reference/mutate-joins.html):
    returns matched of x rows, followed by unmatched y rows.

  - [`"full"`](https://dplyr.tidyverse.org/reference/mutate-joins.html):
    returns all x rows, followed by unmatched y rows.

- n:

  Number of rows to collect.

- x:

  Numeric vector or data.frame, to convert Oracle Julian days to common
  dates. In case of data frames, only columns with numeric values that
  have Julian days between 1900-01-01 and 2100-01-01 will be converted.

- tz:

  Target time zone.

- qry_type:

  Type of query, see `Query Types` below.

- limit:

  Maximum number of rows to return. The end result will probably have
  fewer results due to post-process data cleaning; the `limit` setting
  only applies to the original database query.

- convert_julian:

  Logical, whether to convert Oracle Julian date fields to Date or
  POSIXct.

- convert_logicals:

  Logical, whether to convert binary numeric fields (0/1) to logicals.

- convert_column_names:

  Logical, whether to rename column names using a lookup table from the
  package.

- date_range:

  Date range, can be length 1 or 2 (or more to use the min/max) to
  filter the `ORD_RECEIPTTIME` column. Supports date/time, date, and
  years. Use `NULL` to set no date filter.

## Value

Returns a live `DBIConnection` object that can be used with `DBI`,
`dplyr`, or `dbplyr` for querying and manipulating data.

## Environment Variables

When `db` is set to `"Oracle"`, the following environment variables must
be defined before using this function:

- **`MMBI_EPIDS_DRIVER`** - Full path to, or name of, the Oracle ODBC
  driver shared library

- **`MMBI_EPIDS_HOST`** - Hostname or IP address of the Oracle database
  server

- **`MMBI_EPIDS_PORT`** - Port number for the Oracle service (typically
  `1521`)

- **`MMBI_EPIDS_SVC`** - Oracle Service Name (SVC) or SID identifying
  the database instance

- **`MMBI_EPIDS_USER`** - Oracle database username

- **`MMBI_EPIDS_PASS`** - Oracle database password

The environment variables are read at runtime using
[`Sys.getenv()`](https://rdrr.io/r/base/Sys.getenv.html) and passed to
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
This approach ensures credentials are never exposed in the source code
or logs.

## Query Types

Various query types have been defined:

- `"orders"`: 1 row per order, info about request (department, ward,
  room, etc.)

- `"results"`: 1 row per result, can be multiple in an order

- `"isolates"`: 1 row per isolate, can be multiple in a result

These types can be combined.

## See also

[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html),
[`odbc::odbc()`](https://odbc.r-dbi.org/reference/dbConnect-OdbcDriver-method.html)

## Examples

``` r
if (FALSE) { # \dontrun{

# Opening Connection ---------------------------------------------------

# open connection and save as object
conn <- connect_db()

# connect to a table
conn |>
  glims_tbl("ANTIBIOTICRESULT")


# Running Queries ------------------------------------------------------

library(dplyr)

# download first 500 rows
first_500 <- conn |>
  glims_tbl("ANTIBIOTICRESULT") |>
  retrieve(500)

# dplyr functions are automatically translated into SQL
conn |>
  glims_tbl("ANTIBIOTICRESULT") |>
  filter(ABRS_RISRAWVALUE == 3) |>
  show_query() # use collect() or retrieve() instead to download

# more advanced queries can still run server-side
my_count <- conn |>
  glims_tbl("ANTIBIOTICRESULT") |>
  filter(ABRS_RISRAWVALUE == 3) |>
  count(ABRS_RISREPORTVALUECHANGED) |>
  show_query() |> # first show query
  collect()       # then immediately run and download data

# join to other columns server-side
conn |>
  glims_tbl("ANTIBIOTICRESULT") |>
  glims_join_tbl("ANOTHERTABLE", by = "some_column") |>
  show_query()


# Closing Connection ---------------------------------------------------

disconnect_db(conn)

} # }


# Other ----------------------------------------------------------------

datetime_to_oracle_julian(Sys.Date())
#> [1] 2460988

Sys.Date()
#> [1] "2025-11-08"
Sys.Date() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime()
#> [1] "2025-11-08"

Sys.time()
#> [1] "2025-11-08 15:30:10 UTC"
Sys.time() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime()
#> [1] "2025-11-08 16:30:10 CET"
Sys.time() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime(tz = "UTC")
#> [1] "2025-11-08 15:30:10 UTC"
```
