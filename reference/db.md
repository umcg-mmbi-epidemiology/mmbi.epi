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

get_glims_data(
  date_range = NULL,
  ...,
  additional_columns = character(0),
  convert_julian = TRUE,
  convert_logicals = TRUE,
  convert_column_names = TRUE,
  limit = Inf,
  qry_type = c("results", "orders"),
  db = "Oracle",
  only_include_labs = "Medische Microbiologie",
  ab_type = c("reported_sir", "raw_sir", "raw_mic", "raw_disk", "raw_etest"),
  review_qry = interactive()
)

db

glims_join_tbl(
  qry,
  table_name,
  by = NULL,
  ...,
  type = "left",
  schema = "ORAGLIMS"
)

oracle_julian_to_datetime(x, tz = "Europe/Amsterdam")

datetime_to_oracle_julian(x)

build_query(
  con,
  ...,
  qry_type = "results",
  additional_columns = character(0),
  only_include_labs = "Medische Microbiologie"
)

preview(qry, n = 100)

retrieve(
  qry,
  limit = Inf,
  convert_julian = TRUE,
  convert_logicals = TRUE,
  convert_column_names = TRUE,
  message_text = "Collecting data"
)

retrieve_query(x)
```

## Format

An object of class `list` of length 985.

## Arguments

- db:

  Database to connect to.

- con:

  Connection object.

- table_name:

  Name of the table to query.

- schema:

  Database schema; the upper structure name of `table_name`.

- date_range:

  Date range, can be length 1 or 2 (or more to use the min/max) to
  filter the `ORD_RECEIPTTIME` column. Supports date/time, date, and
  years. Use `NULL` to set no date filter.

- ...:

  - `glims_join_tbl()`: Arguments passed on the join functions.

    - Otherwise: Arguments passed on the `WHERE` clause in the query.
      Supports `dplyr` language.

- additional_columns:

  Character vector of additional column names to return.

- convert_julian:

  Logical, whether to convert Oracle Julian date fields to Date or
  POSIXct.

- convert_logicals:

  Logical, whether to convert binary numeric fields (0/1) to logicals.

- convert_column_names:

  Logical, whether to rename column names using a lookup table from the
  package.

- limit:

  Maximum number of rows to return. The end result will probably have
  fewer results due to post-process data cleaning; the `limit` setting
  only applies to the original database query.

- qry_type:

  Type of query, see `Query Types` below.

- only_include_labs:

  Laboratories to include, defaults to only `"Medische Microbiologie"`.
  This sets a `WHERE` on `DEPARTMENT.DEPTNAME`.

- ab_type:

  Type of AMR results to return. Defaults to `"reported_sir"` to reflect
  the final lab result. Can also be `"raw_sir"`, `"raw_mic"`,
  `"raw_disk"`, or `"raw_etest"`.

- review_qry:

  Logical to indicate whether the query must be reviewed before running.

- qry:

  A `tbl_dbi` object representing the database query.

- by:

  Column name(s) to join by. Can be a named vector to allow different
  names:  
  `by = c("col_A" = "col_B")`.

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

- x:

  Numeric vector or data.frame, to convert Oracle Julian days to common
  dates. In case of data frames, only columns with numeric values that
  have Julian days between 1900-01-01 and 2100-01-01 will be converted.

- tz:

  Target time zone.

- n:

  Number of rows to collect.

- message_text:

  Text to show in the console while retrieving data.

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

## Picking Columns with `db$`

Always use the `db` object (a [list](https://rdrr.io/r/base/list.html))
to pick database columns for filtering. It contains all GLIMS column
names. They must **always** be preceded by the injection operator
[`!!`](https://rlang.r-lib.org/reference/injection-operator.html)
(pronounced "bang-bang"). For example:

    get_glims_data(2025, !!db$specimen_name == "Sputum")   # writing `db$` will bring up all columns

    get_glims_data(2025, ORD_ID == 461098)                 # Does not work
    get_glims_data(2025, !!str2lang("ORD_ID") == 461098)   # Works
    get_glims_data(2025, !!db$ORDER_.ORD_ID == 461098)     # Works and easier to write

    con |> build_query(!!db$specimen_name == "Sputum") |> retrieve()

## Query Types

Various query types have been defined:

- `"orders"` (always included): 1 row per order (which can contain
  multiple specimens): info about request (department, ward, room, etc.)

  - `"stays"`: 1 row per patient movement with admissions dates: info
    about every ward admission and room, can be multiple in an order

  - `"results"`: 1 row per result, can be multiple in an order

    - `"isolates"`: 1 row per isolate, can be multiple in a result

      - `"carriers"`: 1 row per carrier (such as an agar plate), can be
        multiple of an isolate

    - `"microscopy"`: 1 row per microscopy result, can be multiple in a
      result

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
#> [1] 2461050

Sys.Date()
#> [1] "2026-01-09"
Sys.Date() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime()
#> [1] "2026-01-09"

Sys.time()
#> [1] "2026-01-09 22:04:32 UTC"
Sys.time() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime()
#> [1] "2026-01-09 23:04:32 CET"
Sys.time() |> datetime_to_oracle_julian() |> oracle_julian_to_datetime(tz = "UTC")
#> [1] "2026-01-09 22:04:32 UTC"
```
