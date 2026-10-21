# `mmbi.epi`

This is a UMCG R package containing helper functions for functionalities
used in their department of Medical Microbiology, unit Epidemiology &
Data Science.

Upon loading, official UMCG colours are available too:

``` r

library(mmbi.epi)

get_colour("umcgdonkerblauw")
#> [1] "#003183"

get_colour("umcgoranje")
#> [1] "#FF7D00"

get_colour("umcg", 2)
#> [1] "#003183"  "#FF7D00"
```

## Progress OpenEdge (JDBC)

`connect_db("ProgressJ")` connects to a Progress OpenEdge database
through JDBC. On Posit Workbench this is the supported route: the
OpenEdge ODBC driver (`connect_db("Progress")`) needs Linux driver files
that we don’t have, because the database servers run on Windows. The
JDBC driver is a single, platform-independent `.jar` file.

### Requirements

- **Java (JDK)** and the R packages **rJava** and **RJDBC**. If
  `install.packages("rJava")` fails on Workbench, see
  dev/install-rjava.R.
- **`openedge.jar`**, from `%DLC%\java` on the OpenEdge server. Ask the
  database administrator for a copy that matches the database version,
  and store it in your home directory, for example
  `~/progress/openedge.jar`.
- **dbplyr \>= 2.6.0**, for the OpenEdge SQL dialect (see below).
- Network access from Workbench to the **SQL broker port** of the
  database. This is often a different port from the ABL/4GL broker. Test
  it with `nc -zv <host> <port>` in the Terminal.

### Configuration

Add the following to `~/.Renviron` and restart R:

    JAVA_HOME=/usr/lib/jvm/java-17-openjdk
    PROGRESS_JAR=${HOME}/progress/openedge.jar
    PROGRESS_HOST=dbserver.example.local
    PROGRESS_PORT=20931
    PROGRESS_DB=mydb
    PROGRESS_USER=sqluser
    PROGRESS_PASS=secret

Set `JAVA_HOME` to the JDK that was used to install rJava.

### Usage

``` r

conn <- connect_db("ProgressJ")

# Plain SQL: use TOP instead of LIMIT, and quote table names
DBI::dbGetQuery(conn, 'SELECT TOP 5 * FROM <schema>."<tablename>"')

# dbplyr
dplyr::tbl(conn, dbplyr::in_schema("<schema>", "<tablename>")) |>
  head(5) |>
  dplyr::collect()
```

Application tables are in the `<schema>` schema. Always refer to tables
as `<schema>."TableName"` (with
[`dbplyr::in_schema()`](https://dbplyr.tidyverse.org/reference/in_schema.html)
in dbplyr code): table names with hyphens or reserved words fail without
quotes.

### What `connect_db("ProgressJ")` does

- **Read uncommitted:** it sets the transaction isolation level to
  `READ UNCOMMITTED`, so read queries don’t lock records that
  application users are working on.
- **OpenEdge SQL dialect:** it attaches an OpenEdge dialect to the
  connection with
  [`dbplyr::with_dialect()`](https://dbplyr.tidyverse.org/reference/with_dialect.html).
  dbplyr does not recognise OpenEdge and would otherwise fall back to
  generic SQL with `LIMIT n`, which OpenEdge rejects. The dialect
  (`R/openedge-dialect.R`) builds on dbplyr’s ODBC dialect and only
  changes row limits to `SELECT TOP n`.
- **Loading the JVM:** before starting the JDBC driver, it loads
  `libjvm.so` from `JAVA_HOME` with
  [`dyn.load()`](https://rdrr.io/r/base/dynload.html). R’s Java
  configuration on Workbench still points to
  `/usr/lib/jvm/jre-11-openjdk`, which no longer exists, so rJava can’t
  find the JVM on its own. The line can be removed once an administrator
  has run `sudo R CMD javareconf` on the server.

The dialect uses a few internal dbplyr functions. After updating dbplyr,
run the package tests, or check that `show_query()` still shows `TOP`
instead of `LIMIT`.

### Troubleshooting

| Error | Cause |
|----|----|
| `java.lang.ClassNotFoundException` | `PROGRESS_JAR` is empty, or the file doesn’t exist. Check with `file.exists(Sys.getenv("PROGRESS_JAR"))`. |
| `Table/View/Synonym not found (7519)` | Wrong schema, or an unquoted table name. Use `<schema>."TableName"`. |
| `Syntax error in SQL statement at or about " 5" (10713)` | The query uses `LIMIT`. Use `TOP n` in SQL, and check that the dialect is attached in dbplyr code. |
| `No ResultSet set was produced` | A statement that returns no rows (`SET`, `UPDATE`, …) was sent with `dbGetQuery()`. Use [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) (dbplyr \>= 2.6.0) or [`RJDBC::dbSendUpdate()`](https://rdrr.io/pkg/RJDBC/man/JDBCConnection-methods.html). |
| `libjvm.so: cannot open shared object file` | `JAVA_HOME` is not set or points to a directory without `lib/server/libjvm.so`. Check `JAVA_HOME` in `~/.Renviron` and restart R. |

# GLIMS read support

Should work for any GLIMS v9.\* and v10.\*, if an ODBC connection is
available.

``` r

library(mmbi.epi)

conn <- connect_db()

# download first 500 rows
first_500 <- conn |>
  glims_tbl("ANTIBIOTICRESULT", schema = "<schema>") |>
  retrieve(500)

# joins
conn |>
  glims_tbl("REQUEST") |>
  glims_join_tbl("ORDER_", by = c("RQST_ORDER" = "ORD_ID")) |>
  glims_join_tbl("SPECIMEN", by = c("RQST_SPECIMEN" = "SPMN_ID")) |>
  glims_join_tbl("MATERIAL", by = c("SPMN_MATERIAL" = "MAT_ID")) |>
  show_query()
```
