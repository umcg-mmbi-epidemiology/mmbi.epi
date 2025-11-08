# `mmbi.epi`

This is a UMCG R package containing helper functions for functionalities used in their department of Medical Microbiology, unit Epidemiology & Data Science.

Upon loading, official UMCG colours are available too:

```r
library(mmbi.epi)

get_colour("umcgdonkerblauw")
#> [1] "#003183"

get_colour("umcgoranje")
#> [1] "#FF7D00"

get_colour("umcg", 2)
#> [1] "#003183"  "#FF7D00"
```

# GLIMS read support

Should work for any GLIMS v9.* and v10.*, if an ODBC connection is available.

```r
library(mmbi.epi)

conn <- connect_db()

# download first 500 rows
first_500 <- conn |>
  glims_tbl("ANTIBIOTICRESULT") |>
  retrieve(500)

# joins
conn |>
  glims_tbl("REQUEST") |>
  glims_join_tbl("ORDER_", by = c("RQST_ORDER" = "ORD_ID")) |>
  glims_join_tbl("SPECIMEN", by = c("RQST_SPECIMEN" = "SPMN_ID")) |>
  glims_join_tbl("MATERIAL", by = c("SPMN_MATERIAL" = "MAT_ID")) |>
  show_query()
```

