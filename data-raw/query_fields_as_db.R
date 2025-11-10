
# Create a list for this package with all fields that can be queried

library(dplyr)

readable_col_names <- read.csv(system.file("readable_column_names.csv", package = "mmbi.epi"), strip.white = TRUE)
full_table_overview <- read.delim("data-raw/full_table_overview.txt") |>
  filter(TABLE_NAME %in% c("REQUEST",
                           "RESULT",
                           "ORDER_",
                           "ENCOUNTER",
                           "DEPARTMENT",
                           "PERSON",
                           "SPECIMEN",
                           "MATERIAL",
                           "STAY",
                           "WARD",
                           "SPECIALISM",
                           "HCPROVIDER",
                           "RESULTOUTPUT",
                           "SC_USER",
                           "PROPERTYOUTPUT",
                           "PROCEDUREOUTPUT",
                           "PROPERTY",
                           "PROCEDURE_",
                           "STATION",
                           "WORKPLACE",
                           "CHOICE")) |>
  left_join(readable_col_names, by = "COLUMN_NAME") |>
  mutate(nm = paste0(TABLE_NAME, ".", COLUMN_NAME))

part1 <- lapply(full_table_overview$COLUMN_NAME, str2lang)
names(part1) <- full_table_overview$nm

part2 <- lapply(full_table_overview$COLUMN_NAME[!is.na(full_table_overview$NEW_COLUMN_NAME)], str2lang)
names(part2) <- full_table_overview$NEW_COLUMN_NAME[!is.na(full_table_overview$NEW_COLUMN_NAME)]

db <- c(part2, part1)

saveRDS(db, "inst/db.rds")
