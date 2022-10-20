library(DBI)
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite")
files <- list.files("AOL-user-ct-collection/")

for (i in files) {
  temp_data <- rio::import(str_c("AOL-user-ct-collection/", i))
  dbWriteTable(conn, "aol_data", temp_data, append = TRUE)
}

dbListTables(conn)
# dbRemoveTable(conn, "aol_data")

dbDisconnect(conn)
