library(tidyverse)
library(DBI)
# library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)
files <- list.files("AOL-user-ct-collection/")

for (i in files) {
  temp_data <- rio::import(str_c("AOL-user-ct-collection/", i))
  dbWriteTable(conn, "aol_data", temp_data, append = TRUE)
}

sales_data <- rio::import("vgsales.csv") %>% mutate_if(is.numeric, ~.*1000000)
dbWriteTable(conn, "game_sales", sales_data)

ydbDataType()

dbListTables(conn)
# dbRemoveTable(conn, "aol_data")

dbDisconnect(conn)
