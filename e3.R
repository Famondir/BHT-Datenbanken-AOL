# library(tidyverse)
library(DBI)
# library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite")

dbGetQuery(conn, 'SELECT * FROM aol_data LIMIT 5')
dbGetQuery(conn, 'SELECT * FROM aol_data WHERE ClickURL LIKE "%e3e%"')

dbDisconnect(conn)
