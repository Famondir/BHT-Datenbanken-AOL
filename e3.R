library(tidyverse)
library(DBI)
# library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite")

dbGetQuery(conn, 'SELECT * FROM aol_data LIMIT 5')
e3_website_visitors <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE ClickURL LIKE "%e3expo%"')

temp_data <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE Query LIKE "%empire state building%"')
t <- temp_data$Query %>% unique() %>% as_tibble()
# 22748 könnte ein deutscher Tourist sein (sucht Deutsche Bank in New York)

dbDisconnect(conn)
