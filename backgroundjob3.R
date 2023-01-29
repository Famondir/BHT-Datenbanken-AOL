library(tidyverse)
library(DBI)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

for (i in seq_along(platforms$platform)) {
  print(i)
  data_list3[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query
          where query like lower('%{platforms$platform[i]}%');")) %>% as_tibble
}

#### Close connection ####
dbDisconnect(conn)