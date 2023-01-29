library(tidyverse)
library(DBI)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

for (i in games_id$ID[276:338]) {
  print(i)
  data_list[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query, 
  (select * from game_searches where id = {i})
          where query like '%' || lower(game) || '%';")) %>% as_tibble
}

#### Close connection ####
dbDisconnect(conn)