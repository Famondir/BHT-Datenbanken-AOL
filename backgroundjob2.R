library(tidyverse)
library(DBI)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

for (i in games_id$ID[101:200]) {
  print(i)
  if (!is.null(data_list[[i]]) && data_list[[i]] %>% nrow() > 0) {
    data_list2[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query, 
          (select * from game_searches where id = {i})
          where query like '% ' || lower(game) || ' %'
          or query like '% ' || lower(game)
          or query like lower(game) || ' %'
          or query like lower(game);")) %>% as_tibble 
  } else {
    data_list2[[i]] <- data_list[[i]]
  }
}

#### Close connection ####
dbDisconnect(conn)