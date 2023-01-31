library(tidyverse)
library(DBI)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

for (i in seq_along(publisher$ID)) {
  print(i)
  data_list_publisher[[i]] <- dbGetQuery(conn, glue::glue("Select * from aol_data where
    clickurl like '%' || (select url from website where id = {i}) || '%'
    or query like lower('%{publisher$publisher[i]}%');")) %>% as_tibble %>% 
    mutate(publisherquery = publisher$publisher[i], 
           websitequery = website$url[publisher$WebsiteID[i]])
}
data_list_publisher %>% saveRDS("data/potentialpublishersearchqueries.rds")

#### Close connection ####
dbDisconnect(conn)