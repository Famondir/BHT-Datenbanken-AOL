library(tidyverse)
library(DBI)

searchinlist <- function(prestatement, list, poststatement) {
  last <- last(list)
  # list <- list[-length(list)]
  length(list) <- length(list)-1
  column <- last(str_split(prestatement, " ")[[1]])
  searchterms <- ""
  
  for (s in list) {
    searchterms <- str_c(searchterms, " like \'", s,"\' or ", column)
  }
  searchterms <- str_c(searchterms, " like \'", last, "\' ")
  
  query = str_c(prestatement, searchterms, poststatement)
  return(query)
}

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

dbListTables(conn)

#### Q1: Wie viele eindeutige Nutzer suchten nach der E3? ####

query <- searchinlist("SELECT * FROM aol_data where query", 
                      c('e3', 'e3 %', '% e3', '% e3 %'),
                      "group by anonid, querytime, clickurl;")
q1_data_a <- dbGetQuery(conn, query) %>% as_tibble()

query <- searchinlist("SELECT * FROM aol_data where query",
                      c('e3', 'e3 %', '% e3', '% e3 %'),
                      "group by anonid, querytime;")
q1_data_a_queries <- dbGetQuery(conn, query) %>% as_tibble()

query <- searchinlist("SELECT count(distinct anonid) FROM aol_data where query", 
                      c('e3', 'e3 %', '% e3', '% e3 %'),
                      "")
anzahl_nutzer_e3_query <- dbGetQuery(conn, query) %>% pull()

# Frage abändern: Wie viele eindeutige Nutzer besuchten Seiten der E3?

query <- searchinlist("SELECT * FROM aol_data where clickurl", 
                      c('%e3expo.com%', '%e3insider.com%'),
                      "group by anonid, querytime, clickurl;")
q1_data_b <- dbGetQuery(conn, query) %>% as_tibble()

query <- searchinlist("SELECT count(distinct anonid) FROM aol_data where clickurl", 
                      c('%e3expo.com%', '%e3insider.com%'),
                      "")
anzahl_nutzer_e3_click <- dbGetQuery(conn, query) %>% pull()

#### Q2 ####

union(q1_data_b, q1_data_a) %>% ggplot() + 
  geom_histogram(aes(x = QueryTime), color = "black") +
  # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
  # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
  annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
                ymin = -10, ymax = 120, fill = "#88FF55", alpha = .4) +
  coord_cartesian(ylim = c(0, 110))

#### Close connection ####
dbDisconnect(conn)
