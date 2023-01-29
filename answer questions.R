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

generate_searchqueries <- function(list) {
  search_queries <- list %>% filter(str_length(Query)>1) %>% unique()
  search_queries_sql <- search_queries %>% mutate(q1 = str_c(Query),
                                                  q2 = str_c("% ", Query),
                                                  q3 = str_c("% ", Query, " %"),
                                                  q4 = str_c(Query, " %")
                                                  ) %>%
    pivot_longer(cols = -Query) %>% pull(value)
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

# liefert 348 Personen:
# search_queries <- q1_data_b %>% select(Query) %>% filter(str_length(Query)>1) %>% unique()
# search_queries_sql <- search_queries %>% mutate(q1 = str_c(Query),
#                                                 q2 = str_c("% ", Query),
#                                                 q3 = str_c("% ", Query, " %"),
#                                                 q4 = str_c(Query, " %")
#                                                 ) %>% 
#   pivot_longer(cols = -Query) %>% pull(value)
# 
# query <- searchinlist("SELECT * FROM aol_data where query", 
#                       search_queries_sql,
#                       "group by anonid, querytime, clickurl;")
# q1_data_c <- dbGetQuery(conn, query) %>% as_tibble()

#### Q2 ####

union(q1_data_b, q1_data_a) %>% ggplot() + 
  geom_histogram(aes(x = QueryTime), color = "black") +
  # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
  # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
  annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
                ymin = -10, ymax = 120, fill = "#88FF55", alpha = .4) +
  coord_cartesian(ylim = c(0, 110))

#### Q3: Wie viele Nutzer suchten nach Titeln, die auf der E3 vertreten waren? ####

dbExecute(conn, "create view distinct_query as 
          Select anonid, querytime, query From aol_data 
          group by anonid, querytime;")
dbExecute(conn, "create view game_searches as  
          Select id, game from game
          union
          select gameid as id, alias as game from gamealias")
# zu rechenintensiv
# data_q3 <- dbGetQuery(conn, "Select * from distinct_query, game_searches
#            where query like '%' || lower(game) || '%';") %>% as_tibble

games <- dbGetQuery(conn, "select id, game from game")
games_id <- dbGetQuery(conn, "select id from game")

# data_list <- list()
for (i in games_id$ID[101:200]) {
  print(i)
  data_list[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query, 
  (select * from game_searches where id = {i})
          where query like '%' || lower(game) || '%';")) %>% as_tibble
}
data_list %>% saveRDS("data/potentialgamesearchqueries.rds")

# data_list2 <- list()
for (i in games_id$ID[1:10]) {
  print(i)
  data_list2[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query, 
  (select * from game_searches where id = {i})
          where query like '% ' || lower(game) || ' %'
          or query like '% ' || lower(game)
          or query like lower(game) || ' %'
          or query like lower(game);")) %>% as_tibble
}
data_list2 %>% saveRDS("data/potentialgamesearchqueries2.rds")

# # nichts zu sehen
# sims2_data <- dbGetQuery(conn, "Select * from distinct_query where query like '% sims 2 %'
#            or query like 'sims 2 %' or query like '% sims 2' or query like 'sims 2';") %>% as_tibble
# 
# sims2_data %>% ggplot() + 
#   geom_histogram(aes(x = QueryTime), color = "black") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
#   annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
#            ymin = -10, ymax = 250, fill = "#88FF55", alpha = .4) +
#   coord_cartesian(ylim = c(0, 200))

# peak ab reveal
# wii_data <- dbGetQuery(conn, "Select * from distinct_query where query like '% wii %'
#            or query like 'wii %' or query like '% wii' or query like 'wii';") %>% as_tibble

# wii_data %>% ggplot() + 
#   geom_histogram(aes(x = QueryTime), color = "black") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
#   annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
#            ymin = -10, ymax = 250, fill = "#88FF55", alpha = .4) +
#   coord_cartesian(ylim = c(0, 200))

# ggp <- wii_data %>% ggplot() + 
#   geom_histogram(aes(x = QueryTime))
# ggp_build <- ggplot_build(ggp)
# ymax <- max(ggp_build$data[[1]]$count)
# 
# g <- wii_data %>% ggplot() + 
#   geom_histogram(aes(x = QueryTime), color = "black") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
#   annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
#            ymin = -20, ymax = ymax+100, fill = "#88FF55", alpha = .4) +
#   coord_cartesian(ylim = c(0, ymax+5), 
#                   xlim = c(as.POSIXct("2006-03-01"), as.POSIXct("2006-06-01"))) +
#   labs(title = "Wii")
# 
# g %>% ggsave(filename = glue::glue("plots/q4/single/Wii.pdf"), plot = ., width = 25, height = 19, units = "cm")  

# # nicht zu sehen
# revolution_data <- dbGetQuery(conn, "Select * from distinct_query where query like '% revolution %'
#            or query like 'revolution %' or query like '% revolution' or query like 'revolution';") %>% as_tibble
# 
# revolution_data %>% ggplot() + 
#   geom_histogram(aes(x = QueryTime), color = "black") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
#   # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
#   annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
#            ymin = -10, ymax = 250, fill = "#88FF55", alpha = .4) +
#   coord_cartesian(ylim = c(0, 200))

for (i in games_id$ID) {
  if (!is.null(data_list[[i]]) && nrow(data_list[[i]]) > 0) {
    print(games %>% filter(ID == i) %>% pull(game) %>% str_remove('//'))
    ggp <- data_list[[i]] %>% ggplot() + 
      geom_histogram(aes(x = QueryTime))
    ggp_build <- ggplot_build(ggp)
    ymax <- max(ggp_build$data[[1]]$count)
    
    g <- data_list[[i]] %>% ggplot() + 
      geom_histogram(aes(x = QueryTime), color = "black") +
      # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
      # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
      annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
               ymin = -20, ymax = ymax+100, fill = "#88FF55", alpha = .4) +
      coord_cartesian(ylim = c(0, ymax+5), 
                      xlim = c(as.POSIXct("2006-03-01"), as.POSIXct("2006-06-01"))) +
      labs(title = games %>% filter(ID == i) %>% pull(game)) +
      facet_grid(~game)
    
    g %>% ggsave(filename = glue::glue("plots/q4/single/{games %>% filter(ID == i) %>% pull(game) %>% str_remove('//') %>% str_replace(':', '-')}.pdf"), plot = ., width = 25, height = 19, units = "cm")  
  }
}

for (i in games_id$ID) {
  if (!is.null(data_list[[i]]) && nrow(data_list[[i]]) > 0) {
    print(games %>% filter(ID == i) %>% pull(game) %>% str_remove('//'))
    if (data_list[[i]] %>% filter(str_length(game) <= 3) %>% nrow() > 0) {
      message(data_list[[i]] %>% filter(str_length(game) <= 3) %>% pull(game) %>% unique())
      if (data_list[[i]] %>% filter(str_length(game) > 3) %>% nrow() == 0) next
      
      ggp <- data_list[[i]] %>% filter(str_length(game) > 2) %>% ggplot() + 
        geom_histogram(aes(x = QueryTime))
      ggp_build <- ggplot_build(ggp)
      ymax <- max(ggp_build$data[[1]]$count)
      
      g <- data_list[[i]] %>% filter(str_length(game) > 3) %>% ggplot() + 
        geom_histogram(aes(x = QueryTime), color = "black") +
        # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
        # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
        annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
                 ymin = -20, ymax = ymax+100, fill = "#88FF55", alpha = .4) +
        coord_cartesian(ylim = c(0, ymax+5), 
                        xlim = c(as.POSIXct("2006-03-01"), as.POSIXct("2006-06-01"))) +
        labs(title = games %>% filter(ID == i) %>% pull(game)) +
        facet_grid(~game)
      
      g %>% ggsave(filename = glue::glue("plots/q4/single long names/{games %>% filter(ID == i) %>% pull(game) %>% str_remove('//') %>% str_replace(':', '-')}.pdf"), plot = ., width = 25, height = 19, units = "cm")    
    }
  }
}

for (i in games_id$ID) {
  if (!is.null(data_list2[[i]]) && nrow(data_list2[[i]]) > 0) {
    print(games %>% filter(ID == i) %>% pull(game) %>% str_remove('//'))
    ggp <- data_list2[[i]] %>% ggplot() + 
      geom_histogram(aes(x = QueryTime))
    ggp_build <- ggplot_build(ggp)
    ymax <- max(ggp_build$data[[1]]$count)
    
    g <- data_list2[[i]] %>% ggplot() + 
      geom_histogram(aes(x = QueryTime), color = "black") +
      # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
      # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
      annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
               ymin = -20, ymax = ymax+100, fill = "#88FF55", alpha = .4) +
      coord_cartesian(ylim = c(0, ymax+5), 
                      xlim = c(as.POSIXct("2006-03-01"), as.POSIXct("2006-06-01"))) +
      labs(title = games %>% filter(ID == i) %>% pull(game)) +
      facet_grid(~game)
    
    g %>% ggsave(filename = glue::glue("plots/q4/single/{games %>% filter(ID == i) %>% pull(game) %>% str_remove('//') %>% str_replace(':', '-')}_filtered.pdf"), plot = ., width = 25, height = 19, units = "cm")  
  }
}

platforms <- dbGetQuery(conn, "select * from platform")

# data_list3 <- list()
for (i in seq_along(platforms$platform)) {
  print(i)
  data_list3[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query
          where query like lower('%{platforms$platform[i]}%');")) %>% as_tibble
}
data_list3 %>% saveRDS("data/potentialgamesearchqueries3.rds")

for ( i in platforms$id) {
  if (nrow(data_list3[[i]]) > 0) {
    ggp <- data_list3[[i]] %>% ggplot() + 
      geom_histogram(aes(x = QueryTime))
    ggp_build <- ggplot_build(ggp)
    ymax <- max(ggp_build$data[[1]]$count)
    
    g <- data_list3[[i]] %>% ggplot() + 
      geom_histogram(aes(x = QueryTime), color = "black") +
      # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
      # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
      annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
               ymin = -20, ymax = ymax+100, fill = "#88FF55", alpha = .4) +
      coord_cartesian(ylim = c(0, ymax+5), 
                      xlim = c(as.POSIXct("2006-03-01"), as.POSIXct("2006-06-01"))) +
      labs(title = platforms$platform[[i]])
    
    g %>% ggsave(filename = glue::glue("plots/platforms/single/{platforms$platform[[i]]}.pdf"), plot = ., width = 25, height = 19, units = "cm")  
  }
}

#### Close connection ####
dbDisconnect(conn)
