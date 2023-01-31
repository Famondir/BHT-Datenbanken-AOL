library(tidyverse)
library(DBI)

#### define functions ####

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

saveplot <- function(df, name, path, facet = NULL) {
  ggp <- df %>% ggplot() + 
    geom_histogram(aes(x = QueryTime))
  ggp_build <- ggplot_build(ggp)
  ymax <- max(ggp_build$data[[1]]$count)
  
  g <- df %>% ggplot() + 
    geom_histogram(aes(x = QueryTime), color = "black") +
    # geom_vline(xintercept = as.POSIXct("2006-05-10"), color = "#88FF55") +
    # geom_vline(xintercept = as.POSIXct("2006-05-12"), color = "#88FF55") +
    annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
             ymin = -20, ymax = ymax+100, fill = "#88FF55", alpha = .4) +
    coord_cartesian(ylim = c(0, ymax+5), 
                    xlim = c(as.POSIXct("2006-03-01"), as.POSIXct("2006-06-01"))) +
    labs(title = name)
  if (!is.null(facet)) {
    g <- g + facet_grid(cols = vars((!!facet)))
  }
  
  g %>% ggsave(filename = str_c(path, glue::glue("{name %>% str_remove('//') %>% str_replace(':', '-')}.pdf")), plot = ., width = 25, height = 19, units = "cm")  
}

#### connect to DB ####

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

dbListTables(conn)

#### create and save data (long runtime only) ####

# data_list_game <- list()
# for (i in games_id$ID) {
#   print(i)
#   data_list_game[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query, 
#   (select * from game_searches where id = {i})
#           where query like '%' || lower(game) || '%';")) %>% as_tibble
# }
# 
# data_list_game %>% saveRDS("data/potentialgamesearchqueries.rds")
# 
# data_list_game_stronger_filter <- list()
# for (i in games_id$ID) {
#   print(i)
#   data_list_game_stronger_filter[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query, 
#   (select * from game_searches where id = {i})
#           where query like '% ' || lower(game) || ' %'
#           or query like '% ' || lower(game)
#           or query like lower(game) || ' %'
#           or query like lower(game);")) %>% as_tibble
# }
# data_list_game_stronger_filter %>% saveRDS("data/potentialgamesearchqueries2.rds")
# 
# data_list_platform <- list()
# for (i in seq_along(platforms$platform)) {
#   print(i)
#   data_list_platform[[i]] <- dbGetQuery(conn, glue::glue("Select * from distinct_query
#           where query like lower('%{platforms$platform[i]}%');")) %>% as_tibble
# }
# data_list_platform %>% saveRDS("data/potentialplatformsearchqueries.rds")

# Arbeit auf distinc_query sehr langsam, da keine materialized view und über ganz AOL aggregiert wurde
# data_list_publisher <- list()
# for (i in seq_along(publisher$ID)) {
#   print(i)
#   data_list_publisher[[i]] <- dbGetQuery(conn, glue::glue("Select * from aol_data where
#     clickurl like '%' || (select url from website where id = {i}) || '%'
#     or query like lower('%{publisher$publisher[i]}%');")) %>% as_tibble %>% 
#     mutate(publisherquery = publisher$publisher[i], 
#            websitequery = website$url[publisher$WebsiteID[i]])
# }
# data_list_publisher %>% saveRDS("data/potentialpublishersearchqueries.rds")

#### read data ####

games <- dbGetQuery(conn, "select id, game from game")
games_id <- dbGetQuery(conn, "select id from game")
platforms <- dbGetQuery(conn, "select * from platform")
publisher <- dbGetQuery(conn, "select * from publisher")
website <- dbGetQuery(conn, "select * from website")

data_list_game <- readRDS("data/potentialgamesearchqueries.rds")
data_list_game_stronger_filter <- readRDS("data/potentialgamesearchqueries2.rds")
data_list_platform <- readRDS("data/potentialplatformsearchqueries.rds")
data_list_publisher <- readRDS("data/potentialpublishersearchqueries.rds")

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
  annotate("rect", xmin = as.POSIXct("2006-05-10"), xmax = as.POSIXct("2006-05-12"), 
                ymin = -10, ymax = 120, fill = "#88FF55", alpha = .4) +
  coord_cartesian(ylim = c(0, 110))

union(q1_data_b, q1_data_a) %>% saveplot("E3", "plots/q2/")

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

extract_distinct_ids <- function(l) {
  if (is.null(l)) return(c())
  else {
    l %>% filter(str_length(game) > 2, 
                 str_to_lower(game) != "cars",
                 str_to_lower(game) != "contact",
                 str_to_lower(game) != "eba",
                 str_to_lower(game) != "eee",
                 str_to_lower(game) != "fear",
                 str_to_lower(game) != "haze",
                 str_to_lower(game) != "lair",
                 str_to_lower(game) != "msa",
                 str_to_lower(game) != "mp3",
                 str_to_lower(game) != "over the hedge",
                 str_to_lower(game) != "resistance",
                 str_to_lower(game) != "scarface",
                 str_to_lower(game) != "superman returns",
                 str_to_lower(game) != "the club",
                 str_to_lower(game) != "the da vinci code",
                 str_to_lower(game) != "the darkness",
                 str_to_lower(game) != "wic",
                 ) %>% pull(AnonID) %>% unique()
  }
}

# ergibt 58487 unique ids
distinct_ids <- map(data_list_game, extract_distinct_ids) %>% unlist() %>% unique()
# ergibt 2200 unique IDs
distinct_ids_games <- map(data_list_game_stronger_filter, extract_distinct_ids) %>% unlist() %>% unique()

#### Q4: Wie sieht die zeitliche Verteilung der Suchanfragen aus Frage 3 aus? ####

for (i in games_id$ID) {
  if (!is.null(data_list_game[[i]]) && nrow(data_list_game[[i]]) > 0) {
    print(games %>% filter(ID == i) %>% pull(game) %>% str_remove('//'))
    
    saveplot(data_list_game[[i]], games %>% filter(ID == i) %>% pull(game), "plots/q4/single/", quo(game))
  }
}

for (i in games_id$ID) {
  if (!is.null(data_list_game[[i]]) && nrow(data_list_game[[i]]) > 0) {
    print(games %>% filter(ID == i) %>% pull(game) %>% str_remove('//'))
    if (data_list_game[[i]] %>% filter(str_length(game) <= 3) %>% nrow() > 0) {
      message(data_list_game[[i]] %>% filter(str_length(game) <= 3) %>% pull(game) %>% unique())
      if (data_list_game[[i]] %>% filter(str_length(game) > 3) %>% nrow() == 0) next
      
      saveplot(data_list_game[[i]] %>% filter(str_length(game) > 3), 
               games %>% filter(ID == i) %>% pull(game), "plots/q4/single long names/", quo(game)
               )
      
    }
  }
}

for (i in games_id$ID) {
  if (!is.null(data_list_game_stronger_filter[[i]]) && nrow(data_list_game_stronger_filter[[i]]) > 0) {
    print(games %>% filter(ID == i) %>% pull(game) %>% str_remove('//'))
    
    saveplot(data_list_game_stronger_filter[[i]], games %>% filter(ID == i) %>% pull(game), 
             "plots/q4/single stronger filter/", quo(game)
             )
  }
}


#### Q4: Handarbeit ####
ac_data <- dbGetQuery(conn, "select * from distinct_query where query like ('%assassin%creed%')") # 31 Treffer
ac_data_auto <- data_list_game_stronger_filter[[321]] # 2 Treffer und 3394 false positives

saveplot(ac_data, "Assassins Creed manual", "plots/q4/manual/")

#### Q5: Wie groß ist die Schnittmenge zwischen den Nutzern aus Frage 1 und Frage 3? ####

distinct_ids_e3 <- union(q1_data_b, q1_data_a) %>% pull(AnonID) %>% unique()

length(intersect(distinct_ids, distinct_ids_e3)) # 111 aus 265 und 58487
length(intersect(distinct_ids_games, distinct_ids_e3)) # 43 aus 256 und 2200

#### Q6: Was suchten diese Leute (Frage 1) sonst charakteristisches? ####

query1 <- searchinlist("SELECT anonid FROM aol_data where query",
                      c('e3', 'e3 %', '% e3', '% e3 %'),
                      "group by anonid")

query2 <- searchinlist("SELECT anonid FROM aol_data where clickurl", 
                      c('%e3expo.com%', '%e3insider.com%'),
                      "group by anonid")

query_rank_e3_user <- dbGetQuery(conn, glue::glue("select query, count(query) as querycount from distinct_query 
                                               where anonid in ({query1} union {query2}) 
                                                  group by query order by querycount desc;")) %>% as_tibble()

# query_rank_non_e3_user <- dbGetQuery(conn, glue::glue("select query, count(query) as querycount from distinct_query 
#                                                where not (anonid in ({query1} union {query2}))
#                                                   group by query order by querycount desc limit 33000;")) %>% as_tibble()
# 
# perc_queries <- query_rank_e3_user %>% rename(qc_e3 = querycount) %>% full_join(
#   query_rank_non_e3_user %>% rename(qc_non_e3 = querycount)
# ) %>% mutate_all(~ifelse(is.na(.x), 0, .x)) %>% 
#   mutate(Query = str_remove(Query, "www.") %>% str_remove("http") %>% str_remove(".com") %>% 
#            str_remove_all("'") %>% str_remove_all("-") %>% 
#            str_remove_all(" ") %>% str_remove_all("\\.")) %>% 
#   filter(Query != "-", Query != "", Query != " ") %>% 
#   group_by(Query) %>% 
#   summarise(qc_e3 = sum(qc_e3), qc_non_e3 = sum(qc_non_e3)) %>% 
#   mutate(qc_e3 = qc_e3/sum(qc_e3)*100, qc_non_e3 = qc_non_e3/sum(qc_non_e3)*100)

query_rank_e3_user_cleaned <- query_rank_e3_user %>% mutate(Query = str_remove(Query, "www.") %>% 
                                str_remove("http") %>% str_remove(".com") %>% 
                                str_remove_all("'") %>% str_remove_all("-") %>%
                                str_remove_all(" ") %>% str_remove_all("\\.")) %>%
  filter(Query != "-", Query != "", Query != " ")

wordcloud::wordcloud(words = query_rank_e3_user_cleaned$Query, 
                     freq = query_rank_e3_user_cleaned$querycount, min.freq = 5,           
                     max.words=500, random.order=FALSE, rot.per=0.35,            
                     colors=RColorBrewer::brewer.pal(8, "Dark2"))

wordcloud2::wordcloud2(data = query_rank_e3_user_cleaned)

#### Q7: Was unterscheidet sie (Frage 1)  in ihrem Suchverhalten von anderen Nutzern? ####

query_rank_non_e3_user2 <- dbGetQuery(conn, glue::glue("select query, count(query) as querycount from distinct_query 
                                               where not (anonid in ({query1} union {query2}))
                                                  group by query order by querycount desc;")) %>% as_tibble()

# perc_queries2 <- query_rank_e3_user %>% rename(qc_e3 = querycount) %>% full_join(
#   query_rank_non_e3_user2 %>% rename(qc_non_e3 = querycount)
# ) %>% mutate_all(~ifelse(is.na(.x), 0, .x)) %>% 
#   mutate(Query = str_remove(Query, "www.") %>% str_remove("http") %>% str_remove(".com") %>% 
#            str_remove_all("[[^a-zA-Z0-9\u00C0-\u00FF]]")) %>% 
#   filter(Query != "-", Query != "", Query != " ") %>% 
#   group_by(Query) %>% 
#   summarise(qc_e3 = sum(qc_e3), qc_non_e3 = sum(qc_non_e3)) %>% 
#   mutate(qc_e3 = qc_e3/sum(qc_e3)*100, qc_non_e3 = qc_non_e3/sum(qc_non_e3)*100)
# perc_queries2 %>% saveRDS("data/perc_searchqueries.rds")

perc_queries2 <- saveRDS("data/perc_searchqueries.rds")

# perc_queries2 %>% arrange(desc(qc_e3)) %>% slice(1:100) %>% ggplot() +
#   geom_point(aes(x = qc_e3, y = qc_non_e3)) +
#   geom_label(aes(x = qc_e3, y = qc_non_e3, label = Query))
# 
# perc_queries2 %>% arrange(desc(qc_non_e3)) %>% slice(1:100) %>% ggplot() +
#   geom_point(aes(x = qc_e3, y = qc_non_e3)) +
#   geom_label(aes(x = qc_e3, y = qc_non_e3, label = Query))

search_compare_plot <- perc_queries2 %>% pivot_longer(cols = -Query) %>% arrange(desc(value)) %>% 
  group_by(name) %>% slice_head(n = 25) %>%
  ggplot() +
  geom_col(aes(x = value, y = forcats::fct_reorder(Query, value), fill = name), 
           position = position_dodge2(preserve = "single")) +
  labs(title = "Vergleich der top 25 Suchanfragenhäufigkeiten",
       subtitle = "zwischen e3-Interessierten und anderen") +
  xlab("Anteil in % (also unter 1 %)") +
  ylab("Suchanfrage") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "e3-Interessiert", labels = c("Ja", "Nein"))

search_compare_plot %>% ggsave(path = "plots/q7/", filename = "Vergleich der top 25 Suchanfragenhäufigkeiten.pdf", units = "cm", width = 25, height = 19)

#### Q8: Suchten die Nutzer auch nach Google? ####
google_data <- dbGetQuery(conn, "select * from aol_data where query like ('%google%')")
google_data_distinc_queries <- google_data %>% select(-ItemRank, -ClickURL) %>% unique()

norm_n <- google_data_distinc_queries %>% mutate(e3 = ifelse(AnonID %in% distinct_ids_e3, TRUE, FALSE)) %>% 
  group_by(e3) %>% summarise(n = n())
google_count_data <- google_data_distinc_queries %>% mutate(e3 = ifelse(AnonID %in% distinct_ids_e3, TRUE, FALSE)) %>% 
  mutate(QueryTime = as.Date(QueryTime)) %>% 
  group_by(e3, QueryTime) %>% summarise(count = n()) %>% 
  mutate(norm_count = ifelse(e3, count/norm_n$n[[2]], count/norm_n$n[[1]])) 

google_plot <- google_count_data %>% ggplot(aes(fill = e3, x = QueryTime, y = norm_count)) +
  geom_col(alpha = .5, position = position_identity()) +
  ylab("count per day divided by number of groupmembers") +
  annotate("rect", xmin = as.Date(as.POSIXct("2006-05-10")), xmax = as.Date(as.POSIXct("2006-05-12")), 
           ymin = -20, ymax = 100, fill = "#88FF55", alpha = .4) +
  coord_cartesian(ylim = c(0, 0.06), 
                  xlim = c(as.Date(as.POSIXct("2006-03-01")), as.Date(as.POSIXct("2006-06-01"))))

google_plot %>% ggsave(path = "plots/q8/", filename = "Google.pdf", units = "cm", width = 25, height = 19)

#### Q9: Über welche Anfragen landeten die Nutzer bei der E3 Seite (über Umwege)? ####

q1_data_b %>% filter(!str_detect(Query, "e3|e 3|electronic entertainment expo|electronic expo"))

#### Q10: Hatte die E3 einen Einfluss auf Suchanfragen zu LA? ####
la_data <- dbGetQuery(conn, "select * from aol_data where query like '%los angeles%'
                      or query like 'la' or query like '% la %'
                      or query like 'la %' or query like '% la'")
la_data <- la_data %>% mutate(LosAngeles = ifelse(str_detect(Query, "los angeles"), "Los Angeles", "LA"))
la_data %>% filter(str_detect(Query, "e3")) # einer suchte LA und e3! Man könnte sich aber passende peaks einbilden

saveplot(la_data, "Los Angeles", "plots/q10/", quo(LosAngeles))

#### Bonusfrage: Suchverteilung nach Plattformen ####

for ( i in platforms$id) {
  if (nrow(data_list_platform[[i]]) > 0) {
    saveplot(data_list_platform[[i]], platforms$platform[[i]], "plots/platforms/single/")
    }
}

#### Handarbeit ####
ps3_data <- dbGetQuery(conn, "select * from distinct_query 
                       where query like '%ps 3%' or query like '%ps3%'")

saveplot(ps3_data, "PS3", "plots/platforms/manual/")

#### Bonusfrage 2: Suchverteilung nach Publishern ####

for ( i in publisher$ID) {
  if (nrow(data_list_publisher[[i]]) > 0) {
    saveplot(data_list_publisher[[i]], publisher$publisher[[i]], "plots/publisher/single/")
  }
}

#### Close connection ####
dbDisconnect(conn)
