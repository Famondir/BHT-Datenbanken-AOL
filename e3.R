library(tidyverse)
library(DBI)
# library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite")

dbGetQuery(conn, 'SELECT * FROM aol_data LIMIT 5')
e3_website_visitors <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE ClickURL LIKE "%e3expo%"')
e3_website_visitors_querys <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE AnonID IN(SELECT AnonID FROM aol_data WHERE ClickURL LIKE "%e3expo%")')

x <- e3_website_visitors_querys %>% 
  select(Query, AnonID) %>% 
  distinct() %>% # wiederholte Suchen von einem Nutzer herausfiltern
  group_by(Query) %>% 
  summarise(Anzahl = n()) %>% 
  arrange(desc(Anzahl))
x2 <- e3_website_visitors_querys %>% 
  select(Query, AnonID) %>% 
  # distinct() %>% # wiederholte Suchen von einem Nutzer herausfiltern
  group_by(Query) %>% 
  summarise(Anzahl = n()) %>% 
  arrange(desc(Anzahl))

e3_website_visitors_querys %>% filter(Query == "taz") # User 1996909 suchte 137 mal taz hat aber nier geklickt
# nicht die Tageszeitung sondern die Figur von den Looney Tunes
y <- e3_website_visitors_querys %>% filter(AnonID == 1996909)

temp_data <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE Query LIKE "%empire state building%"')
temp_data <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE Query LIKE "%twitter%"')
temp_data <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE Query LIKE "%google%"')
t <- temp_data$Query %>% unique() %>% as_tibble()
temp_data %>% filter(str_detect(Query, "maps"))
temp_data <- dbGetQuery(conn, 'SELECT * FROM aol_data WHERE Query LIKE "%facebook%"')

n_queries <- dbGetQuery(conn, "SELECT COUNT(*) FROM aol_data") %>% pull() # COUNT(AnonID) geht auch
n_user <- dbGetQuery(conn, "SELECT COUNT(DISTINCT AnonID) FROM aol_data") %>% pull()
# n_distinc_queries <- dbGetQuery(conn, "SELECT COUNT(DISTINCT *) FROM aol_data") %>% pull() # das klappt nicht
n_e3_website_visitors <- dbGetQuery(conn, 'SELECT COUNT(DISTINCT AnonID) FROM aol_data WHERE ClickURL LIKE "%e3expo%"') %>% pull()
# length(unique(e3_website_visitors$AnonID))
n_e3_website_visitors/n_user*100 # 0.0166 % der Nutzer besuchten die e3 website über die AOL Suche

# 22748 könnte ein deutscher Tourist sein (sucht Deutsche Bank in New York)

dbDisconnect(conn)
