library(tidyverse)
library(DBI)
# library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

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

nonunique_querys <- dbGetQuery(conn, "SELECT Query, COUNT(Query) FROM aol_data GROUP BY Query HAVING COUNT(Query) > 1 ORDER BY COUNT(Query) DESC")
wasIstPogo <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query='pogo'") # eine Online-Spiele-Seite
pokemon <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%pokemon%'")
pokemon_porn_guy <- dbGetQuery(conn, "SELECT * From aol_data WHERE AnonID=120726")
porn <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%porn%'")
porn %>% ggplot() + geom_histogram(aes(QueryTime), bins =184)
porn %>% filter(QueryTime > as.Date("2006-05-17"), QueryTime < as.Date("2006-05-18"))
xxx <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%xxx%'")
sex <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%sex%'")
tits <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%tits%'")
boob <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%boob%'")
ass <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%ass%'")
dick <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%dick%'")
cock <- dbGetQuery(conn, "SELECT * From aol_data WHERE Query LIKE '%cock%'")

dbDisconnect(conn)
