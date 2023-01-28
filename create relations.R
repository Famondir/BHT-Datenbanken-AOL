library(tidyverse)

#### load data ####

games_data <- rio::import("crawled_games_data.csv") %>% rowid_to_column() %>% 
  rename(ID = rowid) %>% filter(Name != "")
pub_data <-  rio::import("publisher_scrape.csv") %>% rowid_to_column() %>% rename(ID = rowid)

pub_data[which(pub_data$Website=="WebsiteWebsite of this company."), "Website"] <- NA
pub_data <- pub_data %>% mutate(Website = ifelse(startsWith(Website, "s://"), str_c("http", Website), Website))

#### Publisher-Games ####

games_publisher_relation <- games_data %>% select(ID, Name, Publisher) %>% 
  separate_rows(Publisher, sep = "\r\r\n") %>% 
  mutate(Publisher = str_remove(Publisher, "[ ]*")) %>% 
  filter(Publisher != "", Publisher != "PublisherPublisher of this game.") %>%   
  left_join(pub_data, by = c("Publisher" = "Company Name")) %>% 
  rename("GameID" = "ID.x", "PublisherID" = "ID.y")%>% select(GameID, PublisherID)

games_publisher_relation %>% rio::export("../dbs-ws2223/data4humans/game_publisher.xlsx")
games_publisher_relation %>% select(GameID, PublisherID) %>% rio::export("../dbs-ws2223/data/gamepublisher.csv")

#### Websites ####

website <- pub_data %>% select(Website) %>% unique() %>% drop_na() %>% rowid_to_column() %>% 
  rename(ID = rowid, url = Website)
website %>% rio::export("../dbs-ws2223/data/website.csv")

#### Publisher ####

publisher <- pub_data %>% left_join(website %>% rename(WebsiteID = ID), by=c("Website" = "url")) %>% 
  select(-Website) %>% rename("Publisher" = "Company Name")
publisher %>% rio::export("../dbs-ws2223/data/publisher.csv")

#### Game ####

game <- games_data %>% rename("releasedate" = "First release date") %>% select(ID, Name, releasedate) %>%
  rename(game = Name) %>% 
  mutate(at_e3 = 1, releasedate = lubridate::mdy(releasedate),
       game = str_remove(game, "'"))
game %>% rio::export("../dbs-ws2223/data/game.csv")

#### Platform-Games ####

games_platform_relation <- games_data %>% select(ID, Name, Platform) %>% 
  separate_rows(Platform, sep = "\r\r\n") %>% 
  mutate(Platform = str_remove(Platform, "[ ]*")) %>% 
  filter(Platform != "", Platform != "PlatformPlatform of this game.")

platform <- games_platform_relation %>% select(Platform) %>% unique() %>% 
  rowid_to_column() %>% rename(ID = rowid)
platform %>% rio::export("../dbs-ws2223/data/platform.csv")

games_platform_relation <- games_platform_relation %>% rename(GameID = ID) %>% 
  left_join(platform %>% rename(PlatformID = ID)) %>% select(GameID, PlatformID)
games_platform_relation %>% rio::export("../dbs-ws2223/data4humans/game_platform.xlsx")
games_platform_relation%>% select(GameID, PlatformID) %>% rio::export("../dbs-ws2223/data/gameplatform.csv")

#### Game Search Terms ####

games_alias_relation <- games_data %>% select(ID, Name, Aliases) %>% 
  separate_rows(Aliases, sep = "\r\r\n") %>% 
  mutate(Aliases = str_remove(Aliases, "[ ]*")) %>% 
  filter(Aliases != "", Aliases != "PlatformPlatform of this game.",
         Name != "") %>% unique() %>% 
  mutate(Aliases = str_remove(Aliases, "\n") %>% str_remove("\r") %>% 
           str_remove("'") %>% iconv("UTF-8", "ASCII",sub='')
       ) %>% filter(Aliases != " ", Aliases != "II ") %>% 
  select(-Name) %>% rename(GameID = ID, alias = Aliases)
games_alias_relation %>% rio::export("../dbs-ws2223/data/gamealias.csv")
  # write.csv("../dbs-ws2223/data/gamealias.csv", fileEncoding = "UTF-16LE", row.names=FALSE)
