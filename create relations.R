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
  rename("GameID" = "ID.x", "PublisherID" = "ID.y")

games_publisher_relation %>% rio::export("../dbs-ws2223/data4humans/game_publisher.xlsx")
games_publisher_relation %>% select(GameID, PublisherID) %>% rio::export("../dbs-ws2223/data/game_publisher.csv")

#### Websites ####

website <- pub_data %>% select(Website) %>% unique() %>% drop_na() %>% rowid_to_column() %>% 
  rename(ID = rowid, url = Website)
website %>% rio::export("../dbs-ws2223/data/website.csv")

#### Publisher ####

pub_data %>% left_join(website %>% rename(WebsiteID = ID), by=c("Website" = "url")) %>% 
  select(-Website) %>% rename("Publisher" = "Company Name") %>% 
  rio::export("../dbs-ws2223/data/publisher.csv")

#### Game ####

games_data %>% rename("releasedate" = "First release date") %>% select(ID, Name, releasedate) %>% 
  mutate(at_e3 = TRUE, releasedate = lubridate::mdy(releasedate)) %>% 
  rio::export("../dbs-ws2223/data/game.csv")

#### Platform-Games ####

games_platform_relation <- games_data %>% select(ID, Name, Platform) %>% 
  separate_rows(Platform, sep = "\r\r\n") %>% 
  mutate(Platform = str_remove(Platform, "[ ]*")) %>% 
  filter(Platform != "", Platform != "PlatformPlatform of this game.")

platform <- games_platform_relation %>% select(Platform) %>% unique() %>% 
  rowid_to_column() %>% rename(ID = rowid)
platform %>% rio::export("../dbs-ws2223/data/platform.csv")

games_platform_relation <- games_platform_relation %>% rename(GameID = ID) %>% 
  left_join(platform %>% rename(PlatformID = ID))
games_platform_relation %>% rio::export("../dbs-ws2223/data4humans/game_platform.xlsx")
games_platform_relation%>% select(GameID, PlatformID) %>% rio::export("../dbs-ws2223/data/game_platform.csv")

#### Game Search Terms ####

games_alias_relation <- games_data %>% select(ID, Name, Aliases) %>% 
  separate_rows(Aliases, sep = "\r\r\n") %>% 
  mutate(Aliases = str_remove(Aliases, "[ ]*")) %>% 
  filter(Aliases != "", Aliases != "PlatformPlatform of this game.",
         Name != "") %>% unique()
games_alias_relation %>% select(-Name) %>% rename(GameID = ID) %>%rio::export("../dbs-ws2223/data/game_alias.csv")
