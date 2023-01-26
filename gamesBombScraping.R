library(rvest)
library(tidyverse)

simple <- read_html("https://www.giantbomb.com/e3-2006/3015-3439/")

game_names <- simple %>%
  html_nodes(".wiki-item-display li a") %>%
  html_text2()
game_urls <- simple %>%
  html_nodes(".wiki-item-display li a") %>%
  html_attr('href')

games <- data.frame(Names = game_names, url = game_urls)[1:348,] %>% 
  mutate(url = str_c("https://www.giantbomb.com", url))

crawlPublisherSiteLinks <- function(link) {
  target_site <- tryCatch(
    read_html(link),
    error = function(e){NULL}
  )
  if (is.null(target_site)) return(NULL)
  data <- target_site %>% html_nodes(".wiki-details .table:not(.js-release-table)") %>% 
    html_nodes("div[data-field=publishers]") %>% html_nodes("a") %>% html_attr("href")
  if (length(data)<1) return(NULL)
  # df <- data[[1]] %>% pivot_wider(names_from = "X1", values_from = "X2") %>% 
  #   mutate(rating = starrating, raters = n_raters)
  df <- data %>% as.data.frame()
}

publisher_urls <- map_df_progress(games$url, crawlPublisherSiteLinks)
publisher_urls <- publisher_urls %>% unique() %>% rename(url = ".") %>% 
  mutate(url = str_c("https://www.giantbomb.com", url))

crawlGameData <- function(link) {
 target_site <- tryCatch(
   read_html(link),
   error = function(e){NULL}
 )
 if (is.null(target_site)) return(NULL)
 data <- target_site %>% html_nodes(".wiki-details .table:not(.js-release-table)") %>% html_table()
 starrating <- target_site %>% html_node(".average-score") %>% html_text2() %>% str_remove(" stars") %>% as.double()
 n_raters <- target_site %>% html_node(".average p") %>% html_text2() %>% str_extract_all("[0-9]*") %>% purrr::pluck(1) %>% str_flatten() %>% as.integer()
 if (length(data)<1) return(NULL)
 df <- data[[1]] %>% pivot_wider(names_from = "X1", values_from = "X2") %>% 
   mutate(rating = starrating, raters = n_raters)
}

map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_df(.x, f, ..., .id = .id)
}

# games_data1 <- map_df_progress(games$url[1:100], crawlGameData)
# games_data2 <- map_df_progress(games$url[101:200], crawlGameData)
# games_data3 <- map_df_progress(games$url[201:300], crawlGameData)
# games_data4 <- map_df_progress(games$url[301:nrow(games)], crawlGameData)
# games_data <- games_data1 %>% bind_rows(games_data2, games_data3, games_data4)
# games_data <- map_df_progress(games$url, crawlGameData)
# games_data %>% rio::export("crawled_games_data.xlsx")

games_data <- rio::import("crawled_games_data.xlsx")
games_data <- games_data %>% select(Name, `First release date`, Publisher, Platform, Aliases, rating, raters) %>% 
  rowwise() %>%  mutate_at(vars("Name", "First release date"), ~str_split(.x, "\n")[[1]][1]) %>% 
  mutate_at(vars("Name", "First release date"), ~str_split(.x, "\r")[[1]][1])
games_data %>% rio::export("crawled_games_data.csv")

#### Sales Data ####

sales_data <- rio::import("vgsales.csv") %>% mutate_if(is.numeric, ~.*1000000) %>% mutate(id = str_to_lower(Name))
games <- games %>% mutate(id = str_to_lower(Names))
setdiff(games$id, sales_data$id)

games[which(games$id == "dragon ball z tenkaichi 2"),1] <- "dragon ball z: budokai tenkaichi 2"
games[which(games$id == "arthur and the invisibles: the game"),1] <- "arthur and the invisibles"
games[which(games$id == "dungeons & dragons: tactics"),1] <- "dungeons & dragons tactics"
games[which(games$id == "hot pxl"),1] <- "hot pixel"
games[which(games$id == "blade dancer"),1] <- "blade dancer: lineage of light"
games[which(games$id == "disgaea 2"),1] <- "disgaea 2: cursed memories"
games[which(games$id == "yggdra union"),1] <- "yggdra union: we'll never fight alone"
games[which(games$id == "mercenaries 2"),1] <- "mercenaries 2: world in flames"
games[which(games$id == "lumines ii"),1] <- "lumines ll"
games[which(games$id == "lost planet"),1] <- "lost planet: extreme condition"
games[which(games$id == "mega man battle network 6: cybeast gregar"),1] <- "mega man battle network 6: cybeast falzar / gregar"
games[which(games$id == "monster hunter: freedom"),1] <- "monster hunter freedom"
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""
games[which(games$id == ""),1] <- ""

temp <- games %>% left_join(sales_data, by = c("id" = "id"))


#### Publisher ####

pub_names <- simple %>%
  html_nodes("h3 a") %>%
  html_text2()
pub_urls <- simple %>%
  html_nodes("h3 a") %>%
  html_attr('href')

publisher <- data.frame(Names = pub_names, url = pub_urls) %>% 
  mutate(url = str_c("https://www.giantbomb.com", url))


crawlPublisherData <- function(link) {
  target_site <- tryCatch(
    read_html(link),
    error = function(e){NULL}
  )
  if (is.null(target_site)) return(NULL)
  data <- target_site %>% html_nodes(".wiki-details .table:not(.js-release-table)") %>% html_table()
  # starrating <- target_site %>% html_node(".average-score") %>% html_text2() %>% str_remove(" stars") %>% as.double()
  # n_raters <- target_site %>% html_node(".average p") %>% html_text2() %>% str_extract_all("[0-9]*") %>% purrr::pluck(1) %>% str_flatten() %>% as.integer()
  if (length(data)<1) return(NULL)
  df <- data[[1]] %>% pivot_wider(names_from = "X1", values_from = "X2")
}

pub_data <- map_df_progress(publisher_urls$url, crawlPublisherData)
pub_data <- pub_data %>% select(`Company Name`, Website) %>% 
  rowwise() %>%  mutate_all(~str_split(.x, "\n")[[1]][1]) %>% 
  mutate_all(~str_split(.x, "\r")[[1]][1])
pub_data %>% rio::export("publisher_scrape.csv")

# todo: ersetze website durch website id und erstelle website table

