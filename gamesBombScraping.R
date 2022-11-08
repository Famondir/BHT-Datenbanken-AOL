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
  mutate(url = str_c("https://www.giantbomb.com/", url))
