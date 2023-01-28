library(tidyverse)
library(DBI)
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "aol.sqlite", extended_types = TRUE)

# files <- list.files("AOL-user-ct-collection/")
# 
# for (i in files) {
#   temp_data <- rio::import(str_c("AOL-user-ct-collection/", i))
#   dbWriteTable(conn, "aol_data", temp_data, append = TRUE)
# }

# sales_data <- rio::import("vgsales.csv") %>% mutate_if(is.numeric, ~.*1000000)
# dbWriteTable(conn, "game_sales", sales_data)

dbExecute(conn, "create view person_entries as 
    select AnonID, min(querytime) as firstquery, max(querytime) as lastquery, count(AnonId) as n_entries 
    from aol_data group by AnonID;")
dbExecute(conn, "create view person_searches as 
    select AnonID, count(distinct querytime) as n_searches 
    from aol_data group by AnonID;")
dbExecute(conn, "create view person_clicks as 
    select AnonID, count(AnonId) as n_clicks 
    from aol_data where clickurl is not null group by AnonID;")

# es gibt in sqlite keine materialized view
# full outer joins brauchen ewig!
# person <- dbGetQuery(conn, "select person_entries.AnonID, firstquery, lastquery, n_entries, ifnull(n_clicks, 0) as n_clicks, n_searches 
#     from person_entries 
#     full outer join person_clicks on person_entries.anonid = person_clicks.anonid 
#     full outer join person_searches on person_entries.anonid = person_searches.anonid;")
p_clicks <- dbGetQuery(conn, 'Select * from person_clicks')
p_searches <- dbGetQuery(conn, 'Select * from person_searches')
p_entries <- dbGetQuery(conn, 'Select * from person_entries')
person <- p_entries %>% left_join(p_clicks) %>% left_join(p_searches)
dbWriteTable(conn, "person", person)

# dbRemoveTable(conn, "aol_data")
# dbRemoveTable(conn, "game")

dbExecute(conn, "create table website (
    ID int not null primary key,
    /* Name varchar(255), */
    URL varchar(255)
);")
dbWriteTable(conn, "website", website, append = TRUE, row.names = FALSE)

dbExecute(conn, "create table publisher (
    ID int not null primary key,
    publisher varchar(255),
    WebsiteID int,
    FOREIGN KEY (WebsiteID) REFERENCES website(ID)
);")
dbWriteTable(conn, "publisher", publisher, append = TRUE, row.names = FALSE)

dbExecute(conn, "create table platform (
    id integer primary key,
    platform varchar(255)/*,
    abbreviation varchar(16),
    kategorie integer references kategorie(id),
    website integer references website(id) */
);")
dbWriteTable(conn, "platform", platform, append = TRUE, row.names = FALSE)

dbExecute(conn, "create table game (
    ID int not null primary key,
    game varchar(255),
    /* Sales int, */
    atE3 NUMBER(1),
    releasedate char(10)
    /* PublisherId int,
    WebsiteId int,
    PlatformId int,
    FOREIGN KEY (PublisherId) REFERENCES publisher(ID),
    FOREIGN KEY (WebsiteId) REFERENCES website(ID),
    FOREIGN KEY (PlatformId) REFERENCES platform(ID) */
);")
dbWriteTable(conn, "game", game %>% rename(atE3 = at_e3), append = TRUE, row.names = FALSE)

dbExecute(conn, "create table gamealias (
    gameID int not null,
    alias varchar(255) not null,
    primary key (gameID, alias),
    foreign key (gameID) references game(ID)
);")
dbWriteTable(conn, "gamealias", games_alias_relation, append = TRUE, row.names = FALSE)

dbExecute(conn, "create table gameplatform (
    gameID int not null,
    platformID int not null,
    primary key (gameID, platformID),
    foreign key (gameID) references game(ID),
    foreign key (platformID) references platform(ID)
);")
dbWriteTable(conn, "gameplatform", games_platform_relation, append = TRUE, row.names = FALSE)

dbExecute(conn, "create table gamepublisher (
    gameID int not null,
    publisherID int not null,
    primary key (gameID, publisherID),
    foreign key (gameID) references game(ID),
    foreign key (publisherID) references publisher(ID)
);")
dbWriteTable(conn, "gamepublisher", games_publisher_relation, append = TRUE, row.names = FALSE)



dbDisconnect(conn)
