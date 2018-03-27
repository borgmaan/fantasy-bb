#!/usr/bin/env Rscript
# yanking projections and rankings from various sources
options(stringsAsFactors = F)
library(dplyr)
library(rvest)
library(borgmisc)
library(humanparser) # devtools::install_github("hrbrmstr/humanparser")

# positions 
pos <- c('C', '1B', '2B', 'SS', '3B', 'OF', 'SP', 'RP')

# get URLs for all the players to get their MLBAMID
player_ids <- do.call(plyr::rbind.fill, lapply(pos, function(p) {
  pg <- read_html(sprintf("https://rotochamp.com/Baseball/PlayerRankings.aspx?Position=%s", p))
  vl <- pg %>% 
    html_nodes('.table a') 
  
  sel <- grep("MLBAMID", vl %>% html_attr('href'))
  nms <- vl[sel] %>% html_text() %>% parse_names()
  nms$MLBAMID <- vl[sel] %>% html_attr('href') %>% gsub("^[^=]+|=", "", .) 
  nms$POS <- p
  nms
}))

# scrape all the available projections for each player
proj <- lapply(1:nrow(player_ids), function(i) {
  message(player_ids$fullName[i])

  cname <- ""  

  if (player_ids$POS[i] %in% c('SP', 'RP')) {
    cname <- "#MainContent_gridPitcherProjections"
  } else {
    cname <- "#MainContent_gridHitterProjections"
  }
  
  e <- try({
    u <- sprintf("https://rotochamp.com/Baseball/Player.aspx?MLBAMID=%s", player_ids$MLBAMID[i])
    pg <- read_html(u)
    prj <- pg %>% 
      html_node(cname) %>% 
      html_table()
    prj <- prj[prj[, 1] != 2017,]
    
  
    u2 <- sprintf("https://rotochamp.com/Baseball/PlayerPositions.aspx?MLBAMID=%s", player_ids$MLBAMID[i])
    pg2 <- read_html(u2)
    ps <- pg2 %>% 
      html_node('#MainContent_gridGamesPlayed_Primary') %>% 
      html_table()
    })
  if (inherits(e, 'try-error')) prj <- ps <- NULL
  
  Sys.sleep(.5)
  return(list(
    player_ids[i,],
    prj,
    ps
  ))
  
})

saveRDS(object = proj, file = 'projections/2018/2018-projections-parsed.rds')

nnul <- sapply(proj, function(x) !is.null(x[[2]]))

proj <- proj[nnul]
saveRDS(object = proj, file = 'projections/2018/2018-projections-parsed-clean.rds')


