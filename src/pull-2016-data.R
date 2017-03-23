#!/usr/bin/env Rscript
# yanking projections and rankings from various sources
library(dplyr)
library(rvest)
library(borgmisc)
library(humanparser) # devtools::install_github("hrbrmstr/humanparser")

# espn 6x6 rotisserie rankings
u <- 'http://espn.go.com/fantasy/baseball/story/_/page/mlbdk2k16_6x6rankings/top-250-rotisserie-6x6-rankings-fantasy-baseball'
page <- read_html(u)
tab_text <- page %>% 
  html_nodes('td') %>% 
  html_text()

rr <- grep('\\.', tab_text, value = T)
rexp <- "^(\\w+)\\s?(.*)$"
espn_ranks <- data.frame(
  rank = as.numeric(sub(rexp,"\\1",rr)), 
  player = gsub('\\. ', '', sub(rexp,"\\2",rr)),
  stringsAsFactors = FALSE
  )


# fantasypros overall ECR
u <- 'http://www.fantasypros.com/mlb/rankings/overall.php'
page <- read_html(u)

fp_overall <- u %>% 
  read_html() %>% 
  html_nodes('.mobile-table .table') %>% 
  html_table() %>% 
  first

fp_overall$player <- trim(sapply(strsplit(fp_overall$`Player (team, position)`, '\\('), '[', 1))
fp_overall$position <- gsub('\\)', '', sapply(strsplit(trim(gsub('(DTD)|(SUS)', '', sapply(strsplit(fp_overall$`Player (team, position)`, '\\('), '[', 2))), ' - '), '[', 2))
saveRDS(fp_overall, 'projections/2016/2016-fp-overall.rds')


# scrape all the available projections for each player
proj <- lapply(1:nrow(fp_overall), function(i) {
  parsed_name <- parse_name(fp_overall$player[i])
  print(parsed_name$fullName)
  tryCatch(
    expr = {
      sprintf('http://www.fantasypros.com/mlb/projections/%s-%s.php', 
                   tolower(gsub("\\.|\\'", '', parsed_name$firstName)), 
                   tolower(parsed_name$lastName)) %>% 
        read_html() %>% 
        html_nodes('.mobile-table .table') %>% 
        html_table() %>% 
        first
    },
    error = function(e) return(NULL)
  )
})


names(proj) <- fp_overall$player

# fill in some key ones we missed
proj[['Jose Abreu']] <- 'http://www.fantasypros.com/mlb/projections/jose-dariel-abreu.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first
  
proj[['Chris Archer']] <- 'http://www.fantasypros.com/mlb/projections/christopher-archer.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Travis d'Arnaud"]] <- 'http://www.fantasypros.com/mlb/projections/travis-darnaud.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Mike Fiers"]] <- 'http://www.fantasypros.com/mlb/projections/michael-fiers.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Howie Kendrick"]] <- 'http://www.fantasypros.com/mlb/projections/howard-kendrick.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["J.T. Realmuto"]] <- 'http://www.fantasypros.com/mlb/projections/j.t.-realmuto.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Darren O'Day"]] <- 'http://www.fantasypros.com/mlb/projections/darren-oday.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Leonys Martin"]] <- 'http://www.fantasypros.com/mlb/projections/lenonys-martin.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Jorge De La Rosa"]] <- 'http://www.fantasypros.com/mlb/projections/jorge-de-la-rosa.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Rubby De La Rosa"]] <- 'http://www.fantasypros.com/mlb/projections/rubby-de-la-rosa.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["J.R. Murphy"]] <- 'http://www.fantasypros.com/mlb/projections/j.r.-murphy.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Tommy Milone"]] <- 'http://www.fantasypros.com/mlb/projections/tom-milone.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first

proj[["Carlos Martinez"]] <- 'http://www.fantasypros.com/mlb/projections/carlos-martinez-p.php' %>% 
  read_html() %>% html_nodes('.mobile-table .table') %>% html_table() %>% first


saveRDS(proj, 'projections/2016/2016-projections-parsed.rds')


# sapply(proj, is.null)
# sizes <- sapply(proj, nrow)
# sizes[sizes>8]



