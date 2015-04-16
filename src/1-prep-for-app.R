#!/usr/bin/env Rscript
# andrew borgman
# 3.14.2015
# prep data for dashboard
options(stringsAsFactors = F)
library(borgmisc)
library(ggplot2)
library(dplyr)
library(R6)

# classes
source('player-classes.R', local = T)

# data
hitter_list <- readRDS('../rds/hitter-list.rds')
pitcher_list <- readRDS('../rds/pitcher-list.rds')


# colnames for our data sets
hpnames <- c("ESPN", "mGURUwt", "mVORP", "FPRO", "adjWERTH", "zwOBA", "zWAR")
ppnames <- c("ESPN", "mSTUFF", "ERAi", "mGURU", "mVORP")
gnames <- c("ExpConsRnk", "ADP", "adjWERTH")

# data access usage
hitter_list[[1]]$get_stats('obp')
hitter_list[[1]]$get_stats('obp', agg_func = 'median')
hitter_list[[1]]$get_stats('obp', agg_func = 'mean', trim = .20)
hitter_list[[1]]$get_stats('obp', agg_func = NULL)


# get all my hitting stats
sapply(names(Hitter$public_fields), function(x) {
  hitter_list[[1]]$get_stats(x, agg_func = NULL)
})

hitter_list[[1]]$get_projections()


# what correlates most with each of our rankings?
hitter_stats <- do.call(rbind, lapply(hitter_list, function(hitter) {
  sapply(names(Hitter$public_fields), function(stat) {
    hitter$get_stats(stat)
  })
}))


an <- as.numeric
hitter_ranks <- do.call(rbind, lapply(hitter_list, function(hitter) {
  hitter$get_projections(proj_names = hpnames)
}))
hitter_ranks[] <- lapply(hitter_ranks, an)

hitter_ranks$mean_rank <- round(apply(hitter_ranks, 1, mean, na.rm = T), digits = 1)
hitter_ranks$median_rank <- round(apply(hitter_ranks, 1, median, na.rm = T), digits = 1)

coors <- sapply(hitter_ranks, function(x) {
  apply(hitter_stats, MARGIN = 2, 
        function(y) cor(an(x), an(y), use = "pairwise.complete.obs"))
})


heat_misc(coors, z_score = F, rang = c(-1, 1))

coors <- sapply(hitter_ranks, function(x) {
  apply(hitter_stats, MARGIN = 2, 
        function(y) cor(an(x), an(y), use = "pairwise.complete.obs"))
})

dim(coors)

comb <- cbind(hitter_stats, hitter_ranks)

comb[] <- lapply(comb, an)

heat_misc(cor(comb,  use = "pairwise.complete.obs"), z_score = F, rang = c(-1, 1))

# library(GGally)
# jpeg('images/pairs-coors.jpeg', height = 1500, width = 1500)
# comb %>% 
#   sample_frac(0.5, replace=TRUE) %>% 
#   ggpairs(axisLabels="none") + 
#   theme_bw()
# dev.off()


# we need a big map file for all of our player attributes we need to track
positions <- unique(unlist(sapply(hitter_list, function(hitter) hitter[['positions']])))

pos_map <- t(sapply(hitter_list, function(h){
  sapply(positions, function(p) h$check_pos(p))
}))

mapper <- data.frame(
  full_name = paste(
    sapply(hitter_list, function(hitter) hitter[['first_name']]),
    sapply(hitter_list, function(hitter) hitter[['last_name']])
  ), 
  team = sapply(hitter_list, function(hitter) hitter[['team']]),
  curr_owner = sapply(hitter_list, function(hitter) hitter[['curr_owner']]),
  pos_string = sapply(hitter_list, function(hitter) paste(hitter[['positions']], collapse = ','))
)

hitter_mapper <- cbind.data.frame(mapper, pos_map)





# now the same thing for pitchers ----------------------------------------------

pitcher_stats <- do.call(rbind, lapply(pitcher_list, function(pitcher) {
  sapply(names(Pitcher$public_fields), function(stat) {
    pitcher$get_stats(stat)
  })
}))

pitcher_ranks <- do.call(rbind, lapply(pitcher_list, function(pitcher) {
  pitcher$get_projections(proj_names = ppnames)
}))
pitcher_ranks[] <- lapply(pitcher_ranks, an)

pitcher_ranks$mean_rank <- round(apply(pitcher_ranks, 1, mean, na.rm = T), digits = 1)
pitcher_ranks$median_rank <- round(apply(pitcher_ranks, 1, median, na.rm = T), digits = 1)

coors <- sapply(pitcher_ranks, function(x) {
  apply(pitcher_stats, MARGIN = 2, 
        function(y) cor(an(x), an(y), use = "pairwise.complete.obs"))
})
heat_misc(coors, z_score = F, rang = c(-1, 1), row_clust = F, col_clust = F)


# we need a big map file for all of our player attributes we need to track
positions <- unique(unlist(sapply(pitcher_list, function(hitter) hitter[['positions']])))

pos_map <- t(sapply(pitcher_list, function(h){
  sapply(positions, function(p) h$check_pos(p))
}))

mapper <- data.frame(
  full_name = paste(
    sapply(pitcher_list, function(hitter) hitter[['first_name']]),
    sapply(pitcher_list, function(hitter) hitter[['last_name']])
  ), 
  team = sapply(pitcher_list, function(hitter) hitter[['team']]),
  curr_owner = sapply(pitcher_list, function(hitter) hitter[['curr_owner']]),
  pos_string = sapply(pitcher_list, function(hitter) paste(hitter[['positions']], collapse = ','))
)

pitcher_mapper <- cbind.data.frame(mapper, pos_map)




#### 
# do the global rankings
####

all_players <- c(hitter_list, pitcher_list)

ml <- list()
for (i in seq_along(all_players)) {
  curr_player <- all_players[[i]]
  tmp <- data.frame(
    Name = paste(curr_player$first_name, curr_player$last_name),
    Owner = curr_player$curr_owner,
    Pos = paste(curr_player$positions, collapse = ', ')
  )
  tt <- curr_player$get_stats(stat_name = 'global_fields', agg_func = NULL)
  vals <- data.frame(
    ExpConsRnk = NA,
    ADP = NA,
    adjWERTH = NA
  )
  
  if (length(tt) == 3)  {
    vals[1, ] <- tt
  }
  
  ml[[i]] <- cbind.data.frame(
    tmp,
    vals
  )
}

global_vals <- do.call(rbind, ml)
global_vals <- global_vals[order(global_vals$adjWERTH, decreasing = T), ]

global_vals <- global_vals[!is.na(global_vals$ExpConsRnk), ]
global_vals <- global_vals[global_vals$ExpConsRnk != '', ]


# output everything we need for our app -- ordered by median rank
hitter_ord <- order(hitter_ranks$median_rank)
pitcher_ord <- order(pitcher_ranks$median_rank)
cout <- list(
  hitter_mapper = hitter_mapper[hitter_ord, ],
  pitcher_mapper = pitcher_mapper[pitcher_ord, ],
  
  hitter_rankings = hitter_ranks[hitter_ord, ],
  hitter_stats = hitter_stats[hitter_ord, ],
  
  pitcher_rankings = pitcher_ranks[pitcher_ord, ],
  pitcher_stats = pitcher_stats[pitcher_ord, ],
  
  global_vals = global_vals
  
)

saveRDS(cout, '../app-data.rds')

