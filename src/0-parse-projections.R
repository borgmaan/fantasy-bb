#!/usr/bin/env Rscript
# andrew borgman
# 3.13.2015
# parsing and formatting all of our fantasy projections

####
# Notes:
#   * Offensive categories are OBP, SLG, HR, R, RBI, SB and 
#   * Pitching categories are WHIP, ERA, K, K/9, QS and Saves
####

options(stringsAsFactors = F)
library(borgmisc)
library(ggplot2)
library(dplyr)
library(R6)

# classes
source('src/player-classes.R', local = T)

# locate player by name
team_map <- read.delim('projections/clean/team-mapper.tsv')
team_lu <- setNames(team_map$guru, team_map$espn)

name_finder <- function(full_names = NULL, first_names = NULL, last_names = NULL, 
                        teams = NULL, hitter = NULL) {
  
  # split out first and last names
  if (!is.null(full_names)) {
    first_names <- trim(sapply(strsplit(full_names, ' '), function(x) x[1]))
    last_names <- trim(sapply(strsplit(full_names, ' '), function(x) paste(x[2:length(x)], collapse = " ")))
  }

  # try to find a match in the new dataset
  found_match <- FALSE
  gs <- intersect(
    grep(hitter[['first_name']], first_names),
    grep(hitter[['last_name']], last_names)
  )
  
  if (length(gs) == 1) {
    found_match <- TRUE
  } else {
    
    if (!is.null(teams)) {
      # see if we can approx match the name and match the team
      mm <- data.frame(
        fns <- adist(hitter[['first_name']], first_names, fixed = F)[1,],
        lns <- adist(hitter[['last_name']], last_names, fixed = F)[1,]
      )
      mm$score <- mm[,1] + mm[,2]
      top_scores <- which(mm$score == min(mm$score))
      team_matches <- teams[top_scores]
      for (k in seq_along(team_matches)) {
        if (team_matches[k] == team_lu[hitter[['team']]] | team_matches[k] == '') {
          gs = top_scores[k]
          found_match <- TRUE
          break
        }
      }    
    } else {
      warning('no match found...')
    }
  }
  
  if (!found_match) {
    gs <- '' # gives us NAs for everything we try to set
    print('cant find match for')
    print(hitter)
    # missed <- c(missed, i)
  }
  
  return(gs)

}





# ESPN players --------

# espn is just in rank order -- use these as a base to make all our players...
# system('src/clean-espn.py')
espn_b <- read.delim('projections/clean/espn-hitters.tsv', check.names = F, 
                     na.strings = '--')
espn_p <- read.delim('projections/clean/espn-pitchers.tsv', check.names = F, 
                     na.strings = '--')

# populate all our objects
hitter_list <- vector(mode = 'list', length = nrow(espn_b))
pitcher_list <- vector(mode = 'list', length = nrow(espn_p))
for (i in seq(nrow(espn_b))) {
  
  p <- Hitter$new()
  
  # player info
  p$set_field('first_name', espn_b$first_name[i])
  p$set_field('last_name', espn_b$last_name[i])
  p$set_field('team', espn_b$team[i])
  p$set_pos(strsplit(espn_b$pos_str[i], ';')[[1]])
  p$set_field('curr_owner', espn_b$TYPE[i])
  
  # statistical projections
  p$set_field('obp', espn_b$OBP[i], name = 'ESPN')
  p$set_field('slg', espn_b$SLG[i], name = 'ESPN')
  p$set_field('hr', espn_b$HR[i], name = 'ESPN')
  p$set_field('r', espn_b$R[i], name = 'ESPN')
  p$set_field('rbi', espn_b$RBI[i], name = 'ESPN')
  p$set_field('sb', espn_b$SB[i], name = 'ESPN')
  
  # overall projections
  p$set_field('projection_rankings', espn_b$RNK[i], name = 'ESPN')
  
  hitter_list[[i]] <- p
  
  rm(p)
  
}

for (i in seq(nrow(espn_p))) {
  
  p <- Pitcher$new()
  
  # player info
  p$set_field('first_name', espn_p$first_name[i])
  p$set_field('last_name', espn_p$last_name[i])
  p$set_field('team', espn_p$team[i])
  p$set_pos(strsplit(espn_p$pos_str[i], ';')[[1]])
  p$set_field('curr_owner', espn_p$TYPE[i])
  
  # statistical projections
  p$set_field('whip', espn_p$WHIP[i], name = 'ESPN')
  p$set_field('era', espn_p$ERA[i], name = 'ESPN')
  p$set_field('k', espn_p$K[i], name = 'ESPN')
  p$set_field('k9', espn_p[['K/9']][i], name = 'ESPN')
  p$set_field('qs', espn_p$QS[i], name = 'ESPN')
  p$set_field('saves', espn_p$SV[i], name = 'ESPN')
  
  # overall projections
  p$set_field('projection_rankings', espn_p$RNK[i], name = 'ESPN')
  
  pitcher_list[[i]] <- p
  
  rm(p)
  
}



# GURU players --------

# mwOBA	"Predicted rate of performace, like OPS but more accurate. Based on Tom Tango's ""The Book"""
# mPRO	Predicted production, like linear weights. It is wOBA * mTAP
# mGURU	Forecasted batting value, based on weighting between m WOBA and mPRO
# mGURUwt	Forecasted batting value, where mGURU is weighted by positional scarcity
# mVORP	Value (runs contributed)  Over Replacement  Player at same position. It is (mwOBA-xwOBA)*mTAP. Uses xwOBA of non-starters.
guru_b <- read.delim('projections/clean/guru-hitters.tsv', check.names = F, 
                     skip = 1)

missed <- c()
for (i in seq_along(hitter_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = NULL, 
    first_names = guru_b$nameFirst, 
    last_names = guru_b$nameLast, 
    teams = guru_b$teamID, 
    hitter = hitter_list[[i]]
    )
    
  # statistical projections
  hitter_list[[i]]$set_field('obp', guru_b$mOBP[gs], name = 'GURU')
  hitter_list[[i]]$set_field('slg', guru_b$mSLG[gs], name = 'GURU')
  hitter_list[[i]]$set_field('hr', guru_b$mHR[gs], name = 'GURU')
  hitter_list[[i]]$set_field('r', guru_b$mR[gs], name = 'GURU')
  hitter_list[[i]]$set_field('rbi', guru_b$mRBI[gs], name = 'GURU')
  hitter_list[[i]]$set_field('sb', guru_b$mSB[gs], name = 'GURU')
  
  # overall projections -- need to store ranks here
  hitter_list[[i]]$set_field('projection_rankings', 
                             rank(-guru_b$mWOBA, na.last = 'keep')[gs], 
                             name = 'mWOBA')
  hitter_list[[i]]$set_field('projection_rankings', 
                             rank(-guru_b$mPRO, na.last = 'keep')[gs], 
                             name = 'mPRO')
  hitter_list[[i]]$set_field('projection_rankings', 
                             rank(-guru_b$mGURU, na.last = 'keep')[gs], 
                             name = 'mGURU')
  hitter_list[[i]]$set_field('projection_rankings', 
                             rank(-guru_b$mGURUwt, na.last = 'keep')[gs], 
                             name = 'mGURUwt')
  hitter_list[[i]]$set_field('projection_rankings', 
                             rank(-guru_b$mVORP, na.last = 'keep')[gs], 
                             name = 'mVORP')
  
  
}


# mSTUFF	Forecasted STUFF. This is NOT the same definition as PECOTA's. The formula is: mDOM*6- 1.333*(mDERA+ mDERA)- 3*mCTL -5*mHR9;  if mSTUFF<1 then mSTUFF=1;mSTUFF=mSTUFF*mSTUFF/10;
# ERAi	Forecasted Indexed ERA, where 100 is average and 80 is 20% lower than average
# mGURU	Forecasted Pitching Value (note the formula changed beginning with the 2013 forecasts to be more in line with batters)
# mVORP	Value (runs allowed)  Over Replacement  Player at same position. It is (mDERA-xDERA)*mIP. xDERA ranges from 6 for someone who only starts to 5 for somone never starts.
guru_p <- read.delim('projections/clean/guru-pitchers.tsv', check.names = F, 
                     skip = 1)


for (i in seq_along(pitcher_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = NULL, 
    first_names = guru_p$nameFirst, 
    last_names = guru_p$nameLast, 
    teams = guru_p$teamID, 
    hitter = pitcher_list[[i]]
  )

  # statistical projections
  pitcher_list[[i]]$set_field('whip', guru_p$mWHIP[gs], name = 'GURU')
  pitcher_list[[i]]$set_field('era', guru_p$mDERA[gs], name = 'GURU')
  pitcher_list[[i]]$set_field('k', guru_p$mSO[gs], name = 'GURU')
  pitcher_list[[i]]$set_field('k9', guru_p$mDOM[gs], name = 'GURU')
  pitcher_list[[i]]$set_field('qs', NA, name = 'GURU')
  pitcher_list[[i]]$set_field('saves', guru_p$mSV[gs], name = 'GURU')
  
  
  # overall projections -- need to store ranks here
  pitcher_list[[i]]$set_field('projection_rankings', 
                              rank(-guru_p$mSTUFF, na.last = 'keep')[gs], 
                              name = 'mSTUFF')
  pitcher_list[[i]]$set_field('projection_rankings', 
                              rank(guru_p$ERAi, na.last = 'keep')[gs], 
                              name = 'ERAi')
  pitcher_list[[i]]$set_field('projection_rankings', 
                              rank(-guru_p$mGURU, na.last = 'keep')[gs], 
                              name = 'mGURU')
  pitcher_list[[i]]$set_field('projection_rankings', 
                              rank(-guru_p$mVORP, na.last = 'keep')[gs], 
                              name = 'mVORP')
  
}



# FantasyPros players --------

# these are just ordered -- ECS has consensus rankings for lots of fantasy
# experts
fp_b <- read.delim('projections/clean/FantasyPros_2015_Projections_H.xls',
                   check.names = F)

for (i in seq_along(hitter_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = trim(fp_b[['Player Name']]), 
    teams = trim(fp_b$Team), 
    hitter = hitter_list[[i]]
  )

  # statistical projections
  hitter_list[[i]]$set_field('obp', fp_b$obp[gs], name = 'FPRO')
  hitter_list[[i]]$set_field('slg', fp_b$slg[gs], name = 'FPRO')
  hitter_list[[i]]$set_field('hr', fp_b$hrs[gs], name = 'FPRO')
  hitter_list[[i]]$set_field('r', fp_b$runs[gs], name = 'FPRO')
  hitter_list[[i]]$set_field('rbi', fp_b$rbi[gs], name = 'FPRO')
  hitter_list[[i]]$set_field('sb', fp_b$sb[gs], name = 'FPRO')
  
  # overall projections -- need to store ranks here
  hitter_list[[i]]$set_field('projection_rankings', gs, name = 'FPRO')
  
}


# and the pitchers now 
fp_p <- read.delim('projections/clean/FantasyPros_2015_Projections_P.xls',
                   check.names = F)

for (i in seq_along(pitcher_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = trim(fp_p[['Player Name']]), 
    teams = trim(fp_p$Team), 
    hitter = pitcher_list[[i]]
  )
 
  # statistical projections
  pitcher_list[[i]]$set_field('whip', fp_p$whip[gs], name = 'FPRO')
  pitcher_list[[i]]$set_field('era', fp_p$era[gs], name = 'FPRO')
  pitcher_list[[i]]$set_field('k', fp_p$k[gs], name = 'FPRO')
  pitcher_list[[i]]$set_field('k9', NA, name = 'FPRO')
  pitcher_list[[i]]$set_field('qs', NA, name = 'FPRO')
  pitcher_list[[i]]$set_field('saves', fp_p$sv[gs], name = 'FPRO')
  
  
  # overall projections -- need to store ranks here
  pitcher_list[[i]]$set_field('projection_rankings', gs, name = 'FPRO')

}


# global rankins here -- need to set in both lists of players
fp_ecs <- read.delim('projections/clean/FantasyPros-expert-consensus-picks.tsv',
                     check.names = F)

all_players <- c(pitcher_list, hitter_list)
for (i in seq_along(all_players)) {
  gs <- name_finder(
    full_names = trim(fp_ecs[['Player Name']]), 
    teams = trim(fp_ecs$Team), 
    hitter = all_players[[i]]
  )

  # set some of these metrix
  all_players[[i]]$set_field('global_fields', fp_ecs[['ECR']][gs], name = 'ExpConsRnk')
  all_players[[i]]$set_field('global_fields', as.numeric(fp_ecs[['ADP']][gs]), name = 'ADP')
}

pitcher_list <- all_players[1:length(pitcher_list)]
hitter_list <- all_players[(length(pitcher_list)+1):length(all_players)]


# Mr. Cheatsheet players --------

# sorted on Raw Total WERTH, but also has pos adjusted with Position Adj WERTH
mcs <- read.delim('projections/clean/mr-cheatsheet-hitters.tsv', 
                  check.names = F)

for (i in seq_along(hitter_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = trim(mcs$Player), 
    teams = trim(mcs$Team), 
    hitter = hitter_list[[i]]
  )
  
  # statistical projections
  hitter_list[[i]]$set_field('obp', mcs$obp[gs], name = 'MCS')
  hitter_list[[i]]$set_field('slg', mcs$slg[gs], name = 'MCS')
  hitter_list[[i]]$set_field('hr', mcs$HR[gs], name = 'MCS')
  hitter_list[[i]]$set_field('r', mcs$R[gs], name = 'MCS')
  hitter_list[[i]]$set_field('rbi', mcs$RBI[gs], name = 'MCS')
  hitter_list[[i]]$set_field('sb', mcs$SB[gs], name = 'MCS')
  
  # overall projections -- need to store ranks here
  hitter_list[[i]]$set_field('projection_rankings', 
                             order(-mcs[['Raw Total WERTH']])[gs], 
                             name = 'rawWERTH')
  hitter_list[[i]]$set_field('projection_rankings', 
                             order(-mcs[['Position Adj WERTH']])[gs], 
                             name = 'adjWERTH')
  
  # adjWERTH is global projection too
  hitter_list[[i]]$set_field('global_fields', 
                             mcs[['Position Adj WERTH']][gs], 
                             name = 'adjWERTH')
}


# pitchers too -- TODO
mcs_p <- read.delim('projections/clean/mr-cheatsheet-pitchers.tsv', 
                  check.names = F)

for (i in seq_along(pitcher_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = trim(mcs_p$Player), 
    teams = trim(mcs_p$Team), 
    hitter = pitcher_list[[i]]
  )
  
  # statistical projections
  pitcher_list[[i]]$set_field('whip', mcs_p$WHIP[gs], name = 'MCS')
  pitcher_list[[i]]$set_field('era', mcs_p$ERA[gs], name = 'MCS')
  pitcher_list[[i]]$set_field('k', mcs_p$K[gs], name = 'MCS')
  pitcher_list[[i]]$set_field('k9', mcs_p[['K/9']][gs], name = 'MCS')
  pitcher_list[[i]]$set_field('qs', mcs_p$QS[gs], name = 'MCS')
  pitcher_list[[i]]$set_field('saves', mcs_p$S[gs], name = 'MCS')

  # overall projections -- need to store ranks here
  pitcher_list[[i]]$set_field('projection_rankings', 
                             order(-mcs_p[['Raw Total WERTH']])[gs], 
                             name = 'rawWERTH')
  pitcher_list[[i]]$set_field('projection_rankings', 
                             order(-mcs_p[['Position Adj WERTH']])[gs], 
                             name = 'adjWERTH')
  
  # adjWERTH is global projection too
  pitcher_list[[i]]$set_field('global_fields', 
                             mcs_p[['Position Adj WERTH']][gs], 
                             name = 'adjWERTH')
}



# ZiPs players --------

# zips is sorted on wOBA
zips <- read.csv('projections/clean/zips-projections.csv', 
                 check.names = F)

for (i in seq_along(hitter_list)) {
  
  # try to find a match in the new dataset
  gs <- name_finder(
    full_names = trim(zips$Name), 
    teams = NULL, 
    hitter = hitter_list[[i]]
  )

  # statistical projections
  hitter_list[[i]]$set_field('obp', zips$OBP[gs], name = 'zips')
  hitter_list[[i]]$set_field('slg', zips$SLG[gs], name = 'zips')
  hitter_list[[i]]$set_field('hr', zips$HR[gs], name = 'zips')
  hitter_list[[i]]$set_field('r', zips$R[gs], name = 'zips')
  hitter_list[[i]]$set_field('rbi', zips$RBI[gs], name = 'zips')
  hitter_list[[i]]$set_field('sb', zips$SB[gs], name = 'zips')
  
  # overall projections -- need to store ranks here
  hitter_list[[i]]$set_field('projection_rankings', 
                             order(-zips[['wOBA']])[gs], 
                             name = 'zwOBA')
  hitter_list[[i]]$set_field('projection_rankings', 
                             order(-zips[['WAR']])[gs], 
                             name = 'zWAR')
}



# marcel
mc_b <- read.csv('projections/clean/marcel_batters_2015.csv', 
                  check.names = F)
mc_p <- read.csv('projections/clean/marcel_pitchers_2015.csv', 
                 check.names = F)

# cairo

# wOBA  Weighted on-base average	http://www.insidethebook.com/woba.shtml
# BR	Linear weights batting runs	http://tangotiger.net/wiki/index.php?title=Batting_Runs
# BR/650	BR per 650 PA
# BRAA	Batting runs above average, not position adjusted
# Pos	Position adjustment based on playing time and primary position
# Repl	Replacement level adjustment based on playing time
# BRAR	Batting runs above replacement level (BRAA + Pos + Repl) adjusted for park
# oWAR	Wins above replacement (BRAR divided by ten) offense only
ca_b <- read.delim('projections/clean/cairo-2015-batters.tsv', 
                   check.names = F)

# FIP  Fielding-independent pitching	http://www.fangraphs.com/library/index.php/pitching/fip/
# RAR	Runs saved above a replacement level pitcher, adjusted for park and role and using RA/9
# WAR	Wins above replacement level (RAR divided by 10)
ca_p <- read.delim('projections/clean/cairo-2015-pitchers.tsv', 
                   check.names = F)




# grab some info for individual players ----------------------------------------

get_info <- function(last_name, ...) {
  idx <- which(sapply(hitter_list, function(x) grepl(last_name, x[['last_name']], ...)))
  
  if (length(idx) == 1) {
    return(
      hitter_list[[idx]]$get_projections(
        proj_names = c("ESPN", "mWOBA", "mPRO", "mGURU", "mGURUwt", "mVORP", 
                       "FPRO", "rawWERTH", "adjWERTH", "zwOBA", "zWAR"))
    )
  } else if (length(idx) > 1) {
    df <- do.call(rbind.data.frame, lapply(idx, function(x) {
      hitter_list[[x]]$get_projections(
        proj_names = c("ESPN", "mWOBA", "mPRO", "mGURU", "mGURUwt", "mVORP", 
                       "FPRO", "rawWERTH", "adjWERTH", "zwOBA", "zWAR"))
    }))
    return(df)    
  }
}

knitr::kable(get_info(last_name = 'zobr', ignore.case = T))
knitr::kable(get_info(last_name = 'Yelich', ignore.case = T))
knitr::kable(get_info(last_name = 'desmond', ignore.case = T))


# Make some exploratory plots --------------------------------------------------

pres <- do.call('rbind',lapply(hitter_list, function(x) x[['projection_rankings']]))
cnames <- colnames(pres)
dims <- dim(pres)
pres <- as.numeric(pres)
dim(pres) <- dims
rownames(pres) <- lapply(hitter_list, function(x) paste(x[['first_name']],x[['last_name']]))
colnames(pres) <- cnames
pdf('images/rankings-heatmap.pdf', height = 30, width = 4)
heat_misc(pres[1:200,], row_clust = F, col_clust = F, z_score = F, 
          rang = c(0, 200), mar_padding = c(0, 5, -3, 0))
dev.off()


means <- guru_b %>%
  group_by(pos) %>%
  summarize(md = mean(mGURUwt, na.rm = T, trim = .1))


guru_b$pos <- factor(guru_b$pos, levels = means$pos[order(means$md)])

# ggplot(guru_b, aes(x = mGURUwt)) + 
#   geom_density(aes(fill = pos, y = ..density..)) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)  
#   )

# p1 <- ggplot(guru_b, aes(x = mGURU, y = mGURUwt)) + 
#   geom_point(aes(shape = pos)) +
#   ggtitle('mGURUwt thinks SS/2B/C are very scarce') +
#   geom_text(
#     data = NULL,
#     x = 10, 
#     y = 80, 
#     label = paste(
#       head(guru_b$nameFirst[order(diffs, decreasing = T)]), 
#       head(guru_b$nameLast[order(diffs, decreasing = T)]),
#       collapse = ', '
#     )
#   )

saveRDS(object = hitter_list, 'rds/hitter-list.rds')
saveRDS(object = pitcher_list, 'rds/pitcher-list.rds')
