#!/usr/bin/env Rscript

# Hitters
# C = 1
# 1B = 1
# 2B = 1
# 3B = 1
# SS = 1
# 2B/SS = 1
# 1B/3B = 1
# OF = 5
# UTIL = 1
# Total Hitters = 13
# 
# Pitchers
# SP = 6
# RP = 3
# Total Pitchers = 9
# 
# Total Bench = 4
# 
# Total Player Pool = 260
# max hitters = 170
# min hitters = 130
# max pitchers = 130
# max hitters = 90

# projecting quality starts
# http://fantasybaseballcalculator.webs.com/quality-starts-predictor
# http://www.baseballprof.com/2013/01/fantasy-baseball-strategy-how-to-project-wins-quality-starts/
# http://www.fangraphs.com/plus/predicting-the-quality-start/
# numbers! http://razzball.com/playerrater-preseason-espnmlb12-7x7qsholds/

options(stringsAsFactors = F)
library(R6)
library(humanparser)
library(borgmisc)
library(parallel)

# classes
source('src/player-classes.R', local = T)

# new projections and consensus ranks
fp_rank <- readRDS('projections/2016/2016-fp-overall.rds')
fp_proj <- readRDS('projections/2016/2016-projections-parsed.rds')

# old player objects -- move these to our new class definition
hitters <- readRDS('rds/2015-hitter-list.rds')
pitchers <- readRDS('rds/2015-pitcher-list.rds')

# null out all of the fields we need to populate with new knowledge and fix some other stuff
for (i in seq_along(hitters)) {
  new_hitter <- Hitter$new()
  new_hitter$set_field('first_name', hitters[[i]]$first_name)
  new_hitter$set_field('last_name', hitters[[i]]$last_name)
  new_hitter$set_field('team', hitters[[i]]$team)
  new_hitter$set_pos(hitters[[i]]$positions)
  hitters[[i]] <- new_hitter
}

for (i in seq_along(pitchers)) {
  new_pitch <- Pitcher$new()
  new_pitch$set_field('first_name', pitchers[[i]]$first_name)
  new_pitch$set_field('last_name', pitchers[[i]]$last_name)
  new_pitch$set_field('team', pitchers[[i]]$team)
  new_pitch$set_pos(pitchers[[i]]$positions)
  pitchers[[i]] <- new_pitch
}


# fill up with our new projections and create any new players for this year
all(fp_rank$player == names(fp_proj))

hitter_first_names <- sapply(hitters, function(x) x$first_name)
hitter_last_names <- sapply(hitters, function(x) x$last_name)
pitcher_first_names <- sapply(pitchers, function(x) x$first_name)
pitcher_last_names <- sapply(pitchers, function(x) x$last_name)


for (i in 1:nrow(fp_rank)) {
  
  # try to find a match for the 
  curr_name <- parse_name(fp_rank$player[i])
  
  # do we have a hitter or a pitcher
  if (grepl('(SP|RP)', fp_rank$position[i])) {
    fm <- grep(curr_name$firstName, pitcher_first_names)
    lm <- grep(curr_name$lastName, pitcher_last_names)
    mm <- intersect(fm, lm)
    if (length(mm) == 1) {
      # found perfect pitcher match
      
      pitchers[[mm]]$bb <- fp_proj[[i]]$BB
      pitchers[[mm]]$h <- fp_proj[[i]]$H
      pitchers[[mm]]$era <- fp_proj[[i]]$ERA
      pitchers[[mm]]$whip <- fp_proj[[i]]$WHIP
      pitchers[[mm]]$k <- fp_proj[[i]]$K
      pitchers[[mm]]$k9 <- (fp_proj[[i]]$K / fp_proj[[i]]$IP) * 9
      pitchers[[mm]]$saves <- fp_proj[[i]]$SV
      pitchers[[mm]]$ip <- fp_proj[[i]]$IP
      pitchers[[mm]]$er <- fp_proj[[i]]$ER

      # quality starts: http://fantasybaseballcalculator.webs.com/quality-starts-predictor
      # xqs <- (gs / (er * (gs / gp))) * (ip * (gs / gp)) * (((gs + gp) / (2 * GP))^2)
      gs <- fp_proj[[i]]$GS
      gs[gs == 0] <- mean(gs[gs != 0])
      gp <- fp_proj[[i]]$G
      gp[gp == 0] <- round(mean(gp[gp != 0]))
      er <- fp_proj[[i]]$ER
      ip <- fp_proj[[i]]$IP
      xqscore <- (gs / (er * (gs / gp))) * (ip * (gs / gp)) * (((gs + gp) / (2 * gp))^2)
      xqs <- xqscore / 4.115
      pitchers[[mm]]$qs <- xqs

    } else if (length(mm) == 0) {
      # didn't find match - might need to create new player
      # also might need to check if 
      message('no matches for ', curr_name$fullName)
    } else {
      # found multiple matches
      message('multiple matches for ', curr_name$fullName)
    }
  } else {
    fm <- grep(curr_name$firstName, hitter_first_names)
    lm <- grep(curr_name$lastName, hitter_last_names)
    mm <- intersect(fm, lm)
    if (length(mm) == 1) {
      # found perfect hitter match
      hitters[[mm]]$ab <- fp_proj[[i]]$AB
      hitters[[mm]]$h <- fp_proj[[i]]$H
      hitters[[mm]]$hr <- fp_proj[[i]]$HR
      hitters[[mm]]$rbi <- fp_proj[[i]]$RBI
      hitters[[mm]]$obp <- fp_proj[[i]]$OBP
      hitters[[mm]]$sb <- fp_proj[[i]]$SB
      hitters[[mm]]$slg <- fp_proj[[i]]$SLG
      hitters[[mm]]$r <- fp_proj[[i]]$R
    } else if (length(mm) == 0) {
      # didn't find match - might need to create new player
      # also might need to check if
      message('no matches for ', curr_name$fullName)
    } else {
      # found multiple matches
      message('multiple matches for ', curr_name$fullName)
    }
  }
}


## our batting categories: R, HR, RBI, SB, OBP, SLG



# compute all of our xSTATs
# xSLG = TB - (AB * league_average)
# xOBP = (H + BB + HBP) - ( (AB + BB + HBP + SF) * league_average)
hf <- do.call(rbind, lapply(hitters, function(x) {
  data.frame(
    'ab' = x$get_stats(stat_name = 'ab'), 
    'slg' = x$get_stats(stat_name = 'slg'),
    'obp' = x$get_stats(stat_name = 'obp')
  )
}))
hf$tb <- hf[,'ab'] * hf[,'slg']
slg_mean <- mean(hf$slg, na.rm = T)
obp_mean <- mean(hf$obp, na.rm = T)

for (i in seq_along(hitters)) {
  hitters[[i]]$xslg <- (hitters[[i]]$ab * hitters[[i]]$slg) - (hitters[[i]]$ab * slg_mean)
  hitters[[i]]$xobp <- (hitters[[i]]$ab * hitters[[i]]$obp) - (hitters[[i]]$ab * obp_mean)
}


hp <- do.call(rbind, lapply(hitters, function(x) {
  data.frame(
    slg = x$slg,
    xslg = x$xslg,
    obp = x$obp,
    xopb = x$xobp
  )
}))
plot(hp[,'xslg']~hp[,'slg'])
plot(hp[,'xopb']~hp[,'obp'])


# xERA = ( ER - (IP * league_average) ) * -1
# xWHIP - ( (BB + H) - (IP * league_average) ) * -1
pf <- do.call(rbind, lapply(pitchers, function(x) {
  data.frame(
    'er' = x$get_stats(stat_name = 'er'), 
    'ip' = x$get_stats(stat_name = 'ip'),
    'bb' = x$get_stats(stat_name = 'bb'),
    'h' = x$get_stats(stat_name = 'h'),
    'era' = x$get_stats(stat_name = 'era'),
    'whip' = x$get_stats(stat_name = 'whip')
  )
}))

era_mean <- mean(pf$era, na.rm = T)
whip_mean <- mean(pf$whip, na.rm = T)

for (i in seq_along(pitchers)) {
  pitchers[[i]]$xera <- (pitchers[[i]]$er * 9 - (pitchers[[i]]$ip * era_mean)) * -1
  pitchers[[i]]$xwhip <- ((pitchers[[i]]$bb + pitchers[[i]]$h) - (pitchers[[i]]$ip * whip_mean)) * -1
}

pp <- do.call(rbind, lapply(pitchers, function(x) {
  data.frame(
    era = x$era,
    xera = x$xera,
    whip = x$whip,
    xwhip = x$xwhip
  )
}))
plot(pp[, 'xera'] ~ pp[, 'era'])
plot(pp[, 'xwhip'] ~ pp[, 'whip'])



## compute relevant player pool prior to z-scoring....
hstats <- c('hr', 'r', 'rbi', 'sb', 'xslg', 'xobp')
hd <- do.call(rbind, lapply(hitters, function(x){
  sapply(hstats, function(y) x$get_stats(y))
}))
rownames(hd) <- sapply(hitters, function(x) paste(x$first_name, x$last_name))

# only keep our players w/ projections
hd <- hd[is.finite(hd[,'hr']),]
hds <- scale(hd)
ord <- order(rowSums(hds), decreasing = T)

pdf('reports/category-z-scores.pdf', height = 20, width = 2.5)
heat_misc(hds[ord,], z_score = F, axis_scale = .4, row_clust = F)
dev.off()


pstats <- c('k', 'k9', 'qs', 'saves', 'xera', 'xwhip')
pd <- do.call(rbind, lapply(pitchers, function(x){
  sapply(pstats, function(y) x$get_stats(y))
}))
rownames(pd) <- sapply(pitchers, function(x) paste(x$first_name, x$last_name))
pd <- pd[is.finite(pd[,'k']),]
pds <- scale(pd)

fp <- plyr::rbind.fill(data.frame(hds), data.frame(pds))
rownames(fp) = c(rownames(hds), rownames(pds))
ord <- order(rowSums(fp, na.rm = T), decreasing = T)
pdf('reports/category-z-scores-all.pdf', height = 40, width = 2.5)
heat_misc(fp[ord,], z_score = F, axis_scale = .4, row_clust = F, col_clust = F)
dev.off()


hd <- do.call(rbind, lapply(hitters, function(x){
  sapply(hstats, function(y) x$get_stats(y))
}))
rownames(hd) <- sapply(hitters, function(x) paste(x$first_name, x$last_name))



# one option is to use our end of season rosters from last seasons -- these are in 
# projections/2016/eos-rosters/
eos <- 'projections/2016/eos-rosters'
peos <- do.call(rbind, lapply(dir(eos, pattern = '^p', full.names = T), read.delim))
heos <- do.call(rbind, lapply(dir(eos, pattern = '^h', full.names = T), read.delim))
peos$type <- 'pitcher'
heos$type <- 'hitter'
aos <- rbind(peos, heos)


# pull in our updated ESPN info with everyone's choices for keepers and 
# our updated position info for all our players...
ed <- 'projections/2016/espn-players-post-keep/'
espn <- do.call(plyr::rbind.fill, lapply(dir(ed, full.names = T), read.delim))
espn$nfull <- sapply(strsplit(espn$PLAYER..TEAM.POS, ','), '[', 1)
parsed <- parse_names(espn$nfull)
team_pos <- sapply(strsplit(trim(gsub('(DL60)|(DTD)', '', espn$PLAYER..TEAM.POS)), ','), '[', -1)
espn$positions <- sapply(team_pos, function(x) {
  if (length(x) == 1) {
    trim(substr(x, nchar(x)-1, nchar(x)))
  } else {
    paste(c(trim(substr(x[1], nchar(x[1])-1, nchar(x))), trim(x[-1])), collapse = ',')
  }
})

# merge our two lists of players
fp_rank$idx <- 1:nrow(fp_rank)

# fix a few of the names that don't line up
espn$nfull[which(espn$nfull == 'Jacob deGrom')] <- "Jacob DeGrom"
espn$nfull[which(espn$nfull == 'Ken Giles')] <- "Kenneth Giles"
espn$nfull[which(espn$nfull == 'Brad Boxberger')] <- "Bradley Boxberger"
espn$nfull[which(espn$nfull == 'Byung Ho Park')] <- "Byung-ho Park"
espn$nfull[which(espn$nfull == 'Jung Ho Kang')] <- "Jung-Ho Kang"
espn$nfull[which(espn$nfull == 'Steven Souza Jr.')] <- "Steven Souza"

#Jacob deGrom -- Jacob DeGrom
# Ken Giles -- Kenneth Giles
# Brad Boxberger -- Bradley Boxberger
# Byung Ho Park -- Byung-ho Park
# Jung Ho Kang -- Jung-Ho Kang
# Steven Souza Jr. -- Steven Souza

mm <- merge(fp_rank, espn, by.x = 'player', by.y = 'nfull')
mm <- mm[order(mm$idx, na.last = T),]
mm$positions[which(mm$player == 'Aroldis Chapman')] <- 'RP'

# build up our new list of players
ppos <- c("RP", "RP,SP", "SP", "SP,RP")
updated_players <- lapply(1:nrow(mm), function(ix) {

  i <- mm$idx[ix]
    
  if (mm$positions[ix] %in% ppos) {
    player <- Pitcher$new()
    player$bb <- fp_proj[[i]]$BB
    player$h <- fp_proj[[i]]$H
    player$era <- fp_proj[[i]]$ERA
    player$whip <- fp_proj[[i]]$WHIP
    player$k <- fp_proj[[i]]$K
    player$k9 <- (fp_proj[[i]]$K / fp_proj[[i]]$IP) * 9
    player$saves <- fp_proj[[i]]$SV
    player$ip <- fp_proj[[i]]$IP
    player$er <- fp_proj[[i]]$ER
    gs <- fp_proj[[i]]$GS
    gs[gs == 0] <- mean(gs[gs != 0])
    gp <- fp_proj[[i]]$G
    gp[gp == 0] <- round(mean(gp[gp != 0]))
    er <- fp_proj[[i]]$ER
    ip <- fp_proj[[i]]$IP
    xqscore <- (gs / (er * (gs / gp))) * (ip * (gs / gp)) * (((gs + gp) / (2 * gp))^2)
    xqs <- xqscore / 4.115
    player$qs <- xqs

  } else {
    player <- Hitter$new()
    player$ab <- fp_proj[[i]]$AB
    player$h <- fp_proj[[i]]$H
    player$hr <- fp_proj[[i]]$HR
    player$rbi <- fp_proj[[i]]$RBI
    player$obp <- fp_proj[[i]]$OBP
    player$sb <- fp_proj[[i]]$SB
    player$slg <- fp_proj[[i]]$SLG
    player$r <- fp_proj[[i]]$R
  }
  
  player$set_pos(unlist(strsplit(mm$positions[ix], ',')))
  player$set_field('curr_owner', mm$TYPE[ix])
  np <- parse_name(mm$player[ix])
  player$set_field('first_name', np$firstName)
  player$set_field('last_name', np$lastName)

  return(player)
})


# compute all of our xSTATs
# xSLG = TB - (AB * league_average)
# xOBP = (H + BB + HBP) - ( (AB + BB + HBP + SF) * league_average)
# xERA = ( ER - (IP * league_average) ) * -1
# xWHIP - ( (BB + H) - (IP * league_average) ) * -1
rate_stats <- do.call(rbind, lapply(updated_players, function(x) {
  data.frame(
    'ab' = x$get_stats(stat_name = 'ab'), 
    'slg' = x$get_stats(stat_name = 'slg'),
    'obp' = x$get_stats(stat_name = 'obp'),
    'er' = x$get_stats(stat_name = 'er'), 
    'ip' = x$get_stats(stat_name = 'ip'),
    'bb' = x$get_stats(stat_name = 'bb'),
    'h' = x$get_stats(stat_name = 'h'),
    'era' = x$get_stats(stat_name = 'era'),
    'whip' = x$get_stats(stat_name = 'whip'),
    'k9' = x$get_stats(stat_name = 'k9')
  )
}))
rate_stats$tb <- rate_stats[,'ab'] * rate_stats[,'slg']
slg_mean <- mean(rate_stats$slg, na.rm = T)
obp_mean <- mean(rate_stats$obp, na.rm = T)
era_mean <- mean(rate_stats$era, na.rm = T)
whip_mean <- mean(rate_stats$whip, na.rm = T)
k9_mean <- mean(rate_stats$k9, na.rm = T)

for (i in seq_along(updated_players)) {
  if (any(updated_players[[i]]$positions %in% ppos)) {
    updated_players[[i]]$xera <- (updated_players[[i]]$er * 9 - (updated_players[[i]]$ip * era_mean)) * -1
    updated_players[[i]]$xwhip <- ((updated_players[[i]]$bb + updated_players[[i]]$h) - (updated_players[[i]]$ip * whip_mean)) * -1
    updated_players[[i]]$xk9 <- (updated_players[[i]]$ip * updated_players[[i]]$k9) - (updated_players[[i]]$ip * k9_mean)
  } else {
    updated_players[[i]]$xslg <- (updated_players[[i]]$ab * updated_players[[i]]$slg) - (updated_players[[i]]$ab * slg_mean)
    updated_players[[i]]$xobp <- (updated_players[[i]]$ab * updated_players[[i]]$obp) - (updated_players[[i]]$ab * obp_mean)
  }
}


# plot(sapply(updated_players, function(x) x$get_stats(stat_name = 'k9')), sapply(updated_players, function(x) x$get_stats(stat_name = 'xk9')))


# compute our raw z-scores for each player
## our batting categories: R, HR, RBI, SB, OBP, SLG
## our pitching categories: K, QS, SV, ERA, WHIP, K/9
zframe <- do.call(rbind, lapply(updated_players, function(x) {
  data.frame(
    'r' = x$get_stats(stat_name = 'r'), 
    'hr' = x$get_stats(stat_name = 'hr'),
    'rbi' = x$get_stats(stat_name = 'rbi'),
    'sb' = x$get_stats(stat_name = 'sb'),
    'xslg' = x$get_stats(stat_name = 'xslg'),
    'xobp' = x$get_stats(stat_name = 'xobp'),
    'k' = x$get_stats(stat_name = 'k'), 
    'qs' = x$get_stats(stat_name = 'qs'),
    'saves' = x$get_stats(stat_name = 'saves'),
    'xera' = x$get_stats(stat_name = 'xera'),
    'xwhip' = x$get_stats(stat_name = 'xwhip'),
    'xk9' = x$get_stats(stat_name = 'xk9')
  )
}))
zmeans <- apply(zframe, 2, mean, na.rm = T)
zsds <- apply(zframe, 2, sd, na.rm = T)

for (i in seq_along(updated_players)) {
  if (any(updated_players[[i]]$positions %in% ppos)) {
    if (is.null(updated_players[[i]]$saves)) updated_players[[i]]$saves <- rep(0, length(updated_players[[i]]$k))
    if (is.null(updated_players[[i]]$qs) | !length(updated_players[[i]]$qs)) updated_players[[i]]$qs <- rep(0, length(updated_players[[i]]$k))
    updated_players[[i]]$z_scores <- data.frame(
      k = (updated_players[[i]]$k - zmeans['k']) / zsds['k'],
      qs = (updated_players[[i]]$qs - zmeans['qs']) / zsds['qs'],
      saves = (updated_players[[i]]$saves - zmeans['saves']) / zsds['saves'],
      xera = (updated_players[[i]]$xera - zmeans['xera']) / zsds['xera'],
      xwhip = (updated_players[[i]]$xwhip - zmeans['xwhip']) / zsds['xwhip'],
      xk9 = (updated_players[[i]]$xk9 - zmeans['xk9']) / zsds['xk9']
    )
  } else {
    updated_players[[i]]$z_scores <- data.frame(
      r = (updated_players[[i]]$r - zmeans['r']) / zsds['r'],
      hr = (updated_players[[i]]$hr - zmeans['hr']) / zsds['hr'],
      rbi = (updated_players[[i]]$rbi - zmeans['rbi']) / zsds['rbi'],
      sb = (updated_players[[i]]$sb - zmeans['sb']) / zsds['sb'],
      xslg = (updated_players[[i]]$xslg - zmeans['xslg']) / zsds['xslg'],
      xobp = (updated_players[[i]]$xobp - zmeans['xobp']) / zsds['xobp']
    )
  }
  updated_players[[i]]$z_sum <- rowSums(updated_players[[i]]$z_scores)
}


# positional breakdowns

# one way to deal with it - remove the top 10 1B and 3B, remove these from the,
# them compute the positional breakdown for the next 1B/3B and use those to
# figure out who the replacement level player is for each position

# grab all 1B/3B players and rank them
corners <- do.call(rbind, lapply(updated_players, function(p) {
  if (p$check_pos('1B') | p$check_pos('3B')) {
    return(data.frame(
      name = paste(p$first_name, p$last_name),
      pos = paste(p$positions, collapse = ','),
      is_first = p$check_pos('1B'),
      is_third = p$check_pos('3B'),
      mean_z = p$get_stats('z_sum')
    ))
  } else {
    return(NULL)
  }
}))
corners <- corners[order(corners$mean_z, decreasing = T),]
corners$top3b <- corners$name %in% corners[corners$is_third, 'name'][1:10]
corners$top1b <- corners$name %in% corners[corners$is_first, 'name'][1:10]
not_top <- corners[!corners$top3b & !corners$top1b, ]
extra_1b <- sum(not_top[1:10,"is_first"])
extra_3b <- sum(not_top[1:10,"is_third"])

# grab all 1B/3B players and rank them
middle <- do.call(rbind, lapply(updated_players, function(p) {
  if (p$check_pos('2B') | p$check_pos('SS')) {
    return(data.frame(
      name = paste(p$first_name, p$last_name),
      pos = paste(p$positions, collapse = ','),
      is_second = p$check_pos('2B'),
      is_short = p$check_pos('SS'),
      mean_z = p$get_stats('z_sum')
    ))
  } else {
    return(NULL)
  }
}))
middle <- middle[order(middle$mean_z, decreasing = T),]
middle$top2b <- middle$name %in% middle[middle$is_second, 'name'][1:10]
middle$topss <- middle$name %in% middle[middle$is_short, 'name'][1:10]
not_top <- middle[!middle$top2b & !middle$topss, ]
extra_2b <- sum(not_top[1:10,"is_second"])
extra_ss <- sum(not_top[1:10,"is_short"])

plu <- c(
  'C' = 10,
  '1B' = 10 + extra_1b,
  '2B' = 10 + extra_2b,
  '3B' = 10 + extra_3b,
  'SS' = 10 + extra_ss,
  'OF' = 50,
  'SP' = 60,
  'RP' = 30
)

replace_vals <- do.call(rbind, lapply(seq_along(plu), function(i) {
  curr_pos <- names(plu)[i]
  replace_idx <- unname(plu[i]) + 1
  pos_zs <- sapply(updated_players, function(x) {
    if (x$check_pos(curr_pos)) return(x$get_stats('z_sum'))
    else return(NA)
  })
  pos_zs <- sort(pos_zs, decreasing = T)
  data.frame(position = curr_pos, replacement_z = pos_zs[replace_idx])
}))

# set replacemnet level for each player
for (i in seq_along(updated_players)) {
  sel <- which(replace_vals$position %in% updated_players[[i]]$positions)
  if (!length(sel) & updated_players[[i]]$positions ==  'DH') {
    updated_players[[i]]$z_replace <- replace_vals$replacement_z[replace_vals$position == '1B']
  } else {
    updated_players[[i]]$z_replace <- max(replace_vals$replacement_z[sel])
  }
  updated_players[[i]]$z_vorp <- updated_players[[i]]$z_sum - updated_players[[i]]$z_replace
}


# get our positionally adjusted rankings
zvorps <- sapply(updated_players, function(x) x$get_stats('z_vorp'))
pord <- order(zvorps, decreasing = T)

league_stats <- c("hr", "r", "rbi", "sb", "xslg", "xobp", "k", "k9", "qs", "saves", "xera", "xwhip")
for_plot <- do.call(rbind, lapply(updated_players, function(x){
  sapply(league_stats, function(y) x$get_stats(y))
}))
rownames(for_plot) <- sapply(updated_players, function(x) paste(x$first_name, x$last_name))
for_plot2 <- for_plot[pord, ]
for_plot2 <- scale(for_plot2)

pdf('reports/category-z-scores-pos-adj.pdf', height = 40, width = 2.5)
heat_misc(for_plot2, z_score = F, axis_scale = .4, row_clust = F, col_clust = F)
dev.off()


# only keep free agents
fas <- unlist(sapply(updated_players, function(x) {
  if (x$curr_owner == 'FA') {
    return(paste(x$first_name, x$last_name))
  }
}))

for_plot3 <- for_plot2[rownames(for_plot2) %in% fas, ]

pdf('reports/category-z-scores-pos-adj-FA.pdf', height = 40, width = 2.5)
heat_misc(for_plot3, z_score = F, axis_scale = .4, row_clust = F, col_clust = F)
dev.off()

# save.image(file = 'bak.Rdata')
# print(load('bak.Rdata'))

# compute bootstrap distributions of z-scores for all of our players


for (i in seq_along(updated_players)) {
  print(i)
  reps <- as.vector(replicate(n = 10000, expr = {
    rowSums(apply(updated_players[[i]]$z_scores, MARGIN = 2, sample, size = nrow(updated_players[[i]]$z_scores), replace = T)) 
  }))
  updated_players[[i]]$boot_z_sum <- quantile(reps, seq(.01, .99, .01))
  updated_players[[i]]$boot_z_vorp <- updated_players[[i]]$boot_z_sum - updated_players[[i]]$z_replace
}

saveRDS(updated_players, 'projections/2016/2016-projections-final.rds')



