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
# P = 3
# SP = 3
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


options(stringsAsFactors = F)
library(R6)
library(humanparser)
library(borgmisc)
library(parallel)

# classes
source('src/player-classes.R', local = T)

# new projections
fp_proj <- readRDS('projections/2018/2018-projections-parsed-clean.rds')

# old player objects -- move these to our new class definition
# hitters <- readRDS('rds/2015-hitter-list.rds')
# pitchers <- readRDS('rds/2015-pitcher-list.rds')

hitters <- list()
pitchers <- list()

# all_pos <- table(unlist(lapply(fp_proj, function(x) {
#   x[[3]]$Position
# })))

for (i in seq_along(fp_proj)) {
  if (i %% 100 == 0) print(i)
  p <- fp_proj[[i]]
  pp <- fp_proj[[i]][[3]]
  cpos <- ''
  if (any(pp$Games > 10)) {
    cpos <- pp$Position[pp$Games >= 10]
  } else {
    cpos <- pp$Position[which.max(pp$Games)]
  }
  if ('SP' %in% cpos || 'RP' %in% cpos) {
    new_pitch <- Pitcher$new()
    new_pitch$set_field('first_name', fp_proj[[i]][[1]]$firstName)
    new_pitch$set_field('last_name', fp_proj[[i]][[1]]$lastName)
    new_hitter$set_field('MLBAMID', fp_proj[[i]][[1]]$MLBAMID)
    new_pitch$set_pos(cpos)
    
    new_pitch$bb <- fp_proj[[i]][[2]]$BB
    new_pitch$h <- (fp_proj[[i]][[2]]$WHIP * fp_proj[[i]][[2]]$IP) - fp_proj[[i]][[2]]$BB
    new_pitch$era <- fp_proj[[i]][[2]]$ERA
    new_pitch$whip <- fp_proj[[i]][[2]]$WHIP
    new_pitch$k <- fp_proj[[i]][[2]]$K
    new_pitch$k9 <- (fp_proj[[i]][[2]]$K / fp_proj[[i]][[2]]$IP) * 9
    new_pitch$saves <- fp_proj[[i]][[2]]$SV
    new_pitch$ip <- fp_proj[[i]][[2]]$IP
    new_pitch$er <- fp_proj[[i]][[2]]$ER
    new_pitch$wins <- fp_proj[[i]][[2]]$W

    # quality starts: http://fantasybaseballcalculator.webs.com/quality-starts-predictor
    # xqs <- (gs / (er * (gs / gp))) * (ip * (gs / gp)) * (((gs + gp) / (2 * GP))^2)
    if ('SP' %in% cpos) {
      gs <- floor(fp_proj[[i]][[2]]$IP / 6)
      gs[gs == 0] <- mean(gs[gs != 0])
      gp <- sum(pp$Games)
      gp[gp == 0] <- round(mean(gp[gp != 0]))
      ip <- fp_proj[[i]][[2]]$IP
      er <- (fp_proj[[i]][[2]]$ERA / 9) * ip
      xqscore <- (gs / (er * (gs / gp))) * (ip * (gs / gp)) * (((gs + gp) / (2 * gp))^2)
      xqs <- xqscore / 4.115
      new_pitch$qs <- xqs
    } else {
      new_pitch$qs <- 0
    }
    pitchers <- c(pitchers, new_pitch)
    
  } else {
    new_hitter <- Hitter$new()
    new_hitter$set_field('first_name', fp_proj[[i]][[1]]$firstName)
    new_hitter$set_field('last_name', fp_proj[[i]][[1]]$lastName)
    new_hitter$set_field('MLBAMID', fp_proj[[i]][[1]]$MLBAMID)
    new_hitter$set_pos(cpos)
    
    new_hitter$ab <- fp_proj[[i]][[2]]$AB
    new_hitter$hr <- fp_proj[[i]][[2]]$HR
    new_hitter$rbi <- fp_proj[[i]][[2]]$RBI
    new_hitter$obp <- fp_proj[[i]][[2]]$OBP
    new_hitter$sb <- fp_proj[[i]][[2]]$SB
    new_hitter$slg <- fp_proj[[i]][[2]]$SLG
    new_hitter$r <- fp_proj[[i]][[2]]$R
    new_hitter$avg <- fp_proj[[i]][[2]]$AVG
    
    hitters <- c(hitters, new_hitter)
  }
}

# kill some dupes
hnms <- sapply(hitters, function(x) paste(x$first_name, x$last_name))
hdup <- which(duplicated(hnms))
# hnms[hdup]
hitters <- hitters[-hdup]

pnms <- sapply(pitchers, function(x) paste(x$first_name, x$last_name))
pdup <- which(duplicated(pnms))
# pnms[pdup]
pitchers <- pitchers[-pdup]



# compute all of our xSTATs
# xSLG = TB - (AB * league_average)
# xOBP = (H + BB + HBP) - ( (AB + BB + HBP + SF) * league_average)
hf <- do.call(rbind, lapply(hitters, function(x) {
  data.frame(
    'ab' = x$get_stats(stat_name = 'ab'), 
    'avg' = x$get_stats(stat_name = 'avg'),
    'slg' = x$get_stats(stat_name = 'slg'),
    'obp' = x$get_stats(stat_name = 'obp')
  )
}))
hf$tb <- hf[,'ab'] * hf[,'slg']
slg_mean <- mean(sort(hf$slg, decreasing = T)[1:(12*11)], na.rm = T)
obp_mean <- mean(sort(hf$obp, decreasing = T)[1:(12*11)], na.rm = T)
avg_mean <- mean(sort(hf$avg, decreasing = T)[1:(12*11)], na.rm = T)

for (i in seq_along(hitters)) {
  hitters[[i]]$xslg <- (hitters[[i]]$ab * hitters[[i]]$slg) - (hitters[[i]]$ab * slg_mean)
  hitters[[i]]$xobp <- (hitters[[i]]$ab * hitters[[i]]$obp) - (hitters[[i]]$ab * obp_mean)
  hitters[[i]]$xavg <- (hitters[[i]]$ab * hitters[[i]]$avg) - (hitters[[i]]$ab * avg_mean)
}


hp <- do.call(rbind, lapply(hitters, function(x) {
  data.frame(
    slg = x$slg,
    xslg = x$xslg,
    obp = x$obp,
    xopb = x$xobp,
    avg = x$avg,
    xavg = x$xavg
  )
}))
plot(hp[,'xslg']~hp[,'slg'])
plot(hp[,'xopb']~hp[,'obp'])
plot(hp[,'xavg']~hp[,'avg'])

# xERA = ( ER - (IP * league_average) ) * -1
# xWHIP - ( (BB + H) - (IP * league_average) ) * -1
pf <- do.call(rbind, lapply(pitchers, function(x) {
  data.frame(
    'er' = x$get_stats(stat_name = 'er'), 
    'ip' = x$get_stats(stat_name = 'ip'),
    'bb' = x$get_stats(stat_name = 'bb'),
    'h' = x$get_stats(stat_name = 'h'),
    'era' = x$get_stats(stat_name = 'era'),
    'whip' = x$get_stats(stat_name = 'whip'),
    'k9' = x$get_stats(stat_name = 'k9')
  )
}))

era_mean <- mean(sort(pf$era, decreasing = T)[1:(12*11)], na.rm = T)
whip_mean <- mean(sort(pf$whip, decreasing = T)[1:(12*11)], na.rm = T)
k9_mean <- mean(sort(pf$k9, decreasing = T)[1:(12*11)], na.rm = T)

for (i in seq_along(pitchers)) {
  pitchers[[i]]$xera <- (pitchers[[i]]$er * 9 - (pitchers[[i]]$ip * era_mean)) * -1
  pitchers[[i]]$xwhip <- ((pitchers[[i]]$bb + pitchers[[i]]$h) - (pitchers[[i]]$ip * whip_mean)) * -1
  pitchers[[i]]$xk9 <- (pitchers[[i]]$ip * pitchers[[i]]$k9) - (pitchers[[i]]$ip * k9_mean)
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
hstats <- c('hr', 'r', 'rbi', 'sb', 'xavg')
hd <- do.call(rbind, lapply(hitters, function(x){
  sapply(hstats, function(y) x$get_stats(y))
}))
rownames(hd) <- sapply(hitters, function(x) paste(x$first_name, x$last_name))

# only keep our players w/ projections
# hd <- hd[is.finite(hd[,'hr']),]
hds <- scale(hd)
ord <- order(rowSums(hds), decreasing = T)

pdf('reports/2018-category-z-scores-league-2.pdf', height = 40, width = 2)
heat_misc(hds[ord,], z_score = F, axis_scale = .4, row_clust = F)
dev.off()


pstats <- c('k', 'wins', 'saves', 'xera', 'xwhip')
pd <- do.call(rbind, lapply(pitchers, function(x){
  sapply(pstats, function(y) x$get_stats(y))
}))
rownames(pd) <- sapply(pitchers, function(x) paste(x$first_name, x$last_name))
pd <- pd[is.finite(pd[,'k']),]
pds <- scale(pd)

fp <- plyr::rbind.fill(data.frame(hds), data.frame(pds))
nms <- c(rownames(hds), rownames(pds))
nms[which(duplicated(nms))] <- paste(nms[which(duplicated(nms))], "2")
rownames(fp) = nms
ord <- order(rowSums(fp, na.rm = T), decreasing = T)
pdf('reports/2018-category-z-scores-all-league-2.pdf', height = 40, width = 2.5)
heat_misc(fp[ord,], z_score = F, axis_scale = .4, row_clust = F, col_clust = F)
dev.off()



## compute relevant player pool prior to z-scoring....
hstats <- c('hr', 'r', 'rbi', 'sb', 'xobp', 'xslg')
hd <- do.call(rbind, lapply(hitters, function(x){
  sapply(hstats, function(y) x$get_stats(y))
}))
rownames(hd) <- sapply(hitters, function(x) paste(x$first_name, x$last_name))

# only keep our players w/ projections
# hd <- hd[is.finite(hd[,'hr']),]
hds <- scale(hd)
ord <- order(rowSums(hds), decreasing = T)

pdf('reports/2018-category-z-scores-league.pdf', height = 40, width = 2)
heat_misc(hds[ord,], z_score = F, axis_scale = .4, row_clust = F)
dev.off()


pstats <- c('k', 'qs', 'saves', 'xera', 'xwhip', 'xk9')
pd <- do.call(rbind, lapply(pitchers, function(x){
  sapply(pstats, function(y) x$get_stats(y))
}))
rownames(pd) <- sapply(pitchers, function(x) paste(x$first_name, x$last_name))
pd <- pd[is.finite(pd[,'k']),]
pds <- scale(pd)

fp <- plyr::rbind.fill(data.frame(hds), data.frame(pds))
nms <- c(rownames(hds), rownames(pds))
nms[which(duplicated(nms))] <- paste(nms[which(duplicated(nms))], "2")
rownames(fp) = nms
ord <- order(rowSums(fp, na.rm = T), decreasing = T)
pdf('reports/2018-category-z-scores-all-league.pdf', height = 60, width = 2.5)
heat_misc(fp[ord,], z_score = F, axis_scale = .4, row_clust = F, col_clust = F)
dev.off()



ppos <- c("RP", "SP")
updated_players <- c(hitters, pitchers)
sp_locs <- sapply(updated_players, function(x) x$check_pos('SP'))
sp_locs <- which(sp_locs)
rp_locs <- sapply(updated_players, function(x) x$check_pos('RP'))
rp_locs <- which(rp_locs)


###
# roto league 2 ----------------------------------------------------------------
###


# compute our raw z-scores for each player
## our batting categories: R, HR, RBI, SB, OBP, SLG
## our pitching categories: K, QS, SV, ERA, WHIP, K/9
zframe <- do.call(rbind, lapply(updated_players, function(x) {
  data.frame(
    'r' = x$get_stats(stat_name = 'r'), 
    'hr' = x$get_stats(stat_name = 'hr'),
    'rbi' = x$get_stats(stat_name = 'rbi'),
    'sb' = x$get_stats(stat_name = 'sb'),
    'xavg' = x$get_stats(stat_name = 'xavg'),
    'wins' = x$get_stats(stat_name = 'wins'), 
    'k' = x$get_stats(stat_name = 'k'),
    'saves' = x$get_stats(stat_name = 'saves'),
    'xera' = x$get_stats(stat_name = 'xera'),
    'xwhip' = x$get_stats(stat_name = 'xwhip')
  )
}))

zframe[sp_locs, 'saves'] <- NA
zframe[rp_locs, 'wins'] <- NA
zmeans <- apply(zframe, 2, function(x) {
  mean(sort(x, decreasing = T)[1:(12*11)], na.rm = T)
})
zsds <- apply(zframe, 2, sd, na.rm = T)

for (i in seq_along(updated_players)) {
  print(i)
  if (any(updated_players[[i]]$positions %in% ppos)) {
    if (is.null(updated_players[[i]]$saves)) updated_players[[i]]$saves <- rep(0, length(updated_players[[i]]$k))
    if (is.null(updated_players[[i]]$qs) | !length(updated_players[[i]]$qs)) updated_players[[i]]$qs <- rep(0, length(updated_players[[i]]$k))
    updated_players[[i]]$z_scores <- data.frame(
      k = (updated_players[[i]]$k - zmeans['k']) / zsds['k'],
      saves = (updated_players[[i]]$saves - zmeans['saves']) / zsds['saves'],
      xera = (updated_players[[i]]$xera - zmeans['xera']) / zsds['xera'],
      xwhip = (updated_players[[i]]$xwhip - zmeans['xwhip']) / zsds['xwhip'],
      wins = (updated_players[[i]]$wins - zmeans['wins']) / zsds['wins']
    )
  } else {
    try({
      updated_players[[i]]$z_scores <- data.frame(
        r = (updated_players[[i]]$r - zmeans['r']) / zsds['r'],
        hr = (updated_players[[i]]$hr - zmeans['hr']) / zsds['hr'],
        rbi = (updated_players[[i]]$rbi - zmeans['rbi']) / zsds['rbi'],
        sb = (updated_players[[i]]$sb - zmeans['sb']) / zsds['sb'],
        xavg = (updated_players[[i]]$xavg - zmeans['xavg']) / zsds['xavg']
      )
    })
  }
  try(updated_players[[i]]$z_sum <- rowSums(updated_players[[i]]$z_scores))
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
  print(curr_pos)
  replace_idx <- unname(plu[i]) + 1
  pos_zs <- sapply(updated_players, function(x) {
    if (x$check_pos(curr_pos)) return(x$get_stats('z_sum'))
    else return(NA)
  })
  pos_zs <- sort(pos_zs, decreasing = T)
  data.frame(position = curr_pos, replacement_z = pos_zs[replace_idx])
}))

# replace_vals$replacement_z[7:8] <- 1

# set replacemnet level for each player
for (i in seq_along(updated_players)) {
  sel <- which(replace_vals$position %in% updated_players[[i]]$positions)
  if (!length(sel)) {
    updated_players[[i]]$z_replace <- replace_vals$replacement_z[replace_vals$position == '1B']
  }
  else if (!length(sel) & updated_players[[i]]$positions ==  'DH') {
    updated_players[[i]]$z_replace <- replace_vals$replacement_z[replace_vals$position == '1B']
  } else {
    updated_players[[i]]$z_replace <- max(replace_vals$replacement_z[sel])
  }
  updated_players[[i]]$z_vorp <- updated_players[[i]]$z_sum - updated_players[[i]]$z_replace
}


# get our positionally adjusted rankings
zvorps <- sapply(updated_players, function(x) x$get_stats('z_vorp'))
pord <- order(zvorps, decreasing = T)

league_stats <- c("hr", "r", "rbi", "sb", "xavg", "k", "wins", "saves", "xera", "xwhip")
for_plot <- do.call(rbind, lapply(updated_players, function(x){
  sapply(league_stats, function(y) x$get_stats(y))
}))
rownames(for_plot) <- sapply(updated_players, function(x) paste(x$first_name, x$last_name))
for_plot2 <- for_plot[pord, ]
for_plot2 <- scale(for_plot2)

pdf('reports/2018-category-z-scores-pos-adj-league-2.pdf', height = 60, width = 2.3)
heat_misc(for_plot2, z_score = F, axis_scale = .4, row_clust = F, col_clust = F)
dev.off()

fd <- as.data.frame(for_plot2)
fd$pick <- 1:nrow(fd)
fd$round <- ceiling(1:nrow(fd) / 11)
fd$player <- rownames(fd)

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
  try({
    reps <- as.vector(replicate(n = 10000, expr = {
      rowSums(apply(updated_players[[i]]$z_scores, MARGIN = 2, sample, size = nrow(updated_players[[i]]$z_scores), replace = T)) 
    }))
    updated_players[[i]]$boot_z_sum <- quantile(reps, seq(.01, .99, .01))
    updated_players[[i]]$boot_z_vorp <- updated_players[[i]]$boot_z_sum - updated_players[[i]]$z_replace
  })
}


for (i in seq_along(updated_players)) {
  updated_players[[i]]$curr_owner <- "FA"
}


saveRDS(updated_players, 'projections/2018/2018-projections-final-league-2.rds')
# updated_players <- readRDS('projections/2018/2018-projections-final-league-2.rds')

nn <- sapply(updated_players, function(x) {
  paste(x$first_name, x$last_name)
})
names(updated_players) <- nn

# set all our keeper manually.... there aren't that many
updated_players$`Manny Machado`$curr_owner <- "JoshGinsberg"
updated_players$`Freddie Freeman`$curr_owner <- "Faisal"
updated_players$`DJ LeMahieu`$curr_owner <- "Jason"
updated_players$`Charlie Blackmon`$curr_owner <- "Joe"
updated_players$`Christian Yelich`$curr_owner <- "Dan"
updated_players$`Xander Bogaerts`$curr_owner <- "JoshGinsberg"
updated_players$`Anthony Rendon`$curr_owner <- "JoshGinsberg"
updated_players$`Daniel Murphy`$curr_owner <- "Matt"
updated_players$`Hanley Ramirez`$curr_owner <- "Andrew"
updated_players$`Francisco Lindor`$curr_owner <- "Dan"
updated_players$`Billy Hamilton`$curr_owner <- "Joe"
updated_players$`Danny Duffy`$curr_owner <- "Tom"
updated_players$`Dustin Pedroia`$curr_owner <- "Andrew"
updated_players$`Noah Syndergaard`$curr_owner <- "Dan"
updated_players$`Willson Contreras`$curr_owner <- "Faisal"
updated_players$`Corey Seager`$curr_owner <- "MarkDonnelly"
updated_players$`Seung-Hwan Oh`$curr_owner <- "Matt"
updated_players$`A.J. Pollock`$curr_owner <- "Brett"
updated_players$`Odubel Herrera`$curr_owner <- "Brett"
updated_players$`Carlos Correa`$curr_owner <- "Tom"
updated_players$`Evan Longoria`$curr_owner <- "Jason"
updated_players$`Andrew Benintendi`$curr_owner <- "Andrew"
updated_players$`Yu Darvish`$curr_owner <- "Joe"
updated_players$`Jose Ramirez`$curr_owner <- "Faisal"
updated_players$`Kyle Hendricks`$curr_owner <- "MarkDonnelly"
updated_players$`Jean Segura`$curr_owner <- "Mark Brown"
updated_players$`Jose Quintana`$curr_owner <- "Matt"
updated_players$`Eduardo Nunez`$curr_owner <- "Brett"
updated_players$`Alex Bregman`$curr_owner <- "Tom"
updated_players$`J.T. Realmuto`$curr_owner <- "Jason"
updated_players$`Stephen Piscotty`$curr_owner <- "Andrew"
updated_players$`Wil Myers`$curr_owner <- "Dan"
updated_players$`Jonathan Villar`$curr_owner <- "Joe"
updated_players$`Trevor Story`$curr_owner <- "Faisal"
updated_players$`Gary Sanchez`$curr_owner <- "MarkDonnelly"
updated_players$`Roberto Osuna`$curr_owner <- "Mark Brown"
updated_players$`Mark Trumbo`$curr_owner <- "Matt"
updated_players$`Jackie Bradley`$curr_owner <- "Brett"
updated_players$`Trea Turner`$curr_owner <- "Tom"

saveRDS(updated_players, 'projections/2017/2017-projections-final-league-2.rds')



#####
# can we adjust any of the z-scores
#####


zdists <- do.call(plyr::rbind.fill, lapply(updated_players, function (x) {
  tryCatch(expr = {
    data.frame(x$z_scores)
  }, 
  error = function(e) {
    return(NULL)
  }
  )
}))

zdists <- do.call(rbind, lapply(updated_players, function (x) {
  tryCatch(expr = {
    data.frame(
      pos = paste(x$positions, collapse = ','),
      zs = x$z_scores
    )
  }, 
  error = function(e) {
    return(NULL)
  }
  )
}))

boxplot(zs~pos, data = zdists, las = 2)














###
# H2H league 1 -----------------------------------------------------------------
###

ppos <- c("RP", "SP")
updated_players <- c(hitters, pitchers)
sp_locs <- sapply(updated_players, function(x) x$check_pos('SP'))
sp_locs <- which(sp_locs)
rp_locs <- sapply(updated_players, function(x) x$check_pos('RP'))
rp_locs <- which(rp_locs)


# compute our raw z-scores for each player
## our batting categories: R, HR, RBI, SB, OBP, SLG
## our pitching categories: K, QS, SV, ERA, WHIP, K/9
zframe <- do.call(rbind, lapply(updated_players, function(x) {
  data.frame(
    'r' = x$get_stats(stat_name = 'r'), 
    'hr' = x$get_stats(stat_name = 'hr'),
    'rbi' = x$get_stats(stat_name = 'rbi'),
    'sb' = x$get_stats(stat_name = 'sb'),
    'xobp' = x$get_stats(stat_name = 'xobp'),
    'xslg' = x$get_stats(stat_name = 'xslg'),
    'qs' = x$get_stats(stat_name = 'qs'), 
    'k' = x$get_stats(stat_name = 'k'),
    'saves' = x$get_stats(stat_name = 'saves'),
    'xera' = x$get_stats(stat_name = 'xera'),
    'xwhip' = x$get_stats(stat_name = 'xwhip'),
    'xk9' = x$get_stats(stat_name = 'xk9')
  )
}))

zframe[sp_locs, 'saves'] <- NA
zframe[rp_locs, 'wins'] <- NA
zmeans <- apply(zframe, 2, function(x) {
  mean(sort(x, decreasing = T)[1:(15*10)], na.rm = T)
})
zsds <- apply(zframe, 2, sd, na.rm = T)

for (i in seq_along(updated_players)) {
  print(i)
  if (any(updated_players[[i]]$positions %in% ppos)) {
    if (is.null(updated_players[[i]]$saves)) updated_players[[i]]$saves <- rep(0, length(updated_players[[i]]$k))
    if (is.null(updated_players[[i]]$qs) | !length(updated_players[[i]]$qs)) updated_players[[i]]$qs <- rep(0, length(updated_players[[i]]$k))
    updated_players[[i]]$z_scores <- data.frame(
      k = (updated_players[[i]]$k - zmeans['k']) / zsds['k'],
      saves = (updated_players[[i]]$saves - zmeans['saves']) / zsds['saves'],
      xera = (updated_players[[i]]$xera - zmeans['xera']) / zsds['xera'],
      xwhip = (updated_players[[i]]$xwhip - zmeans['xwhip']) / zsds['xwhip'],
      xwhip = (updated_players[[i]]$xk9 - zmeans['xk9']) / zsds['xk9'],
      wins = (updated_players[[i]]$qs - zmeans['qs']) / zsds['qs']
    )
  } else {
    try({
      updated_players[[i]]$z_scores <- data.frame(
        r = (updated_players[[i]]$r - zmeans['r']) / zsds['r'],
        hr = (updated_players[[i]]$hr - zmeans['hr']) / zsds['hr'],
        rbi = (updated_players[[i]]$rbi - zmeans['rbi']) / zsds['rbi'],
        sb = (updated_players[[i]]$sb - zmeans['sb']) / zsds['sb'],
        xavg = (updated_players[[i]]$xslg - zmeans['xslg']) / zsds['xslg'],
        xavg = (updated_players[[i]]$xobp - zmeans['xobp']) / zsds['xobp']
      )
    })
  }
  try(updated_players[[i]]$z_sum <- rowSums(updated_players[[i]]$z_scores))
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
  print(curr_pos)
  replace_idx <- unname(plu[i]) + 1
  pos_zs <- sapply(updated_players, function(x) {
    if (x$check_pos(curr_pos)) return(x$get_stats('z_sum'))
    else return(NA)
  })
  pos_zs <- sort(pos_zs, decreasing = T)
  data.frame(position = curr_pos, replacement_z = pos_zs[replace_idx])
}))

# replace_vals$replacement_z[7:8] <- 1

# set replacemnet level for each player
for (i in seq_along(updated_players)) {
  sel <- which(replace_vals$position %in% updated_players[[i]]$positions)
  if (!length(sel)) {
    updated_players[[i]]$z_replace <- replace_vals$replacement_z[replace_vals$position == '1B']
  }
  else if (!length(sel) & updated_players[[i]]$positions ==  'DH') {
    updated_players[[i]]$z_replace <- replace_vals$replacement_z[replace_vals$position == '1B']
  } else {
    updated_players[[i]]$z_replace <- max(replace_vals$replacement_z[sel])
  }
  updated_players[[i]]$z_vorp <- updated_players[[i]]$z_sum - updated_players[[i]]$z_replace
}


# get our positionally adjusted rankings
zvorps <- sapply(updated_players, function(x) x$get_stats('z_vorp'))
pord <- order(zvorps, decreasing = T)

league_stats <- c("hr", "r", "rbi", "sb", "xobp", "xslg", "k", "qs", "saves", "xwhip", "xera", "xk9")
for_plot <- do.call(rbind, lapply(updated_players, function(x){
  sapply(league_stats, function(y) x$get_stats(y))
}))
rownames(for_plot) <- sapply(updated_players, function(x) paste(x$first_name, x$last_name))
for_plot2 <- for_plot[pord, ]
for_plot2 <- scale(for_plot2)

pdf('reports/2018-category-z-scores-pos-adj-league-1.pdf', height = 60, width = 2.3)
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
  try({
    reps <- as.vector(replicate(n = 10000, expr = {
      rowSums(apply(updated_players[[i]]$z_scores, MARGIN = 2, sample, size = nrow(updated_players[[i]]$z_scores), replace = T)) 
    }))
    updated_players[[i]]$boot_z_sum <- quantile(reps, seq(.01, .99, .01))
    updated_players[[i]]$boot_z_vorp <- updated_players[[i]]$boot_z_sum - updated_players[[i]]$z_replace
  })
}


for (i in seq_along(updated_players)) {
  updated_players[[i]]$curr_owner <- "FA"
}


saveRDS(updated_players, 'projections/2018/2018-projections-final-league-2.rds')

nn <- sapply(updated_players, function(x) {
  paste(x$first_name, x$last_name)
})
names(updated_players) <- nn

# Sachin: RO needs a Closer
# Christian: Jobu Needs a Refill
# Josh: Oh no he didn't
# Mark Donnelly: Car Ram Rod 
# Bull Dozier: Brett Wilson
# Oh No You Di'int: Joshua Sanborn 
# Pedro gives me a Hardy Johnson: Joshua Ginsberg
# It's Wattles ....bitches: Tom Wattles 
# Defending Bronze Medalist: Matt Sterling
# Cottage Cheese Industry: Joe Finland 
# SaNo Means Yes: Faisal Khan
# Judge, Jury and Executioner: christian
# Jobu Needs A Refill: Sachin K
lu <- function(nme) {
  grep(nme, nn)
}

# set all our keeper manually.... there aren't that many
updated_players$`Cody Bellinger`$curr_owner <- "It's Wattles ....bitches"
updated_players$`Luis Severino`$curr_owner <- "It's Wattles ....bitches"
updated_players$`Matt Olson`$curr_owner <- "It's Wattles ....bitches"
updated_players$`Ozzie Albies`$curr_owner <- "It's Wattles ....bitches"

updated_players$`Giancarlo Stanton`$curr_owner <- "Bull Dozier"
updated_players$`Yasiel Puig`$curr_owner <- "Bull Dozier"
updated_players$`Eddie Rosario`$curr_owner <- "Bull Dozier"

updated_players$`Elvis Andrus`$curr_owner <- "Defending Bronze Medalist"
updated_players$`Jose Quintana`$curr_owner <- "Defending Bronze Medalist"
updated_players$`Raisel Iglesias`$curr_owner <- "Defending Bronze Medalist"
updated_players$`Brett Gardner`$curr_owner <- "Defending Bronze Medalist"

updated_players$`Andrew Benintendi`$curr_owner <- "Me"
updated_players$`Trea Turner`$curr_owner <- "Me"

updated_players$`Marcell Ozuna`$curr_owner <- "Pedro gives me a Hardy Johnson"
updated_players$`Mike Moustakas`$curr_owner <- "Pedro gives me a Hardy Johnson"
updated_players$`Aaron Nola`$curr_owner <- "Pedro gives me a Hardy Johnson"

updated_players$`Gary Sanchez`$curr_owner <- "Car Ram Rod"
updated_players$`Tommy Pham`$curr_owner <- "Car Ram Rod"
updated_players$`Whit Merrifield`$curr_owner <- "Car Ram Rod"
updated_players$`Rhys Hoskins`$curr_owner <- "Car Ram Rod"

updated_players$`Willson Contreras`$curr_owner <- "SaNo Means Yes"
updated_players$`Jose Ramirez`$curr_owner <- "SaNo Means Yes"
updated_players$`Trevor Story`$curr_owner <- "SaNo Means Yes"
updated_players$`Paul DeJong`$curr_owner <- "SaNo Means Yes"

updated_players$`Billy Hamilton`$curr_owner <- "Cottage Cheese Industry"
updated_players$`Yu Darvish`$curr_owner <- "Cottage Cheese Industry"

updated_players$`Byron Buxton`$curr_owner <- "Judge, Jury and Executioner"
updated_players$`Aaron Judge`$curr_owner <- "Judge, Jury and Executioner"
updated_players$`Alex Bregman`$curr_owner <- "Judge, Jury and Executioner"
updated_players$`Rafael Devers`$curr_owner <- "Judge, Jury and Executioner"

updated_players$`Jonathan Schoop`$curr_owner <- "Oh No You Di'int"
updated_players$`Travis Shaw`$curr_owner <- "Oh No You Di'int"
updated_players$`Jake Lamb`$curr_owner <- "Oh No You Di'int"
updated_players$`Jay Bruce`$curr_owner <- "Oh No You Di'int"

updated_players$`Christian Yelich`$curr_owner <- "Jobu Needs A Refill"
updated_players$`Wil Myers`$curr_owner <- "Jobu Needs A Refill"
updated_players$`Robbie Ray`$curr_owner <- "Jobu Needs A Refill"
updated_players$`James Paxton`$curr_owner <- "Jobu Needs A Refill"




saveRDS(updated_players, 'projections/2018/2018-league-2-draft.rds')



#####
# can we adjust any of the z-scores
#####


zdists <- do.call(plyr::rbind.fill, lapply(updated_players, function (x) {
  tryCatch(expr = {
    data.frame(x$z_scores)
  }, 
  error = function(e) {
    return(NULL)
  }
  )
}))

zdists <- do.call(rbind, lapply(updated_players, function (x) {
  tryCatch(expr = {
    data.frame(
      pos = paste(x$positions, collapse = ','),
      zs = x$z_scores
    )
  }, 
  error = function(e) {
    return(NULL)
  }
  )
}))

boxplot(zs~pos, data = zdists, las = 2)




