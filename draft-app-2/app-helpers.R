
get_player_frame <- function(dat, positions = 'all', stats = 
                               NULL) {
  
  if (positions == 'all') positions = c("3B", "1B", "SP", "2B", "OF", "DH", "C", "SS", "RP")
  else if (positions == 'hitters') positions = c("3B", "1B", "2B", "OF", "DH", "C", "SS")
  else if (positions == 'pitchers') positions = c("SP", "RP")
  
  psub <- do.call(rbind, lapply(dat, function(x) {
    if (x$check_pos(to_check = positions)) {
      suppressWarnings({
        data.frame(
          'Name' = paste(x$first_name, x$last_name),
          'Position' = paste(x$positions, collapse = ','),
          'VORP' = x$boot_z_vorp['50%'],
          'Raw Z' = x$boot_z_sum['50%'],
          'r' = x$get_stats(stat_name = 'r'), 
          'hr' = x$get_stats(stat_name = 'hr'),
          'rbi' = x$get_stats(stat_name = 'rbi'),
          'sb' = x$get_stats(stat_name = 'sb'),
          'xavg' = x$get_stats(stat_name = 'xavg'),
          'k' = x$get_stats(stat_name = 'k'), 
          'wins' = x$get_stats(stat_name = 'wins'),
          'saves' = x$get_stats(stat_name = 'saves'),
          'xera' = x$get_stats(stat_name = 'xera'),
          'xwhip' = x$get_stats(stat_name = 'xwhip'),
          'vorp_dist' = paste(round(x$boot_z_vorp,2), collapse = ','),
          check.names = F,
          stringsAsFactors = FALSE
        )
      })
    } else {
      return(NULL)
    }
  }))
  
  rownames(psub) <- NULL
  psub[] <- lapply(psub, function(x) {
    if (is.numeric(x)) return(round(x, 2))
    else return(x)
  })
  
  if (!is.null(stats)) {
    stats <- union(c('Name', 'Position'), stats)
    tk <- intersect(stats, names(psub))
    psub <- psub[,tk]
  }
  
  return(psub)

  }


###
# maybe these should be stored in our reactiveValuse object so they auto update
###

render_pos <- function(dat, pos) {
  df <- get_player_frame(dat, pos, c('VORP', 'Raw Z', '25%'))
  df <- df[order(df$VORP, decreasing = T),]
  renderTable(df[1:10,], include.rownames = FALSE)
}


# creates a ui output for a single team
get_team <- function(dat, team) {
  
}

# project standings based off of mean of projections and picks thus far
project_standings <- function(dat) {
  teams <- c("Car Ram Rod", "SaNo Means Yes", "It's Wattles ....bitches", 
             "Jobu Needs A Refill", "Oh No You Di'int", "Me", "Judge, Jury and Executioner", 
             "Defending Bronze Medalist", "Pedro gives me a Hardy Johnson", 
             "Bull Dozier", "Cottage Cheese Industry")
  stats <- c('r', 'hr', 'rbi', 'sb', 'xavg', 'k', 'wins', 'saves', 'xera', 
             'xwhip')

  standings <- do.call(rbind, lapply(teams, function(team) {
    suppressWarnings({
      tsel <- which(sapply(dat, function(x) x$curr_owner == team))
      if (length(tsel)) {
        tstat <- do.call(cbind, lapply(stats, function(stat) {
          round(sum(sapply(dat[tsel], function(x) x$get_stats(stat_name = stat)), na.rm = T), 2)
        }))
        colnames(tstat) <- stats
        return(cbind.data.frame(data.frame(Team = team), tstat))
      } else {
        return(data.frame(
          Team = team,
          'r' = NA,
          'hr' = NA,
          'rbi' = NA,
          'sb' = NA,
          'xavg' = NA,
          'k' = NA,
          'wins' = NA,
          'saves' = NA,
          'xera' = NA,
          'xwhip' = NA
        ))
      }
    })
  }))
  
  standings[] <- lapply(standings, function(x) {
    x[is.na(x)] <- 0
    x
  })
  standings$roto_score <- rowSums(apply(standings[,-1], 2, rank))
  return(standings)
}
