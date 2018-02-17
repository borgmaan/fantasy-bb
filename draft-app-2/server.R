library(R6)
library(shiny)
library(dplyr)
library(DT)
library(borgmisc)
library(sparkline)
source('../src/player-classes.R', local = T)
source('app-helpers.R', local = T)

 # dat <- readRDS('../projections/2016/2016-projections-final.rds')
dat <- readRDS('../projections/2017/2017-projections-final-league-2.rds')
names(dat) <- sapply(dat, function(x) paste(x$first_name, x$last_name))
teams <- c("JoshGinsberg", "Faisal", "Jason", "Joe", "Dan", "Matt", "Andrew", 
          "Tom", "MarkDonnelly", "Brett", "MarkBrown")
all_positions <- c("SP", "RP", "1B", "3B", "2B", "SS", "OF", "C", "DH")
hitter_positions <- c("1B", "3B", "2B", "SS", "OF", "C", "DH")
pitcher_positions <- c("SP", "RP")

## sparklines stuff
r <- c(-2, 9)
box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
cb_box2 = JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                    box_string, ", chartRangeMin: ", r[1], ", chartRangeMax: ", r[2], " }); }"), 
             collapse = "")


shinyServer(
  function(input, output, session) {
   
    # config -------------------------------------------------------------------
    
    # trigger objects for application
    triggers <- reactiveValues()
    triggers[['dat']] <- dat
    triggers[['name_idx']] <- sapply(dat, function(x) paste(x$first_name, x$last_name))
    triggers[['fa_sel']] <- sapply(dat, function(x) x$curr_owner == 'FA')
    triggers[['standings']] <- project_standings(dat)
    triggers[['picks']] <- c()
    

    # control drafting of players on to teams ----------------------------------
    
    updateSelectizeInput(session, 'player_choice', 
                         choices = isolate({
                           names(triggers[['dat']])[triggers[['fa_sel']]]
                           }), server = TRUE)
    updateSelectizeInput(session, 'team_choice', choices = teams, server = TRUE)
    
    observeEvent(input$submit_pick, {
      triggers[['picks']] <- c(
        paste(input$player_choice, 'was drafted by', input$team_choice),
        triggers[['picks']]
      )
      nsel <- which(triggers[['name_idx']] == input$player_choice)
      triggers[['dat']][[nsel]]$curr_owner <- input$team_choice
      triggers[['fa_sel']][nsel] <- FALSE
      triggers[['standings']] <- project_standings(triggers[['dat']])
      saveRDS(object = triggers[['dat']], 'last-pick.rds')
    })
    
    output$picks <- renderText(paste(triggers[['picks']], collapse = '\n'))
    
    output$standings <- DT::renderDataTable({
      DT::datatable(triggers[['standings']], rownames = F)
    })
    
    
    # overall table ------------------------------------------------------------
    
    output$overall = DT::renderDataTable({
      pov <- get_player_frame(dat = triggers[['dat']][triggers[['fa_sel']]], positions = 'all')
      cd <- list(list(targets = (which(names(pov) == 'vorp_dist') - 1), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      ovd <- datatable(
        pov,
        rownames = F,
        options = list(columnDefs = cd,  drawCallback = cb_box2)
        )
      ovd$dependencies <- append(ovd$dependencies, htmlwidgets:::getDependency("sparkline"))
      ovd
    })
    
    output$ov_pos <- renderUI({
      lapply(all_positions, function(x) {
        column(3, render_pos(triggers[['dat']][triggers[['fa_sel']]], x))
      })
    })
    
    # hitter table -------------------------------------------------------------
    
    output$hitters = DT::renderDataTable({
      hov <- get_player_frame(
        dat = triggers[['dat']][triggers[['fa_sel']]], 
        positions = 'hitters',
        stats = c('VORP', 'Raw Z', 'r', 'hr', 'rbi', 'sb', 'xavg', 'vorp_dist')
      )
      cd <- list(list(targets = (which(names(hov) == 'vorp_dist') - 1), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      hod <- datatable(
        hov,
        rownames = F,
        options = list(columnDefs = cd,  drawCallback = cb_box2)
      )
      hod$dependencies <- append(hod$dependencies, htmlwidgets:::getDependency("sparkline"))
      hod
    })
    
    output$h_pos <- renderUI({
      lapply(hitter_positions, function(x) {
        column(3, render_pos(triggers[['dat']][triggers[['fa_sel']]], x))
      })
    })
    
    
    
    # pitcher table ------------------------------------------------------------
    
    output$pitchers = DT::renderDataTable({
      pov <- get_player_frame(
        dat = triggers[['dat']][triggers[['fa_sel']]], 
        positions = 'pitchers',
        stats = c('VORP', 'Raw Z', 'k', 'wins', 'saves', 'xera', 'xwhip', 'vorp_dist')
      )
      cd <- list(list(targets = (which(names(pov) == 'vorp_dist') - 1), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))
      pod <- datatable(
        pov,
        rownames = F,
        options = list(columnDefs = cd,  drawCallback = cb_box2)
      )
      pod$dependencies <- append(pod$dependencies, htmlwidgets:::getDependency("sparkline"))
      pod
      
      
    })
    
    output$p_pos <- renderUI({
      lapply(pitcher_positions, function(x) {
        column(3, render_pos(triggers[['dat']][triggers[['fa_sel']]], x))
      })
    })
    
    
  }
)
