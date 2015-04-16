library(shiny)
library(dplyr)
library(borgmisc)

dat <- readRDS('app-data.rds')

shinyServer(
  function(input, output, session) {
    
    
    # filter our group of hitters based on our input
    get_hitter_rows <- reactive({
      
      if (length(input$pos_sel) == 1) {
        curr_log <- dat$hitter_mapper[,input$pos_sel_hit]
      } else {
        rowsel <- dat$hitter_mapper[,input$pos_sel_hit]
        curr_log <- rep(FALSE, nrow(dat$hitter_mapper))
        for (i in 1:ncol(rowsel)) curr_log <- curr_log | rowsel[,i]
      }
      
      if (input$fa_toggle_hit == 'all') {
        return(which(curr_log))
      } else {
        fa_sel <- dat$hitter_mapper$curr_owner == 'FA'
        return(which(fa_sel & curr_log))
      }
      
    })
    
    # outputs for our hitter rankings
    output$hitter_tab = DT::renderDataTable({
      sel <- get_hitter_rows()
      ranks <- dat$hitter_rankings[sel, c("ESPN",  "mGURUwt", "FPRO","adjWERTH", "zWAR", "mean_rank", "median_rank")]
      info <- dat$hitter_mapper[sel,1:4]
      DT::datatable(cbind.data.frame(info, ranks))
    })
    
    # I WOULD REALLY LIKE TO ORDER THIS BY MEDIAN RANK
    output$hitter_plot <- renderPlot({
      sel <- get_hitter_rows()[1:100]
      fp <- scale(dat$hitter_stats[sel, ])
      rownames(fp) <- dat$hitter_mapper$full_name[sel]
      
      fp2 <- cbind.data.frame(
        fp, 
        zwerth = rowSums(fp, na.rm = T)
      )
      
      fp2 <- fp2[order(-fp2$zwerth), ]
      heat_misc(fp2, z_score = F, row_clust = F, col_clust = F, mar_padding = c(0,5,0,0))
    })    
    
    
    
    ### PITCHERS
    
    
    # filter our group of hitters based on our input
    get_pitcher_rows <- reactive({
      
      if (length(input$pos_sel) == 1) {
        curr_log <- dat$pitcher_mapper[,input$pos_sel_pit]
      } else {
        rowsel <- dat$pitcher_mapper[,input$pos_sel_pit]
        curr_log <- rep(FALSE, nrow(dat$pitcher_mapper))
        for (i in 1:ncol(rowsel)) curr_log <- curr_log | rowsel[,i]
      }
      
      if (input$fa_toggle_pit == 'all') {
        return(which(curr_log))
      } else {
        fa_sel <- dat$pitcher_mapper$curr_owner == 'FA'
        return(which(fa_sel & curr_log))
      }
      
    })
    
    # outputs for our pitcher rankings
    output$pitcher_tab = DT::renderDataTable({
      sel <- get_pitcher_rows()
      ranks <- dat$pitcher_rankings[sel, ]
      info <- dat$pitcher_mapper[sel,1:4]
      DT::datatable(cbind.data.frame(info, ranks))
    })
    
    # I WOULD REALLY LIKE TO ORDER THIS BY MEDIAN RANK
    output$pitcher_plot <- renderPlot({
      sel <- get_pitcher_rows()[1:100]
      dd <- dat$pitcher_stats[sel, ]
      dd[,'whip'] <- -dd[,'whip']
      dd[,'era'] <- -dd[,'era']
      fp <- scale(dd)
      rownames(fp) <- dat$pitcher_mapper$full_name[sel]
      
      fp2 <- cbind.data.frame(
        fp, 
        zwerth = rowSums(fp, na.rm = T)
      )
      
      fp2 <- fp2[order(-fp2$zwerth), ]
      heat_misc(fp2, z_score = F, row_clust = F, col_clust = F, mar_padding = c(0,5,0,0))
    })    
    
    
    # overall
    
    # filter our group of hitters based on our input
    
    # outputs for our pitcher rankings
    output$overall_tab = DT::renderDataTable({
      
      if (input$fa_toggle_ov == 'all') {
        DT::datatable(dat$global_vals)
      } else {
        DT::datatable(filter(dat$global_vals, Owner == 'FA'))
      }
    })

    
  }
)