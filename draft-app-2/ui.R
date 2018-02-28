library(shiny)
library(DT)
library(sparkline)

shinyUI(
  navbarPage(
    "FBB", inverse = T, 
    
    tabPanel(
      "Submit Pick",
      column(width = 12, offset = 4,
             selectizeInput('team_choice', choices = NULL, label = 'Team'),
             selectizeInput(inputId = 'player_choice', choices = NULL, label = 'Player'),
             actionButton('submit_pick', label = "Submit Pick")
             ),
      hr(),
      DT::dataTableOutput('standings'),
      verbatimTextOutput('picks')
    ),
    
    tabPanel(
      "Overall",
      column(width = 12,
             div(DT::dataTableOutput('overall'))),
      hr(),
      column(width = 12,
             div(uiOutput('ov_pos'), style = "font-size:80%"))),

    tabPanel(
      "Hitters",
      DT::dataTableOutput('hitters'),
      column(width = 12,
             div(uiOutput('h_pos'), style = "font-size:80%")
             )
    ), 
    
    tabPanel(
      "Pitchers",
      DT::dataTableOutput('pitchers'),
      column(width = 12,
             div(uiOutput('p_pos'), style = "font-size:80%")
      )
      
    )
    
  )
)


