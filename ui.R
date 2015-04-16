library(shiny)

shinyUI(
  navbarPage(
    "Draft Dashboard", inverse = T, 
    
    
    tabPanel(
      "Overall",
      
      # first row has our barcode lookup field
      fluidRow(
        column(
          width = 11,
          align="center",
          DT::dataTableOutput('overall_tab')
        ),
        column(
          width = 1,
          radioButtons(
            inputId = 'fa_toggle_ov', 
            label = "FA Toggle", 
            choices = c("All Available" = 'all', 
                        "FA Only" = 'fa')
          )
        )
      )      
      
    ),
    # -- overall end -- #
    
    tabPanel(
      "Hitters",
      
      # first row has our barcode lookup field
      fluidRow(
        column(
          width = 11,
          align="center",
          DT::dataTableOutput('hitter_tab')
        ),
        column(
          width = 1,
          radioButtons(
            inputId = 'fa_toggle_hit', 
            label = "FA Toggle", 
            choices = c("All Available" = 'all', 
                        "FA Only" = 'fa')
          ),
          checkboxGroupInput(
            inputId = 'pos_sel_hit', 
            label = "Position Selector", 
            choices = c("OF", "1B", "DH", "2B", 
                        "SS", "3B", "C"),
            selected = c("OF", "1B", "DH", "2B", 
                         "SS", "3B", "C")
          )
        )
      ),
      fluidRow(
        column(
          width = 5, 
          offset = 4, 
          plotOutput('hitter_plot', width = '300px', height = '1800px')
        )
      )
    ), # -- hitter end -- #
    
    tabPanel(
      "Pitchers",
      
      # first row has our barcode lookup field
      fluidRow(
        column(
          width = 11,
          align="center",
          DT::dataTableOutput('pitcher_tab')
        ),
        column(
          width = 1,
          radioButtons(
            inputId = 'fa_toggle_pit', 
            label = "FA Toggle", 
            choices = c("All Available" = 'all', 
                        "FA Only" = 'fa')
          ),
          checkboxGroupInput(
            inputId = 'pos_sel_pit', 
            label = "Position Selector", 
            choices = c("SP", "RP"),
            selected = c("SP", "RP")
          )
        )
      ),
      fluidRow(
        column(
          width = 5, 
          offset = 4, 
          plotOutput('pitcher_plot', width = '300px', height = '1800px')
        )
      )      
    )
    # -- pitchers end -- #
    
  )
)


