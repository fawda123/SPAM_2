library(shiny)
library(dygraphs)

source('R/funcs.R')

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # main panel for variable selection
  mainPanel(width = 12,
      
  # spacing
  fluidRow(HTML('<p></p>')),
  
  # top controls
  fluidRow(
    
    column(width = 3,
      actionButton('runmod', label = img(src = "spam.jpg", width = 120))
    ),
    
    column(width = 4, 
      checkboxGroupInput(inputId = 'stats', 
        label = h4('Stations'),
        choices = c('P02', 'P05-S', 'P05-B'),
        selected = c('P02', 'P05-S', 'P05-B'), 
        inline = TRUE
      )
    )
    
  ),
  
  #spacing
  fluidRow(HTML('<p></p>')),
    
    tabsetPanel(
      
      tabPanel("WQM", 
    
        HTML('<p></p>'),
        
        # first row of plots
        fluidRow(
          
          # first variable
          column(width = 6,
            
            selectInput(inputId = 'var1',
              label = NULL,
              choices = vars(), 
              selected = vars()[1], 
              width = '600px'
            ),
                                   
            dygraphOutput("var1plot", height = "300px", width = "600px")
          
          ),
          
          # second variable
          column(width = 6, 
          
            selectInput(inputId = 'var2',
              label = NULL,
              choices = vars(), 
              selected = vars()[2], 
              width = '600px'
            ),
      
            dygraphOutput("var2plot", height = "300px", width = "600px")
            
          )
          
        ), 
        
        HTML('<p></p>'),
        
        # second variable
        fluidRow(
          
          column(width = 6,
            
            selectInput(inputId = 'var3',
              label = NULL,
              choices = vars(), 
              selected = vars()[3], 
              width = '600px'
            ),
                                   
            dygraphOutput("var3plot", height = "300px", width = "600px")
          
          ),
          
          column(width = 6, 
          
            selectInput(inputId = 'var4',
              label = NULL,
              choices = vars(), 
              selected = vars()[4], 
              width = '600px'
            ),
      
            dygraphOutput("var4plot", height = "300px", width = "600px")
            
          )
          
        )
        
      ),
    
    tabPanel("CTD",
        
        h3('Nothing!'),     
        HTML('<br></br>'),
        h3('Nothing again!')
      
      ),
      
    tabPanel("ADCP",
             
        h3('More nothing!')
        
      )
    
    )
            
  )
  
))