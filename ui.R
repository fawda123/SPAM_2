library(shiny)
library(dygraphs)

data(ctd_dat)
ctd_dts <- as.character(sort(unique(ctd_dat$Date)))

data(adcp_datP)
adcp_dts <- unique(as.Date(adcp_datP$datetimestamp))

source('R/funcs.R')

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  # main panel for variable selection
  mainPanel(width = 12,
      
  # spacing
  fluidRow(HTML('<p></p>')),
  
  #spacing
  fluidRow(HTML('<p></p>')),
    
    tabsetPanel(
      
      tabPanel("WQM", 
    
        HTML('<p></p>'),
        
        # first row of plots
        fluidRow(
          
          column(width = 1,
            img(src = "spam.jpg", width = 80)
          ),
          
          column(width = 9, 
            checkboxGroupInput(inputId = 'stats', 
            label = h4('Stations'),
            choices = c('P02', 'P05-S', 'P05-B'),
            selected = c('P02', 'P05-S', 'P05-B'), 
            inline = TRUE
            )
          ),
          
          # first variable
          column(width = 6,
            
            selectInput(inputId = 'wqm1',
              label = NULL,
              choices = vars('wqm'), 
              selected = vars('wqm')[1], 
              width = '600px'
            ),
                                   
            dygraphOutput("wqm1plot", height = "300px", width = "600px")
          
          ),
          
          # second variable
          column(width = 6, 
          
            selectInput(inputId = 'wqm2',
              label = NULL,
              choices = vars('wqm'), 
              selected = vars('wqm')[2], 
              width = '600px'
            ),
      
            dygraphOutput("wqm2plot", height = "300px", width = "600px")
            
          )
          
        ), 
        
        HTML('<p></p>'),
        
        # second variable
        fluidRow(
          
          column(width = 6,
            
            selectInput(inputId = 'wqm3',
              label = NULL,
              choices = vars('wqm'), 
              selected = vars('wqm')[3], 
              width = '600px'
            ),
                                   
            dygraphOutput("wqm3plot", height = "300px", width = "600px")
          
          ),
          
          column(width = 6, 
          
            selectInput(inputId = 'wqm4',
              label = NULL,
              choices = vars('wqm'), 
              selected = vars('wqm')[4], 
              width = '600px'
            ),
      
            dygraphOutput("wqm4plot", height = "300px", width = "600px")
            
          )
          
        )
        
      ),
    
    tabPanel("CTD by date",
        
        HTML('<p></p>'),
        
        # first row of plots
        fluidRow(
          
          column(width = 1,
            img(src = "spam.jpg", width = 80)
          ),
          
          # select dates
          column(width = 9, 
            selectInput(inputId = 'ctd_dts', 
              label = h4('Date'),
              choices = ctd_dts,
              selected = ctd_dts[1]
            )
          ),
          
          # first variable
          column(width = 6,
            
            selectInput(inputId = 'ctd1',
              label = NULL,
              choices = vars('ctd'), 
              selected = vars('ctd')[1], 
              width = '600px'
            ),
                                   
            plotOutput("ctd1plot1", height = "300px", width = "600px")
          
          ),
          
          # second variable
          column(width = 6, 
          
            selectInput(inputId = 'ctd2',
              label = NULL,
              choices = vars('ctd'), 
              selected = vars('ctd')[2], 
              width = '600px'
            ),
      
            plotOutput("ctd1plot2", height = "300px", width = "600px")
            
          )
          
        ), 
        
        HTML('<p></p>'),
        
        fluidRow(
          
          # second variable
          column(width = 6,
            
            selectInput(inputId = 'ctd3',
              label = NULL,
              choices = vars('ctd'), 
              selected = vars('ctd')[3], 
              width = '600px'
            ),
                                   
            plotOutput("ctd1plot3", height = "300px", width = "600px")
          
          ),
          
          # fourth variable
          column(width = 6, 
          
            selectInput(inputId = 'ctd4',
              label = NULL,
              choices = vars('ctd'), 
              selected = vars('ctd')[4], 
              width = '600px'
            ),
      
            plotOutput("ctd1plot4", height = "300px", width = "600px")
            
          )
          
        )
      
      ),
      
    tabPanel("CTD by variable",
        
        HTML('<p></p>'),
        
        # first row of plots
        fluidRow(
          
          column(width = 1,
            img(src = "spam.jpg", width = 80)
          ),
          
          # select variable
          column(width = 9, 
            selectInput(inputId = 'ctd2sel', 
              label = h4('Variable'),
              choices = vars('ctd'),
              selected = vars('ctd')[1]
            )
          ),
          
          # first date
          column(width = 6,
            
            selectInput(inputId = 'dt1',
              label = NULL,
              choices = ctd_dts, 
              selected = ctd_dts[1], 
              width = '600px'
            ),
                                   
            plotOutput("ctd2plot1", height = "300px", width = "600px")
          
          ),
          
          # second date
          column(width = 6, 
          
            selectInput(inputId = 'dt2',
              label = NULL,
              choices = ctd_dts, 
              selected = ctd_dts[2], 
              width = '600px'
            ),
      
            plotOutput("ctd2plot2", height = "300px", width = "600px")
            
          )
          
        ), 
        
        HTML('<p></p>'),
        
        
        fluidRow(
          
          # third date
          column(width = 6,
            
            selectInput(inputId = 'dt3',
              label = NULL,
              choices = ctd_dts, 
              selected = ctd_dts[3], 
              width = '600px'
            ),
                                   
            plotOutput("ctd2plot3", height = "300px", width = "600px")
          
          ),
          
          # fourth date
          column(width = 6, 
          
            selectInput(inputId = 'dt4',
              label = NULL,
              choices = ctd_dts, 
              selected = ctd_dts[4], 
              width = '600px'
            ),
      
            plotOutput("ctd2plot4", height = "300px", width = "600px")
            
          )
          
        )
      
      ),
      
    tabPanel("ADCP",
        
        HTML('<p></p>'),
                     
        # first row of plots
        fluidRow(
          
          column(width = 1,
            img(src = "spam.jpg", width = 80)
          ),
          
          # select variable
          column(width = 3, 
            
            dateInput(inputId = 'dtsel',
              label = h4('Date'), 
              value = adcp_dts[1],
              min = min(adcp_dts), 
              max = max(adcp_dts)
            )
          
          ),
            
          column(width = 3,
            
            numericInput(inputId = 'hrsel', 
              label = h4('Hour'),
              min = 2, 
              value = 12, 
              max = 24, 
              step = 2
            )
            
          ),
          
          column(width = 3,
            
            numericInput(inputId = 'dpwin', 
              label = h4('Depth plot window (days)'),
              min = 2, 
              value = 8, 
              max = Inf, 
              step = 1
            )
            
          )
          
        ),
      
      fluidRow(
          
          column(width = 3,
            plotOutput("adcpplot")
          ),
        
          column(width = 3,
            plotOutput("presplot", height = "150px", width = "500px")
          )
      )

    ), 
      
  tabPanel("Metabolism",
        
        HTML('<p></p>'),
                     
        # first row of plots
        fluidRow(
          
          column(width = 1,
            img(src = "spam.jpg", width = 80)
          ),
          
          column(width = 3, 
            
            selectInput(inputId = 'aggmeth',
              label = h4('Aggregation method'), 
              choices = c('bins', 'window'),
              selected = 'bins'
            )
            
          ),
          
          # select agg period
          column(width = 3, 
            
            uiOutput('aggby')
            
          ), 
          
          # axis limits
          column(width = 3, 
            
            sliderInput("slider", label = h4("Y-axis limits"), min = -2000, 
              max = 2000, value = c(-500, 500)
            )
          
          ), 
          
          # remove err bars
          column(width = 2, 
      
            checkboxInput("errbars", 
                  label = "Include error bars",
                  value = F)
            
          )
      
      ),
    
      plotOutput("metplot1", height = "300px", width = "1000px"),
      plotOutput("metplot3", height = "300px", width = "1000px"),
      plotOutput("metplot2", height = "300px", width = "1000px")
    
  ), 
      
  tabPanel("DO gradients",
        
        HTML('<p></p>'),
                     
        # first row of plots
        fluidRow(
          
          column(width = 1,
            img(src = "spam.jpg", width = 80)
          ),
          
          # select ctd date to use as center of evaluation
          column(width = 3, 
          
            selectInput(inputId = 'grddt',
              label = h4('Pick CTD date for center'),
              choices = ctd_dts, 
              selected = ctd_dts[5], 
              width = '600px'
            )
            
          ),
          
          # select day window around evaluation date
          column(width = 3, 
            
            numericInput(inputId = 'grdwin', 
              label = h4('Evaluation window (days)'),
              min = 1, 
              value = 14, 
              max = Inf, 
              step = 1
            )
            
          )
      
      ),
    
    plotOutput("grad_plo1", height = "300px", width = "1000px"),
    plotOutput("grad_plo2", height = "300px", width = "1000px"), 
    tableOutput("grad_tab")
    
  )
  
))))