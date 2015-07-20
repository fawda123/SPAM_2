library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(htmltools)
devtools::load_all('M:/docs/SWMPr')

source('R/funcs.R')

data(ctd_dat)
data(wqm_dat)
data(adcp_dat)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  # subset wqm_dat by selected station
  wqm <- reactive({

    stats <- input$stats
    out <- wqm_dat[wqm_dat$stat %in% stats, ]
    return(out)
    
  })
  
  # first variable plot
  output$var1plot <- renderDygraph({

    varsel <- input$var1
    plo_fun(varsel, wqm())
    
    })

  # second variable plot
  output$var2plot <- renderDygraph({

    varsel <- input$var2
    plo_fun(varsel, wqm())
    
    })

  # third variable plot
  output$var3plot <- renderDygraph({
    
    varsel <- input$var3
    plo_fun(varsel, wqm())
    
    })
  
  # fourth variable plot
  output$var4plot <- renderDygraph({
     
    varsel <- input$var4
    plo_fun(varsel, wqm())
    
    })
  
})