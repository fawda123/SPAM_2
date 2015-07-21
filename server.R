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
    out$stat <- droplevels(out$stat)
    return(out)
    
  })
  
  # subset ctd_dat by selected date
  ctd1 <- reactive({
    
    sel_date <- as.Date(input$ctd_dts)
    out <- ctd_dat[ctd_dat$Date %in% sel_date, ]
    return(out)
    
  })
  
  # list of ctd data for comparison by variable, need to do this to get rngs
  ctd2 <- reactive({
    
    dtsel1 <- as.Date(input$dt1)
    dtsel2 <- as.Date(input$dt2)
    dtsel3 <- as.Date(input$dt3)
    dtsel4 <- as.Date(input$dt4)
    out <- list(
      ctd_dat[ctd_dat$Date %in% dtsel1, ], 
      ctd_dat[ctd_dat$Date %in% dtsel2, ], 
      ctd_dat[ctd_dat$Date %in% dtsel3, ], 
      ctd_dat[ctd_dat$Date %in% dtsel4, ]
    )
    
    return(out)
    
  })
  
  # range of variables for ctd by variable
  rngs <- reactive({
    
    dtsel1 <- as.Date(input$dt1)
    dtsel2 <- as.Date(input$dt2)
    dtsel3 <- as.Date(input$dt3)
    dtsel4 <- as.Date(input$dt4)
    out <- list(
      ctd_dat[ctd_dat$Date %in% dtsel1, ], 
      ctd_dat[ctd_dat$Date %in% dtsel2, ], 
      ctd_dat[ctd_dat$Date %in% dtsel3, ], 
      ctd_dat[ctd_dat$Date %in% dtsel4, ]
    )
    out <- get_rngs(out)
    
    return(out)
    
  })

  
  ######
  # wqm
  
  # first wqm plot
  output$wqm1plot <- renderDygraph({

    wqmsel <- input$wqm1
    plo_fun(wqmsel, wqm())
    
    })

  # second wqm plot
  output$wqm2plot <- renderDygraph({

    wqmsel <- input$wqm2
    plo_fun(wqmsel, wqm())
    
    })

  # third wqm plot
  output$wqm3plot <- renderDygraph({
    
    wqmsel <- input$wqm3
    plo_fun(wqmsel, wqm())
    
    })
  
  # fourth wqm plot
  output$wqm4plot <- renderDygraph({
     
    wqmsel <- input$wqm4
    plo_fun(wqmsel, wqm())
    
    })
  
  ######
  # ctd by date
  
  # first ctd1 plot
  output$ctd1plot1 <- renderPlot({

    ctdsel <- input$ctd1
    ctd_plot2(ctd1(), ctdsel)
    
    })

  # second ctd1 plot
  output$ctd1plot2 <- renderPlot({

    ctdsel <- input$ctd2
    ctd_plot2(ctd1(), ctdsel)
    
    })

  # third ctd1 plot
  output$ctd1plot3 <- renderPlot({
    
    ctdsel <- input$ctd3
    ctd_plot2(ctd1(), ctdsel)
    
    })
  
  # fourth ctd1 plot
  output$ctd1plot4 <- renderPlot({
     
    ctdsel <- input$ctd4
    ctd_plot2(ctd1(), ctdsel)
    
    })
  
  ######
  # ctd by variable
  
  # first ctd2 plot
  output$ctd2plot1 <- renderPlot({
    
    toplo <- ctd2()[[1]]
    ctd_plot2(toplo, input$ctd2sel, rngs_in = rngs())
    
    })

  # second ctd2 plot
  output$ctd2plot2 <- renderPlot({

    toplo <- ctd2()[[2]]
    ctd_plot2(toplo, input$ctd2sel, rngs_in = rngs())
    
    })

  # third ctd2 plot
  output$ctd2plot3 <- renderPlot({

    toplo <- ctd2()[[3]]
    ctd_plot2(toplo, input$ctd2sel, rngs_in = rngs())
    
    })
  
  # fourth ctd2 plot
  output$ctd2plot4 <- renderPlot({

    toplo <- ctd2()[[4]]
    ctd_plot2(toplo, input$ctd2sel, rngs_in = rngs())
    
    })
  
})