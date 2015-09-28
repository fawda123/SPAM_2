library(reshape)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(htmltools)
library(maptools)
library(SWMPr)
library(WtRegDO)

source('R/funcs.R')

data(ctd_dat)
data(wqm_dat)
data(adcp_datP)
data(met_dat)
data(pbay)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  # ui for metab plots
  output$aggby <- renderUI({

    aggmeth <- input$aggmeth
    
    if(aggmeth == 'bins'){
      
      selectInput(inputId = 'aggperiod',
        label = h4('Binning period'), 
        choices = c('days', 'weeks', 'months'),
        selected = 'days'
        )
    
    } else {
  
      numericInput(inputId = 'aggperiod', 
        label = h4('Averaging window (days)'),
        min = 1, 
        value = 5, 
        max = 1000, 
        step = 1
        )
      
    }
  
  })
  
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
  
  # adcp based on date, hour input
  adcp <- reactive({
    
    adcp_dt <- input$dtsel
    adcp_hr <- input$hrsel
    tosel <- as.POSIXct(paste(as.character(adcp_dt), adcp_hr), format = '%Y-%m-%d %H', 
      tz = 'America/Regina')
    absval <- abs(adcp_datP$datetimestamp - tosel)
    tosel <- absval %in% min(absval)
    out <- adcp_datP[tosel, ]
    
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
    ctd_plot(ctd1(), ctdsel)
    
    })

  # second ctd1 plot
  output$ctd1plot2 <- renderPlot({

    ctdsel <- input$ctd2
    ctd_plot(ctd1(), ctdsel)
    
    })

  # third ctd1 plot
  output$ctd1plot3 <- renderPlot({
    
    ctdsel <- input$ctd3
    ctd_plot(ctd1(), ctdsel)
    
    })
  
  # fourth ctd1 plot
  output$ctd1plot4 <- renderPlot({
     
    ctdsel <- input$ctd4
    ctd_plot(ctd1(), ctdsel)
    
    })
  
  ######
  # ctd by variable
  
  # first ctd2 plot
  output$ctd2plot1 <- renderPlot({
    
    toplo <- ctd2()[[1]]
    ctd_plot(toplo, input$ctd2sel, rngs_in = rngs())
    
    })

  # second ctd2 plot
  output$ctd2plot2 <- renderPlot({

    toplo <- ctd2()[[2]]
    ctd_plot(toplo, input$ctd2sel, rngs_in = rngs())
    
    })

  # third ctd2 plot
  output$ctd2plot3 <- renderPlot({

    toplo <- ctd2()[[3]]
    ctd_plot(toplo, input$ctd2sel, rngs_in = rngs())
    
    })
  
  # fourth ctd2 plot
  output$ctd2plot4 <- renderPlot({

    toplo <- ctd2()[[4]]
    ctd_plot(toplo, input$ctd2sel, rngs_in = rngs())
    
    })
  
  ######
  # adcp plot
  
  output$adcpplot <- renderPlot({
  
    # day window for depth
    dpwin <- input$dpwin
    dpwin <- dpwin * 60 * 60 * 24/2
    browser()
    # plot
    plot_adcp(adcp(), adcp_datP, shp_in = pbay, loc_in = c(-87.13208, 30.45682), 
      fixed_y = FALSE, win = dpwin)
    
    })
  
  ######
  # metab plots
  
  # p02
  output$metplot1 <- renderPlot({

    aggperiod <- input$aggperiod
    if(is.null(aggperiod)) aggperiod <- 'days'
    errbars <- input$errbars
    slider <- input$slider
    if(!errbars) errbars <- NULL
    else errbars <-  0.05

    # plot
    plot(met_dat[[1]], by = aggperiod, alpha = errbars, width = 5) +
      ggtitle('P02') +
      scale_y_continuous(limits = slider)
    
    })
  
  # P05-B
  output$metplot2 <- renderPlot({
  
    aggperiod <- input$aggperiod
    if(is.null(aggperiod)) aggperiod <- 'days'
    errbars <- input$errbars
    slider <- input$slider
    if(!errbars) errbars <- NULL
    else errbars <-  0.05
    
    # plot
    plot(met_dat[[2]], by = aggperiod, alpha = errbars, width = 5) +
      ggtitle('P05-B') +
      scale_y_continuous(limits = slider)
    
    })
  
  # P05-S
  output$metplot3 <- renderPlot({
  
    aggperiod <- input$aggperiod
    if(is.null(aggperiod)) aggperiod <- 'days'
    errbars <- input$errbars
    slider <- input$slider
    if(!errbars) errbars <- NULL
    else errbars <-  0.05
    
    # plot
    plot(met_dat[[3]], by = aggperiod, alpha = errbars, width = 5) +
      ggtitle('P05-S') +
      scale_y_continuous(limits = slider)
    
    })
  
  
  ######
  # DO gradient plots
  
  # cumulative distance plot of tidal excursion
  output$grad_plo1 <- renderPlot({
  
    grdwin <- input$grdwin
    grddt <- input$grddt

    grad_plo(wqm_dat, adcp_datP, ctd_dat, dt_in = grddt, win_in = grdwin, dist_plo = TRUE, stats_out = FALSE)
    
    })
  
  # DO gradient plot
  output$grad_plo2 <- renderPlot({
  
    grdwin <- input$grdwin
    grddt <- input$grddt

    grad_plo(wqm_dat, adcp_datP, ctd_dat, dt_in = grddt, win_in = grdwin, dist_plo = FALSE, stats_out = FALSE)
    
    })
  
  # summary table
  output$grad_tab <- renderTable({
  
    grdwin <- input$grdwin
    grddt <- input$grddt

    grad_plo(wqm_dat, adcp_datP, ctd_dat, dt_in = grddt, win_in = grdwin, stats_out = TRUE)
    
    })
  
})