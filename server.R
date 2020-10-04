library(shiny)

shinyServer(function(input, output, session) {
    
    source("../BezeirDrawer/decasteljau.R")
    dinamic = reactiveValues()
    dinamic$live_tocke_x = c()
    dinamic$live_tocke_y = c()
    dinamic$selectedpoint = 0
    dinamic$tocke = list()
    dinamic$activename = 1
    dinamic$cp_visible = TRUE
    
    observeEvent(input$shrani, {
        tocke = dinamic$tocke
        activename = dinamic$activename
        dump(c('tocke',
               'activename'),
             file = paste("../BezeirDrawer/drawings/", input$filename, sep = ''))
    })
    
    observeEvent(input$odpri, {
        tryCatch({
            source(paste("../BezeirDrawer/drawings/", input$filename, sep = ''))
            dinamic$tocke = tocke
            dinamic$activename = activename
            dinamic$live_tocke_x = c()
            dinamic$live_tocke_y = c()
            dinamic$selectedpoint = 0
            },
            error=function(cond) {
                message("Here's the original error message:")
                message(cond)
            },
            warning=function(cond) {
                message("Here's the original warning message:")
                message(cond)
            }
        )
    })
    
    observeEvent(input$mode, {
        dinamic$selectedpoint = 0
    })
    
    observeEvent(input$canvasclick, {
        if(input$mode == 'Adding'){
            dinamic$live_tocke_x = c(dinamic$live_tocke_x, input$canvasclick$x)
            dinamic$live_tocke_y = c(dinamic$live_tocke_y, input$canvasclick$y)
        }else if(input$mode == 'Moving live point'){
            if(dinamic$selectedpoint == 0){
                nearest_len = 1000.0
                n = length(dinamic$live_tocke_x)
                if(n > 0){
                    for(i in 1:n){
                        len = (input$canvasclick$x - dinamic$live_tocke_x[i])^2 +
                              (input$canvasclick$y - dinamic$live_tocke_y[i])^2
                        if(len < nearest_len){
                            dinamic$selectedpoint = i
                            nearest_len = len
                        }
                    }
                }
            }else{
                dinamic$live_tocke_x[dinamic$selectedpoint] = input$canvasclick$x
                dinamic$live_tocke_y[dinamic$selectedpoint] = input$canvasclick$y
                dinamic$selectedpoint = 0
            }
        }else{
            ##moving live curve
            if(dinamic$selectedpoint == 0){
                nearest_len = 1000.0
                n = length(dinamic$live_tocke_x)
                if(n > 0){
                    for(i in 1:n){
                        len = (input$canvasclick$x - dinamic$live_tocke_x[i])^2 +
                            (input$canvasclick$y - dinamic$live_tocke_y[i])^2
                        if(len < nearest_len){
                            dinamic$selectedpoint = i
                            nearest_len = len
                        }
                    }
                }
            }else{
                dx = input$canvasclick$x - dinamic$live_tocke_x[dinamic$selectedpoint]
                dy = input$canvasclick$y - dinamic$live_tocke_y[dinamic$selectedpoint]
                for(i in 1:length(dinamic$live_tocke_x)){
                    dinamic$live_tocke_x[i] = dinamic$live_tocke_x[i] + dx
                    dinamic$live_tocke_y[i] = dinamic$live_tocke_y[i] + dy
                }
                dinamic$selectedpoint = 0
            }
        }
    })
    
    observeEvent(input$canvashover, {
        if(!is.null(input$canvashover)){
            if(input$mode == 'Moving live point'){
                if(dinamic$selectedpoint > 0){
                    dinamic$live_tocke_x[dinamic$selectedpoint] = input$canvashover$x
                    dinamic$live_tocke_y[dinamic$selectedpoint] = input$canvashover$y
                }
            }else if(input$mode == 'Moving live curve'){
                if(dinamic$selectedpoint > 0){
                    dx = input$canvashover$x - dinamic$live_tocke_x[dinamic$selectedpoint]
                    dy = input$canvashover$y - dinamic$live_tocke_y[dinamic$selectedpoint]
                    for(i in 1:length(dinamic$live_tocke_x)){
                        dinamic$live_tocke_x[i] = dinamic$live_tocke_x[i] + dx
                        dinamic$live_tocke_y[i] = dinamic$live_tocke_y[i] + dy
                    }
                }
            }
        }
    })
    
    observeEvent(input$removeCurve, {
        dinamic$live_tocke_x = c()
        dinamic$live_tocke_y = c()
        dinamic$selectedpoint = 0
        updateSelectInput(session, 'mode', 'Mode:',
                    choices = c('Adding',
                                'Moving live point',
                                'Moving live curve'),
                    selected = 'Adding')
    })
    
    observeEvent(input$addCurve, {
        mat = matrix(nrow = 2, ncol = length(dinamic$live_tocke_x))
        mat[1,] = dinamic$live_tocke_x
        mat[2,] = dinamic$live_tocke_y
        
        dinamic$tocke[[as.character(dinamic$activename)]] = mat
        
        dinamic$activename = dinamic$activename + 1
        
        dinamic$live_tocke_x = c()
        dinamic$live_tocke_y = c()
        dinamic$selectedpoint = 0
        updateSelectInput(session, 'mode', 'Mode:',
                          choices = c('Adding',
                                      'Moving live point',
                                      'Moving live curve'),
                          selected = 'Adding')
    })
    
    observeEvent(input$cp_vis_change, {
        if(dinamic$cp_visible){
            dinamic$cp_visible = FALSE
        }else{
            dinamic$cp_visible = TRUE
        }
    })
    
    observeEvent(input$clear, {
        dinamic$live_tocke_x = c()
        dinamic$live_tocke_y = c()
        dinamic$selectedpoint = 0
        dinamic$tocke = list()
        dinamic$activename = 1
    })
    
    output$cp_visibility <- renderUI({
        lbl = 'Show control points'
        if(dinamic$cp_visible){
            lbl = 'Hide control points'
        }
        actionButton('cp_vis_change', lbl)
    })

    output$canvas <- renderPlot({
        n = length(dinamic$live_tocke_x)
        t_series = seq(0,1,0.01)
        plot(dinamic$live_tocke_x,
             dinamic$live_tocke_y,
             xlim = c(0,1),
             ylim = c(0,1),
             col = 'blue', type = 'b')
        if(dinamic$selectedpoint > 0){
            points(dinamic$live_tocke_x[dinamic$selectedpoint],
                   dinamic$live_tocke_y[dinamic$selectedpoint],
                   col = 'red', cex = 1.3)
        }
        if(n > 1){
            lb = matrix(nrow = 2, ncol = n)
            lb[1,] = dinamic$live_tocke_x
            lb[2,] = dinamic$live_tocke_y
            live_krivulja = decasteljau(lb, t_series)
            lines(live_krivulja[1,],
                  live_krivulja[2,],
                  col = 'green')
        }
        for (kr in names(dinamic$tocke)) {
            b = dinamic$tocke[[kr]]
            tocke = decasteljau(b,
                                t_series)
            lines(tocke[1,],
                  tocke[2,])
            if(dinamic$cp_visible){
                barve = c()
                st_tock = dim(b)[2]
                for(i in 1:st_tock){
                    barve = c(barve, rgb((st_tock-i)/st_tock,
                                         i/st_tock,
                                         0,
                                         1))
                }
                points(b[1,],
                       b[2,],
                       col = barve)
            }
        }
    })
})
