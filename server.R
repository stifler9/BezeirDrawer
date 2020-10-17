library(shiny)

shinyServer(function(input, output, session) {
    
    source("../BezeirDrawer/decasteljau.R")
    dinamic = reactiveValues()
    ## curves to edit
    dinamic$live_tocke_x = list()
    dinamic$live_tocke_y = list()
    dinamic$selectedcurve = 0
    dinamic$selectedpoint = 0 # on selected curve
    
    dinamic$tocke = list()
    dinamic$activename = 1
    
    dinamic$cp_visible = TRUE
    
    ### file actions
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
            
            dinamic$live_tocke_x = list()
            dinamic$live_tocke_y = list()
            dinamic$selectedcurve = 0
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
    
    observeEvent(input$clear, {
        dinamic$live_tocke_x = list()
        dinamic$live_tocke_y = list()
        dinamic$selectedcurve = 0
        dinamic$selectedpoint = 0
        dinamic$tocke = list()
        dinamic$activename = 1
    })
    ###
    
    observeEvent(input$mode, {
        dinamic$selectedcurve = 0
        dinamic$selectedpoint = 0
    })
    
    
    ### clicking functions ###
    curveclicked = function(x, y){
        nearest = 1000.0
        sel = 0
        for (i in 1:length(dinamic$live_tocke_x)) {
            for(j in 1:length(dinamic$live_tocke_x[[i]])){
                len = (x - dinamic$live_tocke_x[[i]][j])^2 +
                      (y - dinamic$live_tocke_y[[i]][j])^2
                if(len < nearest){
                    sel = i
                    nearest = len
                }
            }
        }
        return(sel)
    }
    
    ## called when curveselected != 0
    pointclicked = function(x, y){
        nearest = 1000.0
        sel = 0
        for (i in 1:length(dinamic$live_tocke_x[[dinamic$selectedcurve]])) {
            len = (x - dinamic$live_tocke_x[[dinamic$selectedcurve]][i])^2 +
                  (y - dinamic$live_tocke_y[[dinamic$selectedcurve]][i])^2
            if(len < nearest){
                sel = i
                nearest = len
            }
        }
        return(sel)
    }
    ###
    
    ### plot mouse control
    observeEvent(input$canvasclick, {
        if(input$mode == 'Adding to curve'){
            if(dinamic$selectedcurve == 0){
                if(length(dinamic$live_tocke_x) > 0){
                    dinamic$selectedcurve = curveclicked(input$canvasclick$x,
                                                         input$canvasclick$y)
                }else{ ##no curve to add to
                    updateSelectInput(session, 'mode', 'Mode:',
                                      selected = 'New curve')
                }
            }else{
                dinamic$live_tocke_x[[dinamic$selectedcurve]] = c(dinamic$live_tocke_x[[dinamic$selectedcurve]], input$canvasclick$x)
                dinamic$live_tocke_y[[dinamic$selectedcurve]] = c(dinamic$live_tocke_y[[dinamic$selectedcurve]], input$canvasclick$y)
            }
        }else if(input$mode == 'New curve'){
            nxt = length(dinamic$live_tocke_x) + 1
            dinamic$live_tocke_x[[nxt]] = c(input$canvasclick$x)
            dinamic$live_tocke_y[[nxt]] = c(input$canvasclick$y)
            updateSelectInput(session, 'mode', 'Mode:',
                              selected = 'Adding to curve')
        }else if(input$mode == 'Moving live point'){
            if(dinamic$selectedcurve == 0){
                if(length(dinamic$live_tocke_x) > 0){
                    dinamic$selectedcurve = curveclicked(input$canvasclick$x,
                                                         input$canvasclick$y)
                }else{ ##no curve to add to
                    updateSelectInput(session, 'mode', 'Mode:',
                                      selected = 'New curve')
                }
            }else{
                if(dinamic$selectedpoint == 0){
                    dinamic$selectedpoint = pointclicked(input$canvasclick$x,
                                                         input$canvasclick$y)
                }else{
                    dinamic$live_tocke_x[[dinamic$selectedcurve]][dinamic$selectedpoint] = input$canvasclick$x
                    dinamic$live_tocke_y[[dinamic$selectedcurve]][dinamic$selectedpoint] = input$canvasclick$y
                    dinamic$selectedpoint = 0
                }
            }
        }else{
            ##moving live curve
            if(dinamic$selectedcurve == 0){
                if(length(dinamic$live_tocke_x) > 0){
                    dinamic$selectedcurve = curveclicked(input$canvasclick$x,
                                                         input$canvasclick$y)
                }else{ ##no curve to add to
                    updateSelectInput(session, 'mode', 'Mode:',
                                      selected = 'New curve')
                }
            }else{
                if(dinamic$selectedpoint == 0){
                    dinamic$selectedpoint = pointclicked(input$canvasclick$x,
                                                         input$canvasclick$y)
                }else{
                    dx = input$canvasclick$x - dinamic$live_tocke_x[[dinamic$selectedcurve]][dinamic$selectedpoint]
                    dy = input$canvasclick$y - dinamic$live_tocke_y[[dinamic$selectedcurve]][dinamic$selectedpoint]
                    for(i in 1:length(dinamic$live_tocke_x[[dinamic$selectedcurve]])){
                        dinamic$live_tocke_x[[dinamic$selectedcurve]][i] = dinamic$live_tocke_x[[dinamic$selectedcurve]][i] + dx
                        dinamic$live_tocke_y[[dinamic$selectedcurve]][i] = dinamic$live_tocke_y[[dinamic$selectedcurve]][i] + dy
                    }
                    dinamic$selectedpoint = 0
                }
            }
        }
    })
    
    observeEvent(input$canvashover, {
        if(!is.null(input$canvashover)){
            if(input$mode == 'Moving live point'){
                if(dinamic$selectedcurve > 0){
                    if(dinamic$selectedpoint > 0){
                        dinamic$live_tocke_x[[dinamic$selectedcurve]][dinamic$selectedpoint] = input$canvashover$x
                        dinamic$live_tocke_y[[dinamic$selectedcurve]][dinamic$selectedpoint] = input$canvashover$y
                    }
                }
            }else if(input$mode == 'Moving live curve'){
                if(dinamic$selectedcurve > 0){
                    if(dinamic$selectedpoint > 0){
                        dx = input$canvashover$x - dinamic$live_tocke_x[[dinamic$selectedcurve]][dinamic$selectedpoint]
                        dy = input$canvashover$y - dinamic$live_tocke_y[[dinamic$selectedcurve]][dinamic$selectedpoint]
                        for(i in 1:length(dinamic$live_tocke_x[[dinamic$selectedcurve]])){
                            dinamic$live_tocke_x[[dinamic$selectedcurve]][i] = dinamic$live_tocke_x[[dinamic$selectedcurve]][i] + dx
                            dinamic$live_tocke_y[[dinamic$selectedcurve]][i] = dinamic$live_tocke_y[[dinamic$selectedcurve]][i] + dy
                        }
                    }
                }
            }
        }
    })
    ###
    
    ### curve control
    observeEvent(input$selectCurve, {
        dinamic$selectedpoint = 0
        dinamic$selectedcurve = 0
        if(input$mode == 'New curve'){
        updateSelectInput(session, 'mode', 'Mode:',
                          selected = 'Adding to curve')
        }
    })
    
    observeEvent(input$removeCurve, {
        if(dinamic$selectedcurve > 0){
            dinamic$live_tocke_x[[dinamic$selectedcurve]] = NULL
            dinamic$live_tocke_y[[dinamic$selectedcurve]] = NULL
            dinamic$selectedpoint = 0
            dinamic$selectedcurve = 0
            updateSelectInput(session, 'mode', 'Mode:',
                              selected = 'New curve')
        }
    })
    
    observeEvent(input$addCurve, {
        if(dinamic$selectedcurve > 0){
            mat = matrix(nrow = 2, ncol = length(dinamic$live_tocke_x[[dinamic$selectedcurve]]))
            mat[1,] = dinamic$live_tocke_x[[dinamic$selectedcurve]]
            mat[2,] = dinamic$live_tocke_y[[dinamic$selectedcurve]]
            
            dinamic$tocke[[as.character(dinamic$activename)]] = mat
            
            dinamic$activename = dinamic$activename + 1
            
            dinamic$live_tocke_x[[dinamic$selectedcurve]] = NULL
            dinamic$live_tocke_y[[dinamic$selectedcurve]] = NULL
            dinamic$selectedpoint = 0
            dinamic$selectedcurve = 0
            updateSelectInput(session, 'mode', 'Mode:',
                              selected = 'New curve')
        }
    })
    ###
    
    ### hide/show control points
    observeEvent(input$cp_vis_change, {
        if(dinamic$cp_visible){
            dinamic$cp_visible = FALSE
        }else{
            dinamic$cp_visible = TRUE
        }
    })
    
    output$cp_visibility <- renderUI({
        lbl = 'Show control points'
        if(dinamic$cp_visible){
            lbl = 'Hide control points'
        }
        actionButton('cp_vis_change', lbl)
    })
    ###
    
    output$canvas <- renderPlot({
        plot(c(), c(), 
             xlim = c(0,1),
             ylim = c(0,1),
             xlab = 'x',
             ylab = 'y')
        t_series = seq(0,1,0.01)
        n_kr = length(dinamic$live_tocke_x)
        if(n_kr > 0){
            for(i in 1:n_kr){
                n = length(dinamic$live_tocke_x[[i]])
                colr = 'blue'
                thiscurve = (i == dinamic$selectedcurve)
                if(thiscurve){
                    colr = 'green'
                }
                lines(dinamic$live_tocke_x[[i]],
                      dinamic$live_tocke_y[[i]],
                      col = colr)
                points(dinamic$live_tocke_x[[i]],
                       dinamic$live_tocke_y[[i]],
                       col = colr)
                if(thiscurve){
                    if(dinamic$selectedpoint > 0){
                        points(dinamic$live_tocke_x[[i]][dinamic$selectedpoint],
                               dinamic$live_tocke_y[[i]][dinamic$selectedpoint],
                               col = 'red', cex = 1.4)
                    }
                }
                if(n > 1){
                    lb = matrix(nrow = 2, ncol = n)
                    lb[1,] = dinamic$live_tocke_x[[i]]
                    lb[2,] = dinamic$live_tocke_y[[i]]
                    live_krivulja = decasteljau(lb, t_series)
                    lines(live_krivulja[1,],
                          live_krivulja[2,],
                          col = colr)
                }
            }
        }
        
        ##fiksne krivulje
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
