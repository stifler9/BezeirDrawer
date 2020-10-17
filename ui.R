library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Bezeir Drawer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        #empty sidebar
        sidebarPanel(width = 0
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("canvas",
                       click = 'canvasclick',
                       hover = hoverOpts(id = 'canvashover',
                                         delay = 200,
                                         delayType = "throttle",
                                         clip = FALSE,
                                         nullOutside = TRUE),
                       height = '600px'),
            column(4,
                fluidRow(
                    selectInput('mode', 'Mode:',
                                choices = c('Adding to curve',
                                            'New curve',
                                            'Moving live point',
                                            'Moving live curve'),
                                size = 4,
                                selected = 'New curve',
                                selectize = FALSE)
                ),
                fluidRow(
                    wellPanel(
                        fluidRow(
                            column(4,
                                   actionButton('addCurve', 'Add Curve to Plot',
                                                style = 'color: #000000; background-color: #3e4de3; border-color: #000000')
                            ),
                            column(4,
                                   actionButton('selectCurve', 'Select Curve',
                                                style="color: #1a1a1a; background-color: #64ca45; border-color: #000000")
                            )
                        ),
                        fluidRow(
                            column(4,
                                   actionButton('removeCurve', 'Remove curve',
                                                style = 'border-color: #d20000')
                            ),
                            column(4,
                                   actionButton('removePoint', 'Remove point',
                                                style = 'border-color: #d20000')
                            )
                        )
                    )
                    ### todo remove selected point
                    #column(4,
                    #       actionButton('removePoint', 'Remove selected point')
                    #)
                ),
                offset = 1
            ),
            column(3,
                    textInput('filename', 'File name:', value = 'MyDrawing.R'),
                    wellPanel(
                        actionButton('shrani', 'Save drawing'),
                        actionButton('odpri', 'Open drawing'),
                        actionButton('clear', 'Clear All')
                    )
            ),
            column(4,
                   wellPanel(
                        uiOutput('cp_visibility'),
                        numericInput('new_t', 'To which t:',
                                     value = 1.0,
                                     step = 0.01),
                        uiOutput('new_t_action')
                   )
            ),
            width = '100%'
        )
    )
))
