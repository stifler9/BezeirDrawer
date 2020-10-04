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
                       height = '650px'),
            wellPanel(
                column(4,
                    selectInput('mode', 'Mode:',
                                choices = c('Adding',
                                            'Moving live point',
                                            'Moving live curve'),
                                size = 3,
                                selected = 'Adding',
                                selectize = FALSE)
                ),
                column(4,
                    textInput('filename', 'File name:', value = 'MyDrawing.R'),
                    actionButton('shrani', 'Save drawing'),
                    actionButton('odpri', 'Open drawing'),
                    actionButton('clear', 'Clear All')
                ),
                column(4,
                       uiOutput('cp_visibility'),
                       wellPanel(
                           actionButton('addCurve', 'Add Curve to Plot'),
                           actionButton('removeCurve', 'Remove Curve')
                       )
                )
            ),
            width = '100%'
        )
    )
))
