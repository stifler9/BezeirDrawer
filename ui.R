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
                       height = '650px'),
            wellPanel(
                column(4,
                    selectInput('mode', 'Mode:',
                                choices = c('Adding', 'Moving'),
                                selected = 'Adding'),
                    actionButton('addCurve', 'Add Curve to Plot'),
                    actionButton('removeCurve', 'Remove Curve')
                ),
                column(4,
                    textInput('filename', 'File name:', value = 'MyDrawing.R'),
                    actionButton('shrani', 'Save drawing'),
                    actionButton('odpri', 'Open drawing')
                )
            ),
            width = '100%'
        )
    )
))
