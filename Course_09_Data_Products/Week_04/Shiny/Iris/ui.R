library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Iris Classification Tree"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("Splits",
                        "Number of splits:",
                        min = 1,
                        max = 4,
                        value = 3,
                        step = 1,
                        ticks = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Tree", plotOutput("rpartPlot")),
                        tabPanel("ROC Curve", plotOutput("roc"))
                        
            )
        )
    )
))
