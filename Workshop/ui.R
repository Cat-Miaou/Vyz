#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Best shiny app ever"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "aesx",
                        label = "Abcsissa",
                        choices = colnames(data)),
            selectInput(inputId = "aesy",
                        label = "Ordinate",
                        choices = colnames(data)),
            selectInput("fill_variable", 
                        label = "Choisissez une variable pour 'fill':",
                        choices = c(colnames(data), "NULL"),
                        selected = "NULL"),
            selectInput(inputId = "layer",
                        label = "Choose which layer you want to change",
                        choices = c("point" = 7,
                                    "bar" = 2,
                                    "boxplot" = 3,
                                    "vline" = 5,
                                    "abline" = 1,
                                    "Title" = 8,
                                    "Abcsissa" = 9,
                                    "Ordinate" = 10)
                        ),
            uiOutput("dynamic_inputs")
            ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("graph")
          )
    )
)
