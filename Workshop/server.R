#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$dynamic_inputs <- renderUI({
    if (input$layer == "1") {
      tagList(
        checkboxInput(inputId = "afficheabline",
                      label = "Display this layer ?"),
        textInput(inputId = "slopeabline",
                  label = "Slope"),
        textInput(inputId = "interceptabline",
                  label = "Intercept"),
        textInput(inputId = "colorabline",
                  label = "Color"),
        textInput(inputId = "linewidthabline",
                  label = "linewidth"),
        radioButtons(inputId = "linetypeabline",
                           label = "linetype",
                           choices = c("Solid" = "solid",
                                       "Dashed" = "dashed",
                                       "Dotted" = "dotted",
                                       "Dotdash" = "dotdash",
                                       "Longdash" = "longdash",
                                       "Twodash" = "twodash")),
      )
    } else if (input$layer == "2") {
      tagList(
        checkboxInput(inputId = "affichebar",
                      label = "Display this layer ?"),
        checkboxInput(inputId = "showlegendbar",
                      label = "Show legend ?"),
        radioButtons(inputId = "positionbar",
                     label = "position",
                     choices = c("Jitter" = "jitter",
                                 "Stack" = "stack",
                                 "Dodge" = "dodge",
                                 "Identity" = "identity"))
      )
    } else {
      tagList(
        dateInput("date1", "Choisissez une date pour Option 3:"),
        radioButtons("radio1", "Choisissez une option:", choices = c("A", "B", "C"))
      )
    }
  })
  
  # Affichage des valeurs des inputs dans le panneau principal
  output$graph <- renderPlot(data %>% ggplot(aes_string(x = input$aesx, y = input$aesy, fill = input$fill_variable))+
                               theme_minimal()+
                               geom_point(alpha = 0)+
                               geom_abline(intercept = as.numeric(input$interceptabline),
                                           slope = as.numeric(input$slopeabline),
                                           alpha = input$afficheabline,
                                           color = input$colorabline,
                                           linetype = input$linetypeabline,
                                           linewidth = as.numeric(input$linewidthabline))+
                               geom_bar(alpha = input$affichebar,
                                        color = NA,
                                        show.legend = input$showlegendbar,
                                        position = input$positionbar,
                                        stat = "identity")
                               
                             )

}
