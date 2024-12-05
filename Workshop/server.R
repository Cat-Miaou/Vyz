#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(tidyverse)


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
                                       "Twodash" = "twodash"))
      )
    } else if (input$layer == "2") {
      tagList(
        checkboxInput(inputId = "affichebar",
                      label = "Display this layer ?"),
        checkboxInput(inputId = "showlegendbar",
                      label = "Show legend ?"),
        radioButtons(inputId = "positionbar",
                     label = "position",
                     choices = c("Identity" = "identity",
                                 "Jitter" = "jitter",
                                 "Stack" = "stack",
                                 "Dodge" = "dodge"))
      )
    } else if (input$layer == "3") {
      tagList(
        checkboxInput(inputId = "afficheboxplot",
                      label = "Display this layer ?"),
        checkboxInput(inputId = "showlegendboxplot",
                      label = "Show legend ?"),
        textInput(inputId = "colorboxplot",
                  label = "Color",
                  value = "black"),
        textInput(inputId = "linewidthboxplot",
                  label = "linewidth"),
        radioButtons(inputId = "linetypeboxplot",
                     label = "linetype",
                     choices = c("Solid" = "solid",
                                 "Dashed" = "dashed",
                                 "Dotted" = "dotted",
                                 "Dotdash" = "dotdash",
                                 "Longdash" = "longdash",
                                 "Twodash" = "twodash"))
        )
    } else if (input$layer == "4") {
      tagList(
        checkboxInput(inputId = "afficheboxplot",
                      label = "Display this layer ?"),
        checkboxInput(inputId = "showlegendboxplot",
                      label = "Show legend ?"),
        textInput(inputId = "colorboxplot",
                  label = "Color",
                  value = "black")
      )
    } else if (input$layer == "5") {
      tagList(
        checkboxInput(inputId = "affichevline",
                      label = "Display this layer ?"),
        textInput(inputId = "interceptvline",
                  label = "Intercept"),
        textInput(inputId = "colorvline",
                  label = "Color",
                  value = "black"),
        textInput(inputId = "linewidthvline",
                  label = "linewidth"),
        radioButtons(inputId = "linetypevline",
                     label = "linetype",
                     choices = c("Solid" = "solid",
                                 "Dashed" = "dashed",
                                 "Dotted" = "dotted",
                                 "Dotdash" = "dotdash",
                                 "Longdash" = "longdash",
                                 "Twodash" = "twodash"))
      )
    } else if (input$layer == "7") {
      tagList(
        checkboxInput(inputId = "affichepoint",
                      label = "Display this layer ?"),
        radioButtons(inputId = "positionpoint",
                     label = "position",
                     choices = c("Identity" = "identity",
                                 "Jitter" = "jitter")),
        textInput(inputId = "shapepoint",
                  label = "shape"),
        textInput(inputId = "sizepoint",
                  label = "size")
      )
    } else if (input$layer == "8") {
      tagList(
        
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
                               geom_abline(intercept = ifelse(!nzchar(input$interceptabline), 0, as.numeric(input$interceptabline)),
                                           slope = ifelse(!nzchar(input$slopeabline), 0 ,as.numeric(input$slopeabline)),
                                           alpha = input$afficheabline,
                                           color = ifelse(!nzchar(input$colorabline), "black", input$colorabline),
                                           linetype = input$linetypeabline,
                                           linewidth = ifelse(!nzchar(input$linewidthabline), 0.3 ,as.numeric(input$linewidthabline)))+
                               geom_bar(alpha = input$affichebar,
                                        show.legend = input$showlegendbar,
                                        position = input$positionbar,
                                        stat = "identity") +
                               geom_boxplot(alpha = input$afficheboxplot,
                                            color = ifelse(input$afficheboxplot == FALSE, NA, input$colorboxplot),
                                            linetype = input$linetypeboxplot,
                                            linewidth = ifelse(!nzchar(input$linewidthboxplot), 0.3, as.numeric(input$linewidthboxplot)),
                                            show.legend = input$showlegendboxplot) +
                               geom_vline(xintercept = ifelse(!nzchar(input$interceptvline), 0, as.numeric(input$interceptvline)),
                                          alpha = input$affichevline,
                                          color = ifelse(input$affichevline == FALSE, "black", input$colorvline),
                                          linetype = input$linetypevline,
                                          linewidth = ifelse(!nzchar(input$linewidthvline), 0.3, as.numeric(input$linewidtvline)))+
                               geom_point(alpha = input$affichepoint,
                                          shape = ifelse(!nzchar(input$shapepoint), 19, as.numeric(input$shapepoint)),
                                          size = ifelse(!nzchar(input$sizepoint), 1.5, as.numeric(input$sizepoint)))
                               
                             )

}
