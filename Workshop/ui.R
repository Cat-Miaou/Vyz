#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(glue)

# Define UI for application that draws a histogram
fluidPage(
  theme = bs_theme(bg = "#3E47BA", 
                   fg = "black"),
  
  style = "height: 110vh;",  
  # Application title
  tags$h1("Best shiny app ever", style = "color: white; ; text-align: left; font-size: 36px; font-weight: bold;"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #FFCE32;",  # Fond jaune de la sidebar
      
      # Ajouter le CSS pour personnaliser les éléments selectInput
      tags$head(
        tags$style(HTML("
              /* Style pour les éléments selectInput (menu déroulant) */
              .selectize-input, .selectize-dropdown {
                background-color: #EFE3D4 !important;  /* Fond beige pour le selectInput et son menu */
                color: black;                          /* Couleur du texte */
                border: 1px solid #ccc;               /* Bordure grise */
              }

              /* Style pour les radioButtons, checkbox, etc. */
              .radio input, .checkbox input {
                background-color: white !important;  /* Fond blanc pour le bouton */
                border: 1px solid #ccc !important;  /* Bordure grise pour les boutons */
              }
              .radio, .checkbox {
                background-color: transparent !important;  /* Pas de fond pour le conteneur autour du bouton */
                padding: 5px 0 !important;                   /* Espace entre les boutons */
              }

              /* Style pour les autres types d'input */
              .form-control {
                background-color: #EFE3D4 !important;  /* Fond beige pour les autres inputs */
                color: black;                          /* Couleur du texte */
                border: 1px solid #ccc;               /* Bordure grise */
              }

              label {
                font-weight: bold;                    /* Mettre les labels des boutons en gras */
              }
              .shiny-text-output {
        padding: 0 !important;  /* Enlève tout padding interne */
        margin: 0 !important;   /* Enlève tout margin externe */
        border: none !important; /* Enlève la bordure */
        display: block;          /* S'assurer que le conteneur se comporte comme un bloc */
        width: 100%;             /* S'assurer qu'il prend toute la largeur de son conteneur */
        height: 100%;            /* S'assurer qu'il prend toute la hauteur disponible */
        box-sizing: border-box; /* Inclure la bordure et padding dans la taille de l'élément */
      }

            "))
      ),
      selectInput(inputId = "aesx",
                        label = "Abcsissa",
                        choices = colnames(data)),
            selectInput(inputId = "aesy",
                        label = "Ordinate",
                        choices = colnames(data)),
            selectInput("fill_variable", 
                        label = "Choose a variable to 'fill':",
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
          plotOutput("graph", height = "660px", width = "100%",
                     verbatimTextOutput("code")),
          div(style = "margin-top: 20px;"),
          div(
            verbatimTextOutput("code"),
            style = "max-height: 250px; 
            overflow-y: scroll;
            border: none;
            margin: 0; 
            padding: 0; 
            white-space: pre-wrap; 
            background-color: #EFE3D4;"
          )
          
          )
    )
)
