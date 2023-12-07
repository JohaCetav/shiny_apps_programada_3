#Tarea Programada # 3 Shiny
#Johanna Salazar Ramírez
#AVD

library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(gapminder)
library(shinythemes)
library(writexl)
library(utils)
library(plotly)  


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Spotify 2000 - 2023"),
  
  dashboardSidebar(
    selectInput("anyo", "Año", choices = NULL, selected = NULL),
    selectInput("genero", "Género:", choices = NULL, selected = NULL),
    fluidRow(
      column(width = 12, offset = 2,
             downloadButton("downloadDATOS", "Descargar Datos")
      )
    )
  ),
  
  dashboardBody(
    box(
      title = "Pestañas y Gráfico",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      navbarPage(
        title = "Pestañas -->",
        tabPanel("Tabla",
                 fluidRow(
                   p("Seleccione el", strong("año"), "que desea reflejar y el", strong("género")),
                   p("En la tabla se reflejan los datos del año y género seleccionados")
                 ),
                 fluidRow(
                   column(9, tableOutput("resumen")),
                   column(2)
                 )
        ),
        tabPanel("Gráfico",
                 p("Elija las variables que desea observar"),
                 fluidRow(
                   column(6, 
                          selectInput(inputId = "variable_x",
                                      label = "Elija una de las siguientes variables para estudiar:",
                                      choices = c("bpm", "energy", "danceability", "dB","liveness", "acousticness", "speechiness")),
                          selectInput(inputId = "variable_y",
                                      label = "Elija la variable por la que desea comparar:",
                                      choices = c("popularity","duration" )),
                          plotlyOutput("dispersion")
                   )
                 ),
                 theme = shinythemes::shinytheme("simplex")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  spotify_data <- read.csv2("Datos/spotify_2000_2023.csv")
  
  View(spotify_data)
  
  observe({
    updateSelectInput(session, "anyo", choices = seq(2000, 2023), selected = 2023)
    updateSelectInput(session, "genero", choices = unique(spotify_data$top.genre))
  })
  
  output$resumen <- renderTable({
    datos_resumen <- spotify_data |>
      filter(year == input$anyo, top.genre == input$genero)
    datos_resumen
  })
  
  output$dispersion <- renderPlotly({
    datos_filtrados <- spotify_data |>
      filter(year == input$anyo, top.genre == input$genero)
    
    plot_ly(data = datos_filtrados, x = ~get(input$variable_x), y = ~get(input$variable_y), color = ~popularity) |>
      add_markers() |>
      layout(title = "Relación entre características musicales y popularidad o duración",
             xaxis = list(title = input$variable_x),
             yaxis = list(title = input$variable_y))
  })
  
  output$downloadDATOS <- downloadHandler(
    filename = function() {
      paste0("Resultados_Spotify", ".csv")
    },
    
    content = function(file) {
      tabla_genero <- spotify_data |>
        filter(year == input$anyo, top.genre == input$genero)
      write.csv(tabla_genero, file)
    }
  )
}

shinyApp(ui, server)




