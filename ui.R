library(shiny)
library(plotly)

# read municipal data
mx_mun <- sf::read_sf('data/shp/municipios/muni_2018gw.shp')

# make options

opts <-
  mx_mun$NOM_ENT |>
  unique()

# Define UI for application that draws a histogram
fluidPage(
  
  tags$head(
    tags$link(rel='stylesheet', href='styles.css')
  ),
  
  titlePanel("Mexicanos que se identifican como afrodescendientes"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("state",
                  "Estado",
                  choices = opts,
                  selected = 'Ciudad de México'),
      
      selectInput('metric',
                  'Representación',
                  choices = c('Población total','Población relativa')),
      
      actionButton("get_set_submitbutton", "Crear mapa", class = "btn btn-primary")
      
      ),
    
    mainPanel(
      plotOutput("state_map",
                 height = '700px')
      )
    
    )
)
