library(shiny)
library(plotly)

# read municipal data
mx_mun <- sf::read_sf('data/shp/municipios/muni_2018gw.shp')

# make options

opts <-
  mx_mun$NOM_ENT |>
  unique()

# Read the html file
htmlTemplate(
  'www/index.html',
  selection1 = selectInput("state",
                          "Estado",
                          choices = opts,
                          selected = 'Ciudad de México'),
  selection2 = selectInput('metric',
                           'Representación',
                           choices = c('Población relativa','Población total')),
  
  button = actionButton("get_set_submitbutton", "Obtener mapa", class = "btn btn-primary"),
  mapplot = plotOutput("state_map",
                       height = '700px'),
  
  #pieplot = plotOutput("state_pie")
  pieplot = plotOutput('state_pie'),
  
  statesum = uiOutput(
    'state_summary',
    class = 'state_summary_container'
    ),
  
  municipal_data = conditionalPanel(
    condition = "output.map_status == true",
    
    uiOutput('selection_town.UI')
    ),
  
  mucipio_tbl = tableOutput('df_municipio')
  )