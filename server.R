library(shiny)
library(dplyr)
library(ggplot2)

mx_mun <- sf::read_sf('data/shp/municipios/muni_2018gw.shp')
inegi_afro <- read.csv('data/inegi/inegi_afro.csv')

function(input, output, session) {
  
  output$df_municipio <- renderTable({
    town_data()
  })
  
  town_data <- reactive({
    if (length(input$town) != 0) {
      geo_reactive() |>
        sf::st_drop_geometry() |>
        subset(NOM_MUN == input$town,
               select = c(NOM_MUN, POB_AFRO, POB_AFRO_PER)) |>
        rename('Municipio' = NOM_MUN,
               'Población afrodescendiente' = POB_AFRO,
               'Porcentaje' = POB_AFRO_PER)
    }
  })
  
  
  output$selection_town.UI <- renderUI({
    selectInput("town",
                "Obtener datos de un Municipio",
                choices = town.list(),
                selected = '')
  })
  
  town.list <- reactive ({
    geo_reactive() |>
      subset(select = NOM_MUN) |>
      sf::st_drop_geometry() |>
      unlist() |>
      unname()
  })  
  
  output$map_status <- reactive(
    if (input$get_set_submitbutton>0) T
  )
  outputOptions(output, "map_status", suspendWhenHidden = FALSE)
  
  #get afro population
  data_reactive <- eventReactive(input$get_set_submitbutton,{
    get_state_data(municipal_data = mx_mun,
                     state = input$state,
                     inegi_data = inegi_afro)
  })
  
  output$state_summary <- renderUI({
    state_text_reactive()
  })
  
  state_text_reactive <- reactive(
    make_summary_text(data_afro = data_reactive(),
                      state = input$state)
    )
  
  state_sex_per_reactive <- reactive(
    make_pie_plot(data_afro = data_reactive())
    )
  
  geo_reactive <- reactive(
    merge_shp_data(data = data_reactive(),
                   municipal_data = mx_mun,
                   state = input$state)
    )
  
  map_reactive <- reactive(
    make_map(metric = input$metric,
             geo_data = geo_reactive())
  )

    output$state_map <- renderPlot({

      map_reactive()

    }, bg="transparent")
    
    output$state_pie <- renderPlot({
      
      state_sex_per_reactive()
      
    }, bg="transparent")
    

}
