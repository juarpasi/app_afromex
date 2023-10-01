library(shiny)
library(dplyr)
library(ggplot2)

matplot_colors <-
  colorRampPalette(c("white",
                     "#9644d4", "#4e44d4",
                     #"#007FFF",
                     "#44c1d4",
                     "#44d457", "#cad444",
                     "#d49144","#d44944"))(50)

tot_colors <-
  colorRampPalette(c("#d9d9d9",
                     "#d44944"))(50)

mx_mun <- sf::read_sf('data/shp/municipios/muni_2018gw.shp')
inegi_afro <- read.csv('data/inegi/inegi_afro.csv')

function(input, output, session) {
  
  #get afro population
  geo_reactive <- eventReactive(input$get_set_submitbutton,{
    
    state_id <-
      mx_mun |>
      subset(NOM_ENT == input$state,
             select = CVE_ENT) |>
      sf::st_drop_geometry() |>
      head(1) |>
      unlist() |>
      as.numeric()
    
    #get population data
    state_afro_population <-
      inegi_afro |> 
      subset(ENTIDAD == state_id) |>
      group_by(ENTIDAD, MUNICIPIO) |>
      summarise(POB_AFRO = sum(POB_AFRO),
                POBTOT = sum(POBTOT)) |>
      mutate(POB_AFRO_PER = 100*POB_AFRO/POBTOT)
    
    #join with map data
    afro_geo <-
      mx_mun |>
      subset(NOM_ENT == input$state) |> 
      select(CVE_MUN,NOM_MUN, geometry) |>
      mutate(CVE_MUN =
               gsub('0+','', CVE_MUN) |>
               as.numeric(),
             NOM_MUN2 = sub(' .*','', NOM_MUN)
      ) |>
      left_join(state_afro_population,
                by = c('CVE_MUN'='MUNICIPIO'))
    
    return(afro_geo)
    
  })
  
  map_reactive <- reactive({
    
    is_total <- input$metric == 'Población total'
    
    map <-
      geo_reactive() |>
      #select(NOM_MUN, POB_AFRO_PER) |>
      ggplot() +
      geom_sf(aes(fill = if(is_total) POB_AFRO else POB_AFRO_PER),
      ) +
      scale_fill_gradientn(colours  = if(is_total) tot_colors else matplot_colors) +
      labs(fill = if(is_total) 'Población total' else 'Porcentaje')+
      theme(panel.background = element_rect(fill = NA,colour = NA),
            plot.background = element_rect(fill = NA,colour = NA),
            panel.border = element_rect(colour = '#F0F0F0',fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(colour = "#F0F0F0"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            legend.background = element_rect(fill = NA,colour = NA),
            legend.title = element_text(colour="white", size=16, face="bold"),
            legend.text = element_text(colour="gray80", size=16, face="bold")
      )
    
    return(map)
    
  })

    output$state_map <- renderPlot({

      map_reactive()

    }, bg="transparent")

}
