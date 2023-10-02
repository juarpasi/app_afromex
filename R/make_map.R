make_map <- function(metric = input$metric,
                     geo_data = geo_reactive()) {
  
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
  
  is_total <- metric == 'Población total'
  
  map <-
    geo_data |>
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
}