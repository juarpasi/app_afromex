get_state_data <- function(municipal_data = mx_mun,
                           state = input$state,
                           inegi_data = inegi_afro) {
  state_id <-
    municipal_data |>
    subset(NOM_ENT == state,
           select = CVE_ENT) |>
    sf::st_drop_geometry() |>
    head(1) |>
    unlist() |>
    as.numeric()
  
  #get population data
  state_afro_population <-
    inegi_data |> 
    subset(ENTIDAD == state_id)
}