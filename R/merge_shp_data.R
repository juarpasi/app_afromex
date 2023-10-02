merge_shp_data <- function(data = data_reactive(),
                           municipal_data = mx_mun,
                           state = input$state) {
  
  #compute population data
  state_afro_population <-
    data |>
    group_by(ENTIDAD, MUNICIPIO) |>
    summarise(POB_AFRO = sum(POB_AFRO),
              POBTOT = sum(POBTOT)) |>
    mutate(POB_AFRO_PER = 100*POB_AFRO/POBTOT)
  
  #join with map data
  afro_geo <-
    municipal_data |>
    subset(NOM_ENT == state) |> 
    select(CVE_MUN,NOM_MUN, geometry) |>
    mutate(CVE_MUN =
             gsub('0+','', CVE_MUN) |>
             as.numeric()
    ) |>
    left_join(state_afro_population,
              by = c('CVE_MUN'='MUNICIPIO'))
  
  return(afro_geo)
}