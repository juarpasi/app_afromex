make_summary_text <- function(data_afro = data_reactive(),
                              state = input$state) {
  
  afro_pop <-
    data_afro |>
    summarise(POB_AFRO = sum(POB_AFRO),
              POBTOT = sum(POBTOT)) |>
    mutate(POB_AFRO_PER = 100*POB_AFRO/POBTOT)
  
  state_summary <-
    tags$span(
      HTML('En '),
      tags$span(state,
                class = 'state_text'),
      HTML(' existen'),
      tags$span(afro_pop$POB_AFRO,
                class = 'population_text'),
      HTML(' afrodescendientes.\nLo cual representa el '),
      tags$span(paste0(round(afro_pop$POB_AFRO_PER, 2),
                       '%'),
                class = 'population_text'),
      HTML(' de la población del estado.'),
      class = 'state_summary_class'
    )
  
  return(state_summary)
}