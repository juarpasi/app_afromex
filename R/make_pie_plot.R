make_pie_plot <- function(data_afro = data_reactive()) {
  
  data_afro |>
    summarise(POB_AFRO_F = sum(POB_AFRO_F),
              POB_AFRO_M = sum(POB_AFRO_M)) |>
    tidyr::pivot_longer(cols = everything(),
                        names_to = "POB",
                        values_to = "n") |>
    mutate(sex = c('Femenino','Masculino'),
           sex_per = round(100*n/sum(n),2)) |>
    
    ggplot(aes(x="", y=n, fill = sex)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c('#d44944', '#44d457'),
                      name = "Sexo" 
    ) + 
    geom_text(aes(label = paste0(sex_per, '%')),
              position = position_stack(vjust = 0.5),
              color = 'white',
              size = 6) +
    theme_void() +
    theme(
      legend.background = element_rect(fill = NA,colour = NA),
      legend.title = element_text(colour="white", size=16, face="bold"),
      legend.text = element_text(colour="gray80", size=16, face="bold")
    )
}