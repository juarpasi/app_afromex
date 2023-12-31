# Read and select ---------------------------------------------------------
dat <- read.csv('data/inegi/INE_SECCION_2020.csv')

inegi_afro <-
  dat |>
  subset(select = c(ENTIDAD, MUNICIPIO, POBTOT,
                    POB_AFRO, POB_AFRO_F, POB_AFRO_M))

write.csv(inegi_afro,
          'data/inegi/inegi_afro.csv',
          row.names = F,quote = F)
