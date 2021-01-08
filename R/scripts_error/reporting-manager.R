
saveResults      <- function(agente, errorBucket){
  ## header ----
  header %>% 
    write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_header.csv"))
  ## errorbucket ----
  errorBucket %>%
    mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = "; "))) %>%
    write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_errorbucket.csv"))
}