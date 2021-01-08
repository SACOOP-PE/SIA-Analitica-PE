
saveResults <- function(agente, errorBucket){
  ## agente ----
  agente %>%
    writexl::write_xlsx(paste0(paste(getwd(), "test/", sep = "/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProceso(agente),
                                     agente %>% pull(PeriodoInicial),
                                     agente %>% pull(PeriodoFinal),
                                     sep = "_"),
                               "_agent.xlsx"))
  ## errorbucket ----
  errorBucket %>%
    mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = "; "))) %>%
    writexl::write_xlsx(paste0(paste(getwd(), "test/", sep = "/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProceso(agente),
                                     agente %>% pull(PeriodoInicial),
                                     agente %>% pull(PeriodoFinal),
                                     sep = "_"),
                               "_errorbucket.xlsx"))
}