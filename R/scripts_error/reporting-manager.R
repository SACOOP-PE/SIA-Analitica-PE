
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
    write_csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(agente %>% pull(Coopac),
                           getIdProceso(agente),
                           agente %>% pull(PeriodoInicial),
                           agente %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_errorbucket.csv"))
}