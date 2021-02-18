####' Script de análisis layer1
####' 
layer0_Analisis <- function(idCoopac){
  
  agente2 <- createAgent(idCoopac,
                         periodoInicial = "201701",
                         periodoFinal   = "202009")
  
  cod_coopac <- agente2 %>% pull(Coopac) %>% first()
  id_bds     <- (agente2 %>% pull(Alcance))[[1]]
  periodos   <- c(201701:201712, 201801:201812, 201901:201912, 202001:202012, 202101:202112)
  periodo_inicio <- agente2 %>% pull(PeriodoInicial)
  periodo_final  <- agente2 %>% pull(PeriodoFinal)
  
  archivos <- apply(expand.grid(cod_coopac, id_bds,
                                paste0(periodos[(periodos >= periodo_inicio) & (periodos <= periodo_final)], ".txt")),
                    1, paste, collapse = "_")
  
  sabanaBD01 <- getSabanaCartera(agente2, archivos, "BD01")
  
    sabanaBD01 %>% 
      write.xlsx(file = paste0(getwd(), "/test/output/","sabanaBD01.xlsx"))
  
  sabanadaBD04 <- getSabanaCarteraCancelada(agente2, archivos, "BD04")
  
    sabanaBD04 %>% 
      write.xlsx(file = paste0(getwd(), "/test/output/","sabanaBD04.xlsx"))
}

####' 1. Obtener reprogramados
analizarReprogramados <- function(idCoopac, fechaInicial, fechaFinal){
  
  agente2 <- createAgent(idCoopac,
                         periodoInicial = fechaInicial,
                         periodoFinal   = fechaFinal)
  
  sabanaBD01 <- getSabana(agente2, getArchivosExigiblesFromAgent(agente2), "BD01")
  
  reprogramados <- sabanaBD01 %>% 
    rowwise() %>% 
    select(Periodo, CCR, KVI,KVE, KRF, KJU, FVEG, SKCR) %>% 
    group_by(CCR) %>% filter(n()>1) %>% 
    arrange(CCR) %>% 
    mutate(PeriodoRepro = lead(Periodo),
           FVEG_repro   = lead(FVEG)) %>% 
    rowwise() %>% 
    mutate(diffDias = as.numeric(difftime(dmy(FVEG_repro), dmy(FVEG), units = "days")),
           REPRO01  = if_else((dmy(FVEG_repro) > dmy(FVEG))== TRUE &  diffDias >28,
                              "X", "")) %>% 
    select(PeriodoRepro, CCR, FVEG_repro, FVEG, diffDias, REPRO01, KVI, KVE, KRF, KJU, SKCR) %>% rowwise() %>% 
    mutate(SKCR = as.numeric(str_remove_all(SKCR, ","))) %>% 
    filter(REPRO01 == "X")

  totalesReprograMes <- reprogramados %>% group_by(PeriodoRepro) %>%
    summarise(Total = n(), SaldoColocaciones = sum(SKCR, na.rm = T)) %>% arrange(PeriodoRepro) 

  lista_data <- list("sabanaBD01" = sabanaBD01, "Reprogramados" = reprogramados, "TotalesporMes" = totalesReprograMes)

  write.xlsx(lista_data,
             file = paste0(getwd(), "/test/output/ReprogramadosCartera_", idCoopac, ".xlsx"))

  return(reprogramados)
}