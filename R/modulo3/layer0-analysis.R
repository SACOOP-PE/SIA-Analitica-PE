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

####' 1. Obtener sabana de cartera
####' 2. Obtener reprogramados
getSabana  <- function(agente, archivos, bd) {
  
  carpeta         <- getCarpetaFromAgent(agente)
  archivosCreditos <- archivos[str_detect(archivos, bd)]
  
  sabana <- evaluarFile(getRuta(carpeta, archivosCreditos[1])) %>%
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, archivosCreditos[1])))
  
  if (getPeriodosFromAgent(agente) == 1) {
    
    if (bd == "BD01") {
      cartera <- sabana[c(51, 1:50)]
      return(cartera)
    }
    if (bd == "BD02B") {
      cronoCanc <- sabana[c(18, 1:17)]
      return(cronoCanc)
    }
    if (bd == "BD04") {
      carteraCanc <- sabana[c(36, 1:35)]
      return(carteraCanc)
    }
    
  }
  
  for (i in 2:length(archivosCreditos)-1) {
    sabana <- sabana %>% bind_rows(evaluarFile(getRuta(carpeta, archivosCreditos[i+1])) %>% 
                                       mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, archivosCreditos[i+1]))))
  }
  
    if (bd == "BD01") {
      cartera <- sabana[c(51, 1:50)]
      return(cartera)
    }
    if (bd == "BD02B") {
      cronoCanc <- sabana[c(18, 1:17)]
      return(cronoCanc)
    }
    if (bd == "BD04") {
      carteraCanc <- sabana[c(36, 1:35)]
      return(carteraCanc)
    }

}

analizarReprogramados <- function(idCoopac){
  
  agente2 <- createAgent(idCoopac,
                         periodoInicial = "202001",
                         periodoFinal   = "202012")
  
  sabanaBD01 <- getSabana(agente2, getArchivosExigiblesFromAgent(agente2), "BD01")
  
  reprogramados <-  sabanaBD01 %>%
    filter(KVI != "0" | KRF != "0") %>% 
    rowwise() %>% 
    mutate(VENCIDO_AL_202012      = if_else(as.numeric(KVI) > 0, "X", ""),
           REFINANCIADO_AL_202012 = if_else(as.numeric(KRF) > 0, "X", "")) %>% 
    select(Periodo, CCR, KVI, KRF, FVEG, VENCIDO_AL_202012, REFINANCIADO_AL_202012) %>% 
    group_by(CCR) %>% filter(n()>1) %>% 
    arrange(CCR) %>% 
    mutate(PeriodoRepro = lead(Periodo),
           FVEG_repro   = lead(FVEG)) %>% 
    rowwise() %>% 
    mutate(diffDias = as.numeric(difftime(dmy(FVEG_repro), dmy(FVEG), units = "days")),
           REPRO01  = if_else((dmy(FVEG_repro) > dmy(FVEG))== TRUE &  diffDias >28,
                              "X", "")) %>% 
    select(PeriodoRepro, CCR, FVEG_repro, FVEG, diffDias, REPRO01, KVI, KRF, VENCIDO_AL_202012, REFINANCIADO_AL_202012) %>%
    filter(REPRO01 == "X")
  
  totalesReprograMes <- tibble(Periodos = reprogramados %>% pull(PeriodoRepro) %>% unique()) %>% rowwise() %>%
    mutate(Totales = nrow(reprogramados %>% filter(PeriodoRepro %in% Periodos)))

  lista_data <- list("sabanaBD01" = sabanaBD01, "Reprogramados" = reprogramados, "TotalesporMes" = totalesReprograMes)

  write.xlsx(lista_data,
             file = paste0(paste0(getwd(), "/test/output/ReprogramadosCartera.xlsx")))

  return(reprogramados)
}
