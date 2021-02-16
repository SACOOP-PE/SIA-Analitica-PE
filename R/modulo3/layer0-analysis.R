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
  
  agente <- createAgent(idCoopac,
                        periodoInicial = "201901",
                        periodoFinal   = "202009")
    
  sabanaBD01 <- getSabanaCartera(agente, getArchivosExigiblesFromAgent(agente), "BD01") %>% 
    select(Periodo, CCR, FVEG, NCPR, TEA)
  
  creditos <- sabanaBD01 %>% pull(CCR) %>% unique()
  
  sabanaBD01 <- sabanaBD01 %>% filter(CCR %in% creditos[2]) %>% 
    mutate(Periodo_post = lead(Periodo),
           FVEG_post = lead(FVEG),
           NCPR_post = lead(NCPR),
           TEA_post  = lead(TEA)) %>% rowwise() %>% 
    mutate(diff_NCPR = if_else(NCPR == NCPR_post, 
                               0, as.numeric(NCPR)- as.numeric(NCPR_post)),
           diff_TEA = if_else(TEA == TEA_post, 
                               0, as.numeric(TEA)- as.numeric(TEA_post)),
           diff_FVEG = if_else(FVEG == FVEG_post,
                               0, 
                               difftime(dmy(FVEG_post), dmy(FVEG), units = "days") %>% as.numeric())
           )
  
  
   
  return(sabanaBD01[1:nrow(sabanaBD01)-1,])
}