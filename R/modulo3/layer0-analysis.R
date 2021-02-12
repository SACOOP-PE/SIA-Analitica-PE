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
  
  # getSabanaCartera(agente2, archivos)
  getSabanaCarteraCancelada(agente2, archivos)
}

####' 1. Obtener sabana de cartera 
####' 2. Obtener sabana de cartera cancelada  
getSabanaCartera          <- function(agente2, archivos) {
  carpeta         <- getCarpetaFromAgent(agente2)
  archivoscartera <- archivos[str_detect(archivos, "BD01")]
  
  cartera <-  evaluarFile(getRuta(carpeta, archivoscartera[1])) %>%
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, archivoscartera[1])))
  
  for (i in 2:length(archivoscartera)-1) {
    cartera <- cartera %>% bind_rows(evaluarFile(getRuta(carpeta, archivoscartera[i+1])) %>% 
                                       mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, archivoscartera[i+1]))))
  }
  
  cartera <- cartera[c(51, 1:50)]
  
  cartera %>% 
    write.xlsx(file = paste0(getwd(), "/test/output/","sabanaBD01.xlsx"))
  
  return(cartera)
}
getSabanaCarteraCancelada <- function(agente2, archivos) {
  
  carpeta             <- getCarpetaFromAgent(agente2)
  archivoscarteraCanc <- archivos[str_detect(archivos, "BD04")]
  
  carteraCanc <- evaluarFile(getRuta(carpeta, archivoscarteraCanc[1])) %>%
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, archivoscarteraCanc[1])))
  
  for (i in 2:length(archivoscarteraCanc)-1) {
    carteraCanc <- carteraCanc %>% bind_rows(evaluarFile(getRuta(carpeta, archivoscarteraCanc[i+1])) %>% 
                                               mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, archivoscarteraCanc[i+1]))))
  }
  
  carteraCanc <- carteraCanc[c(36, 1:35)]
  
  carteraCanc %>% 
    write.xlsx(file = paste0(getwd(), "/test/output/","sabanaBD04.xlsx"))
  
  return(carteraCanc)
}