####' Script de análisis layer1
####' 
layer0_Analisis <- function(agente){
  
  agente2 <- createAgent(idCoopac = agente %>% pull(idCoopac),
                         periodoInicial = "201701",
                         periodoFinal   = "202012")
  
  getSabanaCartera(agente2)
  getSabanaCarteraCancelada(agente2)
}

####' 1. Obtener sabana de cartera 
####' 2. Obtener sabana de cartera cancelada  
getSabanaCartera          <- function(agente2) {
  carpeta  <- getCarpetaFromAgent(agente2)
  archivos <- getArchivosCartera(agente2)
  
  archivoscartera <- archivos[str_detect(archivos, "BD01")]
  
  cartera <-  evaluarFile(getRuta(carpeta, filesCartera[1])) %>%
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, filesCartera[1])))
  
  for (i in 2:length(filesCartera)-1) {
    cartera <- cartera %>% bind_rows(evaluarFile(getRuta(carpeta, filesCartera[i+1])) %>% 
                                       mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta,filesCartera[i+1]))))
  }
  
  cartera <- cartera[c(51, 1:50)]
  
  cartera %>% 
    write_delim(path = paste0(paste0(getwd(), "/test/"),
                              paste(paste0("(",agente2 %>% pull(PeriodoInicial),"-", agente2 %>% pull(PeriodoFinal),")"),sep = "_"),
                              "_sabanaCartera.txt"),
                delim = "\t", na = "", col_names = T, append = T)
  
  return(cartera)
}
getSabanaCarteraCancelada <- function(agente2) {
  carpeta  <- getCarpetaFromAgent(agente2)
  archivos <- getArchivosCartera(agente2)
  
  archivoscarteraCanc <- archivos[str_detect(archivos, "BD04")]
  
  carteraCanc <- evaluarFile(getRuta(carpeta, filesCartera[1])) %>%
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, filesCartera[1])))
  
  for (i in 2:length(filesCartera)-1) {
    carteraCanc <- carteraCanc %>% bind_rows(evaluarFile(getRuta(carpeta, filesCartera[i+1])) %>% 
                                               mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta,filesCartera[i+1]))))
  }
  
  carteraCanc <- carteraCanc[c(51, 1:50)]
  
  carteraCanc %>% 
    write_delim(path = paste0(paste0(getwd(), "/test/"),
                              paste(paste0("(",agente2 %>% pull(PeriodoInicial),"-", agente2 %>% pull(PeriodoFinal),")"),sep = "_"),
                              "_sabanaCarteraCancelada.txt"),
                delim = "\t", na = "", col_names = T, append = T)
  
  return(cartera)

}