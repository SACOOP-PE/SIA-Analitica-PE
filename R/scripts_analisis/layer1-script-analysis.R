####' Script de análisis layer1
####' 1. Obtener sabana de cartera 
####' 2. Obtener sabana de cartera cancelada 
####' 


getSabanaCartera   <- function(agente){
  carpeta      <- getCarpetaFromAgent(agente)
  filesCartera <- getArchivosCartera(agente)
  
  cartera <-  evaluarFile(getRuta(carpeta, filesCartera[1])) %>%
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, filesCartera[1])))
  
  for (i in 1:length(filesCartera)-1) {
    cartera <- cartera %>% bind_rows(evaluarFile(getRuta(carpeta, filesCartera[i+1])) %>% 
                                       mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta,filesCartera[i+1]))))
  }
  
  cartera <- cartera[c(51, 1:50)]
  
  cartera %>% 
    write_delim(path = paste0(paste0(getwd(), "/test/"),
                              paste(paste0("(",agentAelu %>% pull(PeriodoInicial),"-", agentAelu %>% pull(PeriodoFinal),")"),sep = "_"),
                              "_sabanaCarteraAelu.txt"),
                delim = "\t", na = "", col_names = T, append = T)
  
  return(cartera)
}
getSabanaCarteraCancelada <- function(agente) {}