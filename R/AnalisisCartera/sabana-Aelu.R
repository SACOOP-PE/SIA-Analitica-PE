createAgent2       <- function(periodoInicial, periodoFinal) {
  agente <- tibble(Coopac       = "01138",
                   NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                   Carpeta      = "test/datatest",
                   PeriodoInicial = periodoInicial,
                   PeriodoFinal   = periodoFinal,
                   Alcance        = "BD01")
  
  return(agente)
}
getArchivosCartera <- function(agente){
  periodos       <- c(201701:201712, 201801:201812, 201901:201912, 202001:202012)
  periodo_inicio <- agente %>% pull(PeriodoInicial)
  periodo_final  <- agente %>% pull(PeriodoFinal)
  
  archivos <- apply(expand.grid(agente %>% pull(Coopac), 
                                agente %>% pull(Alcance),
                                paste0(periodos[(periodos >= periodo_inicio) & (periodos <= periodo_final)], ".txt")
                                ),
                    1, paste, collapse = "_")
  return(archivos)
}
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

# Generar sábana de Créditos 201701- 202009
agentAelu     <- createAgent2("201701", "202009")
sabanaCartera <- getSabanaCartera(agentAelu)
