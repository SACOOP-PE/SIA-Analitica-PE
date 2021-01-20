createAgent2  <- function(idCoopac = "01138",
                    periodoInicial = "201701", 
                    periodoFinal   = "202009", 
                    usuarioSIA    = default.usuario,
                    coopacCarpeta = "test/datatest/aelu_18012021", 
                    bds = "BD01"){
  
  agente <- tibble(Coopac       = idCoopac,
                   NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                   Carpeta      = coopacCarpeta,
                   IdProceso    = getNextIdProceso(getLogObject("logging/log.txt")),
                   Usuario      = usuarioSIA,
                   InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
                   PeriodoInicial = periodoInicial,
                   PeriodoFinal   = periodoFinal,
                   Alcance        = bds)
  
  return(agente)
}

getArchivosCartera <- function(agente){
  cod_coopac <- agente %>% pull(Coopac)
  id_bds     <- agente %>% pull(Alcance)
  periodos   <- c(201701:201712, 201801:201812, 201901:201912, 202001:202012)
  periodo_inicio <- agente %>% pull(PeriodoInicial)
  periodo_final  <- agente %>% pull(PeriodoFinal)
  
  apply(expand.grid(cod_coopac, id_bds,
                    paste0(periodos[(periodos >= periodo_inicio) &
                                      (periodos <= periodo_final)], ".txt")),
        1, paste, collapse = "_") %>% return()
}

getSabanaCartera <- function(agente){
  carpeta     <- getCarpetaFromAgent(agente)
  filesCartera <- getArchivosCartera(agente)
  
  cartera <-  evaluarFile(getRuta(carpeta, filesCartera[1])) %>%
    rowwise() %>% 
    mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta, filesCartera[1])))
  
  for (i in 1:length(filesCartera)-1) {
    cartera <- cartera %>% 
      bind_rows(evaluarFile(getRuta(carpeta, filesCartera[i+1])) %>%
                  rowwise() %>% 
                  mutate(Periodo = getAnoMesFromRuta(getRuta(carpeta,filesCartera[i+1])))
                )
  }
  
  cartera <- cartera[c(51, 1:50)]
  return(cartera)
}

#Generar sábana de Créditos 201701- 202009
agentAelu     <- createAgent2()
sabanaCartera <- getSabanaCartera(agentAelu)