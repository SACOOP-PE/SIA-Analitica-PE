## Gestion de errores----
addErrorIndividual <- function(eb, agente, codcoopac, idproceso, cod, periodo, bd, 
                               arg_txt1, arg_txt2, arg_txt3,
                               arg_num1, arg_num2, arg_num3){
  t <- tibble(CodCoopac = codcoopac,
              IdProceso  = idproceso, 
              Cod         = cod, 
              Periodo     = periodo,
              BD          = bd,
              txt1     = arg_txt1,
              txt2    = arg_txt2,
              txt3    =  arg_txt3,
              num1    = arg_num1,
              num2    = arg_num2,
              num3    = arg_num3)
 

  return(bind_rows(eb, t))
} 

addErrorMasivo <- function(eb, parte1){
  bind_rows(eb, parte1) %>% return()
}



# Edwin reducir las dos funciones a esto: 

addError <- function(eb, 
                     obj = NULL) {
  
  eb <- bind_rows(eb, obj)
  return(eb)
  
}

# vamos a estandarizar las estructuras y resumir la mayor cantidad de codigo posible. 
# desde ahora, mala practica aplicar return() usando pipes.


getArchivosObservadosFromBucket <- function(eb) {
  eb %>% 
    mutate(filename = paste0(CodCoopac,"_",BD, "_",Periodo,".txt")) %>% 
    pull(filename) %>% unique() %>% return()
}
getArchivosNoObservados         <- function(agente, eb) {
  setdiff(getArchivosExigiblesFromAgent(agente),
          getArchivosObservadosFromBucket(eb)) %>%  return()
}
getArchivosNoObservadosByErrors <- function(agente, eb, cods) {
  v <- eb %>% 
    filter(Cod %in% cods) %>% 
    mutate(filename = paste0(CodCoopac,"_",BD, "_",Periodo,".txt")) %>% 
    pull(filename) %>% unique()
  
  setdiff(getArchivosExigiblesFromAgent(agente),
          v) %>%  return()
}
getArchivosNoObservadosByCols   <- function(agente, eb, cols) {
  v <- eb %>%
    filter(Cod %in% c(201, 202, 203)) %>% 
    rowwise() %>% 
    filter_at(.vars = vars(txt1),
              .vars_predicate = any_vars(str_detect(., paste0(paste(cols, collapse = "|"))))) %>% 
    mutate(filename = paste0(CodCoopac,"_",BD, "_",Periodo,".txt")) %>% 
    pull(filename) %>% unique()
  
  setdiff(getArchivosExigiblesFromAgent(agente),
          v) %>%  return()
}
getPeriodosNoObservados         <- function(agente, eb, colCruce){
  archivos  <- getArchivosNoObservadosByCols(agente, eb, colCruce)
  archCruce <- switch (colCruce,
                       CCR   = archivos[str_detect(archivos, paste(c("BD01","BD02A"), collapse = '|'))],
                       CODGR = archivos[str_detect(archivos, paste(c("BD03A","BD03B"), collapse = '|'))],
                       CIS   = archivos[str_detect(archivos, paste(c("BD03A","BD01"), collapse = '|'))])
  
  getPeriodos <- tibble(Periodos =  str_extract(archCruce, paste(as.character(global.alcance),collapse = '|'))) %>%
    group_by(Periodos) %>%
    filter(n() ==2) %>%
    pull(Periodos) %>% 
    unique() 
  
  return(getPeriodos)
}

#Dp. no me queda claro paQ es esta obs.
getColsNoObservadas             <- function(ruta, eb, tipoError){
  colsError <- eb %>%
    filter(BD == getBDFromRuta(ruta) & Periodo == getAnoMesFromRuta(ruta) & Cod %in%  c(201, 203)) %>% 
    pull(txt1) %>%
    str_split(",") %>% 
    unlist() %>% 
    str_replace_all(pattern=" ", repl="") %>% unique()
  
  
  cols <- switch (tipoError,
                  T1 = setdiff(getColsErrorT1(ruta), colsError),
                  T3 = setdiff(getColsErrorT3(ruta), colsError),
                  saldos = setdiff(c("SKCR", "PCI", "KVI", "KRF", "KVE", "KJU", "SIN", "SID", "SIS", "DGR", "NCPR", "NCPA", "TPINT", "NRPRG"),
                                   colsError))
  
  return(cols)
}


# Filter files, periods, cols from bucket----
getArchivosObservadosFromBucket <- function(eb) {
  eb %>% 
    mutate(filename = paste0(CodCoopac,"_",BD, "_",Periodo,".txt")) %>% 
    pull(filename) %>% unique() %>% return()
}
getArchivosNoObservados         <- function(agente, eb) {
  setdiff(getArchivosExigiblesFromAgent(agente),
          getArchivosObservadosFromBucket(eb)) %>%  return()
}
getArchivosNoObservadosByErrors <- function(agente, eb, cods) {
  v <- eb %>% 
    filter(Cod %in% cods) %>% 
    mutate(filename = paste0(CodCoopac,"_",BD, "_",Periodo,".txt")) %>% 
    pull(filename) %>% unique()
  
  setdiff(getArchivosExigiblesFromAgent(agente),
          v) %>%  return()
}
getArchivosNoObservadosByCols   <- function(agente, eb, cols) {
  v <- eb %>%
    filter(Cod %in% c(201, 202, 203)) %>% 
    rowwise() %>% 
    filter_at(.vars = vars(txt1),
              .vars_predicate = any_vars(str_detect(., paste0(paste(cols, collapse = "|"))))) %>% 
    mutate(filename = paste0(CodCoopac,"_",BD, "_",Periodo,".txt")) %>% 
    pull(filename) %>% unique()
  
  setdiff(getArchivosExigiblesFromAgent(agente),
          v) %>%  return()
}
getPeriodosNoObservados         <- function(agente, eb, colCruce){
  archivos  <- getArchivosNoObservadosByCols(agente, eb, colCruce)
  archCruce <- switch (colCruce,
                       CCR   = archivos[str_detect(archivos, paste(c("BD01","BD02A"), collapse = '|'))],
                       CODGR = archivos[str_detect(archivos, paste(c("BD03A","BD03B"), collapse = '|'))],
                       CIS   = archivos[str_detect(archivos, paste(c("BD03A","BD01"), collapse = '|'))])
  
  getPeriodos <- tibble(Periodos =  str_extract(archCruce, paste(as.character(global.alcance),collapse = '|'))) %>%
    group_by(Periodos) %>%
    filter(n() ==2) %>%
    pull(Periodos) %>% 
    unique() 
  
  return(getPeriodos)
}
getColsNoObservadas             <- function(ruta, eb, tipoError){
  colsError <- eb %>%
    filter(BD == getBDFromRuta(ruta) & Periodo == getAnoMesFromRuta(ruta) & Cod %in%  c(201, 203)) %>% 
    pull(txt1) %>%
    str_split(",") %>% 
    unlist() %>% 
    str_replace_all(pattern=" ", repl="") %>% unique()
  
  
  cols <- switch (tipoError,
                  T1 = setdiff(getColsErrorT1(ruta), colsError),
                  T3 = setdiff(getColsErrorT3(ruta), colsError),
                  saldos = setdiff(c("SKCR", "PCI", "KVI", "KRF", "KVE", "KJU", "SIN", "SID", "SIS", "DGR", "NCPR", "NCPA", "TPINT", "NRPRG"),
                                   colsError))
  
  return(cols)
}

