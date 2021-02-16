## Gestion de errores----
addError <- function(eb, obj) {
  eb <- bind_rows(eb, obj)
  return(eb)
}

# Otras funciones ----
getArchivosNoObservadosByCols <- function(agente, eb, cols) {
  if (nrow(eb %>% filter(Cod %in% c(201,203))) >0) {
    v <- eb %>%
      filter(Cod %in% c(201, 203)) %>% 
      rowwise() %>% 
      mutate(filename     = paste0(CodCoopac,"_",BD, "_",Periodo,".txt"),
             verificarCol = ifelse(length(which(str_split(txt1, ", ")[[1]] == cols)) >=1, 
                                   "TRUE", 
                                   "FALSE")
             ) %>%
      filter(verificarCol == "TRUE") %>% 
      pull(filename) %>% unique()
    
    return(setdiff(getArchivosExigiblesFromAgent(agente), v))
  }
  else{
    return(getArchivosExigiblesFromAgent(agente))
  }
}
getPeriodosNoObservados       <- function(agente, eb, colCruce){
  archivos  <- getArchivosNoObservadosByCols(agente, eb, colCruce)
  archCruce <- switch (colCruce,
                       CCR   = archivos[str_detect(archivos, paste(c("BD01","BD02A"), collapse = '|'))],
                       CODGR = archivos[str_detect(archivos, paste(c("BD03A","BD03B"), collapse = '|'))],
                       CIS   = archivos[str_detect(archivos, paste(c("BD03A","BD01"), collapse = '|'))])
  
  getPeriodos <- tibble(Periodos = str_extract(archCruce, paste(as.character(global.alcance), collapse = '|'))) %>%
    group_by(Periodos) %>%
    filter(n() ==2) %>%
    pull(Periodos) %>% 
    unique() 
  
  return(getPeriodos)
}
getColsNoObservadas           <- function(ruta, eb, tipoError){
  if (nrow(eb %>% filter(Cod %in% c(201,203))) >0) {
    
    colsError <- eb %>%
      filter(BD == getBDFromRuta(ruta) & Periodo == getAnoMesFromRuta(ruta) & Cod %in% c(201, 203)) %>% 
      pull(txt1) %>%
      str_split(", ") %>% 
      unlist()
    
    filtrarCols <- switch (tipoError,
                           T1 = setdiff(getColsErrorT1(ruta), colsError),
                           T2 = setdiff(getColsErrorT2(ruta), colsError),
                           saldos = setdiff(c("SKCR", "PCI", "KVI", "KRF", "KVE", "KJU", "SIN", "SID", "SIS", "DGR", "NCPR", "NCPA", "TPINT", "NRPRG"),
                                            colsError))
    
    return(filtrarCols)
  }
  else{
    
    cols <- switch (tipoError,
                    T1 = getColsErrorT1(ruta),
                    T2 = getColsErrorT2(ruta),
                    saldos = c("SKCR", "PCI", "KVI", "KRF", "KVE", "KJU", "SIN", "SID", "SIS", "DGR", "NCPR", "NCPA", "TPINT", "NRPRG"))
    
    return(cols)
  }
}