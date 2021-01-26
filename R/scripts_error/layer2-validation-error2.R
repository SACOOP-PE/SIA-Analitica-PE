#' Funciones principales
#' layer2(agente, eb)

layer2 <- function(agente, eb){
  eb <- validarOperacionesDuplicadas(agente, eb)
  return(eb)
}

#' Funciones secundarias: nivel I
#' validarOperacionesDuplicadas

validarOperacionesDuplicadas <- function(agente, eb){
  carpeta   <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosNoObservadosByCols(agente, eb, c("CCR", "CCR_C", "CODGR"))
  
  DupsSaldo <- tibble(NombreArchivo = exigibles) %>% rowwise() %>% 
    mutate(ruta       = getRuta(carpeta, NombreArchivo),
           BD         = getBDFromRuta(ruta),
           Periodo    = getAnoMesFromRuta(toString(ruta)),
           Duplicados = getoperacionesDuplicadas(ruta)) %>%
    filter(Duplicados != "" & BD != "BD03B" & BD != "BD04") %>%
    rowwise() %>%
    mutate(Saldo = getSaldoTotal(ruta, Duplicados))
  
  dups_BD01  <- DupsSaldo %>% filter(BD == "BD01")
  dups_BD02A <- DupsSaldo %>% filter(BD == "BD02A")
  dups_BD02B <- DupsSaldo %>% filter(BD == "BD02B")
  dups_BD03A <- DupsSaldo %>% filter(BD == "BD03A")
  
  if (nrow(dups_BD01) > 0) {
    chunk_401 <- dups_BD01 %>% rowwise() %>%
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Cod = 401,
             txt1 = Duplicados,
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]]),
             num2 = Saldo
             ) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1, num2)

    eb <- addError(eb, chunk_401)
  }
  if (nrow(dups_BD02A) > 0) {
    chunk_402 <- dups_BD02A %>% rowwise() %>%
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Cod = 402,
             txt1 = Duplicados,
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]]),
             num2 = Saldo
      ) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1, num2)
    
    eb <- addError(eb, chunk_402)
  }
  if (nrow(dups_BD02B) > 0) {
    chunk_403 <- dups_BD02B %>% rowwise() %>%
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Cod = 403,
             txt1 = Duplicados,
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]]),
             num2 = Saldo
      ) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1, num2)
    
    eb <- addError(eb, chunk_403)
  }
  if (nrow(dups_BD03A) > 0) {
    chunk_404 <- dups_BD03A %>% rowwise() %>%
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Cod = 404,
             txt1 = Duplicados,
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]]),
             num2 = Saldo
      ) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1, num2)
    
    eb <- addError(eb, chunk_404)
  }
  
  n <- eb %>% filter(Cod %in% c(401:404)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("La validación operaciones duplicadas concluyó sin observaciones tipo2. (~ly2) "), "I", "B")
  }
  else{
    addEventLog(agente, paste0("La validación operaciones duplicadas concluyó con ",n," observación(es) tipo2. (~ly2) "), "I", "B")
  }
  
  return(eb)
}

#validarOperacionesDuplicadas
getoperacionesDuplicadas <- function(ruta){
  BD <- evaluarFile(ruta)
  
  if (getBDFromRuta(ruta) == "BD01" | getBDFromRuta(ruta) == "BD03A") {
    operaciones <- BD %>% select(getCodigoBD(getBDFromRuta(ruta))[1]) 
    duplicados  <- operaciones[duplicated(operaciones), ] %>% unique() %>% pull(getCodigoBD(getBDFromRuta(ruta))[1]) %>% toString()
    
    return(duplicados)
  }
  if (getBDFromRuta(ruta) == "BD02A") {
    duplicados <- BD %>%
      group_by(CCR, NCUO) %>%
      filter(n() >1) %>%
      pull(CCR) %>% 
      unique() %>% toString()
    
    return(duplicados)
  }
  if (getBDFromRuta(ruta) == "BD02B") {
    duplicados <- BD %>%
      group_by(CCR_C, NCUO_C) %>%
      filter(n() >1) %>%
      pull(CCR_C) %>% 
      unique() %>% toString()
    
    return(duplicados)
  }
  else{return("")}
}
getSaldoTotal            <- function(ruta, opers){
  
    if (getBDFromRuta(ruta) == "BD01" | getBDFromRuta(ruta) == "BD02A" | getBDFromRuta(ruta) == "BD02B") {
      saldo <- evaluarFile(str_replace(ruta, getBDFromRuta(ruta), "BD01")) %>% 
        filter(CCR %in% unlist(str_split(opers, pattern = ", " ))) %>%
        pull(SKCR) %>% as.numeric() %>% sum()
    }
    else {
      saldo <- evaluarFile(ruta) %>% 
        filter(CODGR %in% unlist(str_split(opers, pattern = ", " ))) %>%
        pull(VCONS) %>%
        unique() %>% as.numeric() %>% sum()
    }
    
    return(saldo)
}
