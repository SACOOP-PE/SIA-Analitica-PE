#' Funciones principales
#' layer2(agente, eb)

layer2 <- function(agente, eb){
  eb <- validarOperacionesDuplicadas(agente, eb)
  return(eb)
}

#' Funciones secundarias: nivel I
#' validarOperacionesDuplicadas
#' 
#' getOperacionesDuplicadas()
#' getDuplicadosCredCancelados()
#' getSaldoTotal()

validarOperacionesDuplicadas <- function(agente, eb){
  carpeta   <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosNoObservadosByCols(agente, eb, c("CCR", "CCR_C", "CODGR"))
  
  Dups <- tibble(NombreArchivo = exigibles[str_detect(exigibles, paste(c("BD01","BD02A", "BD02B", "BD03A"), collapse = '|'))]) %>% 
    rowwise() %>% 
    mutate(ruta       = getRuta(carpeta, NombreArchivo),
           BD         = getBDFromRuta(ruta),
           Periodo    = getAnoMesFromRuta(toString(ruta)),
           Duplicados = getOperacionesDuplicadas(ruta),
           Saldo      = getSaldoTotal(ruta, Duplicados)) %>%
    filter(Duplicados != "")

  dups_BD01  <- Dups %>% filter(BD == "BD01")
  dups_BD02A <- Dups %>% filter(BD == "BD02A")
  dups_BD02B <- Dups %>% filter(BD == "BD02B")
  dups_BD03A <- Dups %>% filter(BD == "BD03A")
  
  dups_BD04  <- getDuplicadosCredCancelados(agente, exigibles) 
  
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
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])
             ) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
    
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
  if (nrow(dups_BD04) > 0) {
    chunk_405 <- tibble(CodCoopac = getCoopacFromAgent(agente),
                        IdProceso = getIdProcesoFromAgent(agente),
                        Cod     = 405,
                        Periodo = dups_BD04 %>% pull(Periodos) %>% first(),
                        BD      = "BD04",
                        txt1 = toString(unique(dups_BD04 %>% pull(CCR_C))),
                        txt2 = toString(unique(dups_BD04 %>% pull(Periodos))),
                        num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])
                        ) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
    
    eb <- addError(eb, chunk_405)
  }
  
  n <- eb %>% filter(Cod %in% c(401:405)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("      Resultado: La validación operaciones duplicadas concluyó sin observaciones."))
  }
  else{
    addEventLog(agente, paste0("      Resultado: La validación operaciones duplicadas concluyó con ",n," observación(es)."))
  }
  
  return(eb)
}

getOperacionesDuplicadas    <- function(ruta){
   BD <- quitarVaciosBD(ruta)
   
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
   else{""}

}
getDuplicadosCredCancelados <- function(agente, exigibles){
  
  carpeta    <- getCarpetaFromAgent(agente)
  cancelados <- exigibles[str_detect(exigibles, "BD04")]
  
  CredCandelados <- evaluarFile(getRuta(carpeta, cancelados[1])) %>% 
    mutate(Periodos = getAnoMesFromRuta(getRuta(carpeta, cancelados[1]))) 
  
  for (i in 2:length(cancelados)-1) {

    CredCandelados <- CredCandelados %>%
      bind_rows(evaluarFile(getRuta(carpeta, cancelados[i+1])) %>% 
                  mutate(Periodos = getAnoMesFromRuta(getRuta(carpeta, cancelados[i+1]))))
    
  }
  
  dupsCancelados <- CredCandelados %>% 
    select(Periodos, CCR_C) %>% 
    group_by(CCR_C) %>%
    filter(n() >1)
  
  return(dupsCancelados)
}
getSaldoTotal               <- function(ruta, opers){
  if (opers != "" & getBDFromRuta(ruta) != "BD02B") {
    
    if (getBDFromRuta(ruta) == "BD01" | getBDFromRuta(ruta) == "BD02A") {
      saldo <- quitarVaciosBD(str_replace(ruta, getBDFromRuta(ruta), "BD01")) %>% 
        filter(CCR %in% unlist(str_split(opers, pattern = ", " ))) %>%
        pull(SKCR) %>% as.numeric() %>% sum()
    }
    else {
      saldo <- quitarVaciosBD(ruta) %>% 
        filter(CODGR %in% unlist(str_split(opers, pattern = ", " ))) %>%
        pull(VCONS) %>%
        unique() %>% as.numeric() %>% sum()
    }
    
    return(saldo)
  }
  return(0)
}