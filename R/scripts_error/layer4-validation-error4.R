#' Funciones principales
#' layer4(agente, eb)

layer4 <- function(agente, eb){
  eb <- validarCampos(agente, eb)
  return(eb)
}

#' Funciones secundarias: nivel I
#' validarCampos
validarCampos <- function(agente, eb){
  carpeta   <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosNoObservadosByCols(agente, eb, c("CCR","CCR_C","CODGR"))
  
  # i. Errores tipo1 ----
  for (x in 1:length(exigibles)){
    eb     <- procesarErroresT1(agente, getRuta(carpeta, exigibles[x]), eb)
  }
  
  n <- eb %>% filter(Cod %in% c(401:457)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("La validación de los campos concluyó sin observaciones tipo1. (~ly4) "), "I", "B")
  }
  else{
    
    addEventLog(agente, paste0("La validación de los campos concluyó con ",n," observaciones tipo1. (~ly4) "), "I", "B")
  }
  
  
  # ii. Errores tipo2 ----
  cod <- 461
  for (y in 461:466){
    eb <- procesarErroresT2(agente, eb, exigibles, cod)
    cod <- cod +1
  }
  
  n <- eb %>% filter(Cod %in% c(461:466)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("La validación de los campos concluyó sin observaciones tipo2. (~ly4) "), "I", "B")
  }
  else{
    
    addEventLog(agente, paste0("La validación de los campos concluyó con ",n," observaciones tipo2. (~ly4) "), "I", "B")
  }
  
  
  # iii. Errores tipo3 ----
  #error 471:478
  exigibles <- exigibles[str_detect(exigibles, paste(c("BD01","BD02A","BD02B","BD04"), collapse = '|'))]
  
  for (z in 1:length(exigibles)) {
    eb     <- procesarErroresT3(agente, getRuta(carpeta, exigibles[z]), eb)
  }
  
  #error 479
  exigibles <- intersect(exigibles[str_detect(exigibles, "BD01")], getArchivosNoObservadosByCols(agent, eb, "FOT"))
  error479  <- tibble(Archivo = exigibles) %>% rowwise() %>%
    mutate(ruta      = getRuta(getCarpetaFromAgent(agente), Archivo),
           verificar = procesarErrorFechaDesembolso(ruta) %>%
             unique() %>% toString(),
           Cod       = 479) %>%
    filter(verificar != "")
  
  if (nrow(error479) >0) {
    chunk479 <- error479 %>% rowwise() %>%
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Periodo = getAnoMesFromRuta(toString(ruta)),
             BD      = getBDFromRuta(toString(ruta)),
             txt1 = verificar,
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
    
    eb <- addError(eb, chunk479)
  }
  
  
  n <- eb %>% filter(Cod %in% c(471:479)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("La validación de los campos concluyó sin observaciones tipo3. (~ly4) "), "I", "B")
  }
  else{
    
    addEventLog(agente, paste0("La validación de los campos concluyó con ",n," observación(es) tipo3. (~ly4) "), "I", "B")
  }
  
  
  ######
  
  n <- eb %>% filter(Cod %in% c(401:479)) %>% nrow()
  
  if (n == 0) {
    addEventLog(agente, paste0("La validación de los campos concluyó sin observaciones. (~ly4) "), "I", "B")
  }
  else{
    
    addEventLog(agente, paste0("La validación de los campos concluyó con un total de ", n, " observación(es). (~ly4) "), "I", "B")
  }
  
  return(eb)
}

#' Tipo 1: validaciones a campos con dígitos específicos
#' Tipo 2: validaciones con condiciones entre campos
#' Tipo 3: validaciones a campos fecha

# Tipo1 ----
getDigitosBD01  <- function(campo){
  digitos <- switch (campo,
                     TID = {c(1,2,3,4,5,6,7)},
                     TCR = {c(6,7,8,9,10,11,12,13,20)},
                     CAL = {c(0,1,2,3,4)},
                     ESAM = {c(1,2,3,4,5)},
                     SEC = {c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,99)},
                     MDCR ={c(1,2,3,4,5,9)},
                     OSD = {c(1,2,3,4,5,6,7,8,9,10,99)})
  return(digitos)
}
getDigitosBD02A <- function(campo){
  digitos <- switch (campo,
                     MON = {c(1,2,3)},
                     FOCAN = {c(1,2,3,4,5)})
  return(digitos)
}
getDigitosBD02B <- function(campo){
  digitos <- switch (campo,
                     MON_C = {c(1,2,3)},
                     FOCAN_C = {c(1,2,3,4)})
  return(digitos)
}
getDigitosBD03A <- function(campo){
  digitos <- switch (campo,
                     CGR = {c(1,2,3,4,5)},
                     COBGR = {c(1,2)},
                     MONGR = {c(1,2,3)})
  return(digitos)
}
getDigitosBD03B <- function(campo){
  digitos <- switch (campo,
                     CGR = {c(1,2,3,4,5)})
  return(digitos)
}
getDigitosBD04  <- function(campo){
  digitos <- switch (campo,
                     TID_C = {c(1,2,3,4,5,6,7)},
                     TCR_C = {c(6,7,8,9,10,11,12,13,20)},
                     MON_C = {c(1,2,3)},
                     CAL_C = {c(0,1,2,3,4)},
                     ESAM_C = {c(1,2,3,4,5)},
                     FOCAN_C = {c(1,2,3,4,5)},
                     MDCR_C = {c(1,2,3,4,5,9)})
  return(digitos)
}

getDigitosBD   <- function(ruta,campo){
  digitos <- switch (getBDFromRuta(ruta),
                     BD01  = {getDigitosBD01(campo)},
                     BD02A = {getDigitosBD02A(campo)},
                     BD02B = {getDigitosBD02B(campo)},
                     BD03A = {getDigitosBD03A(campo)},
                     BD03B = {getDigitosBD03B(campo)},
                     BD04  = {getDigitosBD04(campo)})
  return(digitos)
}
getColsErrorT1 <- function(ruta){
  cols <- switch(getBDFromRuta(ruta),
                 BD01  = {c("TID","TCR","CAL","ESAM","SEC","MDCR","OSD")},
                 BD02A = {c("MON","FOCAN")},
                 BD02B = {c("MON_C","FOCAN_C")},
                 BD03A = {c("CGR","COBGR")},
                 BD03B = {c("CGR")},
                 BD04  = {c("TID_C","TCR_C","MON_C","CAL_C","ESAM_C","FOCAN_C","MDCR_C")}) 
  return(cols)
}
getCodErrorT1  <- function(ruta, campo){
  codError <- switch (getBDFromRuta(ruta),
                      BD01  = {c(401,402,403,404,405,406,407)},
                      BD02A = {c(411,412)},
                      BD02B = {c(421,422)},
                      BD03A = {c(431,432)},
                      BD03B = {c(441)},
                      BD04  = {c(451,452,453,454,455,456,457)})
  
  cod <- tibble(col       = getColsErrorT1(ruta),
                cod_error = codError) %>% 
    filter(col == campo) %>% 
    pull(cod_error)
  return(cod)
}

procesarErroresT1 <- function(agente, ruta, eb){
  BD <- evaluarFile(ruta)
  
  if (length(getColsNoObservadas(ruta, eb, "T1")) >0) {
    erroresTipo1 <- tibble(Columna = getColsNoObservadas(ruta, eb, "T1")) %>%
      rowwise() %>%
      mutate(verificar = BD %>% 
               filter((as.numeric(cgrep(BD, Columna)[[1]]) %in% getDigitosBD(ruta, Columna)) == FALSE) %>%
               pull(getCodigoBD(getBDFromRuta(ruta))) %>%
               unique() %>% toString(),
             Cod       = getCodErrorT1(ruta, Columna)) %>% 
      filter(verificar != "")
    
    if (nrow(erroresTipo1) >0) {
      chunkT1 <- erroresTipo1 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Periodo = getAnoMesFromRuta(toString(ruta)),
               BD      = getBDFromRuta(toString(ruta)),
               txt1 = verificar,
               txt2 = Columna,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
      
      eb <- addError(eb, chunkT1)
    }
    return(eb)
  }
  return(eb)
}

# Tipo2----
procesarErroresT2 <- function(agente, eb, exigibles, codigoError){
  #filter files:
  archivos <- switch (toString(codigoError),
                      "461"= exigibles[str_detect(exigibles, "BD01")],
                      "462"= getArchivosNoObservadosByCols(agente, eb, c("ESAM","NCPR", "PCUO")) %>% intersect(exigibles[str_detect(exigibles, "BD01")]),
                      "463"= getArchivosNoObservadosByCols(agente, eb, c("MORG", "SKCR")) %>% intersect(exigibles[str_detect(exigibles, "BD01")]),
                      "464"= getArchivosNoObservadosByCols(agente, eb, c("KVE", "DAK", "KJU")) %>% intersect(exigibles[str_detect(exigibles, "BD01")]),
                      "465"= getArchivosNoObservadosByCols(agente, eb, c("TID", "NID", "TID_C", "NID_C")) %>% intersect(exigibles[str_detect(exigibles, paste(c("BD01","BD04"), collapse = '|'))]),
                      "466"= getArchivosNoObservadosByCols(agente, eb, c("NCR", "NRCL")) %>% intersect(exigibles[str_detect(exigibles, "BD03A")]))
  
  # errores t2:
  if (codigoError == 461 & length(archivos) >0) {
    for (i in 1:length(archivos)) {
      errorSaldos <- procesarErrorSaldosNegativos(agente, 
                                                  getRuta(getCarpetaFromAgent(agente), archivos[i]),
                                                  eb)
      
      if (nrow(errorSaldos) >0) {
        chunkSaldos <- errorSaldos %>% rowwise() %>%
          mutate(ruta = getRuta(getCarpetaFromAgent(agente), archivos[i]),
                 CodCoopac = getCoopacFromAgent(agente),
                 IdProceso = getIdProcesoFromAgent(agente),
                 Periodo = getAnoMesFromRuta(toString(ruta)),
                 BD      = getBDFromRuta(toString(ruta)),
                 txt1 = verificarSaldos,
                 txt2 = Columna,
                 num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
          select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
        
        eb <- addError(eb, chunkSaldos)
      }
    }
    return(eb)
  }
  
  if (codigoError >= 462 & codigoError <= 466 & length(archivos) >0){
    
    erroresTipo2 <- tibble(Archivo = archivos) %>% rowwise() %>%
      mutate(ruta    = getRuta(getCarpetaFromAgent(agente), Archivo),
             verificar = switch (toString(codigoError),
                                 "462"= procesarErrorModalidadCouta(ruta),
                                 "463"= procesarErrorMontoOtorgado(ruta),
                                 "464"= procesarErrorVencRetraso(ruta),
                                 "465"= procesarErrorDocumentoIdent(ruta),
                                 "466"= procesarErrorNumCredCobertura(ruta)) %>%
               unique() %>% toString(),
             Cod       = codigoError) %>% 
      filter(verificar != "")
    
    if (nrow(erroresTipo2) >0) {
      chunkT2 <- erroresTipo2 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Periodo = getAnoMesFromRuta(toString(ruta)),
               BD      = getBDFromRuta(toString(ruta)),
               txt1 = verificar,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
      
      eb <- addError(eb, chunkT2)
    }
    return(eb)
  }
}

#BD01
procesarErrorSaldosNegativos <- function(agente, ruta, eb){
  BD         <- evaluarFile(ruta)
  saldosCols <- getColsNoObservadas(ruta, eb, "saldos")
  
  if (length(saldosCols) >0) {
    errorSaldos <- tibble(Columna = saldosCols) %>%
      rowwise() %>%
      mutate(verificarSaldos = BD %>% filter(as.numeric(cgrep(BD, Columna)[[1]]) <0) %>%
               pull(getCodigoBD("BD01")) %>% toString(),
             Cod             = 461) %>%
      filter(verificarSaldos != "")
    
    return(errorSaldos)
  }
  return("")
}
procesarErrorModalidadCouta  <- function(ruta){
 error <- evaluarFile(ruta) %>%
    filter(((as.numeric(ESAM) < 5) & (as.numeric(NCPR) == 0 | as.numeric(PCUO)  == 0)) == TRUE) %>%
    pull(CCR)
 
 return(error)
}
procesarErrorMontoOtorgado   <- function(ruta){
  error <- evaluarFile(ruta) %>%
    filter(as.numeric(MORG) < as.numeric(SKCR)) %>%
    pull(CCR)
  
  return(error)
}
procesarErrorVencRetraso     <- function(ruta){
  error <- evaluarFile(ruta) %>%
    filter((as.numeric(KVE) > 0 & as.numeric(DAK) == 0)) %>% 
    pull (CCR)
  
  return(error)
}

#BD01 y BD04
detectarVacios              <- function(ruta,campo){
  BD    <- evaluarFile(ruta)
  
  vacios <- BD %>% 
    filter(is.na(cgrep(BD, campo))) %>% 
    pull(getCodigoBD(getBDFromRuta(ruta)))
  
  return(vacios)
}
validarDocumentoIdent       <- function(tipodocumento, ndocumento){
  if (is.na(ndocumento) | is.na(tipodocumento)) {
    return("FALSE")
  }
  else{
    nCaracteres <- switch (tipodocumento,
                           "1" = "8",
                           "2" = "9",
                           "3" = "13",
                           "4" = "13",
                           "5" = "12",
                           "6" = "11")
    return((nCaracteres == nchar(ndocumento)) %>% toString())
  }
}
procesarErrorDocumentoIdent <- function(ruta){
  BD <- evaluarFile(ruta)
  
  if (getBDFromRuta(ruta) =="BD01"){
    verificar_documento <- BD %>% 
      rowwise() %>%
      mutate(detectarError = validarDocumentoIdent(TID, NID)) %>%
      filter(detectarError == "FALSE") %>%
      pull(getCodigoBD(getBDFromRuta(ruta)))
    
    return(verificar_documento)
  }
  
  if (getBDFromRuta(ruta) =="BD04"){
    verificar_documento <- BD %>% 
      rowwise() %>%
      mutate(detectarError = validarDocumentoIdent(TID_C, NID_C)) %>%
      filter(detectarError == "FALSE") %>%
      pull(getCodigoBD(getBDFromRuta(ruta)))
    
    return(verificar_documento)
  }
}

#BD03A
procesarErrorNumCredCobertura <- function(ruta){
  error <- evaluarFile(ruta) %>% 
    filter(as.numeric(NCR) > 0, as.numeric(NRCL) == 0) %>%
    pull(getCodigoBD("BD03A")) %>%
    unique()
  
  return(error)
}


# Tipo3----
#BD01, BD02A, BD02B, BD04 
getColsErrorT3 <- function(ruta){
  cols <- switch (getBDFromRuta(ruta),
                  BD01  = {c("FOT", "FVEG", "FVEP")},
                  BD02A = {c("FVEP")},
                  BD02B = {c("FVEP_C")},
                  BD04  = {c("FOT_C", "FCAN_C")}) 
  return(cols)
}
getCodErrorT3  <- function(ruta, campo){
  codError <- switch (getBDFromRuta(ruta),
                      BD01  = {c(471,472,473)},
                      BD02A = {c(474)},
                      BD02B = {c(475)},
                      BD04  = {c(476, 477)})
  
  cod <- tibble(col       = getColsErrorT3(ruta),
                cod_error = codError) %>% 
    filter(col == campo) %>% 
    pull(cod_error)
  
  return(cod)
}

procesarErroresT3 <- function(agente, ruta, eb){
  BD <- evaluarFile(ruta)
  
  if (length(getColsNoObservadas(ruta, eb, "T3")) >0) {
    erroresTipo3 <- tibble(Columna = getColsNoObservadas(ruta, eb, "T3")) %>%
      rowwise() %>%
      mutate(verificar = BD %>% 
               filter(dmy(cgrep(BD, Columna)[[1]]) %>% is.na() == TRUE) %>% 
               pull(getCodigoBD(getBDFromRuta(ruta))) %>%
               unique() %>% toString(),
             Cod       = getCodErrorT3(ruta, Columna)) %>% 
      filter(verificar != "")
    
    if (nrow(erroresTipo3) >0) {
      chunkT3 <- erroresTipo3 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Periodo = getAnoMesFromRuta(toString(ruta)),
               BD      = getBDFromRuta(toString(ruta)),
               txt1 = verificar,
               txt2 = Columna,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
      
      eb <- addError(eb, chunkT3)
    }
    return(eb)
  }
  return(eb)
}

#BD01
getFechaCorte                <- function(ruta){
  fecha_corte <- seq(as.Date(paste(getAnoFromRuta(ruta),getMesFromRuta(ruta),"01", sep = "-")),
                     length=1, by="months") %>%
    ceiling_date("month") - days(1)
  return(fecha_corte)
}
procesarErrorFechaDesembolso <- function(ruta){
  BD <- evaluarFile(ruta) 
  
  error <- BD %>%
    filter((dmy(BD %>% pull(FOT)) > getFechaCorte(ruta)) == TRUE) %>% 
    pull(getCodigoBD("BD01"))
  
  return(error)
}

####
