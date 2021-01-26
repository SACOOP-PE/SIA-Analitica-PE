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
  
  ## i. Errores tipo1 ----
  
   # erorres 601:621
    for (i in 1:length(exigibles)){
      eb     <- procesarErroresT1(agente, getRuta(carpeta, exigibles[i]), eb)
    }
  
   # errores 622
    exigiblesError622 <- intersect(getArchivosNoObservadosByCols(agente, eb, c("TID", "NID", "TID_C", "NID_C")),
                                   exigibles[str_detect(exigibles, paste(c("BD01","BD04"), collapse = '|'))])
  
    error622  <- tibble(Archivo = exigiblesError622) %>% rowwise() %>%
      mutate(ruta      = getRuta(getCarpetaFromAgent(agente), Archivo),
             verificar = procesarErrorDocumentoIdent(ruta) %>% unique() %>% toString(),
             Cod       = 622) %>%
      filter(verificar != "")
  
    if (nrow(error622) >0) {
      chunk622 <- error622 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Periodo = getAnoMesFromRuta(toString(ruta)),
               BD      = getBDFromRuta(toString(ruta)),
               txt1 = verificar,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
      
      eb <- addError(eb, chunk622)
    }
  
   #addEvent Tipo1:
    n <- eb %>% filter(Cod %in% c(601:622)) %>% nrow()
    if (n == 0) {
      addEventLog(agente, paste0("La validación de los campos concluyó sin observaciones tipo1. (~ly4) "), "I", "B")
    }
    else{
      addEventLog(agente, paste0("La validación de los campos concluyó con ",n," observaciones tipo1. (~ly4) "), "I", "B")
    }

  ## ii. Errores tipo2 ----
  
   # error 701:707
    exigibles <- exigibles[str_detect(exigibles, paste(c("BD01","BD02A","BD02B","BD04"), collapse = '|'))]
  
    for (ii in 1:length(exigibles)) {
      eb     <- procesarErroresT2(agente, getRuta(carpeta, exigibles[ii]), eb)
    }
  
   # error 708
    exigibles708 <- intersect(exigibles[str_detect(exigibles, "BD01")], getArchivosNoObservadosByCols(agent, eb, "FOT"))
    error708     <- tibble(Archivo = exigibles708) %>% rowwise() %>%
      mutate(ruta      = getRuta(getCarpetaFromAgent(agente), Archivo),
             verificar = procesarErrorFechaDesembolso(ruta) %>% unique() %>% toString(),
             Cod       = 708) %>%
      filter(verificar != "")
  
    if (nrow(error708) >0) {
      chunk708 <- error708 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Periodo = getAnoMesFromRuta(toString(ruta)),
               BD      = getBDFromRuta(toString(ruta)),
               txt1 = verificar,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
      
      eb <- addError(eb, chunk708)
    }
    
   #addEvent Tipo2:
    n <- eb %>% filter(Cod %in% c(701:708)) %>% nrow()
    if (n == 0) {
      addEventLog(agente, paste0("La validación de los campos concluyó sin observaciones tipo2. (~ly4) "), "I", "B")
    }
    else{
      addEventLog(agente, paste0("La validación de los campos concluyó con ",n," observación(es) tipo2. (~ly4) "), "I", "B")
    }
  
  return(eb)
}

#' Tipo 1: validaciones a campos con dígitos específicos
#' Tipo 2: validaciones a campos fecha

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
                      BD01  = {c(601,602,603,604,605,606,607)},
                      BD02A = {c(608,609)},
                      BD02B = {c(610,611)},
                      BD03A = {c(612,613)},
                      BD03B = {c(614)},
                      BD04  = {c(615,616,617,618,619,620,621)})
  
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

# Tipo2----
#BD01, BD02A, BD02B, BD04 
getColsErrorT2 <- function(ruta){
  cols <- switch (getBDFromRuta(ruta),
                  BD01  = {c("FOT", "FVEG", "FVEP")},
                  BD02A = {c("FVEP")},
                  BD02B = {c("FVEP_C")},
                  BD04  = {c("FOT_C", "FCAN_C")}) 
  return(cols)
}
getCodErrorT2  <- function(ruta, campo){
  codError <- switch (getBDFromRuta(ruta),
                      BD01  = {c(701,702,703)},
                      BD02A = {c(704)},
                      BD02B = {c(705)},
                      BD04  = {c(706, 707)})
  
  cod <- tibble(col       = getColsErrorT2(ruta),
                cod_error = codError) %>% 
    filter(col == campo) %>% 
    pull(cod_error)
  
  return(cod)
}

procesarErroresT2 <- function(agente, ruta, eb){
  BD <- evaluarFile(ruta)
  
  if (length(getColsNoObservadas(ruta, eb, "T2")) >0) {
    erroresTipo2 <- tibble(Columna = getColsNoObservadas(ruta, eb, "T2")) %>%
      rowwise() %>%
      mutate(verificar = BD %>% 
               filter(dmy(cgrep(BD, Columna)[[1]]) %>% is.na() == TRUE) %>% 
               pull(getCodigoBD(getBDFromRuta(ruta))) %>%
               unique() %>% toString(),
             Cod       = getCodErrorT2(ruta, Columna)) %>% 
      filter(verificar != "")
    
    if (nrow(erroresTipo2) >0) {
      chunkT2 <- erroresTipo2 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Periodo = getAnoMesFromRuta(toString(ruta)),
               BD      = getBDFromRuta(toString(ruta)),
               txt1 = verificar,
               txt2 = Columna,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
      
      eb <- addError(eb, chunkT2)
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
