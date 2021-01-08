#' Funciones principales
#' layer2(agent, eb)

layer2 <- function(agente, eb){
eb <- validarCuadreContable(agente, eb)
eb <- validarOperacionesVacias(agente, eb)
eb <- validarOperacionesDuplicadas(agente, eb)
eb <- validarCruceInterno(agente, eb)
eb <- validarCampos(agente, eb)
return(eb)
}

#' Funciones secundarias: nivel I
#' validarCuadreContable
#' validarOperacionesDUVA
#' validarCruceInterno
#' validarCampos

#Issue https://github.com/SACOOP-PE/SIA-Analitica-PE/issues/12#issue-775181380
validarCuadreContable        <- function(agente, eb){
  carpeta   <- getCarpeta(agente)
  exigibles <- getArchivosSinErrores(agente, eb, c(201,203), c("KVI","KVE","KRF","KJU"))
  exigibles <- exigibles[str_detect(exigibles, "BD01")]

  tb1 <- tibble(NombreArchivo = exigibles[str_detect(exigibles, 
                                                     paste(unique(initCuadreContable() %>% pull(PERIODO)), collapse = '|'))]
                ) %>%
    rowwise() %>% 
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           Coopac  = as.numeric(getCoopac(Ruta)),
           Periodo = getAnoMes(Ruta),
           KVI_BC   = sum(getCapitalBC(Ruta)[[1]], na.rm=T),
           KVI_BDCC = sum(getCapitalBDCC(Ruta)[[1]], na.rm=T),
           Dif_KVI = sum(getCapitalBDCC(Ruta)[[1]], na.rm=T) - sum(getCapitalBC(Ruta)[[1]], na.rm=T),
           Dif_KVE = sum(getCapitalBDCC(Ruta)[[2]], na.rm=T) - sum(getCapitalBC(Ruta)[[2]], na.rm=T),
           Dif_KRF = sum(getCapitalBDCC(Ruta)[[3]], na.rm=T) - sum(getCapitalBC(Ruta)[[3]], na.rm=T),
           Dif_KJU = sum(getCapitalBDCC(Ruta)[[4]], na.rm=T) - sum(getCapitalBC(Ruta)[[4]], na.rm=T)) %>%
    pivot_longer(starts_with("Dif"),names_to = "Capital", values_to = "Saldo") %>% rowwise() %>%
    mutate(Resultado   = ifelse(abs(Saldo)>100, "ERROR", ""),
           Coopac      = getCoopac(Ruta),
           NombCoopac  = getNomCoopac(Ruta),
           Carpeta     = getCarpeta(agente),
           IdProceso   = eb %>% pull(IdProceso) %>% first(),
           Cod         = ifelse(Resultado == "ERROR",
                                getCodErrorCruceContable(str_split(Capital, "_")[[1]][2]),0),
           Descripcion =  getDescError(Cod)) %>%
    mutate(Detalle     = list(paste0(getAnoMes(Ruta),"-",getBD(Ruta),"-", str_split(Capital,"_")[[1]][2],": S/ ",round(Saldo, digits =2))))

  eb <- bind_rows(eb, tb1 %>%
                    filter(Resultado == "ERROR") %>%
                    select(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion, Detalle))
  n <- tb1 %>%
    filter(Resultado == "ERROR") %>%
    select(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion, Detalle) %>% 
    nrow()
  
  print(paste0("El cuadre contable de la información concluyó con ", n, " observaciones (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  return(eb)
}
validarOperacionesVacias     <- function(agente, eb){
  carpeta   <- getCarpeta(agente)
  exigibles <- getArchivosSinErrores(agente, eb,  c(201, 203), c("CCR", "CCR_C", "CODGR"))
  
  tb <- tibble(NombreArchivo = exigibles) %>% rowwise() %>% 
    mutate(Ruta        = getRuta(carpeta, NombreArchivo),
           CodVacios_n = operacionesVacias(Ruta))
  
  vacios <- tb %>% filter(CodVacios_n != 0) %>% select(NombreArchivo, CodVacios_n)
  
  if (nrow(vacios) > 0) {
    eb <- eb %>%
      addError(312, getDescError(312), vacios %>% apply(1, paste0 , collapse = "=") %>% toString())
  }
  
  n <- eb %>% filter(Cod %in% c(312)) %>% nrow()
  
  if (n == 0) {
    print(paste0("La validación de operaciones vacías concluyó con ", n, " observaciones. (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  }
  else{
    print(paste0("La validación de operaciones vacías concluyó con ", n, " observación. (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  }
  
  return(eb)
}
validarOperacionesDuplicadas <- function(agente, eb){
  carpeta   <- getCarpeta(agente)
  exigibles <- getArchivosSinErrores(agente, eb,  c(201, 203), c("CCR", "CCR_C", "CODGR"))
  
  tb <- tibble(NombreArchivo = exigibles) %>% rowwise() %>% 
    mutate(Ruta          = getRuta(carpeta, NombreArchivo),
           CodDuplicados = operacionesDuplicadas(Ruta))
  
  dups   <- (paste(tb %>% rowwise() %>% pull(CodDuplicados), collapse = ",") %>% strsplit(","))[[1]]
  
  if (length(dups[dups != "character(0)"]) > 0) {
    eb <- eb %>%
      addError(311, getDescError(311), (dups[dups != "character(0)"]) %>% toString())
  }
  
  n <- eb %>% filter(Cod %in% c(311)) %>% nrow()
  
  if (n == 0) {
    print(paste0("La validación de operaciones duplicadas concluyó con ", n, " observaciones. (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  }
  else{
    print(paste0("La validación de operaciones duplicadas concluyó con ", n, " observación. (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  }
  
  return(eb)
}
validarCruceInterno          <- function(agente, eb){
  exigibles <- getArchivosSinErrores(agent, eb,  c(201, 203), c("CCR", "CODGR"))
  
  if (length(restriccionPeriodos(eb, "BD01", "BD02A", "CCR")) >0){
    
    cruce1 <- tibble(Periodo   = restriccionPeriodos(eb, "BD01", "BD02A", "CCR")) %>% rowwise() %>%
      mutate(OpFaltantes_BD01  = realizarCruce(agente, exigibles, Periodo, "BD02A", "BD01"),
             OpFaltantes_BD02A = realizarCruce(agente, exigibles, Periodo, "BD01", "BD02A"))
    
    f_bd01  <- (paste(cruce1 %>% rowwise() %>% pull(OpFaltantes_BD01)    , collapse = ",") %>% strsplit(","))[[1]]
    f_bd02A <- (paste(cruce1 %>% rowwise() %>% pull(OpFaltantes_BD02A)   , collapse = ",") %>% strsplit(","))[[1]]
    
    
    if(length(f_bd01[f_bd01   != "character(0)"]) > 0){
      eb <- eb %>%
        addError(321,getDescError(321), (f_bd01[f_bd01 != "character(0)"]) %>% toString())
    }
    if(length(f_bd02A[f_bd02A != "character(0)"]) > 0){
      eb <- eb %>%
        addError(322,getDescError(322), (f_bd02A[f_bd02A != "character(0)"]) %>% toString())
    }
  }
  
  if (length(restriccionPeriodos(eb, "BD03A", "BD03B", "CODGR")) >0){
    
    cruce2 <- tibble(Periodo = restriccionPeriodos(eb, "BD03A", "BD03B", "CODGR")) %>% rowwise() %>%
      mutate(GaranFaltantes_BD03A = realizarCruce(agente, exigibles, Periodo, "BD03B", "BD03A"))
    
    f_bd03A <- (paste(cruce2 %>% rowwise() %>% pull(GaranFaltantes_BD03A), collapse = ",") %>% strsplit(","))[[1]]
    
    if(length(f_bd03A[f_bd03A != "character(0)"]) > 0){
      eb <- eb %>%
        addError(323, getDescError(323), (f_bd03A[f_bd03A != "character(0)"]) %>% toString())
    }
  }

  n <- eb %>% filter(Cod %in% c(321, 322, 323)) %>% nrow()
  print(paste0("La validación interna BD01/BD02A y BD03A/BD03B concluyó con ", n, " observaciones. (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  
  return(eb)
}
validarCampos                <- function(agente, eb){
  carpeta   <- getCarpeta(agente)
  exigibles <- getArchivosSinErrores(agente, eb, c(201, 203), c("CCR","CCR_C","CODGR"))
  
  # i. Errores tipo1 ----
  exigiblesT1 <- filtrarArchivosErrorT1_T3(agente, eb, exigibles, "tipo1")
  eb_i        <- eb
  for (i in 1:length(exigiblesT1)){
    ruta_i <- getRuta(carpeta, exigiblesT1[i])
    eb_i   <- procesarErroresT1(agente, ruta_i, eb_i)
    }
  eb <- eb_i %>%
    group_by(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion) %>%
    summarise(Detalle = toString(Detalle)) %>%
    ungroup()
  
  # ii. Errores tipo2 ----
  error461 <- eb
    for (ii in 1:length(exigibles[str_detect(exigibles, "BD01")])){
      ruta_ii  <- getRuta(carpeta, exigibles[str_detect(exigibles, "BD01")][ii])
      error461 <- procesarErrorSaldosNegativos(agente, ruta_ii, error461)
    }
    eb <- error461 %>%
      group_by(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion) %>%
      summarise(Detalle = toString(Detalle)) %>%
      ungroup()
  
  error462 <- paste(procesarErroresT2(agente, eb, exigibles, 462), sep= ",")
  error463 <- paste(procesarErroresT2(agente, eb, exigibles, 463), sep= ",")
  error464 <- paste(procesarErroresT2(agente, eb, exigibles, 464), sep= ",")
  error465 <- paste(procesarErroresT2(agente, eb, exigibles, 465), sep= ",")
  error466 <- paste(procesarErroresT2(agente, eb, exigibles, 466), sep= ",")

  error467 <- (tibble(Periodo = restriccionPeriodos(eb, "BD01", "BD03A", "CIS")) %>% rowwise() %>%
                 mutate(vf_CodDeudor = procesarErrorcodDeudor(agente, exigibles, Periodo)) %>% rowwise() %>%
                 pull(vf_CodDeudor) %>%
                 paste(collapse = ",") %>%
                 strsplit(","))[[1]]

  if (length(error462[error462 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(462, getDescError(462), (error462[error462 != "character(0)"]) %>% toString())
  }
  if (length(error463[error463 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(463, getDescError(463), (error463[error463 != "character(0)"]) %>% toString())
  }
  if (length(error464[error464 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(464, getDescError(464), (error464[error464 != "character(0)"]) %>% toString())
  }
  if (length(error465[error465 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(465, getDescError(465), (error465[error465 != "character(0)"]) %>% toString())
  }
  if (length(error466[error466 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(466, getDescError(466), (error466[error466 != "character(0)"]) %>% toString())
  }
  if (length(error467[error467 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(467, getDescError(467), (error467[error467 != "character(0)"]) %>% toString())
  }

  # iii. Errores tipo3 ----
  exigiblesT3 <- filtrarArchivosErrorT1_T3(agente, eb,
                                           exigibles[str_detect(exigibles, paste(c("BD01","BD02A","BD02B","BD04"), collapse = '|'))],
                                           "tipo3")
  eb_iii <- eb
  for (iii in 1:length(exigiblesT3)){
    ruta_iii <- getRuta(carpeta, exigiblesT3[iii])
    eb_iii   <- procesarErroresT3(agente, ruta_iii, eb_iii)
    }
  eb <- eb_iii %>%
    group_by(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion) %>%
    summarise(Detalle = toString(Detalle)) %>%
    ungroup()

  error479 <- (tibble(NombreArchivo = filtrarArchivosErrorT2(agente, eb, exigibles, 479)) %>% rowwise() %>% 
    mutate(Ruta               = getRuta(carpeta, NombreArchivo),
           vf_FechaDesembolso = generarDetalleError3(Ruta, procesarErrorFechaDesembolso(Ruta))) %>%
    pull(vf_FechaDesembolso) %>%
    paste(collapse = ",") %>%
    strsplit(","))[[1]]
  
  if (length(error479[error479 != "character(0)"]) > 0){
    eb <- eb %>%
      addError(479, getDescError(479), (error479[error479 != "character(0)"]) %>% toString())
  }

  n <- eb %>% filter(Cod %in% c(400:500)) %>% nrow()
  print(paste0("La validación interna de acuerdo a la Res.SBS N°22269-2020 concluyó con ", n, " observaciones. (~ly2) ", format(Sys.time(), "%a %b %d %X %Y")))
  return(eb)
}

#validarCuadreContable 
getCapital     <- function(ruta){
  tmp <- read_delim(ruta,"\t",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE, col_types = cols(.default = "c"), progress = T) 
  
  kvi <- sum(tmp$KVI, na.rm = TRUE)
  kve <- sum(tmp$KVE, na.rm = TRUE)
  krf <- sum(tmp$KRF, na.rm = TRUE)
  kju <- sum(tmp$KJU, na.rm = TRUE) 
  kre <- sum(tmp$KRE, na.rm = TRUE) 
  
  lista_bd <- list(kvi,kve,krf,kju) %>% return() 
}
getCapitalBDCC <- function(ruta){
  tmp <- read_delim(ruta,"\t", escape_double = FALSE, col_types = cols(KJU = col_double(),
                                                                       KRF = col_double(), 
                                                                       KVE = col_double(),
                                                                       KVI = col_double(),
                                                                       .default = "c"), 
                    trim_ws = TRUE, progress = T)  
  
  c(sum(tmp$KVI,na.rm=T),sum(tmp$KVE,na.rm=T),sum(tmp$KRF,na.rm=T),sum(tmp$KJU,na.rm=T)) %>% return()
}
getCapitalBC   <- function(ruta){
  tmp <- initCuadreContable() %>% 
    filter(CODIGO_ENTIDAD ==  as.double(getCoopac(ruta)), PERIODO == getAnoMes(ruta))
  
  if (nrow(tmp)==1){ 
    c(sum(tmp$KVI,na.rm=T),sum(tmp$KVE,na.rm=T),sum(tmp$KRF,na.rm=T),sum(tmp$KJU,na.rm=T)) %>% return()
  }
}
getCodErrorCruceContable <- function(dif_Capital){
  codError <- switch (dif_Capital,
                      KVI = 301,
                      KVE = 302,
                      KRF = 303,
                      KJU = 304)
  return(codError)
}

#validarOperacionesDuplicadas
operacionesDuplicadas <- function(ruta){
  if (getBD(ruta) == "BD01" | getBD(ruta) == "BD03A") {
    operaciones <- evaluarFile(ruta) %>% select(getCodigoBD(getBD(ruta))[1]) 
    duplicados <- operaciones[duplicated(operaciones), ] %>% 
      unique() %>%
      pull(getCodigoBD(getBD(ruta))[1])
    
    paste_error <- ifelse(length(duplicados) > 0,
                          list(paste0(getAnoMes(ruta), "-", getBD(ruta), ":", duplicados, collapse=",")),
                          list(character(0))) %>% return()
  }
  list(character(0)) %>% return()
  
}

#validarOperacionesVacias
operacionesVacias     <- function(ruta, BD = evaluarFile(ruta)){
  vacios <- BD %>% 
    select(getCodigoBD(getBD(ruta))[1]) %>%
    sapply(function(x) sum(is.na(x))) %>% return()
}

#validarCruceInterno
getCodigoBD   <- function(bd){
  campo <- case_when(bd == "BD01"  ~ "CCR",
                     bd == "BD02A" ~ "CCR",
                     bd == "BD02B" ~ "CCR_C",
                     bd == "BD03A" ~ "CODGR",
                     bd == "BD03B" ~ "CODGR",
                     bd == "BD04"  ~ "CCR_C")
  return(campo)
}
realizarCruce <- function(agente, exigibles, periodo, BD1, BD2){
  exigibles <- exigibles[str_detect(exigibles, 
                                    paste(c(paste(BD1, periodo, sep = "_"), paste(BD2, periodo, sep = "_")),
                                          collapse = '|')
                                    )]
  
  cruce <- setdiff(evaluarFile(getRuta(getCarpeta(agente),exigibles[str_detect(exigibles, BD1)])) %>%
                     pull(getCodigoBD(BD1)),
                   evaluarFile(getRuta(getCarpeta(agente),exigibles[str_detect(exigibles, BD2)])) %>%
                     pull(getCodigoBD(BD2))
                   ) %>% 
    unique()
  
  resultado <- generarDetalleError2(periodo, cruce)
  return(resultado)
}

#Validar campos

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

elegirDigitosBD <- function(ruta,campo){
  digitos <- switch (getBD(ruta),
                     BD01  = {getDigitosBD01(campo)},
                     BD02A = {getDigitosBD02A(campo)},
                     BD02B = {getDigitosBD02B(campo)},
                     BD03A = {getDigitosBD03A(campo)},
                     BD03B = {getDigitosBD03B(campo)},
                     BD04  = {getDigitosBD04(campo)})
  return(digitos)
}
getColsErrorT1  <- function(ruta){
  cols <- switch(getBD(ruta),
                 BD01  = {c("TID","TCR","CAL","ESAM","SEC","MDCR","OSD")},
                 BD02A = {c("MON","FOCAN")},
                 BD02B = {c("MON_C","FOCAN_C")},
                 BD03A = {c("CGR","COBGR")},
                 BD03B = {c("CGR")},
                 BD04  = {c("TID_C","TCR_C","MON_C","CAL_C","ESAM_C","FOCAN_C","MDCR_C")}) 
  return(cols)
}
getCodErrorT1   <- function(ruta, campo){
  codError <- switch (getBD(ruta),
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

# Tipo2----
detectarVacios <- function(ruta,campo){
  BD    <- evaluarFile(ruta)
  
  vacios <- BD %>% 
    filter(is.na(cgrep(BD, campo))) %>% 
    pull(getCodigoBD(getBD(ruta)))
  
  return(vacios)
}

#BD01
procesarErrorSaldosNegativos <- function(agente, ruta, errorBucket){
  BD         <- evaluarFile(ruta)
  saldosCols <- c("SKCR", "PCI", "KVI", "KRF", "KVE", "KJU", "SIN", "SID", "SIS", "DGR", "NCPR", "NCPA", "TPINT", "NRPRG")
  
  tb <- tibble(Columna = filtrarColsSaldos(ruta, saldosCols, errorBucket)) %>%
    rowwise() %>%
    mutate(verificarSaldos = BD %>% filter(as.numeric(cgrep(BD, Columna)[[1]]) <0) %>%
             pull(getCodigoBD("BD01")) %>% list(),
           resultado  = generarDetalleError3(ruta, verificarSaldos) %>% toString(),
           Coopac     = getCoopac(ruta),
           NombCoopac = getNomCoopac(ruta),
           Carpeta    = getCarpeta(agente),
           IdProceso  = getIdProceso(agente),
           Cod        = ifelse(resultado !="character(0)", 461, 0),
           Descripcion = paste0(getDescError(Cod), " en la columna ", Columna) ,
           Detalle     = toString(resultado))
  
  errorBucket <- bind_rows(errorBucket, tb %>%
                             filter(resultado != "character(0)") %>%
                             select(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion, Detalle))
  
  return(errorBucket)
}
procesarErrorModalidadCouta  <- function(ruta, BD = evaluarFile(ruta)){
  BD %>%
    filter(((as.numeric(ESAM) < 5) & (as.numeric(NCPR) == 0 | as.numeric(PCUO)  == 0)) == TRUE) %>%
    pull(CCR) %>% return()
}
procesarErrorMontoOtorgado   <- function(ruta, BD = evaluarFile(ruta)){
  BD %>% filter(as.numeric(MORG) < as.numeric(SKCR)) %>%
    pull(CCR) %>%
    return()
}
procesarErrorVencJudRetraso  <- function(ruta, BD = evaluarFile(ruta)){
  BD %>% 
    filter((as.numeric(KVE) > 0 & as.numeric(DAK) == 0)) %>% 
    pull (CCR) %>% return()
}

#BD01 y BD04
getnumCaracteresDoc         <- function(documento){
  n_caracteres <- switch (documento,
                          "1" = "8",
                          "2" = "9",
                          "3" = "13",
                          "4" = "13",
                          "5" = "12",
                          "6" = "11")
  return(n_caracteres)
}
procesarErrorDocumentoIdent <- function(ruta, BD = evaluarFile(ruta)){
  if (getBD(ruta) =="BD01"){
    verificar_documento <- BD %>%
      filter((CCR %in% detectarVacios(ruta, "TID")) == FALSE) %>% 
      rowwise() %>%
      mutate(detectarError = if_else(getnumCaracteresDoc(TID) == (nchar(NID) %>% toString()), "TRUE", "FALSE")) %>%
      filter(detectarError == "FALSE") %>%
      pull(getCodigoBD(getBD(ruta))) %>%
      union(detectarVacios(ruta, "TID")) 
    
    return(verificar_documento)
  }
  if (getBD(ruta) =="BD04"){
    verificar_documento <- BD %>%
      filter((CCR_C %in% detectarVacios(ruta, "TID_C")) == FALSE) %>% 
      rowwise() %>%
      mutate(detectarError = if_else(getnumCaracteresDoc(TID_C) == (nchar(NID_C) %>% toString()), "TRUE", "FALSE")) %>%
      filter(detectarError == "FALSE") %>%
      pull(getCodigoBD(getBD(ruta))) %>%
      union(detectarVacios(ruta, "TID_C")) 
    
    return(verificar_documento)
  }
}

#BD03A
procesarErrorNumCredCobertura <- function(ruta, BD = evaluarFile(ruta)){
  BD %>% 
    filter(as.numeric(NCR) > 0, as.numeric(NRCL) == 0) %>%
    pull(getCodigoBD("BD03A")) %>%
    unique() %>% return()
}

#BD3A y BD01
procesarErrorcodDeudor <- function(agente, exigibles, periodo){
  exigibles <- exigibles[str_detect(exigibles, 
                                    paste(c(paste("BD03A", periodo, sep = "_"), paste("BD01", periodo, sep = "_")), collapse = '|')
                                    )]
  
  cruce <- setdiff(evaluarFile(getRuta(getCarpeta(agente),exigibles[str_detect(exigibles, "BD03A")])) %>% pull(CIS),
                   evaluarFile(getRuta(getCarpeta(agente),exigibles[str_detect(exigibles, "BD01")])) %>% pull(CIS)
                   ) %>%  unique()
  
  resultado <- generarDetalleError2(periodo, cruce)
  return(resultado)
}


# Validaciones de campos fechas                             (errores tipo3)----
#BD01, BD02A, BD02B, BD04
getColsErrorT3  <- function(ruta){
  cols <- switch (getBD(ruta),
                  BD01  = {c("FOT", "FVEG", "FVEP")},
                  BD02A = {c("FVEP")},
                  BD02B = {c("FVEP_C")},
                  BD04  = {c("FOT_C", "FCAN_C")}) 
  return(cols)
}
getCodErrorT3   <- function(ruta, campo){
  codError <- switch (getBD(ruta),
                      BD01  = {c(471,472,473)},
                      BD02A = {c(474)},
                      BD02B = {c(475)},
                      BD04  = {c(477, 478)})
  
  cod <- tibble(col       = getColsErrorT3(ruta),
                cod_error = codError) %>% 
    filter(col == campo) %>% 
    pull(cod_error)
  return(cod)
}

#BD01
getFechaCorte                 <- function(ruta){
  fecha_corte <- seq(as.Date(paste(getAno(ruta),getMes(ruta),"01", sep = "-")),
                     length=1, by="months") %>%
    ceiling_date("month") - days(1)
  return(fecha_corte)
}
procesarErrorFechaDesembolso  <- function(ruta, BD = evaluarFile(ruta)){
  error <- BD %>% filter((dmy(BD %>% pull(FOT)) > getFechaCorte(ruta)) == TRUE) %>% 
    pull(getCodigoBD("BD01")) 
  return(error)
}