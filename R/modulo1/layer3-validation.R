#' Funciones principales
#' layer3(agente, eb)

layer3 <- function(agente, eb){
  eb <- validarCruceInterno(agente, eb)
  eb <- validarCreditosFaltantes(agente, eb)
  return(eb)
}

#' Funciones secundarias: nivel I
#' validarCruceInterno
#' validarCreditosFaltantes
#' 
#' realizarCruce()
#' getSabana()

validarCruceInterno      <- function(agente, eb){
  carpeta <- getCarpetaFromAgent(agente)
  
  if (length(getPeriodosNoObservados(agente, eb, "CCR")) >0){
    
    cruce1 <- tibble(Periodo   = getPeriodosNoObservados(agente, eb, "CCR")) %>% rowwise() %>%
      mutate(OpFaltantes_BD01  = realizarCruce(agente, Periodo, "BD02A", "BD01"),
             OpFaltantes_BD02A = realizarCruce(agente, Periodo, "BD01", "BD02A"))
    
    f_bd01  <- cruce1 %>% filter(OpFaltantes_BD01 != "") %>% select(Periodo, OpFaltantes_BD01)
    f_bd02A <- cruce1 %>% filter(OpFaltantes_BD02A != "") %>% select(Periodo, OpFaltantes_BD02A)
    
    if (nrow(f_bd01) >0) {
      chunk_501 <- f_bd01 %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Cod = 501,
               BD  = "BD02A",
               txt1 = OpFaltantes_BD01,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])
               ) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
      
      eb <- addError(eb, chunk_501)
    }
    if (nrow(f_bd02A) >0) {
      chunk_502 <- f_bd02A %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Cod = 502,
               BD  = "BD01",
               txt1 = OpFaltantes_BD02A,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]]),
               num2 = getSaldoTotal(getRuta(carpeta, paste(CodCoopac, "BD01", Periodo, sep = "_")), OpFaltantes_BD02A)
               ) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1, num2)
      
      eb <- addError(eb, chunk_502)
    }
  }
  
  if (length(getPeriodosNoObservados(agente, eb, "CODGR")) >0){
    
    cruce2 <- tibble(Periodo = getPeriodosNoObservados(agente, eb, "CODGR")) %>% rowwise() %>%
      mutate(GaranFaltantes_BD03A = realizarCruce(agente, Periodo, "BD03B", "BD03A"))
    
    f_bd03A <- cruce2 %>% filter(GaranFaltantes_BD03A != "") %>% select(Periodo, GaranFaltantes_BD03A)
    
    if (nrow(f_bd03A) >0) {
      chunk_503 <- f_bd03A %>% rowwise() %>%
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               Cod = 503,
               BD  = "BD03B",
               txt1 = GaranFaltantes_BD03A,
               num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])
               ) %>%
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
      
      eb <- addError(eb, chunk_503)
    }
  }
  
  n <- eb %>% filter(Cod %in% c(501:503)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("      Resultado: La validación de cruce interno concluyó sin observaciones."))
  }
  else{
    addEventLog(agente, paste0("      Resultado: La validación de cruce interno concluyó con ",n ," observación(es)."))
  }

  return(eb)
}
validarCreditosFaltantes <- function(agente, eb) {
  
  archivos <- getArchivosNoObservadosByCols(agente, eb, c("CCR","CCR_C"))
  periodos <- getPeriodosFromAgent(agente)
  
  if (periodos[2:length(periodos)] >1) {
    
    sabanaCartera      <- getSabana(agente, archivos, "BD01")
    sabanaCronoCance   <- getSabana(agente, archivos, "BD02B")
    sabanaCarteraCance <- getSabana(agente, archivos, "BD04")
    
    validacion <- sabanaCartera %>% 
      group_by(CCR) %>% 
      arrange(PeriodoI) %>% 
      filter(row_number() == max(row_number())) %>% rowwise() %>% 
      mutate(PeriodoEncontradoBD02B = sabanaCronoCance %>% filter(CCR_C == CCR) %>% pull(PeriodoI) %>% unique() %>% toString(),
             PeriodoEncontradoBD04  = sabanaCarteraCance %>% filter(CCR_C == CCR) %>% pull(PeriodoI) %>% unique() %>% toString(),
             EncontrarCreBD02B = if_else(PeriodoEncontradoBD02B == "",
                                         "FALSE", "TRUE"),
             EncontrarCreBD04  = if_else(PeriodoEncontradoBD04 == "",
                                         "FALSE", "TRUE")) %>% 
      select(PeriodoI, CCR, PeriodoEncontradoBD02B, EncontrarCreBD02B, PeriodoEncontradoBD04, EncontrarCreBD04)
    
    f_BD02B <- validacion %>% filter(EncontrarCreBD02B == "FALSE" & PeriodoI %in% periodos[1:length(periodos)-1]) %>% select(PeriodoI, CCR)
    f_BD04  <- validacion %>% filter(EncontrarCreBD04 == "FALSE" & PeriodoI %in% periodos[1:length(periodos)-1]) %>% select(PeriodoI, CCR)
    
    if (nrow(f_BD02B)>0) {
      chunk_504 <- f_BD02B %>% group_by(PeriodoI) %>% summarise(CCR = toString(CCR)) %>% rowwise() %>% 
        mutate(Periodo   = PeriodoI,
               CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               BD = "BD01",
               Cod = 504,
               txt1 = CCR,
               txt2 = toString(as.numeric(Periodo) +1),
               num1 = length(str_split(string=txt1, pattern = ",")[[1]])) %>% 
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
      
      eb <- addError(eb, chunk_504)
    }
    if (nrow(f_BD04)>0) {
      chunk_505 <- f_BD04 %>% group_by(PeriodoI) %>% summarise(CCR = toString(CCR)) %>% rowwise() %>% 
        mutate(Periodo   = PeriodoI,
               CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               BD = "BD01",
               Cod = 505,
               txt1 = CCR,
               txt2 = toString(as.numeric(Periodo) +1),
               num1 = length(str_split(string=txt1, pattern = ",")[[1]])) %>% 
        select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, txt2, num1)
      
      eb <- addError(eb, chunk_505)
    }
    
    
    n <- eb %>% filter(Cod %in% c(504:505)) %>% nrow()
    if (n == 0) {
      addEventLog(agente, paste0("      Resultado: La validación de créditos faltantes en la 2B o 4 concluyó sin observaciones."))
    }
    else{
      addEventLog(agente, paste0("      Resultado: La validación de créditos faltantes en la 2B o 4 concluyó con ",n," observación(es)."))
    }
    
    return(eb)
  }
  return(eb)
}


realizarCruce <- function(agente, periodo, data1, data2){
  
  archivo1 <- getRuta(getCarpetaFromAgent(agente), 
                      paste0(paste(getCoopacFromAgent(agente), data1, periodo, sep  = "_"), ".txt"))
  archivo2 <- getRuta(getCarpetaFromAgent(agente), 
                      paste0(paste(getCoopacFromAgent(agente), data2, periodo, sep  = "_"), ".txt"))
  
  cruce <- setdiff(quitarVaciosBD(archivo1) %>% pull(getCodigoBD(data1)),
                   quitarVaciosBD(archivo2) %>% pull(getCodigoBD(data2))) %>%
    unique() %>%
    toString()
  
  return(cruce)
}
getSabana     <- function(agente, archivos, bd) {
  
  carpeta          <- getCarpetaFromAgent(agente)
  archivosCreditos <- archivos[str_detect(archivos, bd)]
  
  sabana <- evaluarFile(getRuta(carpeta, archivosCreditos[1])) %>%
    mutate(PeriodoI = getAnoMesFromRuta(getRuta(carpeta, archivosCreditos[1])))
  
  if (getPeriodosFromAgent(agente) == 1) {
    
    if (bd == "BD01") {
      cartera <- sabana[c(51, 1:50)]
      return(cartera)
    }
    if (bd == "BD02B") {
      cronoCanc <- sabana[c(18, 1:17)]
      return(cronoCanc)
    }
    if (bd == "BD04") {
      carteraCanc <- sabana[c(36, 1:35)]
      return(carteraCanc)
    }
    
  }
  
  for (i in 2:length(archivosCreditos)-1) {
    sabana <- sabana %>% bind_rows(evaluarFile(getRuta(carpeta, archivosCreditos[i+1])) %>% 
                                     mutate(PeriodoI = getAnoMesFromRuta(getRuta(carpeta, archivosCreditos[i+1]))))
  }
  
  if (bd == "BD01") {
    cartera <- sabana[c(51, 1:50)]
    return(cartera)
  }
  if (bd == "BD02B") {
    cronoCanc <- sabana[c(18, 1:17)]
    return(cronoCanc)
  }
  if (bd == "BD04") {
    carteraCanc <- sabana[c(36, 1:35)]
    return(carteraCanc)
  }
  
}