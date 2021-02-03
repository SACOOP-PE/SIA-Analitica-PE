#' Funciones principales
#' layer3(agente, eb)

layer3 <- function(agente, eb){
  eb <- validarCruceInterno(agente, eb)
  return(eb)
}

#' Funciones secundarias: nivel I
#' validarCruceInterno
#' realizarCruce()

validarCruceInterno <- function(agente, eb){
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
realizarCruce       <- function(agente, periodo, data1, data2){
  
  archivo1 <- getRuta(getCarpetaFromAgent(agente), 
                      paste0(paste(getCoopacFromAgent(agente), data1, periodo, sep  = "_"), ".txt"))
  archivo2 <- getRuta(getCarpetaFromAgent(agente), 
                      paste0(paste(getCoopacFromAgent(agente), data2, periodo, sep  = "_"), ".txt"))
  
  cruce <- setdiff(quitarVaciosBD(getRuta(getCarpetaFromAgent(agente), archivo1)) %>% pull(getCodigoBD(data1)),
                   quitarVaciosBD(getRuta(getCarpetaFromAgent(agente), archivo2)) %>% pull(getCodigoBD(data2))) %>%
    unique() %>%
    toString()
  
  return(cruce)
}