####' Script de análisis layer0 
####' 0. Revisión previa del bucket de errores, y soltar advertencias.

layer0_Alertas <- function(agente, eb){
  eb <- ejecutarAlertasCruceContable(agente, eb)
  
  return(eb)
}

# getCruceAnexo06 <- function(Agente) {}
# getCruceAnexo05 <- function(Agente) {}
# getCruceEEFF <- function(Agente) {}
# 
# getOperacionesIntermitentes <- function(agente) {}
# getOperacionesCanceladasDuplicadas <- function(Agente) {} 


getSaldoVigenteSiscor                <- function(agente, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(C1401) %>% 
    return()
}
getSaldoRefinanciadoSiscor           <- function(agente, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(C1404) %>% 
    return()
}
getSaldoVencidoSiscor                <- function(agente, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(C1405) %>% 
    return()
}
getSaldoCobranzaJudicialSiscor       <- function(agente, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(C1406) %>% 
    return()
}
getSaldoRendimientoDevengadoSiscor   <- function(agente, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(C1408) %>% 
    return()
}
getSaldoInteresesDiferidosSiscor     <- function(agente, periodo) {
  initCuadreContable() %>% rowwise() %>% 
    mutate(diferidos = sum(C290101, C290102, C29010207, C29010702, C29010801, na.rm = T)) %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(diferidos) %>% 
    return()
}
getSaldoProvisionesGenericasSiscor   <- function(agente, periodo) {
  initCuadreContable() %>% rowwise() %>% 
    mutate(diferidos = sum(C14090202, 
                           C14090302, 
                           C14090402, 
                           C14090702, 
                           C14090902, 
                           C14091002,
                           C14091102,
                           C14091202,
                           C14091302,
                           na.rm = T) * -1) %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(diferidos) %>% 
    return()
}
getSaldoProvisionesEspecificasSiscor <- function(agente, periodo) {
  initCuadreContable() %>% rowwise() %>% 
    mutate(diferidos = sum(C14090201, 
                           C14090301, 
                           C14090401, 
                           C14090701, 
                           C14090901, 
                           C14091001,
                           C14091101,
                           C14091201,
                           C14091301,
                           na.rm = T) * -1) %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(diferidos) %>% 
    return()
} 

getSaldoVigenteBDCC                <- function(bd01) {
  sum(as.double(bd01$KVI), na.rm=T) %>% return()
}
getSaldoVencidoBDCC                <- function(bd01) {
  sum(as.double(bd01$KVE), na.rm=T) %>% return()
}
getSaldoRefinanciadoBDCC           <- function(bd01) {
  sum(as.double(bd01$KRF), na.rm=T) %>% return()
}
getSaldoCobranzaJudicialBDCC       <- function(bd01) {
  sum(as.double(bd01$KJU), na.rm=T) %>% return()
}
getSaldoRendimientoDevengadoBDCC   <- function(bd01) {
  sum(as.double(bd01$SIN), na.rm=T) %>% return()
}
getSaldoInteresesDiferidosBDCC     <- function(bd01) {
  sum(as.double(bd01$SID), na.rm=T) %>% return()
} 
getSaldoProvisionesGenericasBDCC   <- function(bd01) {
  bd01 %>%  
    filter(as.double(CAL) %in% c(0)) %>% 
    pull(PCI) %>% 
    as.double(.) %>% 
    sum(na.rm = T)
}
getSaldoProvisionesEspecificasBDCC <- function(bd01) {
  bd01 %>% 
    filter(as.double(CAL) %in% c(1,2,3,4)) %>% 
    pull(PCI) %>% 
    as.double(.) %>% 
    sum(na.rm = T)
}

getCodErrorContable               <- function(nameCapital) {
  codError <- switch (nameCapital,
                      KVI = 301,
                      KVE = 302,
                      KRF = 303,
                      KJU = 304,
                      SIN = 305,
                      SID = 306,
                      PCIGEN = 307,
                      PCIESP = 308)
  
  return(codError)
}
getAnoMesCoopacContableFromAgente <- function(agente) {
  initCuadreContable() %>% 
    mutate(PeriodoId = str_sub(as.character(PeriodoId),1,6)) %>% 
    filter(CodigoEntidad == getCoopacFromAgent(agente)) %>% 
    pull(PeriodoId) %>% unique() %>% return()
}

ejecutarAlertasCruceContable <- function(agente, eb){
  carpeta   <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosNoObservadosByCols(agente, eb, c("CCR",
                                                           "KVI", "KVE", "KRF", "KJU",
                                                           "SIN", "SID", "CAL", "PCI"))
  
  exigiblesBD01 <- exigibles[str_detect(exigibles, "BD01")]
  
  tbl_cruce_BC <- tibble(NombreArchivo = exigiblesBD01[str_detect(exigiblesBD01, 
                                                                  paste(getAnoMesCoopacContableFromAgente(agente), collapse = '|'))]
                         ) %>%
    rowwise() %>%
    mutate(Ruta      = getRuta(carpeta, NombreArchivo),
           CodCoopac = getCoopacFromAgent(agente),
           IdProceso = getIdProcesoFromAgent(agente),
           BD        = "BD01",
           Periodo   = getAnoMesFromRuta(toString(Ruta)),
           KVI_SISCOR   = getSaldoVigenteSiscor(agente, Periodo),
           KVE_SISCOR   = getSaldoVencidoSiscor(agente, Periodo),
           KRF_SISCOR   = getSaldoRefinanciadoSiscor(agente, Periodo),
           KJU_SISCOR   = getSaldoCobranzaJudicialSiscor(agente, Periodo),
           SIN_SISCOR   = getSaldoRendimientoDevengadoSiscor(agente, Periodo),
           SID_SISCOR   = getSaldoInteresesDiferidosSiscor(agente, Periodo),
           PCI_GEN_SISCOR = getSaldoProvisionesGenericasSiscor(agente, Periodo),
           PCI_ESP_SISCOR = getSaldoProvisionesEspecificasSiscor(agente, Periodo),
           KVI_BDCC     = getSaldoVigenteBDCC(evaluarFile(Ruta)),
           KVE_BDCC     = getSaldoVencidoBDCC(evaluarFile(Ruta)),
           KRF_BDCC     = getSaldoRefinanciadoBDCC(evaluarFile(Ruta)),
           KJU_BDCC     = getSaldoCobranzaJudicialBDCC(evaluarFile(Ruta)),
           SIN_BDCC     = getSaldoRendimientoDevengadoBDCC(evaluarFile(Ruta)),
           SID_BDCC     = getSaldoInteresesDiferidosBDCC(evaluarFile(Ruta)),
           PCI_GEN_BDCC = getSaldoProvisionesGenericasBDCC(evaluarFile(Ruta)),
           PCI_ESP_BDCC = getSaldoProvisionesEspecificasBDCC(evaluarFile(Ruta)),
           diff_KVI      = (KVI_SISCOR - KVI_BDCC) %>% round(.,2),
           diff_KVE      = (KVE_SISCOR - KVE_BDCC) %>% round(.,2),
           diff_KRF      = (KRF_SISCOR - KRF_BDCC) %>% round(.,2),
           diff_KJU      = (KJU_SISCOR - KJU_BDCC) %>% round(.,2),
           diff_SIN      = (SIN_SISCOR - SIN_BDCC) %>% round(.,2),
           diff_SID      = (SID_SISCOR - SID_BDCC) %>% round(.,2),
           diff_PCIGEN   = (PCI_GEN_SISCOR - PCI_GEN_BDCC) %>% round(.,2),
           diff_PCIESP   = (PCI_ESP_SISCOR - PCI_ESP_BDCC) %>% round(.,2)) %>% 
    pivot_longer(starts_with("diff"), names_to = "Capital", values_to = "Saldo") %>% rowwise() %>% 
    mutate(Resultado = ifelse(abs(Saldo)>100, "ERROR", ""),
           Cod       = ifelse(Resultado == "ERROR",
                              getCodErrorContable(str_split(Capital, "_")[[1]][2]), 0),
           ) %>% 
    filter(Resultado == "ERRROR")
  
  if (nrow(tbl_cruce_BC)> 0) {
    chunkContable <- tbl_cruce_BC %>% rowwise() %>%
      mutate(num2 = Saldo) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, num2)

    eb %>% addError(chunkContable)
  }
    
  n <- eb %>% filter(Cod %in% c(301:308)) %>% nrow()
  if (n == 0) {
    addEventLog(agente, paste0("      Resultado: El análisis contable concluyó sin observaciones."))
  }
  else{
    addEventLog(agente, paste0("      Resultado: El análisis contable concluyó con ",n," observación(es)."))
  }
  
  return(eb)
}