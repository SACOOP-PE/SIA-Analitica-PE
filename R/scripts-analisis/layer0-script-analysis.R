####' Script de análisis layer0 
####' 0. Revisión previa del bucket de errores, y soltar advertencias.
source("R/scripts-entorno/libraries-and-options.R")

# 
# getCruceAnexo06 <- function(Agente) {}
# getCruceAnexo05 <- function(Agente) {}
# getCruceEEFF <- function(Agente) {}
# 
# getOperacionesIntermitentes <- function(agente) {}
# getOperacionesCanceladasDuplicadas <- function(Agente) {} 

##### Create agent -----
agent <- createAgent(idCoopac = "01172",
                     carpeta = "test/datatest/01172/",
                     periodoInicial = "201901",
                     periodoFinal   = "202012")



getSaldoVigenteSiscor <- function(agent, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(C1401) %>% 
    return()
}
getSaldoRefinanciadoSiscor <- function(agent, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(C1404) %>% 
    return()
}
getSaldoVencidoSiscor <- function(agent, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(C1405) %>% 
    return()
}
getSaldoCobranzaJudicialSiscor <- function(agent, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(C1406) %>% 
    return()
}
getSaldoRendimientoDevengadoSiscor <- function(agent, periodo) {
  initCuadreContable() %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(C1408) %>% 
    return()
}
getSaldoInteresesDiferidosSiscor <- function(Agent, periodo) {
  initCuadreContable() %>% rowwise() %>% 
    mutate(diferidos = sum(C290101, C290102, C29010207, C29010702, C29010801, na.rm = T)) %>% 
    filter(str_sub(as.character(PeriodoId),1,6) == periodo, 
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(diferidos) %>% 
    return()
}
getSaldoProvisionesGenericasSiscor <- function(Agent, periodo) {
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
           CodigoEntidad == getCoopacFromAgent(agent)) %>% 
    pull(diferidos) %>% 
    return()
}
getSaldoProvisionesEspecificasSiscor <- function(Agent, periodo) {
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

getSaldoVigenteBDCC <- function(bd01) {
  sum(as.double(bd01$KVI), na.rm=T) %>% return()
}
getSaldoVencidoBDCC <- function(bd01) {
  sum(as.double(bd01$KVE), na.rm=T) %>% return()
} 
getSaldoRefinanciadoBDCC <- function(bd01) {
  sum(as.double(bd01$KRF), na.rm=T) %>% return()
} 
getSaldoCobranzaJudicialBDCC <- function(bd01) {
  sum(as.double(bd01$KJU), na.rm=T) %>% return()
} 
getSaldoRendimientoDevengadoBDCC <- function(bd01) {
  sum(as.double(bd01$SIN), na.rm=T) %>% return()
}
getSaldoInteresesDiferidosBDCC <- function(bd01) {
  sum(as.double(bd01$SID), na.rm=T) %>% return()
} 
getSaldoProvisionesGenericasBDCC <- function(bd01) {
  bd01 %>% rowwise() %>% 
    rowwise() %>% 
    filter(as.double(CAL) %in% c(0)) %>% 
    pull(PCI) %>% 
    as.double(.) %>% 
    sum(.,na.rm = T)
} 
getSaldoProvisionesEspecificasBDCC <- function(bd01) {
  bd01 %>% 
    rowwise() %>% 
    filter(as.double(CAL) %in% c(1,2,3,4)) %>% 
    pull(PCI) %>% 
    as.double(.) %>% 
    sum(.,na.rm = T)
} 
 

## Edwin aquí está la tabla de validación: 

## please continúa hasta incorporarlo en el eb.
carpeta   <- getCarpetaFromAgent(agent)
exigibles <- getArchivosExigiblesFromAgent(agent) 
tbl_cruce_BC <- tibble(NombreArchivo = exigibles[str_detect(exigibles, "BD01")]) %>% rowwise() %>%
  mutate(Ruta      = getRuta(carpeta, NombreArchivo),
         CodCoopac = getCoopacFromAgent(agent),
         IdProceso = getIdProcesoFromAgent(agent),
         BD        = getBDFromRuta(Ruta),
         Periodo   = getAnoMesFromRuta(toString(Ruta)),
         KVI_SISCOR    = getSaldoVigenteSiscor(agent, Periodo),
         KVE_SISCOR    = getSaldoVencidoSiscor(agent, Periodo),
         KRF_SISCOR    = getSaldoRefinanciadoSiscor(agent, Periodo),
         KJU_SISCOR    = getSaldoCobranzaJudicialSiscor(agent, Periodo),
         SIN_SISCOR    = getSaldoRendimientoDevengadoSiscor(agent, Periodo),
         SID_SISCOR    = getSaldoInteresesDiferidosSiscor(agent, Periodo),
         PCI_GEN_SISCOR = getSaldoProvisionesGenericasSiscor(agent, Periodo),
         PCI_ESP_SISCOR = getSaldoProvisionesEspecificasSiscor(agent, Periodo),
         KVI_BDCC   = getSaldoVigenteBDCC(evaluarFile(Ruta)),
         KVE_BDCC    = getSaldoVencidoBDCC(evaluarFile(Ruta)),
         KRF_BDCC    = getSaldoRefinanciadoBDCC(evaluarFile(Ruta)),
         KJU_BDCC    = getSaldoCobranzaJudicialBDCC(evaluarFile(Ruta)),
         SIN_BDCC    = getSaldoRendimientoDevengadoBDCC(evaluarFile(Ruta)),
         SID_BDCC    = getSaldoInteresesDiferidosBDCC(evaluarFile(Ruta)),
         PCI_GEN_BDCC = getSaldoProvisionesGenericasBDCC(evaluarFile(Ruta)),
         PCI_ESP_BDCC = getSaldoProvisionesEspecificasBDCC(evaluarFile(Ruta)),
         diff_KVI     = (KVI_SISCOR - KVI_BDCC) %>% round(.,2),
         diff_KVE     = (KVE_SISCOR - KVE_BDCC) %>% round(.,2),
         diff_KRF     = (KRF_SISCOR - KRF_BDCC) %>% round(.,2),
         diff_KJU     = (KJU_SISCOR - KJU_BDCC) %>% round(.,2),
         diff_SIN     = (SIN_SISCOR - SIN_BDCC) %>% round(.,2),
         diff_SID     = (SID_SISCOR - SID_BDCC) %>% round(.,2),
         diff_PCI_GEN     = (PCI_GEN_SISCOR - PCI_GEN_BDCC) %>% round(.,2),
         diff_PCI_ESP     = (PCI_ESP_SISCOR - PCI_ESP_BDCC) %>% round(.,2)) %>% View()

 


