####' Script de análisis layer0 
####' 0. Revisión previa del bucket de errores, y soltar advertencias.

layer1_Alertas <- function(agente, eb){
  
  return(eb)
}

getArchivosExigiblesAlertas <- function(exigibles, codigoAlerta){
  if(codigoAlerta >= 2003 & codigoAlerta <= 2022){
    archivos <- switch (toString(codigoAlerta),
                        "2003"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("SEC", "MORG")),
                        "2004"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("OSD", "MORG")),
                        "2005"= getArchivosSinErrores(header, listaErrores, c(201, 203), "TEA"),
                        "2006"= getArchivosSinErrores(header, listaErrores, c(201, 203), "DGR"),
                        "2007"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("MORG", "SKCR")),
                        "2008"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CAL", "KRF", "KJU", "SIN")),
                        "2009"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CIS", "CAL")),
                        "2010"= getArchivosSinErrores(header, listaErrores, c(201, 203), "DARK"),
                        "2011"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("ESAM", "NCPR")),
                        "2012"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CAL", "SIN")),
                        "2013"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("KRF", "KVE", "KJU", "SIN")),
                        "2014"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CAL", "KVE", "CIS")),
                        "2015"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CAL", "KVI", "CIS")),
                        "2016"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("DAK", "KJU")),
                        "2017"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CAL", "KJU")),
                        "2018"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("ESAM", "FVEG", "FOT")),
                        "2019"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("TCR", "FVEG", "FOT")),
                        "2020"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("CIS", "CAL", "TCR", "PCI", "SKCR")),
                        "2022"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("FVEG", "FOT"))
    ) %>% 
      intersect(exigibles[str_detect(exigibles, "BD01")])
    return(archivos)
  }
  if(codigoAlerta > 2029){
    archivos <- switch (toString(codigoAlerta),
                        "2030"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("FOCAN_C", "MCT_C")),
                        "2031"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("FOCAN_C", "MCT_C", "FOT_C")),
                        "2032"= getArchivosSinErrores(header, listaErrores, c(201, 203), "MCT_C"),
                        "2033"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("FOT_C", "FCAN_C")),
                        "2034"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("NCPR_C", "NCPA_C"))
    ) %>% 
      intersect(exigibles[str_detect(exigibles, "BD04")])
    return(archivos)
  }
}
getPeriodosAlertas <- function(codigoAlerta){
  periodos <- switch (toString(codigoAlerta),
                      "2025" = restriccionPeriodos(listaErrores, "BD01", "BD02A", c("CCR", "CCR_C", "OSD", "TCUO")),
                      "2026" = restriccionPeriodos(listaErrores, "BD01", "BD02A", c("CCR", "CCR_C", "OSD", "TCUO_C")),
                      "2027" = restriccionPeriodos(listaErrores, "BD01", "BD02B", c("CCR", "CCR_C", "MORG", "MCUO"))
  )
  return(periodos)
}
getcodigoAlerta    <- function(BD){
  cod <- switch (BD,
                 BD01 = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2022),
                 BD04 = c(2030, 2031, 2032, 2033, 2034)
  )
  return(cod)
}

elegiralertasBD <- function(BD, cod, ruta){
  #La "ruta" puede ser también periodos
  if (BD == "BD01"){
    alerta <- switch (toString(cod),
                      "2003"= alertMontosuperiorSector(ruta),
                      "2004"= alertMontosuperiorOcupaciones(ruta),
                      "2005"= alertTea(ruta),
                      "2006"= alertDiasGracia(ruta),
                      "2007"= alertMontOtorsuperiorCapitalVig(ruta),
                      "2008"= alertRendimientoDevengado(ruta),
                      "2009"= alertDeudorCal(ruta),
                      "2010"= alertDiasAtrasonegativo(ruta),
                      "2011"= alertEsquemaAmortizaCuotaPagadas(ruta),
                      "2012"= alertDeudorInteresDevengado(ruta),
                      "2013"= alertCreditoInteresDevengado(ruta),
                      "2014"= alertDeudorContableVencido(ruta),
                      "2015"= alertCreditoContableVigente(ruta),
                      "2016"= alertDiasAtrasoJudicial(ruta),
                      "2017"= alertCreditoCobranzaJudicial(ruta),
                      "2018"= alertCreditosUnicouta(ruta),
                      "2019"= alertCreditosHipotecario(ruta),
                      "2020"= alertCreditosProvisiones(ruta),
                      "2022"= alertDiasAtrasoUltimaCouta(ruta)
    )
    return(alerta) 
  }
  if (BD == "BD02"){
    alerta <- switch (toString(cod),
                      "2025"= alertMontosuperiorOcupaciones2("BD02A", ruta),
                      "2026"= alertMontosuperiorOcupaciones2("BD02B", ruta),
                      "2027"= alertMontOrtorgadoCronograma(ruta)
    )
    return(alerta)
  }
  if (BD == "BD04"){
    alerta <- switch (toString(cod),
                      "2030"= alertCreditosEfectivo(ruta),
                      "2031"= alertCreditosAntesDesembolso(ruta),
                      "2032"= alertMontosuperiorOcupaciones3(getAnoMes(ruta)),
                      "2033"= alertFechaDesembolsoCancelacion(ruta),
                      "2034"= alertNumeroCanceladosyOriginales(ruta)
    )
    return(alerta)
  }
}

procesarAlertas  <- function(exigibles, BD, cod){
  tb <- tibble(CodigoAlerta = getcodigoAlerta(BD)) %>% rowwise() %>%
    mutate(Archivos = list(getArchivosExigiblesAlertas(exigibles, CodigoAlerta)))
  
  alertas <- tibble(NombreArchivo = unlist(tb %>% filter(CodigoAlerta == cod) %>% pull(Archivos))) %>% rowwise() %>%
    mutate(BDCC = BD,
           Ruta = getRuta(getCarpeta(header), NombreArchivo), 
           Alerta = ifelse(cod == 2032,
                           elegiralertasBD(BDCC, cod, Ruta),
                           generarDetalleError2(Ruta, elegiralertasBD(BDCC, cod, Ruta)))) %>% 
    pull(Alerta)
  return(alertas)
}
procesarAlertas2 <- function(cod){
  tb <- tibble(Periodos = getPeriodosAlertas(2025)) %>% rowwise() %>%
    mutate(Alerta = generarDetalleError4(Periodos, elegiralertasBD("BD02", cod, Periodos))) %>% 
    pull(Alerta)
  return(tb)
}

#Alertas PLAFT ----

getOcupacionesAltoRiesgo <- function(ruta){
  if (getBDFromRuta(ruta) != "BD01") {
    quitarVaciosBD(str_replace(ruta, getBDFromRuta(ruta), "BD01")) %>% 
      filter(OSD %in% c(1, 2, 5, 9)) %>%
      pull(getCodigoBD("BD01")) %>%
      return()
  }
  else{
    quitarVaciosBD(ruta) %>% 
      filter(OSD %in% c(1, 2, 5, 9)) %>%
      pull(getCodigoBD("BD01")) %>%
      return()
  }
}


alert1000 <- function(ruta) {
  
  alerta <- quitarVaciosBD(ruta) %>% 
             filter(as.numeric(UAGE) %in% c() & as.numeric(MORG) > 13889) %>%
             pull(getCodigoBD("BD01"))
  
  return(alerta)
}
alert1001 <- function(ruta) {
  
  alerta <- quitarVaciosBD(ruta) %>% 
    filter(as.numeric(SEC) %in% c(3,6,8,9,10) & as.numeric(MORG) > 27778) %>%
    pull(getCodigoBD("BD01"))
  
  return(alerta)
}
alert1002 <- function(ruta) {
  
  alert <-  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(MORG) > 138889) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alert1003 <- function(ruta) {
  
  alert <-  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(TCUO) > 27778) %>%
    pull(getCodigoBD("BD02A")) %>%
    unique() %>% 
    return()
}
alert1004 <- function(ruta) {
  
  alert <-  quitarVaciosBD(ruta) %>% 
    filter(CCR_C %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(TCUO_C) > 27778) %>%
    pull(getCodigoBD("BD02B")) %>%
    unique() %>% 
    return()
}
alert1005 <- function(ruta) {

}
alert1006 <- function(ruta) {
  
}
alert1007 <- function(ruta) {
  
}

#Alertas Prudenciales ----
alert2000 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(TEA) < 1) %>% pull(getCodigoBD("BD01")) %>% 
    return()
}
alert2001 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(DGR) > 90) %>% pull(getCodigoBD("BD01")) %>% 
    return() 
}
alert2002 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MORG) >= as.numeric(SKCR)) %>% pull(getCodigoBD("BD01")) %>%
    return()
}
alert2003 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(CAL %in% c(3,4) & (as.numeric(KRF)> 0 | as.numeric(KJU)> 0) & as.numeric(SIN)> 0) %>%
    pull(getCodigoBD("BD01")) %>%
    return() 
}
alert2004 <- function(ruta) {
  
  deudores   <- quitarVaciosBD(ruta) %>% select(CIS) 
  
  alerta <- tibble(Deudor = unique(deudores[duplicated(deudores), ])) %>% 
    rowwise() %>% 
    mutate(NumeroCalificaciones = quitarVaciosBD(ruta) %>%
             filter(CIS %in% Deudor) %>% 
             pull(CAL) %>% unique() %>% length()) %>% 
    filter(NumeroCalificaciones > 1) %>%
    pull(Deudor)
  
  return(alerta)
}
alert2005 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(DAKR) < 0) %>%
    pull(getCodigoBD("BD01")) %>%
    return() 
}
alert2006 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((as.numeric(ESAM) %in% c(3,4,5)) & as.numeric(NCPR) == 1) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alertDeudorInteresDevengado      <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in% c(3,4)) & as.numeric(SIN) > 0) %>%
    pull(CIS) %>%
    return()
}

# Codigo 2013
alertCreditoInteresDevengado     <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((as.numeric(KRF) >0 | as.numeric(KVE) >0 | as.numeric(KJU) >0) & as.numeric(SIN) > 0) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}

# Codigo 2014
alertDeudorContableVencido       <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(0,1)) & as.numeric(KVE) > 0) %>%
    pull(CIS) %>%
    return()
}

# Codigo 2015
alertCreditoContableVigente      <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(3,4)) & as.numeric(KVI) > 0) %>%
    pull(CIS) %>%
    return()
}

# Codigo 2016
alertDiasAtrasoJudicial          <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(DAK) > 120 & as.numeric(KJU) == 0) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}

# Codigo 2017
alertCreditoCobranzaJudicial     <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(KJU) > 0 & (CAL %in%  c(0,1,2))) %>%
    pull(CIS) %>%
    return()
}

# Codigo 20218
alertCreditosUnicouta            <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(ESAM) %in% c(1,2) & (dmy(BD %>% pull(FVEG)) - dmy(BD %>% pull(FOT))) > 365) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}

# Codigo 2019
alertCreditosHipotecario         <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(TCR) != 13 & (dmy(BD %>% pull(FVEG)) - dmy(BD %>% pull(FOT))) > 3650) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}

# Codigo 2020
getCreditosSinGarantia   <- function(periodo){
  credSinGarantias <- setdiff(getInfoTotal(getCarpeta(header), periodo, "BD01") %>% pull(CIS),
                              getInfoTotal(getCarpeta(header), periodo, "BD03A") %>% pull(CIS))
  
  getInfoTotal(getCarpeta(header), periodo, "BD01") %>% filter(CIS %in% credSinGarantias) %>% 
    pull(CCR) %>% 
    return()
}
asignarProvision         <- function(calificacion, tipoCredito){
  if (toString(calificacion) == "0"){
    provision <- switch (toString(tipoCredito),
                         "6"  = 0.7,
                         "7"  = 0.7,
                         "8"  = 1,
                         "9"  = 1,
                         "10" = 1,
                         "11" = 1,
                         "12" = 1,
                         "13" = 0.7) 
    return(provision)
  }
  if (toString(calificacion) > "0"){
    provision <- if_else(calificacion == 1, 5,
                         if_else(calificacion == 2, 25,
                                 if_else(calificacion == 3, 60, 
                                         if_else(calificacion == 4, 100, 0))))
    return(provision)
  }
}
alertCreditosProvisiones <- function(ruta, BD = evalFile(ruta)){
  tbAlerta <- BD %>% 
    filter(CCR %in% getCreditosSinGarantia(getAnoMes(ruta))) %>%  rowwise() %>%
    mutate(porcentajeProvision = asignarProvision(as.numeric(CAL),as.numeric(TCR)) %>% toString(),
           calcularProvision = (as.numeric(PCI)/as.numeric(SKCR) *100) %>% round(0)) %>%
    filter(porcentajeProvision != calcularProvision) %>% 
    pull(CCR) %>%
    return() 
}

# Codigo 2022
alertDiasAtrasoUltimaCouta       <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(dmy(BD %>% pull(FVEG)) < dmy(BD %>% pull(FOT))) %>%
    pull(getCodigoBD("BD01")) %>%
    return() 
}


#alertas BD01 y BD02A ----
# Codigo 2027
creditosComunes <- function(periodo, BD1, BD2){
  comunes <- intersect(getInfoCruce(getCarpeta(header), periodo, BD1), 
                       getInfoCruce(getCarpeta(header), periodo, BD2)) %>%
    unique() %>%
    return()
}
alertMontOrtorgadoCronograma <- function(periodo){
  creditosComun <- creditosComunes(periodo, "BD01", "BD02A")
  
  montoOtorgado    <- getInfoTotal(getCarpeta(header), periodo, "BD01") %>% filter(CCR %in% creditosComun) %>%
    pull(MORG)
  capitalPorCobrar <- getInfoTotal(getCarpeta(header), periodo, "BD02A") %>% filter(CCR %in% creditosComun) %>%
    pull(MCUO)
  
  creditosComun <- creditosComun[montoOtorgado > capitalPorCobrar] %>%
    unique() %>%
    return()
}
#alertas BD03A ----
# Codigo 2028 (aún por terminar)
getCreditosConGarantia   <- function(periodo){
  credConGarantias <- intersect(getInfoTotal(getCarpeta(header), periodo, "BD01") %>% pull(CIS),
                                getInfoTotal(getCarpeta(header), periodo, "BD03A") %>% pull(CIS)) %>% 
    return()
}
asignarProvisionGarantia <- function(claseGarantia, calificacion){
  if (claseGarantia == 2) {
    provision <- 2 %>% return()
  }
  if (claseGarantia == 3) {
    provision <- switch (calificacion %>% toString(),
                         "1" = 1.25,
                         "2" = 6.25,
                         "3" = 15,
                         "4" = 30
    )
    return(provision)
  }
  if (claseGarantia == 4) {
    provision <- switch (calificacion %>% toString(),
                         "1" = 2.5,
                         "2" = 12.50,
                         "3" = 30,
                         "4" = 60
    )
    return(provision)
  }
}
getDuplicadosCIS  <- function(periodo, BD){
  cis_deudor <- getInfoTotal(getCarpeta(header), periodo, BD) %>%
    filter(CIS %in% getCreditosConGarantia(periodo)) %>%
    select(CIS)
  duplicados <- cis_deudor[duplicated(cis_deudor), ] %>%
    unique() %>% 
    return()
}
alertGarantiaProvisiones <- function(periodo){
  
  deudorBD03 <- getInfoTotal(getCarpeta(header), 201902, "BD03A") %>%
    filter(CIS %in% getCreditosConGarantia(201902)) %>%
    select(CIS, CGR) %>% 
    group_by(CIS) %>%
    summarize(n=n())
  
  deudorBD01 <- getInfoTotal(getCarpeta(header), 201902, "BD01") %>%
    filter(CIS %in% getCreditosConGarantia(201902)) %>%
    select(CCR, CIS)  %>% 
    group_by(CIS) %>%
    summarize(n=n()) %>% 
    select(CIS)
  
  tibble(deudorCartera = getInfoTotal(getCarpeta(header), 201902, "BD01") %>%
           filter(CIS %in% getCreditosConGarantia(201902))) %>%
    rowwise() %>%
    mutate(claseDeGarantia = getInfoTotal(getCarpeta(header), 201902, "BD03A") %>%
             filter(CIS %in% getCreditosConGarantia(201902)) %>%
             select(CIS, CGR) %>%
             filter(CIS %in% deudorCartera$CIS) %>%
             pull(CGR) %>% unique() %>% toString()) %>% 
    select(deudorCartera$claseDeGarantia, )
  #        ,
  #        porcentajeProvision = asignarProvision(as.numeric(claseDeGarantia), as.numeric(CAL)) %>%
  #                             toString(),
  #        calcularProvision   = (as.numeric(PCI)/as.numeric(SKCR) *100) %>% round(0)) %>%
  # select(CCR, CIS, claseDeGarantia, porcentajeProvision, calcularProvision) %>%
  # return()
}

# Codigo 2029
alertGarantiasNumerocobertura    <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(NCR) > 3) %>%
    pull(getCodigoBD("BD03A")) %>% 
    return()
}
#alertas BD04 ----
# Codigo 2030
alertCreditosEfectivo            <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(FOCAN_C) == 1 & as.numeric(MCT_C) > 27778) %>%
    pull(getCodigoBD("BD04")) %>%
    return()
}

# Codigo 2031
alertCreditosAntesDesembolso     <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(MCT_C) > 277778 & (dmy(BD %>% pull(FCAN_C)) - dmy(BD %>% pull(FOT_C))) > 30) %>%
    pull(getCodigoBD("BD04")) %>%
    return()
}

# Codigo 2032
alertMontosuperiorOcupaciones3   <- function(periodo){
  bd4 <- getInfoTotal(getCarpeta(header), periodo, "BD04")
  if (length(intersect(creditosComunes(periodo, "BD01", "BD04"), ocupacionesAltoRiesgo(periodo))) >0) {
    creditos <- bd4 %>% filter(cgrep(bd4, getCodigoBD("BD04"))[[1]] %in% ocupacionesAltoRiesgo(periodo) &
                                 as.numeric(MCT_C) > 277778) %>%
      pull(getCodigoBD("BD04"))
    alerta <- generarDetalleError4(periodo, creditos)
    return(alerta)
  }
  return(list("character(0)"))
}

# Codigo 2033
alertFechaDesembolsoCancelacion  <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(dmy(BD %>% pull(FOT_C)) == (dmy(BD %>% pull(FCAN_C)))) %>% 
    pull(getCodigoBD("BD04")) %>%
    return()
}

# Codigo 2034
alertNumeroCanceladosyOriginales <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(NCPR_C) == as.numeric(NCPA_C)) %>%
    pull(getCodigoBD("BD04")) %>%
    return()
}
# alertMontosuperiorAgencias("C:/Users/eroque/Desktop/Proyecto_BDCC/SIA-Analitica-PE/test/datatest/202001/01172_BD01_202001.txt") %>% view()