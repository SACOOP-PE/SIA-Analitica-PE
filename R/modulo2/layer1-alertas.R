####' Script de análisis layer0 
####' 0. Revisión previa del bucket de errores, y soltar advertencias.

layer1_Alertas <- function(agente, eb){
  
  return(eb)
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
  quitarVaciosBD(ruta) %>% 
    filter(CCR_C %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(TCUO_C) > 27778) %>%
    pull(getCodigoBD("BD02B")) %>% unique() %>% 
    return()
}
alert1005 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(FOCAN_C) == 1 & as.numeric(MCT_C) > 27778) %>%
    pull(getCodigoBD("BD04")) %>% unique()
    return()
}
alert1006 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MCT_C) > 277778 & (dmy(BD %>% pull(FCAN_C)) - dmy(BD %>% pull(FOT_C))) > 30) %>%
    pull(getCodigoBD("BD04")) %>% unique() %>% 
    return()
}
alert1007 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MCT_C) > 277778) %>% 
    pull(CCR_C) %>% 
    return()
}


#Alertas Prudenciales ----

getCreditosSinGarantia <- function(agente, periodo) {
  
  credSinGarantias <- setdiff(evaluarFile(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>% pull(CCR),
                              evaluarFile(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD03B", periodo, sep = "_"))) %>% pull(CCR))
  
  return(credSinGarantias)
}
getCreditosConGarantia <- function(agente, periodo) {
  
  credConGarantias <- intersect(evaluarFile(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%  
                                filter(as.numeric(CAL)>0) %>%
                                  pull(CCR),
                                evaluarFile(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD03B", periodo, sep = "_"))) %>%
                                  filter(CGR !=1 & CGR !=5) %>%
                                  pull(CCR))
  
  return(credConGarantias)
}

asignarProvisionSG <- function(cal, tipoCredito) {
  if (toString(cal) == "0"){
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
  if (toString(cal) > "0"){
    provision <- if_else(cal == 1, 5,
                         if_else(cal == 2, 25,
                                 if_else(cal == 3, 60, 
                                         if_else(cal == 4, 100, 0))))
    return(provision)
  }
}
asignarProvisionCG <- function(cal, claseGarantia) {
  
  if (claseGarantia == 2) {
    provision <- 2
    return(provision)
  }
  if (claseGarantia == 3) {
    provision <- switch (toString(cal),
                         "1" = 1.25,
                         "2" = 6.25,
                         "3" = 15,
                         "4" = 30)
    return(provision)
  }
  if (claseGarantia == 4) {
    provision <- switch (toString(cal),
                         "1" = 2.5,
                         "2" = 12.50,
                         "3" = 30,
                         "4" = 60)
    return(provision)
  }
}

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
alert2007 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((CAL %in% c(3,4)) & as.numeric(SIN) > 0) %>%
    pull(CIS) %>%
    return()
}
alert2008 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((as.numeric(KRF)> 0 | as.numeric(KVE)> 0 | as.numeric(KJU)> 0) & as.numeric(SIN)> 0) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alert2009 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((CAL %in%  c(0,1)) & as.numeric(KVE) > 0) %>%
    pull(CIS) %>%
    return()
}
alert2010 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((CAL %in%  c(3,4)) & as.numeric(KVI) > 0) %>%
    pull(CIS) %>%
    return()
}
alert2011 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(DAK) > 120 & as.numeric(KJU) == 0) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alert2012 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(KJU) > 0 & (CAL %in% c(0,1,2))) %>%
    pull(CIS) %>%
    return()
}
alert2013 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(ESAM) %in% c(1,2) & (dmy(BD %>% pull(FVEG)) - dmy(BD %>% pull(FOT))) > 365) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alert2014 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(TCR) != 13 & (dmy(BD %>% pull(FVEG)) - dmy(BD %>% pull(FOT))) > 3650) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alert2015 <- function(ruta, agente) {
  
  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getCreditosSinGarantia(agente, getAnoMesFromRuta(ruta))) %>% 
    rowwise() %>%
    mutate(porcentajeProvision = asignarProvisionSG(as.numeric(CAL),as.numeric(TCR)),
           calcularProvision   = (as.numeric(PCI)/as.numeric(SKCR) *100) %>% round(0)) %>%
    filter(porcentajeProvision != calcularProvision) %>% 
    pull(CCR) %>%
    return() 
}
alert2016 <- function(ruta) {
  BD <- quitarVaciosBD(ruta)
  
  BD %>% 
    filter(as.numeric(TCR) >=9 & dmy(BD %>% pull(FVEG)) < dmy(BD %>% pull(FOT))) %>%
    pull(CCR) %>%
    return() 
}
alert2017 <- function(ruta) {
  # Fecha de pago última cuota cancelada - 
  # fecha de vencimiento última cuota cancelada
  # 
  # FCAN - FVEP == DAKR ???
}
alert2018 <- function(periodo, agente){
  
  creditosComun <- intersect(quitarVaciosBD(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%
                               pull(CCR),
                             quitarVaciosBD(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD02A", periodo, sep = "_"))) %>%
                               pull(CCR)) %>% unique()
  
  BD01  <- quitarVaciosBD(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%
    filter(CCR %in% creditosComun) %>% 
    select(CCR, MORG)
  
  BD02A <- quitarVaciosBD(getRuta(getCarpetaFromAgent(agente),paste(getCoopacFromAgent(agente), "BD02A", periodo, sep = "_"))) %>% 
    filter(CCR %in% creditosComun) %>% 
    select(CCR, MCUO)
  
  merge(BD01, BD02A, by.x = "CCR", by.y = "CCR") %>% 
    filter(MORG > MCUO) %>% 
    mutate(diff = as.numeric(MORG) - as.numeric(MCUO)) %>%
    filter(abs(diff) >100) %>% 
    pull(CCR) %>% unique() %>% 
    return()
}
alert2019 <- function(ruta, agente){
  
  BD01 <- quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getCreditosConGarantia(agente, getAnoMesFromRuta(ruta))) %>% 
    select(CCR, CAL, PCI, SKCR)
  
  BD03B <- quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getCreditosConGarantia(agente, getAnoMesFromRuta(ruta))) %>% 
    select(CCR, CGR)
    
  merge(BD01, BD03B, by.x = "CCR", by.y = "CCR") %>% 
    rowwise() %>%
    mutate(porcentajeProvision = asignarProvisionCG(as.numeric(CAL),as.numeric(CGR)),
           calcularProvision   = (as.numeric(PCI)/as.numeric(SKCR) *100) %>% round(0)) %>%
    filter(porcentajeProvision != calcularProvision) %>% 
    pull(CCR) %>% unique() %>% 
    return() 
}
alert2020 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(NCR) > 3) %>%
    pull(CODGR) %>% 
    return()
}
alert2021 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(dmy(BD %>% pull(FOT_C)) == (dmy(BD %>% pull(FCAN_C)))) %>% 
    pull(CCR_C) %>%
    return()
}
alert2022 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(NCPR_C) == as.numeric(NCPA_C)) %>%
    pull(getCodigoBD("BD04")) %>%
    return()
}
alert2023 <- function(ruta, eb) {
  BD         <- quitarVaciosBD(ruta)
  saldosCols <- getColsNoObservadas(ruta, eb, "saldos")
  
  if (length(saldosCols) >0) {
    alertSaldos <- tibble(Columna = saldosCols) %>% rowwise() %>%
      mutate(verificarSaldos = BD %>% 
                                filter(as.numeric(cgrep(BD, Columna)[[1]]) <0) %>% 
                                pull(CCR) %>% toString(),
             Cod             = 461) %>%
      filter(verificarSaldos != "")
    
    return(alertSaldos)
  }
  return("")
}
alert2024 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(((as.numeric(ESAM) < 5) & (as.numeric(NCPR) == 0 | as.numeric(PCUO)  == 0)) == TRUE) %>%
    pull(CCR) %>% 
    return()
}
alert2025 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((as.numeric(KVE) > 0 & as.numeric(DAK) == 0)) %>% 
    pull (CCR) %>% 
    return()
}
alert2026 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(NCR) > 0, as.numeric(NRCL) == 0) %>%
    pull(getCodigoBD("BD03A")) %>%
    unique() %>% 
    return()
}
