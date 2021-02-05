####' Script de análisis layer0 
####' 0. Revisión previa del bucket de errores, y soltar advertencias.

layer1_Alertas <- function(agente, eb){
  
  return(eb)
}

# procesarAlertas  <- function(exigibles, BD, codAlerta){
#   tb <- tibble(CodigoAlerta = getcodigoAlerta(BD)) %>% rowwise() %>%
#     mutate(Archivos = list(getArchivosExigiblesAlertas(exigibles, CodigoAlerta)))
#   
#   alertas <- tibble(NombreArchivo = unlist(tb %>% filter(CodigoAlerta == cod) %>% pull(Archivos))) %>% rowwise() %>%
#     mutate(BDCC = BD,
#            Ruta = getRuta(getCarpeta(header), NombreArchivo), 
#            Alerta = ifelse(cod == 2032,
#                            elegiralertasBD(BDCC, cod, Ruta),
#                            generarDetalleError2(Ruta, elegiralertasBD(BDCC, cod, Ruta)))) %>% 
#     pull(Alerta)
#   return(alertas)
# }
# procesarAlertas2 <- function(cod){
#   tb <- tibble(Periodos = getPeriodosAlertas(2025)) %>% rowwise() %>%
#     mutate(Alerta = generarDetalleError4(Periodos, elegiralertasBD("BD02", cod, Periodos))) %>% 
#     pull(Alerta)
#   return(tb)
# }

getArchivosExigiblesAlertas <- function(exigibles, codAlerta){
  
  if (codAlerta %in% c(1000:1002, 2000:2017, 2024,2025)){
    
    archivos <- switch (toString(codAlerta),
                        "1000"= getArchivosNoObservadosByCols(agente, eb, c("UAGE", "MORG")),
                        "1001"= getArchivosNoObservadosByCols(agente, eb, c("SEC", "MORG")),
                        "1002"= getArchivosNoObservadosByCols(agente, eb, c("OSD", "MORG")),
                        "2000"= getArchivosNoObservadosByCols(agente, eb, "TEA"),
                        "2001"= getArchivosNoObservadosByCols(agente, eb, "DGR"),
                        "2002"= getArchivosNoObservadosByCols(agente, eb, c("MORG", "SKCR")),
                        "2003"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "KRF", "KJU", "SIN")),
                        "2004"= getArchivosNoObservadosByCols(agente, eb, c("CIS", "CAL")),
                        "2005"= getArchivosNoObservadosByCols(agente, eb, "DARK"),
                        "2006"= getArchivosNoObservadosByCols(agente, eb, c("ESAM", "NCPR")),
                        "2007"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "SIN")),
                        "2008"= getArchivosNoObservadosByCols(agente, eb, c("KRF", "KVE", "KJU", "SIN")),
                        "2009"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "KVE", "CIS")),
                        "2010"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "KVI", "CIS")),
                        "2011"= getArchivosNoObservadosByCols(agente, eb, c("DAK", "KJU")),
                        "2012"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "KJU", "CIS")),
                        "2013"= getArchivosNoObservadosByCols(agente, eb, c("ESAM", "FVEG", "FOT")),
                        "2014"= getArchivosNoObservadosByCols(agente, eb, c("TCR", "FVEG", "FOT")),
                        "2015"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "TCR", "PCI", "SKCR")),
                        "2016"= getArchivosNoObservadosByCols(agente, eb, c("TCR","FVEG", "FOT")),
                        "2017"= getArchivosNoObservadosByCols(agente, eb, c("FCAN", "FVEP", "DAKR")),
                        "2025"= getArchivosNoObservadosByCols(agente, eb, c("KVE", "DAK")),
                        ) %>% 
      intersect(exigibles[str_detect(exigibles, "BD01")])
    return(archivos)
    
  }
  if (codAlerta %in% c(1003, 2024)) {
    
    archivos <- switch (toString(codAlerta),
                        "1003"= getArchivosNoObservadosByCols(agente, eb, c("OSD", "TCUO")),
                        "2024"= getArchivosNoObservadosByCols(agente, eb, c("ESAM", "NCPR", "PCUO"))
                        ) %>% 
      intersect(exigibles[str_detect(exigibles, "BD02A")])
    return(archivos)
    
  }
  if (codAlerta == 1004) {
    
    archivos <- getArchivosNoObservadosByCols(agente, eb, c("OSD", "TCUO_C")) %>% 
      intersect(exigibles[str_detect(exigibles, "BD02B")])
    return(archivos)
    
  }
  if (codAlerta %in% c(2019,2020, 2026)) {
    
    archivos <- switch (toString(codAlerta),
                        "2019"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "PCI", "SKCR", "CGR")),
                        "2020"= getArchivosNoObservadosByCols(agente, eb, "NCR"),
                        "2026"= getArchivosNoObservadosByCols(agente, eb, c("NCR", "NRCL"))
                        ) %>% 
      intersect(exigibles[str_detect(exigibles, "BD03A")])
    return(archivos)
    
  }
  if (codAlerta %in% c(1005:1007, 2021,2022)) {
    
    archivos <- switch (toString(codAlerta),
                        "1005"= getArchivosNoObservadosByCols(agente, eb, c("FOCAN_C", "MCT_C")),
                        "1006"= getArchivosNoObservadosByCols(agente, eb, c("MCT_C", "FCAN_C", "FOT_C")),
                        "1007"= getArchivosNoObservadosByCols(agente, eb, "MCT_C"),
                        "2021"= getArchivosNoObservadosByCols(agente, eb, c("FOT_C", "FOCAN_C")),
                        "2022"= getArchivosNoObservadosByCols(agente, eb, c("NCPR_C", "NCPA_C"))
                        )%>% 
      intersect(exigibles[str_detect(exigibles, "BD04")])
    return(archivos)
    
  }

}
elegiralertasBD             <- function(ruta, codAlerta, agente, eb){
  if (codAlerta != 2018) {
    if (getBDFromRuta(ruta) == "BD01") {
      
      alerta <- switch (toString(codAlerta),
                        "1000"= alert1000(ruta),
                        "1001"= alert1001(ruta),
                        "1002"= alert1002(ruta),
                        "2000"= alert2000(ruta),
                        "2001"= alert2001(ruta),
                        "2002"= alert2002(ruta),
                        "2003"= alert2003(ruta),
                        "2004"= alert2004(ruta),
                        "2005"= alert2005(ruta),
                        "2006"= alert2006(ruta),
                        "2007"= alert2007(ruta),
                        "2008"= alert2008(ruta),
                        "2009"= alert2009(ruta),
                        "2010"= alert2010(ruta),
                        "2011"= alert2011(ruta),
                        "2012"= alert2012(ruta),
                        "2013"= alert2013(ruta),
                        "2014"= alert2014(ruta),
                        "2015"= alert2015(ruta, agente),
                        "2016"= alert2016(ruta),
                        "2017"= alert2017(ruta),
                        "2023"= alert2023(ruta, eb),
                        "2025"= alert2025(ruta))
      return(alerta)
      
    }
    if (getBDFromRuta(ruta) == "BD02A") {
      
      alerta <- switch (toString(codAlerta),
                        "1002"= alert1002(ruta),
                        "2024"= alert2024(ruta))
      return(alerta)
      
    }
    if (getBDFromRuta(ruta) == "BD02B") {
      alerta <- alert1004(ruta)
      return(alerta)
    }
    if (getBDFromRuta(ruta) == "BD03A"){
      alerta <- switch (toString(codAlerta),
                        "2019"= alert2019(ruta, agente),
                        "2020"= alert2020(ruta),
                        "2026"= alert2026(ruta))
      return(alerta)
    }
    if (getBDFromRuta(ruta) == "BD04") {
      
      alerta <- switch (toString(cod),
                        "1005"= alert1005(ruta),
                        "1007"= alert1007(ruta),
                        "2021"= alert2021(ruta),
                        "2022"= alert2022(ruta))
      return(alerta)
      
    } 
  }
  else{
    alerta <- alert2018(getAnoMesFromRuta(ruta), agente)
    return(alerta)
  }
}
getDescAlerta()  <- function(codAlerta){
  descr <- initRepositorioAlertas() %>% filter(CodAlerta == codAlerta) %>% pull(Descripcion)
  return(descr)
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
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(SEC) %in% c(3,6,8,9,10) & as.numeric(MORG) > 27778) %>%
    pull(getCodigoBD("BD01")) %>% 
    return()
}
alert1002 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(MORG) > 138889) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alert1003 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(TCUO) > 27778) %>%
    pull(getCodigoBD("BD02A")) %>% unique() %>% 
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
    filter(as.numeric(TEA) < 1) %>% pull(CCR) %>% 
    return()
}
alert2001 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(DGR) > 90) %>% pull(CCR) %>% 
    return() 
}
alert2002 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MORG) >= as.numeric(SKCR)) %>% pull(CCR) %>%
    return()
}
alert2003 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(CAL %in% c(3,4) & (as.numeric(KRF)> 0 | as.numeric(KJU)> 0) & as.numeric(SIN)> 0) %>%
    pull(CCR) %>%
    return() 
}
alert2004 <- function(ruta) {
  
  deudores <- quitarVaciosBD(ruta) %>% select(CIS) 
  
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
    filter(as.numeric(DAKR) < 0) %>% pull(CCR) %>%
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
    pull(CCR) %>%
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
    pull(CCR) %>%
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
alert2018 <- function(periodo, agente) {
  
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
alert2019 <- function(ruta, agente) {
  
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
    pull(CCR_C) %>%
    return()
}
alert2023 <- function(ruta, eb) {
  BD         <- quitarVaciosBD(ruta)
  saldosCols <- getColsNoObservadas(ruta, eb, "saldos")
  
  if (length(saldosCols) >0) {
    alertSaldos <- tibble(Columna = saldosCols) %>% rowwise() %>%
      mutate(verificarSaldos = BD %>% 
                                filter(as.numeric(cgrep(BD, Columna)[[1]]) <0) %>% 
                                pull(CCR) %>% toString()) %>%
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
    pull(CODGR) %>%
    unique() %>% 
    return()
}