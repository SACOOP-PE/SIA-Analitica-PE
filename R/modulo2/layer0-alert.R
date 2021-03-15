####' Script de detecccion de altertas layer0.

layer0_Alertas <- function(agente, eb){
  # eb <- detectarAlertasPLAFT(agente, eb)
  eb <- detectarAlertasPrudenciales(agente, eb)
  
  return(eb)
}

#Funciones primarias ----
detectarAlertasPLAFT         <- function(agente, eb) {

  codAlerta <- c(1000:1007)
  
  for (i in 1:length(codAlerta)) {
    eb <- procesarAlertas(codAlerta[i], agente, eb)
  }
  return(eb)
}
detectarAlertasPrudenciales  <- function(agente, eb) {

  codAlerta <- c(2000:2026)
    
  for (i in 1:length(codAlerta)) {
   eb <- procesarAlertas(codAlerta[i], agente, eb)
  }
  return(eb)
}

#Funciones secundarias ----
procesarAlertas             <- function(cod, agente, eb) {
  carpeta         <- getCarpetaFromAgent(agente)
  exigiblesAlerta <- getArchivosExigiblesAlertas(cod, agente, eb)
  
  if (as.numeric(agente %>% pull(PeriodoFinal))>202001 & length(exigiblesAlerta) >0) {
    
    if (cod %in% c(2000:2014, 2016, 2020:2023, 2025, 2026)) {
      
      alertas <- tibble(Nombre = exigiblesAlerta) %>% rowwise() %>% 
        mutate(ruta      = getRuta(carpeta, Nombre),
               CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               BD        = getBDFromRuta(ruta),
               Periodo   = getAnoMesFromRuta(ruta),
               Alert     = seleccionarAlertasBD(ruta, cod, agente, eb) %>% toString()) %>% 
        filter(Alert != "")
      
      if (nrow(alertas) >0) {
        chunkAlert <- alertas %>% rowwise() %>% 
          mutate(Cod = cod,
                 txt1 = Alert,
                 num1 = length(str_split(string=txt1, pattern = ",")[[1]])) %>%  
          select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
        
        eb <- eb %>% addError(chunkAlert)
      }
      return(eb)
    }
    if (cod %in% c(2015, 2019)) {
      
      Periodos  <- tibble(Periodos = str_extract(exigiblesAlerta, paste(as.character(getPeriodosFromAgent(agente)), collapse = '|'))) %>%
        group_by(Periodos) %>%
        filter(n() ==2) %>%
        pull(Periodos) %>% 
        unique()
      
      if (length(Periodos) >0) {
        
        alertas <- tibble(Periodo = Periodos) %>% rowwise() %>% 
          mutate(CodCoopac = getCoopacFromAgent(agente),
                 IdProceso = getIdProcesoFromAgent(agente),
                 BD    = "BD01",
                 ruta  = getRuta(carpeta, paste0(CodCoopac, "_BD01_", Periodo, ".txt")),
                 Alert = seleccionarAlertasBD(ruta, cod, agente, eb) %>% toString()) %>% 
          filter(Alert != "")
        
        if (nrow(alertas) >0) {
          chunkAlert <- alertas %>% rowwise() %>% 
            mutate(Cod  = cod,
                   txt1 = Alert,
                   num1 = length(str_split(string=txt1, pattern = ",")[[1]])) %>%  
            select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
          
          eb <- eb %>% addError(chunkAlert)
        }
        return(eb)
      }
      return(eb)
    }
    if (cod %in% c(2017, 2018, 2024)) {
      
      alertas <- tibble(Periodo = exigiblesAlerta) %>% rowwise() %>% 
        mutate(CodCoopac = getCoopacFromAgent(agente),
               IdProceso = getIdProcesoFromAgent(agente),
               BD        = "BD01",
               Alert     = switch (toString(cod),
                                   "2017" = alert2017(Periodo, agente),
                                   "2018" = alert2018(Periodo, agente),
                                   "2024" = alert2024(Periodo, agente)) %>% toString()) %>% 
        filter(Alert != "")
      
      if (nrow(alertas) >0) {
        chunkAlert <- alertas %>% rowwise() %>% 
          mutate(Cod  = cod,
                 txt1 = Alert,
                 num1 = length(str_split(string=txt1, pattern = ",")[[1]])) %>%  
          select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
        
        eb <- eb %>% addError(chunkAlert)
      }
      return(eb)
    }
  }
  return(eb)
}
seleccionarAlertasBD        <- function(ruta, cod, agente, eb) {
  if (cod %in% c(2017,2018,2024)) {
    return("")
  }
  else {
    if (getBDFromRuta(ruta) == "BD01") {
      
      alerta <- switch (toString(cod),
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
                        "2019"= alert2019(ruta, agente),
                        "2023"= alert2023(ruta, eb),
                        "2025"= alert2025(ruta))
      return(alerta)
      
    }
    if (getBDFromRuta(ruta) == "BD02A") {
      alerta <- alert1003(ruta)
      return(alerta)
    }
    if (getBDFromRuta(ruta) == "BD02B") {
      alerta <- alert1004(ruta)
      return(alerta)
    }
    if (getBDFromRuta(ruta) == "BD03A") {
      alerta <- switch (toString(cod),
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
}
getArchivosExigiblesAlertas <- function(cod, agente, eb) {
  
  exigibles        <- getArchivosNoObservadosByCols(agente, eb, c("CCR","CCR_C","CODGR"))
  exigibles2020    <- exigibles[str_detect(exigibles, 
                                           paste(as.character(202001:as.numeric(agente %>% pull(PeriodoFinal))), collapse = '|'))]
  
  exigiblesAlertas <- switch (toString(cod),
                              "1000"= getArchivosNoObservadosByCols(agente, eb, c("UAGE", "MORG")),
                              "1001"= getArchivosNoObservadosByCols(agente, eb, c("SEC", "MORG")),
                              "1002"= getArchivosNoObservadosByCols(agente, eb, c("OSD", "MORG")),
                              "1003"= getArchivosNoObservadosByCols(agente, eb, c("OSD", "TCUO")),
                              "1004"= getArchivosNoObservadosByCols(agente, eb, c("OSD", "TCUO_C")),
                              "1005"= getArchivosNoObservadosByCols(agente, eb, c("FOCAN_C", "MCT_C")),
                              "1006"= getArchivosNoObservadosByCols(agente, eb, c("MCT_C", "FCAN_C", "FOT_C")),
                              "1007"= getArchivosNoObservadosByCols(agente, eb, "MCT_C"),
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
                              "2018"= getArchivosNoObservadosByCols(agente, eb, c("MORG", "MCUO")),
                              "2019"= getArchivosNoObservadosByCols(agente, eb, c("CAL", "PCI", "SKCR", "CGR")),
                              "2020"= getArchivosNoObservadosByCols(agente, eb, "NCR"),
                              "2021"= getArchivosNoObservadosByCols(agente, eb, c("FOT_C", "FOCAN_C")),
                              "2022"= getArchivosNoObservadosByCols(agente, eb, c("NCPR_C", "NCPA_C")),
                              "2024"= getArchivosNoObservadosByCols(agente, eb, c("ESAM", "NCPR", "PCUO")),
                              "2025"= getArchivosNoObservadosByCols(agente, eb, c("KVE", "DAK")),
                              "2026"= getArchivosNoObservadosByCols(agente, eb, c("NCR", "NRCL"))) %>%
      intersect(exigibles2020)
    
    if (cod %in% c(1000:1002, 2000:2014, 2016, 2025)) {
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, "BD01")]
      return(exigiblesAlertas)
    }
    if (cod == 1003) {
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, "BD02A")]
      return(exigiblesAlertas)
    }
    if (cod == 1004) {
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, "BD02B")]
      return(exigiblesAlertas)
    }
    if (cod %in% c(2017, 2018, 2024)) {
      
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, paste(c("BD01","BD02A"), collapse = '|'))]
      
      Periodos <- tibble(Periodos = str_extract(exigiblesAlertas, paste(as.character(global.alcance), collapse = '|'))) %>%
        group_by(Periodos) %>%
        filter(n() == 2) %>%
        pull(Periodos) %>% 
        unique() 

      return(Periodos)
    }
    if (cod %in% c(2015, 2019)) {
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, paste(c("BD01","BD03B"), collapse = '|'))]
      return(exigiblesAlertas)
    }
    if (cod %in% c(2020, 2026)) {
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, "BD03A")]
      return(exigiblesAlertas)
    }
    if (cod %in% c(1005:1007, 2021,2022)) {
      exigiblesAlertas <- exigiblesAlertas[str_detect(exigiblesAlertas, "BD04")]
      return(exigiblesAlertas)
    }
    if (cod == 2023) {
      exigiblesAlertas <- exigibles2020[str_detect(exigibles2020, "BD01")]
      return(exigiblesAlertas)
    }
}
getDescAlerta               <- function(cod) {
  initRepositorioAlertas() %>% filter(Cod == cod) %>% pull(Descripcion) %>% return()
}

#Alertas PLAFT ----

getOcupacionesAltoRiesgo <- function(ruta){
  if (getBDFromRuta(ruta) != "BD01") {
    quitarVaciosBD(str_replace(ruta, getBDFromRuta(ruta), "BD01")) %>% 
      filter(OSD %in% c(1, 2, 5, 9)) %>%
      pull(CCR) %>%
      return()
  }
  else{
    quitarVaciosBD(ruta) %>% 
      filter(OSD %in% c(1, 2, 5, 9)) %>%
      pull(CCR) %>%
      return()
  }
}

alert1000 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(UAGE) %in% c() & as.numeric(MORG) > 13889) %>%
    pull(CCR) %>% 
    return()
}
alert1001 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(SEC) %in% c(3,6,8,9,10) & as.numeric(MORG) > 27778) %>%
    pull(CCR) %>% 
    return()
}
alert1002 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(MORG) > 138889) %>%
    pull(CCR) %>%
    return()
}
alert1003 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(CCR %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(TCUO) > 27778) %>%
    pull(CCR) %>% unique() %>% 
    return()
}
alert1004 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(CCR_C %in% getOcupacionesAltoRiesgo(ruta) & as.numeric(TCUO_C) > 27778) %>%
    pull(CCR_C) %>% unique() %>% 
    return()
}
alert1005 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(FOCAN_C) == 1 & as.numeric(MCT_C) > 27778) %>%
    pull(CCR_C) %>% unique()
    return()
}
alert1006 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MCT_C) > 277778 & as.numeric(difftime(dmy(FCAN_C), dmy(FOT_C), units = "days")) > 30) %>%
    pull(CCR_C) %>% unique() %>% 
    return()
}
alert1007 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MCT_C) > 277778) %>% 
    pull(CCR_C) %>% 
    return()
}


#Alertas Prudenciales ----
getCreditosComunes <- function(periodo, agente){
  creditosComun <- intersect(quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%
                               pull(CCR),
                             quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD02A", periodo, sep = "_"))) %>%
                               pull(CCR)) %>% unique()
  
  return(creditosComun)
}

getCreditosSinConGarantia <- function(agente, periodo, TipoCredito) {
  
  BD01  <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_")))
  BD03A <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD03A", periodo, sep = "_")))
  BD03B <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD03B", periodo, sep = "_")))
  
  garantiasExistentes <- intersect(BD03A %>% pull(CODGR), BD03B %>% pull(CODGR))
  
  if (TipoCredito == "SG") {
    
    credSinGarantias <- setdiff(BD01 %>% filter(as.numeric(CAL) %in% c(0,1,2,3,4) & as.numeric(TCR) %in% c(6,7,8,9,10,11,12,13)) %>% pull(CCR), 
                                BD03B %>% filter(CODGR %in% garantiasExistentes) %>% pull(CCR))
    return(credSinGarantias)
  }
  if (TipoCredito == "CG") {
    
    credConGarantias <- intersect(BD01 %>% filter(as.numeric(CAL) %in% c(1,2,3,4) & as.numeric(TCR) %in% c(6,7,8,9,10,11,12,13)) %>% pull(CCR),
                                  BD03B %>% filter(as.numeric(CGR) %in% c(1,2,3,4,5) & CODGR %in% garantiasExistentes) %>% pull(CCR))
    
    return(credConGarantias)
  }
}

asignarProvisionSG <- function(cal, tipoCredito) {
  if (cal == 0) {
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
  if (cal >= 1) {
    provision <- if_else(cal == 1, 5,
                         if_else(cal == 2, 25,
                                 if_else(cal == 3, 60,
                                         if_else(cal == 4, 100, 0))))
    return(provision)
  }
}
asignarProvisionCG <- function(cal, claseGarantia, tipoCredito) {
  
  if (claseGarantia %in% c(1,5)) {
    provision <- switch (toString(cal),
                         "1" = 1.25,
                         "2" = 6.25,
                         "3" = 15,
                         "4" = 30)
    return(provision)
  }
  if (claseGarantia == 2) {
    provision <- 1
    return(provision)
  }
  if (claseGarantia == 3 & tipoCredito %in% c(6,7,8,9,10,13)) {
    
    provision <- switch (toString(cal),
                         "1" = 1.25,
                         "2" = 6.25,
                         "3" = 15,
                         "4" = 30)
    return(provision)
  }
  if (claseGarantia == 4 & tipoCredito == 13) {
    provision <- switch (toString(cal),
                         "1" = 2.5,
                         "2" = 12.50,
                         "3" = 30,
                         "4" = 60)
    return(provision)
  }
  if ((claseGarantia == 4 & tipoCredito !=13) | (claseGarantia == 3 & tipoCredito %in% c(11,12))) {
    provisión <- 0
    return(provisión) 
  }
  
  provisión <- 0
  return(provisión)
}

alert2000 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(TEA) < 1) %>% pull(CCR) %>% return()
}
alert2001 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(DGR) > 90) %>% pull(CCR) %>% return() 
}
alert2002 <- function(ruta) {
  quitarVaciosBD(ruta) %>% 
    filter(as.numeric(MORG) > as.numeric(SKCR)) %>% pull(CCR) %>% return()
}
alert2003 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(CAL %in% c(3,4) & (as.numeric(KRF)> 0 | as.numeric(KJU)> 0) & as.numeric(SIN)> 0) %>% pull(CCR) %>%
    return() 
}
alert2004 <- function(ruta) {
  
  quitarVaciosBD(ruta) %>% group_by(CIS) %>% filter(n() >1) %>% 
    summarise(numCal = n_distinct(CAL)) %>% filter(numCal >1) %>% pull(CIS) %>% return()
}
alert2005 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(DAKR) < 0) %>% pull(CCR) %>% return() 
}
alert2006 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((as.numeric(ESAM) %in% c(3,4,5)) & as.numeric(NCPR) == 1) %>% pull(CCR) %>%
    return()
}
alert2007 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(CAL) %in% c(3,4) & as.numeric(SIN) > 0) %>% pull(CCR) %>% return()
}
alert2008 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter((as.numeric(KRF)> 0 | as.numeric(KVE)> 0 | as.numeric(KJU)> 0) & as.numeric(SIN)> 0) %>%
    pull(CCR) %>%
    return()
}
alert2009 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(CAL) %in% c(0,1) & as.numeric(KVE) > 0 & as.numeric(TCR) %in% c(9,10,11,12,13,20)) %>%
    pull(CIS) %>%
    return()
}
alert2010 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(CAL) %in% c(3,4) & as.numeric(KVI) > 0) %>% pull(CIS) %>% return()
}
alert2011 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(DAK) > 120 & as.numeric(KJU) == 0) %>% pull(CCR) %>%return()
}
alert2012 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(KJU) > 0 & as.numeric(CAL) %in% c(0,1,2)) %>% pull(CCR) %>%
    return()
}
alert2013 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(ESAM) %in% c(1,2) & as.numeric(difftime(dmy(FVEG), dmy(FOT), units = "days")) > 365) %>%
    pull(CCR) %>%
    return()
}
alert2014 <- function(ruta) {
  quitarVaciosBD(ruta) %>%
    filter(as.numeric(TCR) != 13 & as.numeric(difftime(dmy(FVEG), dmy(FOT), units = "days")) > 3650) %>%
    pull(CCR) %>%
    return()
}
alert2015 <- function(ruta, agente) {
  
  creditosComunes <- getCreditosSinConGarantia(agente, getAnoMesFromRuta(ruta), "SG")
  
  if (length(creditosComunes)>0) {
    
   alerta <- quitarVaciosBD(ruta) %>% 
      filter(CCR %in% getCreditosSinConGarantia(agente, getAnoMesFromRuta(ruta), "SG")) %>% 
      rowwise() %>%
      mutate(provisionTeorica = asignarProvisionSG(as.numeric(CAL), as.numeric(TCR)),
             provisionReal    = (as.numeric(PCI)/as.numeric(SKCR) *100) %>% round(0)) %>%
      filter(provisionTeorica < provisionReal) %>% 
      pull(CCR)
   
   return(alerta)
  }
  return("")
}
alert2016 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(TCR) >=9 & dmy(FVEG) < dmy(FOT)) %>% pull(CCR) %>% return() 
}
alert2017 <- function(periodo, agente) {
  
  creditosComunes <- getCreditosComunes(periodo, agente)
  
  if (length(creditosComunes)> 0) {
    
    BD01  <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%
      filter(CCR %in% creditosComunes) %>% select(CCR, FVEG, DAKR)
    
    BD02A <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD02A", periodo, sep = "_"))) %>% 
      filter(CCR %in% creditosComunes) %>%
      mutate(NCUO = as.numeric(NCUO)) %>% 
      group_by(CCR) %>% 
      arrange(CCR, NCUO) %>% 
      filter(row_number() == max(row_number())) %>% select(CCR, FCAN)
    
    alerta <- merge(BD01, BD02A, by.x = "CCR", by.y = "CCR") %>% 
      rowwise() %>% 
      mutate(DAKR_teorico = as.numeric(difftime(dmy(FCAN), dmy(FVEG), units = "days")),
             DAKR_real    = as.numeric(DAKR))%>% 
      filter(DAKR_teorico != DAKR_real) %>% pull(CCR) %>% unique()
    
    return(alerta)
  }
  return("")
}
alert2018 <- function(periodo, agente) {
  
  creditosComunes <- getCreditosComunes(periodo, agente)
  
  if (length(creditosComunes)> 0) {
    
    BD01  <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%
      filter(CCR %in% creditosComunes) %>% select(CCR, MORG)
    
    BD02A <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD02A", periodo, sep = "_"))) %>% 
      filter(CCR %in% creditosComunes) %>% select(CCR, MCUO)
    
    alerta <- merge(BD01, BD02A, by.x = "CCR", by.y = "CCR") %>% 
      filter(MORG > MCUO) %>% 
      mutate(diff = as.numeric(MORG) - as.numeric(MCUO)) %>%
      filter(abs(diff) >100) %>% 
      pull(CCR) %>% unique()
    
    return(alerta)
  }
  return("")
}
alert2019 <- function(ruta, agente) {
  
  creditosComunes <- getCreditosSinConGarantia(agente, getAnoMesFromRuta(ruta), "CG")
  
  if (length(creditosComunes)>0) {
    
    BD01 <- quitarVaciosBD(ruta) %>% filter(CCR %in% creditosComunes) %>% select(CCR, CAL, PCI, SKCR, TCR)
    
    BD03B <- quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD03B", getAnoMesFromRuta(ruta), sep = "_"))) %>%
      filter(CCR %in% creditosComunes) %>% select(CCR, CGR)
    
    alerta <- merge(BD01, BD03B, by.x = "CCR", by.y = "CCR") %>% 
      rowwise() %>%
      mutate(provisionTeorica = asignarProvisionCG(as.numeric(CAL), as.numeric(CGR), as.numeric(TCR)),
             provisionReal    = (as.numeric(PCI)/as.numeric(SKCR) *100) %>% round(2)) %>% 
      filter(provisionTeorica !=0, provisionTeorica > provisionReal) %>% 
      pull(CCR)
    
    return(alerta)
  }
  return("")
}
alert2020 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(NCR) > 3) %>% pull(CODGR) %>% return()
}
alert2021 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(dmy(FOT_C) == dmy(FCAN_C)) %>% pull(CCR_C) %>% return()
}
alert2022 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(NCPR_C) == as.numeric(NCPA_C)) %>% pull(CCR_C) %>% return()
}
alert2023 <- function(ruta, eb) {
  BD         <- quitarVaciosBD(ruta)
  saldosCols <- getColsNoObservadas(ruta, eb, "saldos")
  
  if (length(saldosCols) >0) {
    alertSaldos <- tibble(Columna = saldosCols) %>% rowwise() %>%
      mutate(verificarSaldos = BD %>% filter(as.numeric(cgrep(BD, Columna)[[1]]) <0) %>% pull(CCR) %>% toString()) %>%
      filter(verificarSaldos != "") %>% 
      pull(verificarSaldos)
    
    return(alertSaldos)
  }
  return("")
}
alert2024 <- function(periodo, agente) {
  
  creditosComunes <- getCreditosComunes(periodo, agente)
  
  if (length(creditosComunes)> 0) {
    
    alerta <-   quitarVaciosBD(getRuta(default.carpeta, paste(getCoopacFromAgent(agente), "BD01", periodo, sep = "_"))) %>%
      filter(CCR %in% creditosComunes & as.numeric(ESAM) < 5 & (as.numeric(NCPR) == 0 | as.numeric(PCUO)  == 0)) %>% 
      pull(CCR)
    
    return(alerta)
  }
  return("")
}
alert2025 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter((as.numeric(KVE) > 0 & as.numeric(DAK) == 0)) %>% pull (CCR) %>% return()
}
alert2026 <- function(ruta) {
  quitarVaciosBD(ruta) %>% filter(as.numeric(NCR) > 0, as.numeric(NRCL) == 0) %>% pull(CODGR) %>% unique() %>% return()
}