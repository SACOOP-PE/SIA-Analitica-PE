ejecutarDecteccionAlertBD01 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosSinErrores(header, listaErrores, c(201, 203), c("CCR","CCR_C","CODGR"))
  carpeta   <- getCarpeta(header)
  
  #2001, 2002
  exigiblesAlert1 <- getArchivosSinErrores(header, listaErrores, c(201,203), c("MORG","UAGE")) %>%
    intersect(exigibles)
  exigiblesAlert1 <- exigiblesAlert1[str_detect(exigiblesAlert1, "BD01")]
  alertBucket_i   <- alertBucket
  for (i in 1:length(exigiblesAlert1)){
    ruta_i        <- getRuta(carpeta, exigiblesAlert1[i])
    alertBucket_i <- alertMontosuperiorAgencias(ruta_i, alertBucket_i)
  }
  alertBucket <- alertBucket_i %>%
    group_by(Coopac, NombCoopac, Carpeta, IdProceso, CodAlerta, Responsable, Descripcion) %>%
    summarise(Detalle = toString(Detalle)) %>%
    ungroup()
  
  # 2003 ++
  alertasBD01 <- tibble(NombreArchivo = exigibles[str_detect(exigibles,"BD01")]) %>% rowwise() %>%
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           alerta2003 = generarDetalleError2(Ruta, alertMontosuperiorSector(Ruta)),
           alerta2004 = generarDetalleError2(Ruta, alertMontosuperiorOcupaciones(Ruta)),
           alerta2005 = generarDetalleError2(Ruta, alertTea(Ruta)),
           alerta2006 = generarDetalleError2(Ruta, alertDiasGracia(Ruta)),
           alerta2007 = generarDetalleError2(Ruta, alertMontOtorsuperiorCapitalVig(Ruta)),
           alerta2008 = generarDetalleError2(Ruta, alertRendimientoDevengado(Ruta)),
           alerta2009 = alertDeudorCal(Ruta),
           alerta2010 = generarDetalleError2(Ruta, alertDiasAtrasonegativo(Ruta)),
           alerta2011 = generarDetalleError2(Ruta, alertEsquemaAmortizaCuotaPagadas(Ruta)),
           alerta2012 = generarDetalleError2(Ruta, alertDeudorInteresDevengado(Ruta)),
           alerta2013 = generarDetalleError2(Ruta, alertCreditoInteresDevengado(Ruta)),
           alerta2014 = generarDetalleError2(Ruta, alertDeudorContableVencido(Ruta)),
           alerta2015 = generarDetalleError2(Ruta, alertCreditoContableVigente(Ruta)),
           alerta2016 = generarDetalleError2(Ruta, alertDiasAtrasoJudicial(Ruta)),
           alerta2017 = generarDetalleError2(Ruta, alertCreditoCobranzaJudicial(Ruta)),
           alerta2018 = generarDetalleError2(Ruta, alertCreditosUnicouta(Ruta)),
           alerta2019 = generarDetalleError2(Ruta, alertCreditosHipotecario(Ruta)))
  
  alert2003 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2003), collapse = ",") %>% strsplit(","))[[1]]
  alert2004 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2004), collapse = ",") %>% strsplit(","))[[1]]
  alert2005 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2005), collapse = ",") %>% strsplit(","))[[1]]
  alert2006 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2006), collapse = ",") %>% strsplit(","))[[1]]
  alert2007 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2007), collapse = ",") %>% strsplit(","))[[1]]
  alert2008 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2008), collapse = ",") %>% strsplit(","))[[1]]
  alert2009 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2009), collapse = ",") %>% strsplit(","))[[1]]
  alert2010 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2010), collapse = ",") %>% strsplit(","))[[1]]
  alert2011 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2011), collapse = ",") %>% strsplit(","))[[1]]
  alert2012 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2012), collapse = ",") %>% strsplit(","))[[1]]
  alert2013 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2013), collapse = ",") %>% strsplit(","))[[1]]
  alert2014 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2014), collapse = ",") %>% strsplit(","))[[1]]
  alert2015 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2015), collapse = ",") %>% strsplit(","))[[1]]
  alert2016 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2016), collapse = ",") %>% strsplit(","))[[1]]
  alert2017 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2017), collapse = ",") %>% strsplit(","))[[1]]
  alert2018 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2018), collapse = ",") %>% strsplit(","))[[1]]
  alert2019 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2019), collapse = ",") %>% strsplit(","))[[1]]
  
  listAlertBD01 <- list(alert2003, alert2004, alert2005, alert2006, alert2007, alert2008, alert2009, alert2010,
                        alert2011, alert2012, alert2013, alert2014, alert2015, alert2016, alert2017, alert2018, alert2019)
  
  codigoAlerta     <- 2003
  for (i in 1:length(listAlertBD01)){
    alertcodAlerta_i <- listAlertBD01[[i]]
    alerta_i         <- alertcodAlerta_i[alertcodAlerta_i != "character(0)"]

    if (length(alerta_i) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(codigoAlerta, getResponAlerta(codigoAlerta), getDescAlerta(codigoAlerta), (alerta_i) %>% toString())
    }
    codigoAlerta <- codigoAlerta + 1
  }
  alertBucket  <- alertBucket
  return(alertBucket)
}