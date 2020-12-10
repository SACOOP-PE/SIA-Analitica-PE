ejecutarDecteccionAlertBD04 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosSinErrores(header, listaErrores, c(201, 203), "CCR_C")
  carpeta   <- getCarpeta(header)
  
  alertasBD04 <- tibble(NombreArchivo = exigibles[str_detect(exigibles,"BD04")]) %>% rowwise() %>%
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           alerta2030 = generarDetalleError2(Ruta, alertCreditosEfectivo(Ruta)),
           alerta2031 = generarDetalleError2(Ruta, alertCreditosAntesDesembolso(Ruta)),
           alerta2033 = generarDetalleError2(Ruta, alertFechaDesembolsoCancelacion(Ruta)),
           alerta2034 = generarDetalleError2(Ruta, alertNumeroCanceladosyOriginales(Ruta)))
  
  alert2030 <- (paste(alertasBD04 %>% rowwise() %>% pull(alerta2030) , collapse = ",") %>% strsplit(","))[[1]]
  alert2031 <- (paste(alertasBD04 %>% rowwise() %>% pull(alerta2031) , collapse = ",") %>% strsplit(","))[[1]]
  alert2032 <- (tibble(Periodo = restriccionPeriodos(listaErrores, "BD01", "BD04", c("CCR","CCR_C"))) %>% rowwise() %>%
                  mutate(alerta2032  = alertMontosuperiorOcupaciones3(Periodo)) %>% rowwise() %>%
                  pull(alerta2032) %>% 
                  paste(collapse = ",") %>%
                  strsplit(","))[[1]]
  alert2033 <- (paste(alertasBD04 %>% rowwise() %>% pull(alerta2033) , collapse = ",") %>% strsplit(","))[[1]]
  alert2034 <- (paste(alertasBD04 %>% rowwise() %>% pull(alerta2034) , collapse = ",") %>% strsplit(","))[[1]]
  
    if(length(alert2030[alert2030 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2030, getResponAlerta(2030), getDescAlerta(2030), (alert2030[alert2030 != "character(0)"]) %>% toString())
    }
    if(length(alert2031[alert2031 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2031, getResponAlerta(2031), getDescAlerta(2031), (alert2031[alert2031 != "character(0)"]) %>% toString())
    }
    if(length(alert2032[alert2032 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2032, getResponAlerta(2032), getDescAlerta(2032), (alert2032[alert2032 != "character(0)"]) %>% toString())
    }
    if(length(alert2033[alert2033 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2033, getResponAlerta(2033), getDescAlerta(2033), (alert2033[alert2033 != "character(0)"]) %>% toString())
    }
    if(length(alert2034[alert2034 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2034, getResponAlerta(2034), getDescAlerta(2034), (alert2034[alert2034 != "character(0)"]) %>% toString())
    }
  
  return(alertBucket)
}