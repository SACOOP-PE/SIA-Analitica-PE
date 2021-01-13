ejecutarDecteccionAlertBD04 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosSinErrores(header, listaErrores, c(201, 203), "CCR_C")
  
  alert2030 <- paste(procesarAlertas(exigibles, "BD04", 2030), sep= ",")
  alert2031 <- paste(procesarAlertas(exigibles, "BD04", 2031), sep= ",")
  alert2032 <- paste(procesarAlertas(exigibles, "BD04", 2032), sep= ",")
  alert2033 <- paste(procesarAlertas(exigibles, "BD04", 2033), sep= ",")
  alert2034 <- paste(procesarAlertas(exigibles, "BD04", 2034), sep= ",")
  
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
  
  print(paste0("Terminó la detección de alertas BD04: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(alertBucket)
}