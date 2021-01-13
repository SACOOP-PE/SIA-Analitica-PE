ejecutarDecteccionAlertBD01 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosSinErrores(header, listaErrores, c(201, 203), c("CCR","CCR_C"))
  carpeta   <- getCarpeta(header)
  
  #2001, 2002
  exigiblesAlert1 <- getArchivosSinErrores(header, listaErrores, c(201,203), c("MORG","UAGE")) %>%
    intersect(exigibles[str_detect(exigibles,"BD01")])
  alertBucket_i   <- alertBucket
  for (i in 1:length(exigiblesAlert1)){
    ruta_i        <- getRuta(carpeta, exigiblesAlert1[i])
    alertBucket_i <- alertMontosuperiorAgencias(ruta_i, alertBucket_i)
  }
  alertBucket <- alertBucket_i %>%
    group_by(Coopac, NombCoopac, Carpeta, IdProceso, CodAlerta, Responsable, Descripcion) %>%
    summarise(Detalle = toString(Detalle)) %>%
    ungroup()
  
  # 2003+
  codigoAlerta  <- 2003
  
  for (i in 1:19){
    alert_i <- paste(procesarAlertas(exigibles, "BD01", codigoAlerta), sep= ",")
    alert_i <- alert_i[alert_i != "character(0)"]
    
    if(length(alert_i) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(codigoAlerta, getResponAlerta(codigoAlerta), getDescAlerta(codigoAlerta), (alert_i) %>% toString())
      }
    
    codigoAlerta <- codigoAlerta + 1
    if(codigoAlerta == 2021){
      codigoAlerta <- codigoAlerta + 1
      }
  }
  alertBucket  <- alertBucket

  print(paste0("Terminó la detección de alertas BD01: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(alertBucket)
}