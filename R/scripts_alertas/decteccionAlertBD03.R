ejecutarDecteccionAlertBD03 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosSinErrores(header, listaErrores, c(201, 203), "CODGR")
  carpeta   <- getCarpeta(header)
  
  alertasBD03 <- tibble(NombreArchivo = exigibles[str_detect(exigibles,"BD03A")]) %>% rowwise() %>%
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           alerta2029 = generarDetalleError2(Ruta, alertGarantiasNumerocobertura(Ruta)))
  
    alert2029 <- (paste(alertasBD03 %>% rowwise() %>% pull(alerta2029), collapse = ",") %>% strsplit(","))[[1]]
    
    if(length(alert2029[alert2029 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2029, getResponAlerta(2029), getDescAlerta(2029), (alert2029[alert2029 != "character(0)"]) %>% toString())
    }
    
  print(paste0("Terminó la detección de alertas BD03: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(alertBucket)
}