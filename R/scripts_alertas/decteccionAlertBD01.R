ejecutarDecteccionAlertBD01 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosExigibles(header)
  carpeta   <- getCarpeta(header)
 
  #2001, 2002 
  exigiblesAlert1 <- getArchivosSinErrores(header, listaErrores, c(201,203), c("MORG", "UAGE"))
  exigiblesAlert1 <- exigiblesAlert1[str_detect(exigiblesAlert1, "BD01")]
  alertBucket_i   <- alertBucket
  for (i in 1:length(exigiblesAlert1)){
    ruta_i        <- getRuta(carpeta, exigiblesAlert1[i])
    alertBucket_i <- alertMontosuperiorAgencias(ruta_i, alertBucket_i)
  }
  alertBucket <- alertBucket_i %>% group_by(Coopac, NombCoopac, Carpeta, IdProceso, CodAlerta, Responsable, Descripcion) %>%
    summarise(Detalle = toString(Detalle)) %>%
    ungroup()
  
  return(alertBucket)
}