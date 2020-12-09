ejecutarDecteccionAlertBD01 <-function(header, listaErrores, alertBucket){
  exigibles <- getArchivosSinErrores(header, listaErrores, c(201, 203), c("CCR","CCR_C","CODGR"))
  carpeta   <- getCarpeta(header)
  
  tb_main <- tibble(NombreArchivo = exigibles) %>% rowwise() %>%
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           Periodo = getAnoMes(Ruta),
           BDCC    = getBD(Ruta))
 
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
  alertasBD01 <- tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
    mutate(alerta2003 = generarDetalleError2(Ruta, alertMontosuperiorSector(Ruta)),
           alerta2004 = generarDetalleError2(Ruta, alertMontosuperiorOcupaciones(Ruta)),
           alerta2005 = generarDetalleError2(Ruta, alertTea(Ruta)),
           alerta2006 = generarDetalleError2(Ruta, alertDiasGracia(Ruta)),
           alerta2007 = generarDetalleError2(Ruta, alertMontOtorsuperiorCapitalVig(Ruta)))
  
  alert2003 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2003), collapse = ",") %>% strsplit(","))[[1]]
  alert2004 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2004), collapse = ",") %>% strsplit(","))[[1]]
  alert2005 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2005), collapse = ",") %>% strsplit(","))[[1]]
  alert2006 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2006), collapse = ",") %>% strsplit(","))[[1]]
  alert2007 <- (paste(alertasBD01 %>% rowwise() %>% pull(alerta2007), collapse = ",") %>% strsplit(","))[[1]]
  
    if (length(alert2003[alert2003 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2003, getResponAlerta(2003), getDescAlerta(2003), (alert2003[alert2003 != "character(0)"]) %>% toString())
     }
    if (length(alert2004[alert2004 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2004, getResponAlerta(2004), getDescAlerta(2004), (alert2004[alert2004 != "character(0)"]) %>% toString())
      }
    if (length(alert2005[alert2005 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2005, getResponAlerta(2005), getDescAlerta(2005), (alert2005[alert2005 != "character(0)"]) %>% toString())
      }
    if (length(alert2006[alert2006 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2006, getResponAlerta(2006), getDescAlerta(2006), (alert2003[alert2006 != "character(0)"]) %>% toString())
      }
    if (length(alert2007[alert2007 != "character(0)"]) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(2007, getResponAlerta(2007), getDescAlerta(2007), (alert2007[alert2007 != "character(0)"]) %>% toString())
      }
  
  return(alertBucket)
}