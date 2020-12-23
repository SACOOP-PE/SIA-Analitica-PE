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
  alert2003 <- (paste(procesarAlertas(exigibles, "BD01", 2003) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2004 <- (paste(procesarAlertas(exigibles, "BD01", 2004) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2005 <- (paste(procesarAlertas(exigibles, "BD01", 2005) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2006 <- (paste(procesarAlertas(exigibles, "BD01", 2006) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2007 <- (paste(procesarAlertas(exigibles, "BD01", 2007) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2008 <- (paste(procesarAlertas(exigibles, "BD01", 2008) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2009 <- (paste(procesarAlertas(exigibles, "BD01", 2009) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2010 <- (paste(procesarAlertas(exigibles, "BD01", 2010) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2011 <- (paste(procesarAlertas(exigibles, "BD01", 2011) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2012 <- (paste(procesarAlertas(exigibles, "BD01", 2012) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2013 <- (paste(procesarAlertas(exigibles, "BD01", 2013) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2014 <- (paste(procesarAlertas(exigibles, "BD01", 2014) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2015 <- (paste(procesarAlertas(exigibles, "BD01", 2015) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2016 <- (paste(procesarAlertas(exigibles, "BD01", 2016) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2017 <- (paste(procesarAlertas(exigibles, "BD01", 2017) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2018 <- (paste(procesarAlertas(exigibles, "BD01", 2018) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2019 <- (paste(procesarAlertas(exigibles, "BD01", 2019) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2020 <- (paste(procesarAlertas(exigibles, "BD01", 2020) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  alert2022 <- (paste(procesarAlertas(exigibles, "BD01", 2022) %>% rowwise() %>% pull(Alerta), collapse = ",") %>% strsplit(","))[[1]]
  
  listAlertBD01 <- list(alert2003, alert2004, alert2005, alert2006, alert2007, alert2008, alert2009, alert2010, alert2011,
                        alert2012, alert2013, alert2014, alert2015, alert2016, alert2017, alert2018, alert2019, alert2020,
                        alert2022)
  codigoAlerta  <- 2003
  
  for (i in 1:length(listAlertBD01)){
    alertcodAlerta_i <- listAlertBD01[[i]]
    alerta_i         <- alertcodAlerta_i[alertcodAlerta_i != "character(0)"]

    if(length(alerta_i) > 0){
      alertBucket <- alertBucket %>%
        addAlerta(codigoAlerta, getResponAlerta(codigoAlerta), getDescAlerta(codigoAlerta), (alerta_i) %>% toString())
      }
    
    codigoAlerta <- codigoAlerta + 1
    if(codigoAlerta == 2021){
      codigoAlerta <- codigoAlerta + 1
      }
  }
  alertBucket  <- alertBucket

  print(paste0("Terminó la detectación de alertas BD01: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(alertBucket)
}