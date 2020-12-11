ejecutarDecteccionAlertBD02 <-function(header, listaErrores, alertBucket){
  alertaBD02A<- tibble(Periodo = restriccionPeriodos(listaErrores, "BD01", "BD02A", "CCR")) %>% rowwise() %>%
    mutate(alerta2025  = generarDetalleError4(Periodo, alertMontosuperiorOcupaciones2("BD02A", Periodo)),
           alerta2027  = generarDetalleError4(Periodo, alertMontOrtorgadoCronograma(Periodo)))
  
  alertaBD02B<- tibble(Periodo = restriccionPeriodos(listaErrores, "BD01", "BD02B", c("CCR","CCR_C"))) %>% rowwise() %>%
    mutate(alerta2026  = generarDetalleError4(Periodo,alertMontosuperiorOcupaciones2("BD02B", Periodo)))
  
  alert2025 <- (paste(alertaBD02A %>% rowwise() %>% pull(alerta2025) , collapse = ",") %>% strsplit(","))[[1]]
  alert2026 <- (paste(alertaBD02B %>% rowwise() %>% pull(alerta2026) , collapse = ",") %>% strsplit(","))[[1]]
  alert2027 <- (paste(alertaBD02A %>% rowwise() %>% pull(alerta2027) , collapse = ",") %>% strsplit(","))[[1]]
  
  if(length(alert2025[alert2025  != "character(0)"]) > 0){
    alertBucket <- alertBucket %>%
      addAlerta(2025, getResponAlerta(2025), getDescAlerta(2025), (alert2025[alert2025 != "character(0)"]) %>% toString())
  }
  if(length(alert2026[alert2026  != "character(0)"]) > 0){
    alertBucket <- alertBucket %>%
      addAlerta(2026, getResponAlerta(2026), getDescAlerta(2026), (alert2026[alert2025 != "character(0)"]) %>% toString())
  }
  if(length(alert2026[alert2027  != "character(0)"]) > 0){
    alertBucket <- alertBucket %>%
      addAlerta(2027, getResponAlerta(2027), getDescAlerta(2027), (alert2027[alert2027 != "character(0)"]) %>% toString())
  }
  
  print(paste0("Terminó la detectación de alertas BD02: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(alertBucket)
}