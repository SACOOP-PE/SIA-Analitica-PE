ejecutarDecteccionAlertBD02 <-function(header, listaErrores, alertBucket){
  alert2025  = paste(procesarAlertas2(2025), sep= ",")
  alert2026  = paste(procesarAlertas2(2026), sep= ",")
  alert2027  = paste(procesarAlertas2(2027), sep= ",")
  
  if(length(alert2025[alert2025 != "character(0)"]) > 0){
    alertBucket <- alertBucket %>%
      addAlerta(2025, getResponAlerta(2025), getDescAlerta(2025), (alert2025[alert2025 != "character(0)"]) %>% toString())
  }
  if(length(alert2026[alert2026 != "character(0)"]) > 0){
    alertBucket <- alertBucket %>%
      addAlerta(2026, getResponAlerta(2026), getDescAlerta(2026), (alert2026[alert2025 != "character(0)"]) %>% toString())
  }
  if(length(alert2026[alert2027 != "character(0)"]) > 0){
    alertBucket <- alertBucket %>%
      addAlerta(2027, getResponAlerta(2027), getDescAlerta(2027), (alert2027[alert2027 != "character(0)"]) %>% toString())
  }
  
  print(paste0("Terminó la detectación de alertas BD02: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(alertBucket)
}