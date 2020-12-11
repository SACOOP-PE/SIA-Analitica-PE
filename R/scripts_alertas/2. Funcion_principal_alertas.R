main2 <- function(header, ab){
  ab <- ejecutarDecteccionAlertBD01(header, listaErrores, ab)
  ab <- ejecutarDecteccionAlertBD02(header, listaErrores, ab)
  ab <- ejecutarDecteccionAlertBD03(header, listaErrores, ab)
  ab <- ejecutarDecteccionAlertBD04(header, listaErrores, ab)
  return(ab)
}