##### 5. Función principal  -----
main <- function(header, eb){
  eb <- ejecutar_validacion_layer1(header, eb)
  if (restriccion_archivosFaltDups(eb)==1){
    header <- finalizarProceso(header,eb)
    saveResults(header,eb)
    return(eb)}
  eb <- ejecutar_validacion_layer2(header, eb)
  eb <- ejecutar_validacion_layer3(header, eb)
  eb <- ejecutar_validacion_layer4(header, eb)
  header <- finalizarProceso(header,eb)
  saveResults(header,eb)
  return(eb)
}