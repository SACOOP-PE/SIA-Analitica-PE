##### 5. Función principal  -----
main <- function(header, eb){
  eb <- ejecutarValidacionLayer1(header, eb)
  if (restriccionArchivosFaltDups(eb)==1){
    header <- finalizarProceso(header,eb)
    saveResults(header,eb)
    return(eb)
    }
  eb <- ejecutarValidacionLayer2(header, eb)
  eb <- ejecutarValidacionLayer3(header, eb)
  eb <- ejecutarValidacionLayer4(header, eb)
  header <- finalizarProceso(header,eb)
  saveResults(header,eb)
  return(eb)
}