formatBucket <- function(bucket) {
  
  output <- bucket %>% 
    rowwise() %>% 
    mutate(Descripcion = case_when(Cod == 101 ~ paste0("Se identificaron ", pad2(num1), " archivo(s) duplicados dentro de la ruta especificada. (",txt1,")"),
                               Cod == 102 ~ paste0("Se identificaron ", pad2(num1)," archivo(s) faltantes dentro de la ruta especificada. (",txt1,")"),
                               Cod == 201 ~ paste0("Se identificaron ", pad2(num1), " columna(s) faltantes en la ", BD, " correspondiente al periodo de ", Periodo, ".(",txt1,")"),
                               Cod == 202 ~ paste0("Se identificaron ", pad2(num1), " columna(s) sobrantes en la ", BD, " correspondiente al periodo de ", Periodo, ".(", txt1, ")"),
                               Cod == 203 ~ paste0("Se identificaron ", pad2(num1), " columna(s) sobrantes en la ", BD, " correspondiente al periodo de ", Periodo, ".(", txt1, ")"))) %>% 
    select(Cod, Descripcion)
  
  return(output)
}

pad2           <- function(n) {
  return(str_pad(n, width = 2, side = "left", pad = "0"))
}
periodoEscrito <- function(periodo) { 
  base <- tibble(MES = c(1:12),
                 ESCRITO = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","setiembre","octubre","noviembre", "diciembre"))
  m <- base %>% filter(pad2(MES) %in% substr(periodo,5,6)) %>% pull(ESCRITO)
  
  return(paste(m, "del", substr(periodo, 1, 4)))
}