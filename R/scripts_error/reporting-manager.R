formatBucket <- function(eb) {
    
  output <- eb %>% 
    rowwise() %>% 
    mutate(Descripcion = getDescError(Cod)) %>% 
    select(Cod, Descripcion)
  
  return(output)
}

getDescError    <- function(CodError){
  desc <- switch (toString(CodError),
                  "001" = paste0(descPartInicial(num1), " archivo(s) duplicados dentro de la ruta especificada. (",txt1,")"),
                  "002" = paste0(descPartInicial(num1), " archivo(s) faltantes dentro de la ruta especificada. (",txt1,")"),
                  "101" = paste0(descPartInicial(num1), " columna(s) faltantes en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (",txt1,")"),
                  "102" = paste0(descPartInicial(num1), " columna(s) sobrantes en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", txt1, ")"),
                  "103" = paste0(descPartInicial(num1), " columna(s) vacias en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", txt1, ")"),
                  "201" = paste0(descPartInicial(num1), " crédito(s)/garantía(s) vacias en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), "."),
                  "202" = paste0(descPartInicial(num1), " crédito(s)/garantía(s) duplicados en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "301" = paste0(descPartInicial(num1), " crédito(s) que no figuran en la cartera de créditos ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "302" = paste0(descPartInicial(num1), " crédito(s) que no figuran en los cronogramas ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "303" = paste0(descPartInicial(num1), " garantía(s) que no figuran en la base de datos de garantía entrega ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "401" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "402" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "403" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "404" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "405" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "406" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "407" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "408" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "409" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "410" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "411" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "412" = paste0(descPartInicial(num1), " garantia(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD03A", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "413" = paste0(descPartInicial(num1), " garantia(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD03A", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "414" = paste0(descPartInicial(num1), " garantia(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD03B", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "415" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "416" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "417" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "418" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "419" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "420" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "421" = paste0(descPartInicial(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "422" = paste0(descPartInicial(num1), " crédito(s) con errores en la longitud del documento de identidad según el tipo de documento en la ", BD ," correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "423" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de desembolso (FOT) en la BD01"," correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "424" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento general de la operación según cronograma (FVEG) en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "425" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento puntual de la operación (FVEP) en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "426" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento de la cuota (FVEP) en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "427" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de Vencimiento de la Cuota (FVEP_C) en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "428" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de Desembolso (FOT_C) en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", txt1, ")"),
                  "429" = paste0(descPartInicial(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de cancelación de la operación (FCAN_C) en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")"),
                  "430" = paste0(descPartInicial(num1), " crédito(s) con fecha de desembolso posterior a la fecha de corte en la cartera de créditos (BD01)", " correspondiente al periodo de ", periodoEscrito(Periodo), ". (", cutStringError(num1, txt1), ")")
                  ) %>% toString()
  
  return(desc)
}
descPartInicial <- function(num1){

partInicial <- ifelse(num1 == 1, 
                      paste0("Se identificó ", pad2(num1)),
                      paste0("Se identificaron ", pad2(num1)))

return(partInicial)
}
pad2            <- function(n) {
  return(str_pad(n, width = 2, side = "left", pad = "0"))
}
periodoEscrito  <- function(periodo) { 
  base <- tibble(MES = c(1:12),
                 ESCRITO = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","setiembre","octubre","noviembre", "diciembre"))
  m <- base %>% filter(pad2(MES) %in% substr(periodo,5,6)) %>% pull(ESCRITO)
  
  return(paste(m, "del", substr(periodo, 1, 4)))
}
cutStringError  <- function(num1, txt1) {
  if (num1 > 100) {
    txt1 <- str_split(txt1, ", ")[[1]]
    
    txt1 <- paste0(toString(txt1[1:5]), " ...")
    return(txt1)
  }
  else{return(txt1)}
}

saveOutputs <- function(agente, ebFormatt, pidlog) {
  #agent----
  agente %>% 
    writexl::write_xlsx(paste0(paste0(getwd(), "/test/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_"),
                               "_agente.xlsx"))
  
  #ebFormatted ----
  ebFormatt %>%
    writexl::write_xlsx(paste0(paste0(getwd(), "/test/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_"),
                               "_reportErrores.xlsx"))
  
  #pidlog----
  pidlog %>%
    write_delim(path = paste0(getwd(), "/logging/", "PID-", getIdProcesoFromAgent(agente), "_log.txt"), delim = "\t",
                col_names = T, append = T)
}

# bucket personalizado:
# library(reactable)
# library(crosstalk)

# reactable(
#   bucket %>% select(Periodo, Cod, BD, txt1, txt2, txt3, num1),
#   groupBy = "Periodo",
#   details = function(index) paste("Details for row:", index),
#   onClick = "expand",
# 
#   rowStyle = list(cursor = "pointer")
# )
# 
# 
# 
# bucket$names <- rownames(bucket)
# bucket  %>% 
#   scatterD3(x = Periodo, y = Cod, lab = BD,
#             col_var = BD,
#             xlab = "Año-Mes", ylab = "CodError")
  