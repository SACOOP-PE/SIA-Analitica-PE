formatBucket <- function(eb) {
    
  output <- eb %>% 
    rowwise() %>% 
    mutate(Descripcion = switch (toString(Cod),
                                 "101" = paste0("Se identificaron ", pad2(num1)," archivo(s) duplicados dentro de la ruta especificada. (",txt1,")"),
                                 "102" = paste0("Se identificaron ", pad2(num1)," archivo(s) faltantes dentro de la ruta especificada. (",txt1,")"),
                                 "201" = paste0("Se identificaron ", pad2(num1), " columna(s) faltantes en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(",txt1,")"),
                                 "202" = paste0("Se identificaron ", pad2(num1), " columna(s) sobrantes en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt1, ")"),
                                 "203" = paste0("Se identificaron ", pad2(num1), " columna(s) vacias en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt1, ")"),
                                 "311" = paste0("Se identificaron ", pad2(num1), " crédito(s)/garantía(s) vacias en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), "."),
                                 "312" = paste0("Se identificaron ", pad2(num1), " crédito(s)/garantía(s) duplicados en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "321" = paste0("Se identificaron ", pad2(num1), " crédito(s) que no figuran en la cartera de créditos ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "322" = paste0("Se identificaron ", pad2(num1), " crédito(s) que no figuran en los cronogramas ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "323" = paste0("Se identificaron ", pad2(num1), " garantía(s) que no figuran en la base de datos de garantía entrega ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "401" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "402" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "403" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "404" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "405" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "406" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "407" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "411" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "412" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "421" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "422" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "431" = paste0("Se identificaron ", pad2(num1), " garantia(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD03A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "432" = paste0("Se identificaron ", pad2(num1), " garantia(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD03A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "441" = paste0("Se identificaron ", pad2(num1), " garantia(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD03B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "451" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "452" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "453" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "454" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "455" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "456" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "457" = paste0("Se identificaron ", pad2(num1), " crédito(s) con algunos dígitos diferentes o vacíos en la columna ", txt2, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "461" = paste0("Se identificaron ", pad2(num1), " crédito(s) con saldos negativos en la columna ", txt2, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "462" = paste0("Se identificaron ", pad2(num1), " crédito(s) con modalidad de coutas sin periocidad o número de cuotas pactas igual a 0 en la BD01 correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "463" = paste0("Se identificaron ", pad2(num1), " crédito(s) con MORG menor que el saldo de colocaciones en la BD01 correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "464" = paste0("Se identificaron ", pad2(num1), " crédito(s) en situación contable vencidos con 0 días de atraso en la BD01 correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "465" = paste0("Se identificaron ", pad2(num1), " crédito(s) con errores en la longitud del documento de identidad según el tipo de documento en la ", BD ," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "466" = paste0("Se identificaron ", pad2(num1), " garantía(s) donde el número de créditos que cobertura la garantía (NCR) no está vinculado al menos a un deudor en la BD03A"," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "471" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de desembolso (FOT) en la BD01"," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "472" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento general de la operación según cronograma (FVEG) en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "473" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento puntual de la operación (FVEP) en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "474" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento de la cuota (FVEP) en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "475" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de Vencimiento de la Cuota (FVEP_C) en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "476" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de Desembolso (FOT_C) en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt1, ")"),
                                 "477" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fechas vacías o erróneas en la Fecha de cancelación de la operación (FCAN_C) en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")"),
                                 "478" = paste0("Se identificaron ", pad2(num1), " crédito(s) con fecha de desembolso posterior a la fecha de corte en la cartera de créditos (BD01)", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", cutStringError(num1, txt1), ")")
                                 ) %>% toString()) %>% 
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
cutStringError <- function(num1, txt1) {
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
  