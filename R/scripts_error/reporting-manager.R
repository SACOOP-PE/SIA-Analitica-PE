formatBucket <- function(eb) {
    
  output <- eb %>% 
    rowwise() %>% 
    mutate(Descripcion = switch (toString(Cod),
                                 "101" = paste0("Se identificaron ", pad2(num1)," archivo(s) duplicados dentro de la ruta especificada. (",txt1,")"),
                                 "102" = paste0("Se identificaron ", pad2(num1)," archivo(s) faltantes dentro de la ruta especificada. (",txt1,")"),
                                 "201" = paste0("Se identificaron ", pad2(num1), " columna(s) faltantes en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(",txt1,")"),
                                 "202" = paste0("Se identificaron ", pad2(num1), " columna(s) sobrantes en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt1, ")"),
                                 "203" = paste0("Se identificaron ", pad2(num1), " columna(s) vacias en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt1, ")"),
                                 "311" = paste0("Se identificaron ", pad2(num2), " crédito(s)/garantía(s) vacias en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "312" = paste0("Se identificaron ", pad2(num2), " crédito(s)/garantía(s) duplicados en la ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "321" = paste0("Se identificaron ", pad2(num2), " crédito(s) que no figuran en la cartera de créditos ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "322" = paste0("Se identificaron ", pad2(num2), " crédito(s) que no figuran en los cronogramas ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "323" = paste0("Se identificaron ", pad2(num2), " garantía(s) que no figuran en la base de datos de garantía entrega ", BD, " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "401" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "402" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "403" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "404" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "405" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "406" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "407" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "411" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "412" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "421" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "422" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "431" = paste0("Se identificaron ", pad2(num2), " garantia(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD03A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "432" = paste0("Se identificaron ", pad2(num2), " garantia(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD03A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "441" = paste0("Se identificaron ", pad2(num2), " garantia(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD03B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "451" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "452" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "453" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "454" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "455" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "456" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "457" = paste0("Se identificaron ", pad2(num2), " crédito(s) con algunos dígitos diferentes o vacíos en la columna", txt3, " en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "461" = paste0("Se identificaron ", pad2(num2), " crédito(s) con saldos negativos en la columna", txt3, " en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "462" = paste0("Se identificaron ", pad2(num2), " crédito(s) con modalidad de coutas sin periocidad o número de cuotas pactas igual a 0 en la BD01 correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "463" = paste0("Se identificaron ", pad2(num2), " crédito(s) con MORG menor que el saldo de colocaciones en la BD01 correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "464" = paste0("Se identificaron ", pad2(num2), " crédito(s) en situación contable vencidos con 0 días de atraso en la BD01 correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "465" = paste0("Se identificaron ", pad2(num2), " crédito(s) con errores en la longitud del documento de identidad según el tipo de documento en la ", BD ," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "466" = paste0("Se identificaron ", pad2(num2), " garantía(s) donde el número de créditos que cobertura la garantía (NCR) no está vinculado al menos a un deudor en la BD03A"," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "467" = paste0("Se identificaron ", pad2(num2), " garantía(s) con uno o más Códigos de Deudor (CIS) diferentes a los registrados en la BD01"," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "471" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de desembolso (FOT) en la BD01"," correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "472" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento general de la operación según cronograma (FVEG) en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "473" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento puntual de la operación (FVEP) en la BD01", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "474" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de vencimiento de la cuota (FVEP) en la BD02A", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "475" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de Vencimiento de la Cuota (FVEP_C) en la BD02B", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "476" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de Desembolso (FOT_C) en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "477" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fechas vacías o erróneas en la Fecha de cancelación de la operación (FCAN_C) en la BD04", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")"),
                                 "478" = paste0("Se identificaron ", pad2(num2), " crédito(s) con fecha de desembolso posterior a la fecha de corte en la cartera de créditos (BD01)", " correspondiente al periodo de ", periodoEscrito(Periodo), ".(", txt2, ")")
                                 )) %>% 
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

saveOutputs <- function(agente, ebFormatt, pidlog) {
  #agent----
  agente %>% 
    writexl::write_xlsx(paste0(paste(getwd(), "test/", sep = "/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_"),
                               "_agente.xlsx"))
  
  #ebFormatted ----
  ebFormatt %>%
    writexl::write_xlsx(paste0(paste(getwd(), "test/", sep = "/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_"),
                               "_reportErrores.xlsx"))
  
  #log----
  pidlog %>%
    write_delim(paste0(getwd(), 
                       "/logging/",
                       "PID-", 
                       getIdProcesoFromAgent(agente), 
                       ".log",
                       ".txt"),
                delim = "\t")
}