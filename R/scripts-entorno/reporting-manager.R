formatBucket <- function(eb) {
  
  output <- eb %>% rowwise() %>% 
    mutate(num1 = if_else(num1< 1000, 
                          pad2(num1), 
                          format(num1, big.mark = ",", small.interval = 3)),
           num2 = format(num2, big.mark = ",", small.interval = 3),
           Descripcion = replaceString(if_else(Cod <= 102,
                                               str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                    "\\Q{1}"  = txt1)),
                                               if_else(Cod %in% c(201:203),
                                                       str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                            "\\Q{1}"  = txt1,
                                                                                            "\\Q{2}"  = BD,
                                                                                            "\\Q{3}"  = periodoEscrito(Periodo))),
                                                       if_else(Cod %in% c(301:308), 
                                                               str_replace_all(getDescError(Cod), c("\\Q{0}"  = tolower(toString(months(as.Date(paste(substr(Periodo,1,4), substr(Periodo,5,6), "01", sep = "-"))))),
                                                                                                    "\\Q{1}"  = periodoEscrito(Periodo),
                                                                                                    "\\Q{2}"  = toString(num2))),
                                                               if_else(Cod == 401 | Cod == 402,
                                                                       str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                            "\\Q{1}"  = tolower(toString(months(as.Date(paste(substr(Periodo,1,4), substr(Periodo,5,6), "01", sep = "-"))))),
                                                                                                            "\\Q{2}"  = toString(num2),
                                                                                                            "\\Q{3}"  = periodoEscrito(Periodo))),
                                                                       if_else(Cod == 403 | Cod == 501 | Cod == 503,
                                                                               str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                                    "\\Q{1}"  = periodoEscrito(Periodo))),
                                                                               if_else(Cod == 404 | Cod == 502,
                                                                                       str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                                            "\\Q{1}"  = toString(num2),
                                                                                                                            "\\Q{2}"  = periodoEscrito(Periodo))),
                                                                                       if_else(Cod == 405,
                                                                                               str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                                                    "\\Q{1}"  = txt2)),
                                                                                               if_else(Cod >= 601 & Cod <= 621,
                                                                                                       str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                                                            "\\Q{1}"  = periodoEscrito(Periodo),
                                                                                                                                            "\\Q{2}"  = txt3)),
                                                                                                       if_else(Cod == 622,
                                                                                                               str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                                                                    "\\Q{1}"  = txt2,
                                                                                                                                                    "\\Q{2}"  = BD,
                                                                                                                                                    "\\Q{3}"  = periodoEscrito(Periodo))),
                                                                                                 
                                                                                                               if_else(Cod >= 701,
                                                                                                                       str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                                                                                                            "\\Q{1}"  = periodoEscrito(Periodo))),
                                                                                                                       ""
                                                                                                                       )
                                                                                                              )
                                                                                                       )
                                                                                              )
                                                                                      )
                                                                               )
                                                                       )
                                                               )
                                                       )
                                               ), num1, Cod),
           Categoria  = getCateError(Cod),
           Criticidad = getCritError(Cod),
           Tipo       = "Error"
           ) %>% 
    select(Periodo, BD, Cod, Tipo, Descripcion, Categoria, Criticidad) %>% 
    arrange(Periodo, Cod)
  
  return(output)
}

#Descr larga
getDescError   <- function(CodError) {
  descr <- initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Descripcion)
  return(descr)
}
getCateError   <- function(CodError) {
  categoria <- initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Categoria)
  return(categoria)
}
getCritError   <- function(CodError) {
  criticidad <- initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Criticidad)
  return(criticidad)
}

cutStringError <- function(num1, txt1) {
  if (num1 > 100) {
    txt1 <- str_split(txt1, ",") %>% unlist()
    
    txt1 <- paste0(toString(txt1[1:5]), " ...")
    return(txt1)
  }
  else{return(txt1)}
}
replaceString  <- function(desc, num1, cod) {
  desc <- if_else(num1 == "01" & (cod %in% c(301:308) == FALSE), 
                  str_replace(desc, "Se detectaron", "Se detectó"),
                  desc)
  return(desc)
}

#Descr periodos
pad2            <- function(n) {
  return(str_pad(n, width = 2, side = "left", pad = "0"))
}
periodoEscrito  <- function(periodo) { 
  base <- tibble(MES = c(1:12),
                 ESCRITO = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","setiembre","octubre","noviembre", "diciembre"))
  m <- base %>% filter(pad2(MES) %in% substr(periodo,5,6)) %>% pull(ESCRITO)
  
  return(paste(m, "del", substr(periodo, 1, 4)))
}


#Exportar reportes
saveOutputs <- function(agente, ebFormat) {
  list_of_datasets <- list("agente" = agente, "bucketOficio" = ebFormat)
  
  write.xlsx(list_of_datasets, 
             file = paste0(paste0(getwd(), "/test/output/"),
                           paste0("resultados_",
                                  getIdProcesoFromAgent(agente),
                           ".xlsx")))
}