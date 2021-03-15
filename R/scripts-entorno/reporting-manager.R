formatBucket <- function(eb) {
  
  output <- eb %>% rowwise() %>% 
    mutate(num1 = if_else(num1< 1000, pad2(num1), format(num1, big.mark = ",", small.interval = 3)),
           num2 = format(num2, big.mark = ",", small.interval = 3),
           Descripcion = replaceString(getDescFinal(Cod, Periodo, BD, txt1, txt2, txt3, num1, num2, num3), num1, Cod),
           Categoria   = getCateError(Cod),
           Criticidad  = getCritError(Cod),
           Tipo        = if_else(Cod <=709, "Error", "Alerta")) %>% 
    select(Periodo, BD, Cod, Tipo, Descripcion, Categoria, Criticidad) %>% 
    arrange(Periodo, Cod)
  
  return(output)
}

#Descr larga
getDescError   <- function(CodError) {
  initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Descripcion) %>% return()
}
getCateError   <- function(CodError) {
  initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Categoria) %>% return()
}
getCritError   <- function(CodError) {
  initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Criticidad) %>% return()
}

replaceString  <- function(desc, num1, cod) {
  if (num1 == "01" & cod %in% c(101,102,201:203,401:709)) {
    
    desc <- if_else(cod %in% c(201:203, 401:709),
                    str_replace(desc, "Se identificaron", "Se identificó"),
                    if_else(cod == 101,
                            str_replace(desc, "existen", "existe"),
                            if_else(cod == 102,
                                    str_replace(desc, "faltan", "falta"),
                                    desc)
                            )
                    )
    
    return(desc)
  }
  return(desc)
}
getDescFinal   <- function(Cod, Periodo, BD, txt1, txt2, txt3, num1, num2, num3) {
  
  if (Cod %in% c(101:102)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1), 
                                                 "\\Q{1}"  = txt1))
    return(desc)
  }
  if (Cod %in% c(201:203)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}" = toString(num1),
                                                 "\\Q{1}" = txt1,
                                                 "\\Q{2}" = BD,
                                                 "\\Q{3}" = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod %in% c(301:308)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}" = tolower(toString(months(as.Date(paste(substr(Periodo,1,4), substr(Periodo,5,6), "01", sep = "-"))))),
                                                 "\\Q{1}" = periodoEscrito(Periodo),
                                                 "\\Q{2}" = toString(num2)))
    return(desc)
  }
  if (Cod %in% c(401,402)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}" = toString(num1),
                                                 "\\Q{1}" = BD,
                                                 "\\Q{2}" = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod %in% c(403, 404)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                 "\\Q{1}"  = tolower(toString(months(as.Date(paste(substr(Periodo,1,4), substr(Periodo,5,6), "01", sep = "-"))))),
                                                 "\\Q{2}"  = toString(num2),
                                                 "\\Q{3}"  = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod %in% c(405, 501, 503, 506)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                 "\\Q{1}"  = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod %in% c(406, 502)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                 "\\Q{1}"  = toString(num2),
                                                 "\\Q{2}"  = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod == 407) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                 "\\Q{1}"  = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod %in% c(504,505)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                 "\\Q{1}"  = periodoEscrito(Periodo),
                                                 "\\Q{2}"  = periodoEscrito(txt2)))
    return(desc)
  }
  if (Cod %in% c(601:621)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                 "\\Q{1}"  = periodoEscrito(Periodo),
                                                 "\\Q{2}"  = txt3))
    return(desc)
  }
  if (Cod == 622) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}" = toString(num1),
                                                 "\\Q{1}" = txt2,
                                                 "\\Q{2}" = BD,
                                                 "\\Q{3}" = periodoEscrito(Periodo)))
    return(desc)
  }
  if (Cod %in% c(701:709)) {
    desc <- str_replace_all(getDescError(Cod), c("\\Q{0}" = toString(num1),
                                                 "\\Q{1}" = periodoEscrito(Periodo)))
    return(desc)
  }
  
}

#Descr periodos
pad2            <- function(n) {
  return(str_pad(n, width = 2, side = "left", pad = "0"))
}
periodoEscrito  <- function(periodo) { 
  base <- tibble(MES = c(1:12),
                 ESCRITO = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","setiembre","octubre","noviembre","diciembre"))
  m    <- base %>% filter(pad2(MES) %in% substr(periodo,5,6)) %>% pull(ESCRITO)
  
  return(paste(m, "del", substr(periodo, 1, 4)))
}

#Exportar formatBucket
saveOutputs <- function(agente, ebFormat) {
  list_of_datasets <- list("agente" = agente, "bucketOficio" = ebFormat)
  
  write.xlsx(list_of_datasets, 
             file = paste0(getwd(), "/test/output/resultados_", getIdProcesoFromAgent(agente), ".xlsx"))
}