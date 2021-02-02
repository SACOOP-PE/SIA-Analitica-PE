formatBucket <- function(eb) {
  
  output <- eb %>% 
    rowwise() %>% 
    mutate(num1 = pad2(mum1),
           Descripcion = if_else(Cod <= 102,
                                 str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                      "\\Q{1}"  = toString(txt1))),
                                 if_else(Cod >=201 & Cod <= 203,
                                         str_replace_all(getDescError(Cod), c("\\Q{0}"  = toString(num1),
                                                                              "\\Q{1}"  = txt1,
                                                                              "\\Q{2}"  = BD,
                                                                              "\\Q{3}"  = periodoEscrito(Periodo))),
                                         if_else(Cod >= 301 & Cod <= 310, 
                                                 str_replace_all(getDescError(Cod), c("\\Q{0}"  = months(as.Date(paste(substr(Periodo,1,4), substr(Periodo,5,6), "01", sep = "-"))),
                                                                                      "\\Q{1}"  = "")),
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
                                                                                                                      "\\Q{1}"  = txt3)),
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
                                 )
           ) %>% 
    select(Periodo, BD, Cod, Descripcion)
  
  return(output)
}

#Desc códigos
getDescError   <- function(CodError) {
  descr <- initRepositorioErrores() %>% filter(Cod == CodError) %>% pull(Descripcion)
  return(descr)
}
cutStringError <- function(num1, txt1) {
  if (num1 > 100) {
    txt1 <- str_split(txt1, ",") %>% unlist()
    
    txt1 <- paste0(toString(txt1[1:5]), " ...")
    return(txt1)
  }
  else{return(txt1)}
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

saveOutputs <- function(agente, eb, ebFormat) {
 
  agente %>% 
    writexl::write_xlsx(paste0(paste0(getwd(), "/test/output/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_")))
  
  #bucket ----
  eb %>% rowwise() %>%
    mutate(txt1 = if_else(Cod >102, 
                          cutStringError(num1, txt1),
                          txt1)
           ) %>% 
    writexl::write_xlsx(paste0(paste0(getwd(), "/test/output/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_")))
  #ebFormatted ----
  ebFormat %>%
    writexl::write_xlsx(paste0(paste0(getwd(), "/test/output/"),
                               paste(agente %>% pull(Coopac),
                                     getIdProcesoFromAgent(agente),
                                     paste0("(",agente %>% pull(PeriodoInicial),"-", agente %>% pull(PeriodoFinal),")"),
                                     sep = "_") ))
}