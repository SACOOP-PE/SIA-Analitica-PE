#' Funciones principales
#' layer2(agente, eb)

layer2 <- function(agente, eb){
  
eb <- validarOperacionesVacias(agente, eb)
eb <- validarOperacionesDuplicadas(agente, eb)
return(eb)
}

#' Funciones secundarias: nivel I
#' validarCuadreContable
#' validarOperacionesDUVA
#' validarCruceInterno
#' validarCampos

 
validarOperacionesVacias     <- function(agente, eb){
  carpeta   <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosNoObservadosByCols(agente, eb, c("CCR", "CCR_C", "CODGR"))
  
  vacios <- tibble(NombreArchivo = exigibles) %>% rowwise() %>% 
    mutate(ruta    = getRuta(carpeta, NombreArchivo),
           Vacios = getoperacionesVacias(ruta)) %>%
    filter(Vacios != 0)

  if (nrow(vacios) > 0) {
    chunk_311 <- vacios %>% rowwise() %>% 
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Cod = 311,
             Periodo = getAnoMesFromRuta(toString(ruta)),
             BD      = getBDFromRuta(toString(ruta)),
             num1 = Vacios) %>%  
      select(CodCoopac, IdProceso, Cod, Periodo, BD, num1)
    
    eb <- addErrorMasivo(eb, chunk_311)
  }
  
  n <- eb %>% filter(Cod %in% c(311)) %>% nrow()
  
  if (n == 0) {  
    addEventLog(agente, paste0("La validación de operaciones vacías concluyó sin observaciones. (~ly2)"), "I", "B") 
  }
  else{
    addEventLog(agente, paste0("La validación de operaciones vacías concluyó con ", n, " observaciones. (~ly2)"), "I", "B")
  }
  
  return(eb)
}
validarOperacionesDuplicadas <- function(agente, eb){
  carpeta   <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosNoObservadosByCols(agente, eb, c("CCR", "CCR_C", "CODGR"))
  
  dups <- tibble(NombreArchivo = exigibles) %>% rowwise() %>% 
    mutate(ruta       = getRuta(carpeta, NombreArchivo),
           Duplicados = getoperacionesDuplicadas(ruta)) %>%
    filter(Duplicados != "")
  
  if (nrow(dups) > 0) {
    chunk_312 <- dups %>% rowwise() %>%
      mutate(CodCoopac = getCoopacFromAgent(agente),
             IdProceso = getIdProcesoFromAgent(agente),
             Cod = 312,
             Periodo = getAnoMesFromRuta(toString(ruta)),
             BD      = getBDFromRuta(toString(ruta)),
             txt1 = Duplicados,
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)

    eb <- addErrorMasivo(eb, chunk_312)
  }
  
  n <- eb %>% filter(Cod %in% c(312)) %>% nrow()
  
  if (n == 0) {
    addEventLog(agente, paste0("La validación de operaciones duplicadas concluyó sin observaciones. (~ly2) "), "I", "B")
  }
  else{
    
    addEventLog(agente, paste0("La validación de operaciones duplicadas concluyó con ", n, " observación. (~ly2) "), "I", "B")
  }
  
  return(eb)
}

#validarOperacionesVacias #cambiar nombre
getoperacionesVacias <- function(ruta){
  BD <- evaluarFile(ruta)
  
  BD %>% 
    select(getCodigoBD(getBDFromRuta(ruta))[1]) %>%
    sapply(function(x) sum(is.na(x))) %>% return()
}

#validarOperacionesDuplicadas  #cambiar nombre
getoperacionesDuplicadas <- function(ruta){
  if (getBDFromRuta(ruta) == "BD01" | getBDFromRuta(ruta) == "BD03A") {
    operaciones <- evaluarFile(ruta) %>% select(getCodigoBD(getBDFromRuta(ruta))[1]) 
    duplicados  <- operaciones[duplicated(operaciones), ] %>% 
      unique() %>%
      pull(getCodigoBD(getBDFromRuta(ruta))[1]) %>%
      toString()
    return(duplicados)
    }
  else {
    return("")
    }  
}