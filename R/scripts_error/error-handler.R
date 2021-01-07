## Gestion de errores----
addError     <- function(errorBucket, codigoError, DescripcionError, DetalleError){ 
  rbind(errorBucket, tibble(Coopac     = errorBucket %>% pull(Coopac) %>% first(),
                            NombCoopac = errorBucket %>% pull(NombCoopac) %>% first(),
                            Carpeta    = errorBucket %>% pull(Carpeta) %>% first(),
                            IdProceso  = errorBucket %>% pull(IdProceso) %>% first(),
                            Cod         = codigoError, 
                            Descripcion = DescripcionError,
                            Detalle     = list(DetalleError))) %>%
    deleteError(100) %>% return()
}

getDescError <- function(codigoError){
  if ((length(initRepositorioErrores() %>% filter(Cod == codigoError) %>% pull(Descripcion)) == 0)){
    return("Descripción del error no encontrada")}

  initRepositorioErrores() %>% filter(Cod == codigoError) %>% pull(Descripcion) %>% first() %>% return()
}

deleteError  <- function(errorBucket, codigoError){
  errorBucket %>% filter(Cod != codigoError) %>% return()
}

## Restricciones archivos-columnas a partir de los errores del Layer1----

#layer 2 validarCruceInterno (BD01/BD02A, BD03A/BD03B)
getArchivosError      <- function(agente, errorBucket, codError, col){
  detalleError  <- unlist(errorBucket %>% filter(Cod %in% codError) %>% pull(Detalle) %>% str_split(","))
  
  archivosError <- str_extract(detalleError[str_detect(detalleError, paste(col, collapse = '|'))],
                               paste(getArchivosExigibles(agente), collapse = '|'))
  return(archivosError)
}
getArchivosSinErrores <- function(agente, errorBucket, codError, col){
  if (identical(getArchivosError(agente, errorBucket, c(201, 203), "CCR"),
                getArchivosError(agente, errorBucket, c(201, 203), "CCRF")) == TRUE){
    
    archivos  <- setdiff(getArchivosExigibles(agente), 
                         getArchivosError(agent, errorBucket, codError, col)) %>% 
      union(getArchivosError(agent, errorBucket, c(201, 203), "CCRF")) %>%
      unique()
    
    #ordenar archivos
    archivos <- intersect(getArchivosExigibles(agente), archivos) %>% unique()
    return(archivos)
  }
  else{
    archivos <- setdiff(getArchivosExigibles(agente),
                        getArchivosError(agente, errorBucket, codError, col))
    return(archivos) 
  }
}

restriccionPeriodos   <- function(errorBucket, BD1, BD2, colCruce){
  
  filtrarArchivos <- getArchivosSinErrores(agent, errorBucket, c(201, 203), colCruce)
  archivosCruce   <- filtrarArchivos[str_detect(filtrarArchivos, paste(c(BD1, BD2), collapse = '|'))]
  
  filtrarPeriodos <- tibble(Periodos =  str_extract(archivosCruce, paste(as.character(alcanceGeneral),collapse = '|'))) %>%
    group_by(Periodos) %>%
    filter(n() ==2) %>%
    pull(Periodos) %>% 
    unique() 
  
  return(filtrarPeriodos)
}

#layer 2 - validarCampos (tipo 1, 2, 3)
filtrarColsSaldos <- function(ruta, saldos, errorBucket){
  filterError <- unlist(errorBucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>% str_split(","))
  
  saldos <- setdiff(saldos,
                    str_extract(filterError[str_detect(filterError, getNombreArchivo(ruta))],
                                paste(unlist(getColumnasOM("BD01")), collapse = '|'))
                    )
  return(saldos)  
}

filtrarColsErrorT1  <- function(ruta, errorBucket){
  filterError <- unlist(errorBucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>% str_split(","))
  
  cols <- setdiff(getColsErrorT1(ruta),
                  str_extract(filterError[str_detect(filterError, getNombreArchivo(ruta))],
                              paste(getColsErrorT1(ruta), collapse = '|'))
                  )
  return(cols)
}
filtrarColsErrorT3  <- function(ruta, errorBucket){
  filterError <- unlist(errorBucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>% str_split(","))
  
  cols <- setdiff(getColsErrorT3(ruta),
                  str_extract(filterError[str_detect(filterError, getNombreArchivo(ruta))],
                              paste(getColsErrorT3(ruta), collapse = '|'))
  )
  return(cols)
}

filtrarArchivosErrorT1_T3 <- function(agente, errorBucket, exigibles, tipoError){
  archivos <- tibble(NombreArchivo = exigibles) %>% 
    rowwise() %>%
    mutate(nColsFiltradas = switch (tipoError,
                                    tipo1 = filtrarColsErrorT1(getRuta(getCarpeta(agente), NombreArchivo), errorBucket),
                                    tipo3 = filtrarColsErrorT3(getRuta(getCarpeta(agente), NombreArchivo), errorBucket)
                                    ) %>% length()
           ) %>%
    filter(nColsFiltradas > 0) %>%
    pull(NombreArchivo)
  
  return(archivos)
}
filtrarArchivosErrorT2    <- function(agente, errorBucket, exigibles, codigoError){
  if (codigoError >= 462 & codigoError <= 464) {
    archivos <- switch (toString(codigoError),
                        "462"= getArchivosSinErrores(agente, errorBucket, c(201, 203), c("ESAM","NCPR", "PCUO")),
                        "463"= getArchivosSinErrores(agente, errorBucket, c(201, 203), c("MORG", "SKCR")),
                        "464"= getArchivosSinErrores(agente, errorBucket, c(201, 203), c("KVE", "DAK", "KJU"))) %>% 
      intersect(exigibles[str_detect(exigibles, "BD01")])
    return(archivos)
  }
  if (codigoError == 465){
    archivos <- getArchivosSinErrores(agente, errorBucket, c(201, 203), c("TID", "NID", "TID_C", "NID_C")) %>% 
                      intersect(exigibles[str_detect(exigibles,
                                                     paste(c("BD01","BD04"), collapse = '|'))])
    return(archivos)
  }
  if (codigoError == 466){
    archivos <- getArchivosSinErrores(agente, errorBucket, c(201, 203), c("NCR", "NRCL")) %>% 
      intersect(exigibles[str_detect(exigibles, "BD03A")])
    return(archivos)
  }
  if (codigoError == 479){
    archivos <- getArchivosSinErrores(agente, errorBucket, c(201, 203), "FOT") %>% 
      intersect(exigibles[str_detect(exigibles, "BD01")])
    return(archivos)
  }
}

seleccionarErrorT2 <- function(codigoError, ruta){
  error <- switch (toString(codigoError),
                   "462"= procesarErrorModalidadCouta(ruta),
                   "463"= procesarErrorMontoOtorgado(ruta),
                   "464"= procesarErrorVencJudRetraso(ruta),
                   "465"= procesarErrorDocumentoIdent(ruta),
                   "466"= procesarErrorNumCredCobertura(ruta))
  return(error)
}

procesarErroresT1  <- function(agente, ruta, errorBucket){
  BD <- evaluarFile(ruta)
  
  tb <- tibble(Columna = filtrarColsErrorT1(ruta, errorBucket)) %>% rowwise() %>%
    mutate(VerifCols   = BD %>%
             filter((as.numeric(cgrep(BD, Columna)[[1]]) %in% elegirDigitosBD(ruta, Columna)) == FALSE) %>%
             pull(getCodigoBD(getBD(ruta))) %>% unique() %>% list(),
           resultado  = generarDetalleError3(ruta, VerifCols) %>% toString(),
           Coopac     = getCoopac(ruta),
           NombCoopac = getNomCoopac(ruta),
           Carpeta    = getCarpeta(agente),
           IdProceso  = getIdProceso(agente),
           Cod        = ifelse(resultado !="character(0)", getCodErrorT1(ruta, Columna), 0),
           Descripcion = getDescError(Cod),
           Detalle     = list(resultado))
  
  errorBucket <- bind_rows(errorBucket, tb %>% 
                    filter(resultado !="character(0)") %>%  
                    select(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion, Detalle))
  return(errorBucket)
}
procesarErroresT2  <- function(agente, errorBucket, exigibles, codigoError){
  archivos <- filtrarArchivosErrorT2(agente,errorBucket, exigibles, codigoError)
  
  if (list(archivos) == "character(0)"){
    resultado <- list("character(0)")
    
    return(resultado)
  }
  else{
    resultado <- tibble(NombreArchivo = archivos) %>%
      rowwise() %>% 
      mutate(Ruta    = getRuta(getCarpeta(agente), NombreArchivo),
             Errores = generarDetalleError3(Ruta, seleccionarErrorT2(codigoError, Ruta))) %>% 
      pull(Errores)
    
    return(resultado)
  }
}
procesarErroresT3  <- function(agente, ruta, errorBucket){
  BD <- evaluarFile(ruta)
  
  tb <- tibble(Columna    = filtrarColsErrorT3(ruta, errorBucket)) %>%
    rowwise() %>%
    mutate(validarFechas = BD %>%
             filter(dmy(cgrep(BD, Columna)[[1]]) %>% is.na() == TRUE) %>% 
             pull(getCodigoBD(getBD(ruta))) %>% unique() %>% list(),
           resultado   = generarDetalleError3(ruta, validarFechas) %>% toString(),
           Coopac      = getCoopac(ruta),
           NombCoopac  = getNomCoopac(ruta),
           Carpeta     = getCarpeta(agente),
           IdProceso   = getIdProceso(agente),
           Cod         = ifelse(resultado !="character(0)", getCodErrorT3(ruta, Columna), 0),
           Descripcion = getDescError(Cod),
           Detalle     = list(resultado))
  
  errorBucket <- bind_rows(errorBucket, tb %>%
                             filter(resultado !="character(0)") %>%
                             select(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion, Detalle))
  return(errorBucket)
}

## Detalle errores----
generarDetalleError1 <- function(ruta, error){
  detalleError <- ifelse(length(error)>0,
                         list(paste(getNombreArchivo(ruta), error, sep ="$", collapse=",")),
                         list(character(0)))
  return(detalleError)
}
generarDetalleError2 <- function(periodo, error){
  detalleError <- ifelse(length(error)>0,
                         list(paste0(periodo,"(", toString(error), ")")),
                         list(character(0)))
  return(detalleError)
}
generarDetalleError3 <- function(ruta, error){
  detalleError <- ifelse(length(error)>0,
                         list(paste0(getNombreArchivo(ruta),"(", toString(error),")")),
                         list(character(0)))
  return(detalleError)
}