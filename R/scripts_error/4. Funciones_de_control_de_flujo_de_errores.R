##### 4. Funciones de control de flujo de errores  -----
#layer 1
restriccionArchivosFaltDups <- function(errorBucket){
  if (filter(errorBucket, Cod %in% c("101","102")) %>% nrow()){return(1)}
  return(0)
}

#layer 3 (cruces BD01/BD02A, BD03A/BD03B), layer 4 (error_cis)
getArchivosError      <- function(header, errorBucket, codError, col){
  filterError <- unlist(errorBucket %>% filter(Cod %in% codError) %>% pull(Detalle) %>% str_split(","))
  
  str_extract(filterError[str_detect(filterError, paste(col, collapse = '|'))],
              paste(getArchivosExigibles(header), collapse = '|')) %>% 
    return()
}
getArchivosSinErrores <- function(header, errorBucket, codError, col){
  if (col == "CCR" & 
      all(getArchivosError(header, errorBucket, c(201, 203), "CCR") == getArchivosError(header, errorBucket, c(201, 203), "CCRF"))== TRUE &
      length(getArchivosError(header, errorBucket, c(201, 203), "CCR")) > 0){
    
    errorCCRF <- getArchivosError(header, errorBucket, c(201, 203), "CCRF")
    archivos  <- setdiff(getArchivosExigibles(header),
                         getArchivosError(header, errorBucket, codError, col)) %>% 
                  union(errorCCRF) %>% unique()
    
    archivos <- intersect(getArchivosExigibles(header), 
                          archivos) %>% unique()
    return(archivos)
  }
  
  archivos <- setdiff(getArchivosExigibles(header),
                      getArchivosError(header, errorBucket, codError, col))
  return(archivos)
}
restriccionPeriodos   <- function(errorBucket, BD1, BD2, columnas){
  if (columnas == "CCR" & 
      all(getArchivosError(header, errorBucket, c(201, 203), "CCR") == getArchivosError(header, errorBucket, c(201, 203), "CCRF"))== TRUE &
      length(getArchivosError(header, errorBucket, c(201, 203), "CCR")) > 0){
    filtrarArchivos <- getArchivosError(header, errorBucket, c(201, 203), "CCR")
    filtrarArchivos <- union(filtrarArchivos, 
                             getArchivosExigibles(header)[str_detect(getArchivosExigibles(header), "BD02A")])

    PeriodosFiltro <-  tibble(Periodos =  str_extract(filtrarArchivos, paste(as.character(alcanceGeneral),collapse = '|'))) %>%
                            group_by(Periodos) %>%
                            filter(n() ==2) %>%
                            pull(Periodos) %>% 
                            unique()
    return(PeriodosFiltro)
  }
  
  filtrarArchivos <- getArchivosSinErrores(header, errorBucket, c(201, 203), columnas)
  archivosCruce   <- filtrarArchivos[str_detect(filtrarArchivos, paste(c(BD1, BD2), collapse = '|'))]
  
  PeriodosFiltro <- tibble(Periodos =  str_extract(archivosCruce, paste(as.character(alcanceGeneral),collapse = '|'))) %>%
                      group_by(Periodos) %>%
                      filter(n() ==2) %>%
                      pull(Periodos) %>% 
                      unique() 
  return(PeriodosFiltro)
}

#layer 4 (tipo 1 y 3)
depurarColsSaldos <- function(ruta, saldos, errorBucket){
  filterError <- unlist(listaErrores %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>% str_split(","))
  
  setdiff(saldos,
          str_extract(filterError[str_detect(filterError, getNombreArchivo(ruta))],
                      paste(unlist(getColumnasOM("BD01")), collapse = '|')
                      )
          ) %>% return()  
}

getArchivosExigiblesErrores <- function(errorBucket, exigibles, codigoError){
  if (codigoError >= 462 & codigoError <= 464) {
    archivos <- switch (toString(codigoError),
                        "462"= getArchivosSinErrores(header, listaErrores, c(201, 203), c("ESAM","NCPR", "PCUO")),
                        "463"= getArchivosSinErrores(header, errorBucket, c(201, 203), c("MORG", "SKCR")),
                        "464"= getArchivosSinErrores(header, errorBucket, c(201, 203), c("KVE", "DAK", "KJU"))) %>% 
      intersect(exigibles[str_detect(exigibles, "BD01")])
    return(archivos)
  }
  if (codigoError == 465){
    archivos <- getArchivosSinErrores(header, errorBucket, c(201, 203), c("TID", "NID", "TID_C", "NID_C")) %>% 
      intersect(exigibles[str_detect(exigibles,
                                     paste(c("BD01","BD04"), collapse = '|'))])
    return(archivos)
  }
  if (codigoError == 466){
    archivos <- getArchivosSinErrores(header, errorBucket, c(201, 203), c("NCR", "NRCL")) %>% 
      intersect(exigibles[str_detect(exigibles, "BD03A")])
    return(archivos)
  }
}
elegirErrorLayer4 <- function(codigoError, ruta){
  error <- switch (toString(codigoError),
                   "462"= procesarErrorModalidadCouta(ruta),
                   "463"= procesarErrorMontoOtorgado(ruta),
                   "464"= procesarErrorVencJudRetraso(ruta),
                   "465"= procesarErrorDocumentoIdent(ruta),
                   "466"= procesarErrorNumCredCobertura(ruta))
  return(error)
}
procesarErroresT2  <- function(errorBucket, exigibles, codigoError){
  archivos <- getArchivosExigiblesErrores(errorBucket, exigibles, codigoError)
  
  if (list(archivos) == "character(0)"){
    resultado <- list("character(0)") 
    return(resultado)
  }
  tb <- tibble(NombreArchivo = archivos) %>% rowwise() %>% 
    mutate(Ruta = getRuta(getCarpeta(header), NombreArchivo),
           Errores = generarDetalleError2(Ruta, elegirErrorLayer4(codigoError, Ruta))
    ) %>% 
    pull(Errores)
  return(tb)
}

depurarColsErrorT1 <- function(ruta, errorBucket){
  filterError <- unlist(errorBucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>% str_split(","))
  
  setdiff(ColumnasErrorTipo1(ruta),
          str_extract(filterError[str_detect(filterError, getNombreArchivo(ruta))],
                      paste(ColumnasErrorTipo1(ruta), collapse = '|'))
          ) %>% return()
}
depurarColsErrorT3 <- function(ruta, errorBucket){
  filterError <- unlist(errorBucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>%
                                  str_split(","))
  
  setdiff(ColumnasErrorTipo3(ruta),
          str_extract(filterError[str_detect(filterError, getNombreArchivo(ruta))],
                      paste(ColumnasErrorTipo3(ruta), collapse = '|'))
          ) %>% return()
}
restriccionArchivosErroresLayer4 <- function(header, errorBucket, exigibles, tipoError){
  archivosErrorTipo <- tibble(NombreArchivo = exigibles) %>% rowwise() %>%
    mutate(colsFiltradas_n = switch (tipoError,
                                     tipo1 = depurarColsErrorT1(getRuta(getCarpeta(header), NombreArchivo), errorBucket),
                                     tipo3 = depurarColsErrorT3(getRuta(getCarpeta(header), NombreArchivo), errorBucket)
                                     ) %>% length()
           ) %>%
    filter(colsFiltradas_n > 0) %>%
    pull(NombreArchivo) %>%
    return()
}