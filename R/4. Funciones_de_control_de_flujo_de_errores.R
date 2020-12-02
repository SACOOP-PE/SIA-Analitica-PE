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
  setdiff(getArchivosExigibles(header),
          getArchivosError(header, errorBucket, codError, col)) %>% 
    return()
}
restriccionPeriodos         <- function(errorBucket, BD1, BD2, columnas){
  filtrarArchivos <- intersect(getArchivosSinErrores(header, errorBucket, c(201, 203), columnas),
                               setdiff(getArchivosExigibles(header),
                                       str_extract(filter(errorBucket, Cod %in% c(311, 312)) %>% pull(Detalle), paste(getArchivosExigibles(header), collapse = '|')) %>% unique()
                                       )
                               ) %>% unique()
  
  archivosCruce <- filtrarArchivos[str_detect(filtrarArchivos, paste(c(BD1, BD2), collapse = '|'))]
  
  tibble(Periodos =  str_extract(archivosCruce, paste(as.character(alcanceGeneral),collapse = '|'))) %>%
    group_by(Periodos) %>%
    filter(n() ==2) %>%
    pull(Periodos) %>% 
    unique() %>%
    return()
}

#layer 4 (tipo 1 y 3)
depurarColsErrorT1 <- function(ruta, errorBucket){
  filterError <- unlist(errorBucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>%
                                  str_split(","))
  
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