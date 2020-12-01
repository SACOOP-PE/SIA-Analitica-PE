##### 4. Funciones de control de flujo de errores  -----
getArchivos_Error      <- function(header, error_bucket, cod, col){
  detalle_error_split <- unlist(error_bucket %>% filter(Cod %in% cod) %>% pull(Detalle) %>% str_split(","))
  
  str_extract(detalle_error_split[str_detect(detalle_error_split, paste(col,collapse = '|'))],
              paste(getArchivosExigibles(header),collapse = '|')) %>% 
    return()
}
getArchivos_SinErrores <- function(header, error_bucket, cod, col){
  setdiff(getArchivosExigibles(header),
          getArchivos_Error(header, error_bucket, cod, col)) %>% 
    return()
}

#layer 1
restriccion_archivosFaltDups <- function(error_bucket){
  if (filter(error_bucket, Cod %in% c("101","102")) %>% nrow()){return(1)}
  return(0)
}
# layer 3 (cruces BD01/BD02A, BD03A/BD03B), layer 4 (error_cis)
restriccion_periodos         <- function(error_bucket, name_BD1, name_BD2, columnas){
  archivos <- intersect(getArchivos_SinErrores(header, error_bucket, c(201, 203), columnas),
                        setdiff(getArchivosExigibles(header),
                                str_extract(filter(error_bucket, Cod %in% c(311, 312)) %>% pull(Detalle), paste(getArchivosExigibles(header), collapse = '|')) %>%
                                  unique())) %>%
              unique()
  
  archivos_cruce <- archivos[str_detect(archivos, paste(c(name_BD1, name_BD2), collapse = '|'))]
  
  tibble(Periodos =  str_extract(archivos_cruce, paste(as.character(alcance_general),collapse = '|'))) %>%
    group_by(Periodos) %>%
    filter(n() ==2) %>%
    pull(Periodos) %>% 
    unique() %>%
    return()
}
#layer 4 (tipo 1 y 3)
depurarColsErrorT1 <- function(ruta, error_bucket){
  detalle_error_split <- unlist(error_bucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>%
                                  str_split(","))
  
  setdiff(ColumnasErrorTipo1(ruta),
          str_extract(detalle_error_split[str_detect(detalle_error_split, getNombreArchivo(ruta))],
                      paste(ColumnasErrorTipo1(ruta), collapse = '|'))) %>% return()
}
depurarColsErrorT3 <- function(ruta, error_bucket){
  detalle_error_split <- unlist(error_bucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>%
                                  str_split(","))
  
  setdiff(ColumnasErrorTipo3(ruta),
          str_extract(detalle_error_split[str_detect(detalle_error_split, getNombreArchivo(ruta))],
                      paste(ColumnasErrorTipo3(ruta), collapse = '|'))) %>% return()
}
restriccion_archivos_ErroresLayer4 <- function(header, error_bucket, exigibles, tipoError){
  exigibles_errorTipo <- tibble(Nombre_archivo = exigibles) %>% rowwise() %>%
    mutate(colsfiltradas_n = switch (tipoError,
                                     tipo1 = depurarColsErrorT1(getRuta(getCarpeta(header), Nombre_archivo),error_bucket) %>%
                                              length(),
                                     tipo3 = depurarColsErrorT3(getRuta(getCarpeta(header), Nombre_archivo),error_bucket) %>%
                                              length())) %>%
    filter(colsfiltradas_n > 0) %>%
    pull(Nombre_archivo) %>%
    return()
}