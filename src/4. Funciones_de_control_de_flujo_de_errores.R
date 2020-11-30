##### 4. Funciones de control de flujo de errores  -----
getArchivos_Error            <- function(header, error_bucket, cod, col){
  detalle_error_split <- unlist(error_bucket %>% filter(Cod %in% cod) %>% pull(Detalle) %>% str_split(","))
  
  str_extract(detalle_error_split[str_detect(detalle_error_split, paste(col,collapse = '|'))],
              paste(getArchivosExigibles(header),collapse = '|')) %>% 
    return()
}
getArchivos_SinErrores       <- function(header, error_bucket, cod, col){
  setdiff(getArchivosExigibles(header),
          getArchivos_Error(header, error_bucket, cod, col)) %>% 
    return()
}

get_colsVal_SinErrores       <- function(ruta, error_bucket){
  detalle_error_split <- unlist(error_bucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>%
                                  str_split(","))
  
  setdiff(get_colsVal(ruta),
          str_extract(detalle_error_split[str_detect(detalle_error_split,get_NombreArchivo(ruta))],
                      paste(get_colsVal(ruta),collapse = '|'))) %>% return()
}
get_colsFecha_SinErrores     <- function(ruta, error_bucket){
  detalle_error_split <- unlist(error_bucket %>% filter(Cod %in% c(201,203)) %>% pull(Detalle) %>%
                                  str_split(","))
  
  setdiff(get_colFechas(ruta),
          str_extract(detalle_error_split[str_detect(detalle_error_split, get_NombreArchivo(ruta))],
                      paste(get_colFechas(ruta),collapse = '|'))) %>% return()
}

restriccion_archivosFaltDups <- function(error_bucket){
  if (filter(error_bucket, Cod %in% c("101","102")) %>% nrow()){return(1)}
  return(0)
}
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
restriccion_archivos_ErroresLayer4 <- function(header, error_bucket, exigibles, tipoError){
  exigibles_errorTipo <- tibble(Nombre_archivo = exigibles) %>% rowwise() %>%
    mutate(n_cols = switch (tipoError,
                            tipo1 = get_colsVal_SinErrores(getRuta(getCarpeta(header), Nombre_archivo),
                                                           error_bucket) %>% length(),
                            tipo3 = get_colsFecha_SinErrores(getRuta(getCarpeta(header), Nombre_archivo),
                                                             error_bucket) %>% length())) %>%
    filter(n_cols > 0) %>% 
    pull(Nombre_archivo) %>% 
    return()
}