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


generarDetalleError1 <- function(ruta, error){
  #Edwin, podemos incorporar esta función en el procedimiento general del layer 1?
  paste_error <- ifelse(length(error)>0,
                        list(paste0(getAnoMes(ruta), "-", getBD(ruta), ":", error, collapse=",")),
                        list(character(0)))
  return(paste_error)
}

# generarDetalleError2 <- function(ruta, error){
#   paste_error <- ifelse(length(error)>0,
#                         list(paste0(getNombreArchivo(ruta),"(", toString(error),")")),
#                         list(character(0)))
#   return(paste_error)
# }
# generarDetalleError3 <- function(ruta, columna, error){
#   output <- ifelse(length(error)>=500,
#                    paste0(str_sub(error,0,500),"(...)"),
#                    error) %>% str_replace(",",";")
#   
#   paste_error <- ifelse(length(output)>0,
#                         list(paste(getNombreArchivo(ruta), columna, paste0(toString(output), ":"), sep = "$")),
#                         list(character(0)))
#   return(paste_error)
# }
# 
# generarDetalleError4 <- function(periodo, errorCruce){
#   
#   paste_error <- ifelse(length(errorCruce)>0,
#                         list(paste0(periodo,"(", toString(errorCruce), ")")),
#                         list(character(0)))
# }
