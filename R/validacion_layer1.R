ejecutar_validacion_layer1 <- function(header, error_bucket){
  
  carpeta <- getCarpeta(header)
  exigibles <- getArchivosExigibles(header)
  
  if (length(getDuplicados(carpeta, exigibles)) != 0) { 
    error_bucket <- error_bucket %>% addError(101,getDescError(101),
                                              toString(getDuplicados(carpeta, exigibles)))
  }
  if (length(getFaltantes(carpeta, exigibles)) != 0) { 
    error_bucket <- error_bucket %>% addError(102,getDescError(102), 
                                              toString(getFaltantes(carpeta,exigibles)))
  }
  
  print(paste0("El layer 1 terminó: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(error_bucket)
}