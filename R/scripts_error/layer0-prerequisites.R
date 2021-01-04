#' Función principal 

layer0 <- function(agent, eb){
  carpeta <- getCarpeta(agent)
  exigibles <- getArchivosExigibles(agent)
  
  if (length(getDuplicados(carpeta, exigibles)) != 0) { 
    eb <- eb %>% addError(101,getDescError(101),
                                              paste0("Archivos duplicados: ",toString(getDuplicados(carpeta, exigibles))))
  }
  if (length(getFaltantes(carpeta, exigibles)) != 0) { 
    eb <- eb %>% addError(102,getDescError(102), 
                                              paste0("Archivos faltantes: ",toString(getFaltantes(carpeta,exigibles))))
  }
  
  print(paste0("Se validaron los prerequisitos satisfactoriamente. (~ly) ", format(Sys.time(), "%a %b %d %X %Y")))
  return(eb)
}

#' Funciones secundarias

getDuplicados <- function(carpeta, exigibles){ 
  tibble(files = basename(list.files(path = carpeta, full.names = F, recursive =  TRUE))) %>%
    group_by(files) %>%
    filter(files %in% exigibles) %>% 
    filter(n() > 1) %>% 
    pull(files) %>% 
    unique() %>%  
    return()
}

getFaltantes  <- function(carpeta, exigibles){
  setdiff(exigibles,
          basename(list.files(path = carpeta, full.names = FALSE, recursive =  TRUE,  include.dirs = FALSE))) %>%
    return() 
}