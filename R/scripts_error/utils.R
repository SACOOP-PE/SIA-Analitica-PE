# Version utils v2. 

getRuta           <- function(carpeta, 
                              filename){
  
  lista_rutas <- list.files(path=carpeta, 
                            full.names = TRUE, 
                            recursive =  TRUE)
  
  return((lista_rutas[str_detect(lista_rutas, filename)])[1])
  
}
# Funciones auxiliares - Ruta
getAnoFromRuta    <- function(ruta){
  if (is.na(ruta) | ruta == "" ) {
    return("")
  } 
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    substr(1, 4) %>% return()
  
}
getMesFromRuta    <- function(ruta){
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    substr(5, 6) %>% return()
}
getAnoMesFromRuta <- function(ruta){
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    return()
}
getCoopacFromRuta <- function(ruta){ 
  if (is.na(ruta) || ruta == "" ) {
    return("") 
  }
  
  (basename(ruta) %>% strsplit("_"))[[1]][1] %>% return()
}
getNombreCoopacFromRuta   <- function(ruta){
  i <- (basename(ruta) %>% strsplit("_"))[[1]][1]
  initCuadreContable() %>% filter(CODIGO_ENTIDAD == as.numeric(i)) %>% pull(ENTIDAD) %>% first() %>% return()
}
getBDFromRuta             <- function(ruta){ 
  if (is.na(ruta) | ruta == ""  ) {
    return("")
  }
  (basename(ruta) %>% strsplit("_"))[[1]][2] %>% return() 
}
getNombreArchivoFromRuta  <- function(ruta){
  (basename(ruta) %>% strsplit("/"))[[1]] %>% return()
} 


# Funciones auxiliares - Agent
getCarpetaFromAgent           <- function(agente){ 
  agente %>% pull(Carpeta) %>% first() %>% return()
}
getIdProcesoFromAgent         <- function(agente){
  agente %>% pull(IdProceso) %>% first() %>% return()
}
getArchivosExigiblesFromAgent <- function(agente){
  cod_coopac <- agente %>% pull(Coopac) %>% first()
  id_bds     <- (agente %>% pull(Alcance))[[1]]
  periodos   <- global.alcance
  periodo_inicio <- agente %>% pull(PeriodoInicial) %>% first()
  periodo_final  <- agente %>% pull(PeriodoFinal)
  
  apply(expand.grid(cod_coopac, id_bds,
                    paste0(periodos[(periodos >= periodo_inicio) &
                                      (periodos <= periodo_final)], ".txt")),
        1, paste, collapse = "_") %>% return()
}
getNombreCoopacFromAgent      <- function(agente){  
  agente %>% pull(NombreCoopac) %>% first() %>% return()
}
getCoopacFromAgent            <- function(agente){ 
  agente %>% pull(Coopac) %>% first() %>% return()
}
getUsuarioFromAgent           <- function(agente){ 
  agente %>% pull(Usuario) %>% first() %>% return()
}
getInicioProcesoFromAgent     <- function(agente){ 
  agente %>% pull(PeriodoInicial) %>% first() %>% return()
}
getFinProcesoFromAgent        <- function(agente){ 
  agente %>% pull(PeriodoFinal) %>% first() %>% return()
}
getAlcanceFromAgent           <- function(agente){ 
  agente %>% pull(Alcance) %>% first() %>% return()
}
getNombreCoopacFromIdCoopac   <- function(idCoopac){
  initCuadreContable() %>% filter(CODIGO_ENTIDAD == as.integer(idCoopac)) %>% pull(ENTIDAD) %>% first()
}