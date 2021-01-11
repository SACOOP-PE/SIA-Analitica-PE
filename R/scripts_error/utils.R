# Version utils v2. 

# Funciones auxiliares - Ruta 
getRuta           <- function(carpeta, filename){
  lista_rutas <- list.files(path=carpeta, full.names = TRUE, recursive =  TRUE)
  (lista_rutas[str_detect(lista_rutas, filename)])[1]  %>% return()
}
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
GetAnoMesFromRuta <- function(ruta){
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
getCarpetaFromAgent           <- function(agent){ 
  agent %>% pull(Carpeta) %>% first() %>% return()
}
getIdProcesoFromAgent         <- function(agent){
  agent %>% pull(IdProceso) %>% first() %>% return()
}
getArchivosExigiblesFromAgent <- function(agent){
  cod_coopac <- agent %>% pull(Coopac) %>% first()
  id_bds     <- (agent %>% pull(Alcance))[[1]]
  periodos   <- global.alcance
  periodo_inicio <- agent %>% pull(PeriodoInicial) %>% first()
  periodo_final  <- agent %>% pull(PeriodoFinal) %>% first()
  
  apply(expand.grid(cod_coopac, id_bds,
                    paste0(periodos[(periodos >= periodo_inicio) &
                                      (periodos <= periodo_final)], ".txt")),
        1, paste, collapse = "_") %>% return()
}
getNombreCoopacFromAgent      <- function(agent){ 
  agent %>% pull(NombreCoopac) %>% first() %>% return()
}
getCoopacFromAgent            <- function(agent){ 
  agent %>% pull(Coopac) %>% first() %>% return()
}
getUsuarioFromAgent           <- function(agent){ 
  agent %>% pull(Usuario) %>% first() %>% return()
}
getInicioProcesoFromAgent     <- function(agent){ 
  agent %>% pull(PeriodoInicial) %>% first() %>% return()
}
getFinProcesoFromAgent        <- function(agent){ 
  agent %>% pull(PeriodoFinal) %>% first() %>% return()
}
getAlcanceFromAgent           <- function(agent){ 
  agent %>% pull(Alcance) %>% first() %>% return()
}
getNombreCoopacFromIdCoopac   <- function(idCoopac){
  initCuadreContable() %>% filter(CODIGO_ENTIDAD == as.integer(idCoopac)) %>% pull(ENTIDAD) %>% first()
}