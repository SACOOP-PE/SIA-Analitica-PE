getAno    <- function(ruta){
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    substr(1, 4) %>% return()
}
getMes    <- function(ruta){
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    substr(5, 6) %>% return()
}
getAnoMes <- function(ruta){
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    return()
}
getCoopac <- function(ruta){
  (basename(ruta) %>% strsplit("_"))[[1]][1] %>% return()
}
getNomCoopac <- function(ruta){
  i <- (basename(ruta) %>% strsplit("_"))[[1]][1]
  initCuadreContable() %>% filter(CODIGO_ENTIDAD == as.numeric(i)) %>% pull(ENTIDAD) %>% first() %>% return()
}
getBD        <- function(ruta){
  (basename(ruta) %>% strsplit("_"))[[1]][2] %>% return()
}

getCarpeta           <- function(header){ 
  header %>% pull(Carpeta) %>% first() %>% return()
}
getIdProceso         <- function(header){
  header %>% pull(IdProceso) %>% first() %>% return()
}
getRuta              <- function(carpeta, filename){
  lista_rutas <- list.files(path=carpeta, full.names = TRUE, recursive =  TRUE)
  (lista_rutas[str_detect(lista_rutas, filename)])[1]  %>% return()
}

getNombreArchivo     <- function(ruta){
  (basename(ruta) %>% strsplit("/"))[[1]] %>% return()
} 
getArchivosExigibles <- function(agente){
  cod_coopac <- agente %>% pull(Coopac) %>% first()
  id_bds     <- (agente %>% pull(Alcance))[[1]]
  periodos   <- alcanceGeneral
  periodo_inicio <- agente %>% pull(PeriodoInicial) %>% first()
  periodo_final  <- agente %>% pull(PeriodoFinal) %>% first()
  
  apply(expand.grid(cod_coopac, id_bds,
                    paste0(periodos[(periodos >= periodo_inicio) &
                                      (periodos <= periodo_final)], ".txt")),
        1, paste, collapse = "_") %>% return()
}
 
