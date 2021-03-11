
# Funciones auxiliares - Ruta -----
getRuta           <- function(carpeta, filename){
  
  lista_rutas <- list.files(path=carpeta, 
                            full.names = TRUE, 
                            recursive =  TRUE)
  
  return((lista_rutas[str_detect(lista_rutas, filename)])[1])
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
getNombreCoopacFromRuta <- function(ruta){
  i <- (basename(ruta) %>% strsplit("_"))[[1]][1]
  initCuadreContable() %>% filter(CodigoEntidad == i) %>% pull(Entidad) %>% first() %>% return()
}
getBDFromRuta           <- function(ruta){ 
  if (is.na(ruta) | ruta == ""  ) {
    return("")
  }
  (basename(ruta) %>% strsplit("_"))[[1]][2] %>% return() 
}
evaluarFile             <- function(ruta){
  BD <- read_delim(ruta,"\t",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE,
                   col_types = cols(.default = "c"), locale = locale(encoding = "ISO-8859-1"), progress = F)
  
  colnames(BD) <- toupper(str_replace(colnames(BD), " ", "_"))
  
  return(BD)
}
quitarVaciosBD          <- function(ruta){
  BD     <- evaluarFile(ruta)
  codigo <- getCodigoBD(getBDFromRuta(ruta))
  
  BD <- BD %>% filter(!is.na(cgrep(BD, codigo)) | cgrep(BD, codigo) != "")
  
  return(BD)
}

# Funciones auxiliares - Agent -----
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
                    paste0(periodos[(periodos >= periodo_inicio) & (periodos <= periodo_final)], ".txt")),
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
getPeriodosFromAgent          <- function(agente){
  
  periodo_inicio <- agente %>% pull(PeriodoInicial) %>% first()
  periodo_final  <- agente %>% pull(PeriodoFinal)
  
  global.alcance[(global.alcance >= periodo_inicio) & (global.alcance <= periodo_final)] %>% return()
}

getNombreCoopacFromIdCoopac   <- function(idCoopac){
  initCuadreContable() %>% filter(CodigoEntidad == idCoopac) %>% pull(Entidad) %>% first()
}
getNivelCoopacFrpomIdCoopac   <- function(idCoopac){
  (initCuadreContable() %>% filter(CodigoEntidad == idCoopac) %>% pull(TipoEntidad) %>% unique() %>%
     str_split(pattern = " - Nivel "))[[1]][2]
}
getCodigoBD                   <- function(BD){
  campoIdentif  <- switch (BD,
                           BD01  = "CCR",
                           BD02A = "CCR",
                           BD02B = "CCR_C",
                           BD03A = "CODGR",
                           BD03B = "CODGR",
                           BD04  = "CCR_C")
  
  return(campoIdentif)
}

# Funciones auxiliares - Logging -----

getLogObject     <- function(path){
  read_delim(path, "\t", escape_double = FALSE,col_types = cols(Categoria = col_character(), 
                                                                Coopac = col_character(), Criticidad = col_character(), 
                                                                Descripcion = col_character(), Carpeta = col_character(), Fecha = col_character(), 
                                                                Hora = col_character(), IdProceso = col_integer(), Usuario = col_character()), 
             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE) %>% return()
}
getNextIdProceso <- function(logObject){
  if (logObject %>% pull(IdProceso) %>% max(na.rm = T) > 0)
    (logObject %>% pull(IdProceso) %>% max(na.rm = T) + 1) %>% return()
  else 
    return(1) 
}
addEventLog      <- function(agente,
                             descripcion,
                             categoria = "I", 
                             criticidad = "B"){
  
  descripcion <- paste0(paste0("[",Sys.time()[1],"] - "),descripcion)
  
  myLog <- getLogObject("logging/log.txt")
  event <- tibble(IdProceso = getIdProcesoFromAgent(agente),
                  Fecha = toString(Sys.Date()),
                  Hora  = toString(Sys.time()),
                  Usuario = "DPACHECO", 
                  Coopac  = getNombreCoopacFromAgent(agente) ,
                  Carpeta = getCarpetaFromAgent(agente), 
                  Descripcion = descripcion ,
                  Categoria  = ifelse(categoria == "I", "Informativo", "Advertencia"),
                  Criticidad = ifelse(criticidad == "B", "Baja", ifelse(criticidad == "M",Media, Alta)))
  
  write_delim(x = event,path = "logging/log.txt", delim = "\t", col_names = F, append = T)
  
  print(descripcion)
}
 
getLog <- function(pid) {
  contents <- getLogObject(path = "logging/log.txt") %>% filter(IdProceso == pid) %>% pull(Descripcion)
  
  pidlog <- tibble(logEncontrado = contents) %>% rowwise() %>% 
    mutate(Time = str_split(logEncontrado, pattern = " - ")[[1]][1],
           logEncontrado = str_split(logEncontrado, pattern = " - ")[[1]][2]) %>% 
    select(Time, logEncontrado)
  
  return(pidlog)
}