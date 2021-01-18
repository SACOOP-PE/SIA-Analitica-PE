# Create log

getNombreCoopac  <- function(cod){
  #cod - number
  
  initCuadreContable() %>% 
    filter(CODIGO_ENTIDAD == as.integer(cod)) %>%
    pull(ENTIDAD) %>% first()
}

 
getLogObject     <- function(path){
  read_delim(path, "\t", escape_double = FALSE,col_types = cols(Categoria = col_character(), 
                                                                Coopac = col_character(), Criticidad = col_character(), 
                                                                Descripcion = col_character(),  Carpeta = col_character(), Fecha = col_character(), 
                                                                Hora = col_character(), 
                                                                IdProceso = col_integer(),
                                                                Usuario = col_character()), 
             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE) %>% return()
}


addEventLog      <- function(agente,
                             descripcion,
                             categoria = "I", 
                             criticidad = "B"){
  
  descripcion <- paste0(timehead(),descripcion)
  
  myLog <- getLogObject("logging/log.txt")
  event <- tibble(IdProceso = getIdProcesoFromAgent(agente),
                  Fecha = toString(Sys.Date()),
                  Hora  = toString(Sys.time()),
                  Usuario = default.usuario, 
                  Coopac  = getNombreCoopacFromAgent(agente) ,
                  Carpeta = getCarpetaFromAgent(agente), 
                  Descripcion = descripcion ,
                  Categoria  = ifelse(categoria == "I", "Informativo", "Advertencia"),
                  Criticidad = ifelse(criticidad == "B", "Baja", ifelse(criticidad == "M",Media, Alta)))
  
  write_delim(x = event,path = "logging/log.txt", delim = "\t", col_names = F, append = T)
  cat(paste0(padright(descripcion)))
  #print(descripcion)
}

timehead <- function() {
  paste0("[",Sys.time()[1],"] - ")}

getlog <- function(pid) {
  contents <- getLogObject(path = "logging/log.txt") %>% filter(IdProceso == pid) %>% pull(Descripcion)
  
  pidlog <- tibble(logEncontrado = contents) %>% rowwise() %>% 
    mutate(Time = str_split(logEncontrado, pattern = " - ")[[1]][1],
           logEncontrado = str_split(logEncontrado, pattern = " - ")[[1]][2]) %>% 
    select(Time, logEncontrado)

  return(pidlog)
}