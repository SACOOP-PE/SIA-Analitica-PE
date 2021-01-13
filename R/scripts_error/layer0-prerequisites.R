#' Función principal 

layer0 <- function(agente, errorBucket){
  
  carpeta <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosExigiblesFromAgent(agente)
  
  errorBucket <- errorBucket %>% filter(Cod != 100) 
  
  if (length(getDuplicados(carpeta, exigibles)) != 0) { 
    
    errorBucket <- errorBucket %>% addErrorIndividual(agente, codcoopac = getCoopacFromAgent(agente),
                                    idproceso = getIdProcesoFromAgent(agente),
                                    cod = 101,
                                    periodo = "",
                                    bd = "",
                                    arg_txt1 = toString(getDuplicados(carpeta, exigibles)),
                                    arg_txt2 ="",
                                    arg_txt3 = "",
                                    arg_num1 = length(getDuplicados(carpeta, exigibles)),
                                    arg_num2 = 0,
                                    arg_num3 = 0)
    addEventLog(agent, paste0("    Error: Se identificaron archivos duplicados."), "I", "B")
  }
 
  
  if (length(getFaltantes(carpeta, exigibles)) != 0) { 
    
    errorBucket <- errorBucket %>% addErrorIndividual(agente, codcoopac = getCoopacFromAgent(agente),
                                    idproceso = getIdProcesoFromAgent(agente),
                                    cod = 102,
                                    periodo = "",
                                    bd = "",
                                    arg_txt1 = toString(getFaltantes(carpeta, exigibles)),
                                    arg_txt2 ="",
                                    arg_txt3 = "",
                                    arg_num1 = length(getFaltantes(carpeta, exigibles)),
                                    arg_num2 = 0,
                                    arg_num3 = 0)
    addEventLog(agent, paste0("    Error: Se identificaron archivos faltantes."), "I", "B")
  }
 
  
  return(errorBucket)
}
# eb, cod, periodo, bd, arg_txt1, arg_txt2, arg_txt3, arg_num1, arg_num2, arg_num3
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
 