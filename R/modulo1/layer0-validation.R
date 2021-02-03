#' Función principal 

layer0 <- function(agente, eb){
  
  carpeta <- getCarpetaFromAgent(agente)
  exigibles <- getArchivosExigiblesFromAgent(agente)
  
  eb <- eb %>% filter(Cod != 100) 
  
  if (length(getDuplicados(carpeta, exigibles)) != 0) {
    
    eb <- eb %>% addError(obj = tibble(CodCoopac = getCoopacFromAgent(agente),
                                       IdProceso = getIdProcesoFromAgent(agente),
                                       Cod = 101,
                                       txt1 = toString(getDuplicados(carpeta, exigibles)),
                                       num1 = length(unlist(str_split(txt1, ","))))
                          ) 
    
    addEventLog(agent, paste0("      Resultado: Se identificaron archivos duplicados."), "I", "B")
  }
 
  if (length(getFaltantes(carpeta, exigibles)) != 0) { 
    
    eb <- eb %>% addError(obj = tibble(CodCoopac = getCoopacFromAgent(agente),
                                       IdProceso = getIdProcesoFromAgent(agente),
                                       Cod = 102,
                                       txt1 = toString(getFaltantes(carpeta, exigibles)),
                                       num1 = length(unlist(str_split(txt1, ","))))
                          )

    addEventLog(agent, paste0("      Resultado: Se identificaron archivos faltantes."), "I", "B")
  }
 
  return(eb)
}

#' Funciones secundarias
#' getDuplicados()
#' getFaltantes()

getDuplicados <- function(carpeta, exigibles){ 
 dups <- tibble(files = basename(list.files(path = carpeta, full.names = F, recursive =  TRUE))) %>%
            group_by(files) %>%
            filter(files %in% exigibles) %>% 
            filter(n() > 1) %>% 
            pull(files) %>% 
            unique()
 
 return(dups)
}
getFaltantes  <- function(carpeta, exigibles){
  fal <- setdiff(exigibles,
                 basename(list.files(path = carpeta, full.names = FALSE, recursive =  TRUE,  include.dirs = FALSE)))
  return(fal) 
}