#' Función principal 

layer0 <- function(agent, eb){
  
  carpeta <- getCarpetaFromAgent(agent)
  exigibles <- getArchivosExigibles(agent)
  
  eb <- eb %>% filter(Cod != 100) 
  
  if (length(getDuplicados(carpeta, exigibles)) != 0) { 
    
    eb <- eb %>% addErrorIndividual(codcoopac = getCoopacFromAgent(agent),
                                    idproceso = getIdProcesoFromAgent(agent),
                                    cod = 101,
                                    periodo = "",
                                    bd = "",
                                    arg_txt1 = toString(getDuplicados(carpeta, exigibles)),
                                    arg_txt2 ="",
                                    arg_txt3 = "",
                                    arg_num1 = length(getDuplicados(carpeta, exigibles)),
                                    arg_num2 = 0,
                                    arg_num3 = 0)
    
  }
  if (length(getFaltantes(carpeta, exigibles)) != 0) { 
    
    eb <- eb %>% addErrorIndividual(codcoopac = getCoopacFromAgent(agent),
                                    idproceso = getIdProcesoFromAgent(agent),
                                    cod = 102,
                                    periodo = "",
                                    bd = "",
                                    arg_txt1 = toString(getFaltantes(carpeta, exigibles)),
                                    arg_txt2 ="",
                                    arg_txt3 = "",
                                    arg_num1 = length(getFaltantes(carpeta, exigibles)),
                                    arg_num2 = 0,
                                    arg_num3 = 0)
  }
  
  print(eb)
  return(eb)
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


