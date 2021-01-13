createAgent <- function(idCoopac,
                        usuarioSIA,
                        coopacCarpeta, 
                        periodoInicial, 
                        periodoFinal, 
                        bds = c("BD01","BD02A","BD02B","BD03A","BD03B","BD04")){
  
  agent <- tibble(Coopac       = idCoopac,
                  NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                  Carpeta      = coopacCarpeta,
                  IdProceso    = getNextIdProceso(getLogObject("logging/log.txt")),
                  Usuario      = usuarioSIA,
                  InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
                  PeriodoInicial = periodoInicial,
                  PeriodoFinal   = periodoFinal,
                  Alcance        = bds) 
  
  addEventLog(agent, paste0("Validador SIA 1.3.2021 --------------------------------------"), 
              "I", "B")
  
  addEventLog(agent, paste0("Agente creado. PID-", agent %>% pull(IdProceso) %>% first(),
                            ". [",idCoopac,"|", periodoInicial, "~", periodoFinal, "]"), 
              "I", "B")
  
  return(agent)
}

 
createBucket     <- function(agent){
  eb <- tibble(CodCoopac     = agent %>% pull(Coopac) %>% first(),
         IdProceso  = agent %>% pull(IdProceso) %>% first(), 
         Cod         = 100,
         Periodo = "",
         BD = "",
         txt1 = "",
         txt2 = "",
         txt3 = "",
         num1 = 0,
         num2 = 0,
         num3 = 0) 
  
  addEventLog(agent, paste0("Bucket de errores creado. PID-", agent %>% pull(IdProceso) %>% first(),"."), 
              "I", "B")
  
  return(eb)
}

interrogateAgent <- function(agent){
  eb <- createBucket(agent)
  
  addEventLog(agent, paste0("Inicio del interrogatorio. PID-", agent %>% pull(IdProceso) %>% first(),"."), 
              "I", "B")
  
  addEventLog(agent, paste0("Apertura de revisión de pre-requisitos."),  "I", "B")
  
  eb <- layer0(agent, eb) #pre-requisitos
  
  if (nrow(eb) > 0) {
    if ((eb %>% pull(Cod)) %in% c(101,102)) { 
      
      addEventLog(agent, paste0("Fin del proceso de revisión por errores críticos 101-102."), "I", "B")
      return(eb)
      
    }
    else {
      addEventLog(agent, paste0("Revisión de pre-requisitos satisfactoria."), "I", "B")
    }
  }
  else {
    addEventLog(agent, paste0("Revisión de pre-requisitos satisfactoria."), "I", "B")
  }
  
  addEventLog(agent, paste0("Apertura de revisión de estructura de datos."),  "I", "B")
  
  eb <- layer1(agent, eb) #estructura de columnas
  
  if (nrow(eb) > 0) {
    if ((eb %>% pull(Cod)) %in% c(201,202)) { 
      
      addEventLog(agent, paste0("La revisión de estructura de datos tiene observaciones. Continuar con discreción."), "I", "B")
      return(eb)
    }
    else
    {
      addEventLog(agent, paste0("Revisión de estructura de datos satisfatoria."), "I", "B")
    }
  } 
  
  #
  eb <- layer2(agent, eb) #errores OM 22269-2020
  # eb <- layer3(agent, eb) #alertas ad-hoc 11356
   
  return(eb)
}
closeAgent       <- function(agent, eb){
  agent <- agent %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(eb),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal) %>%
    return()
}