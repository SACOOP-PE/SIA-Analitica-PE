createAgent <- function(idCoopac,
                        periodoInicial, 
                        periodoFinal, 
                        usuarioSIA = default.usuario,
                        coopacCarpeta = default.carpeta, 
                        bds = list(default.bd)){
  
  agente <- tibble(Coopac       = idCoopac,
                  NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                  Carpeta      = coopacCarpeta,
                  IdProceso    = getNextIdProceso(getLogObject("logging/log.txt")),
                  Usuario      = usuarioSIA,
                  InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
                  PeriodoInicial = periodoInicial,
                  PeriodoFinal   = periodoFinal,
                  Alcance        = bds) 
  
  addEventLog(agente, paste0("Validador SIA 1.3.2021 --------------------------------------"), 
              "I", "B")
  
  addEventLog(agente, paste0("Agente creado. PID-", agente %>% pull(IdProceso) %>% first(),
                            ". [",idCoopac,"|", periodoInicial, "~", periodoFinal, "]"), 
              "I", "B")
  
  return(agente)
}

createBucket     <- function(agente){
  eb <- tibble(CodCoopac     = agente %>% pull(Coopac) %>% first(),
         IdProceso  = agente %>% pull(IdProceso) %>% first(), 
         Cod         = 100,
         Periodo = "",
         BD = "",
         txt1 = "",
         txt2 = "",
         txt3 = "",
         num1 = 0,
         num2 = 0,
         num3 = 0) 
  
  addEventLog(agente, paste0("Bucket de errores creado. PID-", agente %>% pull(IdProceso) %>% first(),"."), 
              "I", "B")
  
  return(eb)
}


interrogateAgent <- function(agente){
  eb <- createBucket(agente)
  
  addEventLog(agente, paste0("Inicio del interrogatorio. PID-", agente %>% pull(IdProceso) %>% first(),"."), 
              "I", "B")
  
  #pre-requisitos ----
  addEventLog(agente, paste0("Apertura de revisión de pre-requisitos."),  "I", "B")
  
    eb <- layer0(agente, eb)
  
    if (nrow(eb) > 0) {
        addEventLog(agente, paste0("Fin del proceso de revisión por errores críticos 101-102."), "I", "B")
        return(eb)
    }
    else {
    addEventLog(agente, paste0("Revisión de pre-requisitos satisfactoria."), "I", "B")
    }
  
  #estructura de columnas ----
  addEventLog(agente, paste0("Apertura de revisión de estructura de datos."),  "I", "B")
  
    eb <- layer1(agente, eb)
  
    if (nrow(eb) > 0) {
      
      if (nrow(eb %>% filter(Cod %in% c(201, 202))) >0) { 
      
      n <- eb %>% filter(Cod %in% c(201,202)) %>% nrow()
      addEventLog(agente, paste0("La revisión de estructura de datos tiene observaciones. Continuar con discreción."), "I", "B")
        
      }
      else {
        addEventLog(agente, paste0("Revisión de estructura de datos satisfatoria."), "I", "B")
      }
      
    }
    else {
      addEventLog(agente, paste0("Revisión de estructura de datos satisfatoria."), "I", "B")
    }
  
  #errores OM 22269-2020 ----
  addEventLog(agente, paste0("Apertura de revisión de errores OM 22269-2020."),  "I", "B")

    eb <- layer2(agente, eb)
    eb <- layer3(agente, eb)
    eb <- layer4(agente, eb)

    if (nrow(eb) > 0) {
      
      if (nrow(eb %>% filter(Cod %in% c(311:478))) >0) {

        addEventLog(agente, paste0("La revisión errores OM 22269-2020 tiene observaciones."), "I", "B")
      }
      else {
        addEventLog(agente, paste0("Revisión de errores OM 22269-2020 fue satisfatoria."), "I", "B")
      }
      
    }
    else {
      addEventLog(agente, paste0("Revisión de errores OM 22269-2020 fue satisfatoria."), "I", "B")
    }
    
  #alertas ad-hoc 11356 ----
    #eb <- layer3(agent, eb) 

  return(eb)
}

closeAgent       <- function(agente, eb){
  agente <- agente %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(eb),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal)
  
  return(agente)
}