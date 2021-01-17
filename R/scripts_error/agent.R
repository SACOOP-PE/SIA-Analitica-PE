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
 
  addEventLog(agente, log.msg.01)
  addEventLog(agente, log.msg.02)
  
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
  
  addEventLog(agente, log.msg.03) 
  return(eb)
}


interrogateAgent <- function(agente){
  eb <- createBucket(agente)
  
  addEventLog(agente, log.msg.04)
  addEventLog(agente, log.msg.05)
  
  eb <- layer0(agente, eb)
  
    if (nrow(eb) > 0) {
      if ((eb %>% pull(Cod)) %in% c(101,102)) { 
        addEventLog(agente, log.msg.06)
        return(eb)
    } else {
      addEventLog(agente, log.msg.07)
    }
  } else {
    addEventLog(agente, log.msg.07)
  }
  
  #estructura de columnas
  addEventLog(agente, log.msg.08)
  eb <- layer1(agente, eb)
  
    if (nrow(eb) > 0) {
      
      if ((eb %>% pull(Cod)) %in% c(201,202)) { 
      
      n <- eb %>% filter(Cod %in% c(201,202)) %>% nrow()
      addEventLog(agente, log.msg.09)
      } else {
        addEventLog(agente, log.msg.10)
      }
    } else {
      addEventLog(agente, log.msg.10)
    }
  
  # errores OM 22269-2020
  addEventLog(agente, log.msg.11)

    eb <- layer2(agente, eb)

    if (nrow(eb) > 0) {
      if (nrow(eb %>% filter(Cod %in% c(311:478)) > 0)) {
        addEventLog(agente, log.msg.12)
      } else {
        addEventLog(agente, log.msg.13)
        }
      } else {
        addEventLog(agente, log.msg.13)
      }

  # eb <- layer3(agent, eb) #alertas ad-hoc 11356
   
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

log.msg.01 <- "Validador SIA 1.3.2021 --------------------------------------"
log.msg.02 <- paste0("Agente creado. PID-", agente %>% pull(IdProceso) %>% first(),". [",idCoopac,"|", periodoInicial, "~", periodoFinal, "]")
log.msg.03 <- paste0("Bucket de errores creado. PID-", agente %>% pull(IdProceso) %>% first(),".")
log.msg.04 <- paste0("Inicio del interrogatorio. PID-", agente %>% pull(IdProceso) %>% first(),".")
log.msg.05 <- paste0("Apertura de revisión de pre-requisitos.")
log.msg.06 <- paste0("   Fin del proceso de revisión por errores críticos 101-102.")
log.msg.07 <- paste0("   Revisión de pre-requisitos satisfactoria.")
log.msg.08 <- paste0("Apertura de revisión de estructura de datos.")
log.msg.09 <- paste0("   La revisión de estructura de datos tiene observaciones. Continuar con discreción.")
log.msg.10 <- paste0("   Revisión de estructura de datos satisfatoria.")
log.msg.11 <- paste0("Apertura de revisión de errores OM 22269-2020.")
log.msg.12 <- paste0("   La revisión errores OM 22269-2020 tiene observaciones.")
log.msg.13 <- paste0("   La Revisión de errores OM 22269-2020 fue satisfatoria.")