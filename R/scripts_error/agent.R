 
createAgent <- function(idCoopac,
                        periodoInicial, 
                        periodoFinal, 
                        usuarioSIA = default.usuario,
                        coopacCarpeta = default.carpeta, 
                        bds = list(default.bd)){
  
  pid <- getNextIdProceso(getLogObject("logging/log.txt"))
  
  agente <- tibble(Coopac       = idCoopac,
                  NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                  Carpeta      = coopacCarpeta,
                  IdProceso    = pid,
                  Usuario      = usuarioSIA,
                  InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
                  PeriodoInicial = periodoInicial,
                  PeriodoFinal   = periodoFinal,
                  BD        = bds) 
 
  addEventLog(agente, log.01.01)
  addEventLog(agente, log.01.02)
  
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
  
  addEventLog(agente, log.02.01) 
  return(eb)
}


interrogateAgent <- function(agente){
  setConstantVars(agente)
  
  eb <- createBucket(agente)
  
  addEventLog(agente, paste0(log.03.01,"-", agente.pid))
  addEventLog(agente, log.03.02)
   
  eb <- layer0(agente, eb)
  
    if (nrow(eb) > 0) {
      if ((eb %>% pull(Cod)) %in% c(101,102)) { 
        addEventLog(agente, paste0(log.03.03))
        return(eb)
    } else {
      addEventLog(agente, log.03.04)
    }
  } else {
    addEventLog(agente, log.03.04)
  }
  
  #estructura de columnas
  addEventLog(agente, log.03.05)
  eb <- layer1(agente, eb)
  
    if (nrow(eb) > 0) {
      
      if ((eb %>% pull(Cod)) %in% c(201,202)) { 
      
      n <- eb %>% filter(Cod %in% c(201,202)) %>% nrow()
      addEventLog(agente, log.03.06)
      } else {
        addEventLog(agente, log.03.07)
      }
    } else {
      addEventLog(agente, log.03.07)
    }
  
  # errores OM 22269-2020
  addEventLog(agente, log.03.08)

    eb <- layer2(agente, eb)

    if (nrow(eb) > 0) {
      if (nrow(eb %>% filter(Cod %in% c(311:478)) > 0)) {
        addEventLog(agente, log.03.09)
      } else {
        addEventLog(agente, log.03.10)
        }
      } else {
        addEventLog(agente, log.03.10)
      }

  # eb <- layer3(agent, eb) #alertas ad-hoc 11356
   
  return(eb)
}

setConstantVars <- function(agente) {

  agente.nombrecoopac <<-  agente$NombreCoopac
  agente.carpeta <<-  agente$Carpeta
  agente.idcoopac <<-  agente$Coopac
  agente.pid  <<-  agente$IdProceso
  agente.usuario <<-  agente$Usuario
  agente.inicioproceso  <<-  agente$PeriodoInicial
  agente.finproceso  <<-  agente$PeriodoFinal
  agente.BD <<-  agente$BD  
  
}

closeAgent       <- function(agente, 
                             eb){
  agente <- agente %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(eb),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal)
  
  addEventLog(agente, log.04.01)
  return(agente)
}



log.01.01 <- paste0("Validador SIA 1.3.2021 --------------------------------------")
log.01.02 <- paste("Agente creado.")

log.02.01 <- paste0("Bucket de errores creado.")

log.03.01 <- paste0("Inicio del interrogatorio. PID")
log.03.02 <- paste0("Apertura de revisión de pre-requisitos.")
log.03.03 <- paste0("   Fin del proceso de revisión por errores críticos 101-102.")
log.03.04 <- paste0("   Revisión de pre-requisitos satisfactoria.")
log.03.05 <- paste0("Apertura de revisión de estructura de datos.")
log.03.06 <- paste0("   La revisión de estructura de datos tiene observaciones. Continuar con discreción.")
log.03.07 <- paste0("   Revisión de estructura de datos satisfatoria.")
log.03.08 <- paste0("Apertura de revisión de errores OM 22269-2020.")
log.03.09 <- paste0("   La revisión errores OM 22269-2020 tiene observaciones.")
log.03.10 <- paste0("   La Revisión de errores OM 22269-2020 fue satisfatoria.")

log.04.01 <- paste0("El agente fue cerrado. Revisar los archivos generados. ")



# 

# log.01.01 <- paste0("Superintendencia Adjunta de Cooperativas---------------------")
# log.01.01 <- paste0("-------------------------------------------------------------")
# log.01.01 <- paste0("------------------Validador SIA 1.3.2021 --------------------")
# log.01.01 <- paste0("-------------------------------------------------------------")
# log.01.01 <- paste0("-------------------------------------last release 17/01/2021-") 
# log.03.01 <- paste0("Inicio del interrogatorio PID")
# log.03.02 <- paste0("Apertura de revisión de pre-requisitos.")
# log.03.03 <- paste0("   Fin del proceso de revisión por errores críticos 101-102.")
# log.03.04 <- paste0("   Revisión de pre-requisitos satisfactoria.")
# log.03.05 <- paste0("Apertura de revisión de estructura de datos.")
# log.03.06 <- paste0("   La revisión de estructura de datos tiene observaciones. Continuar con discreción.")
# log.03.07 <- paste0("   Revisión de estructura de datos satisfatoria.")
# log.03.08 <- paste0("Apertura de revisión de errores OM 22269-2020.")
# log.03.09 <- paste0("   La revisión errores OM 22269-2020 tiene observaciones.")
# log.03.10 <- paste0("   La Revisión de errores OM 22269-2020 fue satisfatoria.")
# 
# log.04.01 <- paste0("El agente fue cerrado. Revisar los archivos generados. ")