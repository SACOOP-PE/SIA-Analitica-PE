

interrogateAgent <- function(agente){
  setConstantVars(agente)
  
  headLog()
  
  eb <- createBucket(agente)
  
  # addEventLog(agente, paste0(log.03.01,"-", agente.pid))
  # addEventLog(agente, log.03.02)
   
  eb <- layer0(agente, eb)
  
    if (nrow(eb) > 0) {
      if ((eb %>% pull(Cod)) %in% c(101,102)) { 
        # addEventLog(agente, paste0(log.03.03))
        return(eb)
    } else {
      # addEventLog(agente, log.03.04)
    }
  } else {
    # addEventLog(agente, log.03.04)
  }
  
  #estructura de columnas
  # addEventLog(agente, log.03.05)
  eb <- layer1(agente, eb)
  
    if (nrow(eb) > 0) {
      
      if ((eb %>% pull(Cod)) %in% c(201,202)) { 
      
      n <- eb %>% filter(Cod %in% c(201,202)) %>% nrow()
      # addEventLog(agente, log.03.06)
      } else {
        # addEventLog(agente, log.03.07)
      }
    } else {
      # addEventLog(agente, log.03.07)
    }
  
  # errores OM 22269-2020
  # addEventLog(agente, log.03.08)

    eb <- layer2(agente, eb)

    if (nrow(eb) > 0) {
      if (nrow(eb %>% filter(Cod %in% c(311:478)) > 0)) {
        # addEventLog(agente, log.03.09)
      } else {
        # addEventLog(agente, log.03.10)
        }
      } else {
        # addEventLog(agente, log.03.10)
      }

    eb <- layer3(agente, eb)
    eb <- layer4(agente, eb)
   
  return(eb)
}


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

headLog <- function() {
  
  log.01.01 <- paste0(padleft("Superintendencia Adjunta de Cooperativas"))
  log.01.02 <- paste0(padleft(""))
  log.01.03 <- paste0(padboth("REPORTE DE REVISIÓN AUTOMÁTICA SIA 1.4.2021"))
  log.01.04 <- paste0(padleft(""))
  log.01.05 <- paste0(padleft(paste0("Usuario:",agente.usuario)))
  log.01.06 <- paste0(padleft(paste0("Repositorio: ",agente.carpeta)))
  log.01.07 <- paste0(padright(paste0("INSTITUCIÓN: ",agente.nombrecoopac," (",agente.idcoopac,")")))
  log.01.08 <- paste0(padright(paste0("PERIODOS EVALUADOS: ",agente.inicioproceso, "-",agente.finproceso)))
  log.01.09 <- paste0(padright(paste0("BASE DE DATOS CREDITICIAS:: ",toString(unlist(agente.BD)))))
  log.01.10 <- paste0(padleft(""))
  
  cat(log.01.01,log.01.02,log.01.03,log.01.04,log.01.05,log.01.06,log.01.07,log.01.08,log.01.09, log.01.10,  sep = "\n")
 
} 


padleft <- function(str){
  return(str_pad(str,120,"left","_"))
}
padright <- function(str){
  return(str_pad(str,120,"right","_"))
}
padboth <- function(str){
  return(str_pad(str,120,"both" ,"_"))
} 


padboth("Diego")