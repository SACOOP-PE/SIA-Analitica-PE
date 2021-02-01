createAgent  <- function(idCoopac, 
                         periodoInicial, 
                         periodoFinal, 
                         carpeta = default.carpeta, 
                         usuario = default.usuario){
  
agente <- tibble(Coopac      = idCoopac,
                 NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                 Carpeta      = carpeta,
                 IdProceso    = getNextIdProceso(getLogObject("logging/log.txt")),
                 Usuario      = usuario,
                 InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
                 PeriodoInicial = periodoInicial,
                 PeriodoFinal   = periodoFinal,
                 Alcance        = list(default.bd)) 

addEventLog(agente, paste0("---------------------------- VALIDADOR SIA 1.4.2021 --------------------------"))
addEventLog(agente, paste0("Coopac evaluada: ",idCoopac,"-", agente %>% pull(NombreCoopac) %>% first()))
addEventLog(agente, paste0("PID: ", agente %>% pull(IdProceso) %>% first(),
                          "[", periodoInicial, "~", periodoFinal, "]"))
  

  return(agente)
}

createBucket <- function(agente){
  eb <- tibble(CodCoopac = agente %>% pull(Coopac) %>% first(),
               IdProceso  = agente %>% pull(IdProceso) %>% first(), 
               Cod = 100,
               Periodo = "",
               BD = "",
               txt1 = "",
               txt2 = "",
               txt3 = "",
               num1 = 0,
               num2 = 0,
               num3 = 0) 
  return(eb)
}

interrogateAgent_mod1 <- function(agente){
  eb <- createBucket(agente)
   
  addEventLog(agente, paste0("1. MÓDULO DE VALIDACIÓN DE BASE DE DATOS CREDITICIAS --------------------------")) 
  
  addEventLog(agente, paste0("Inicio del interrogatorio. PID-", agente %>% pull(IdProceso) %>% first(),"."))
  
  #layer0 ----
  addEventLog(agente, paste0("Layer 0. Revisión de pre-requisitos."))
  
    eb <- layer0(agente, eb)
  
    if (nrow(eb) > 0) {
        addEventLog(agente, paste0("      Resultado: Fin del proceso de revisión por errores críticos 101-102."))
        return(eb)
    }
    else { 
        addEventLog(agente, paste0("      Resultado: Revisión de pre-requisitos satisfactoria."))
    }
  
  #layer1 ----
  addEventLog(agente, paste0("Layer 1. Revisión de estructura de tablas"))
    
    eb <- layer1(agente, eb)
  
    if (nrow(eb) > 0) {
      
      if (nrow(eb %>% filter(Cod %in% c(201, 202))) >0) { 
      
      n <- eb %>% filter(Cod %in% c(201,202)) %>% nrow()
      addEventLog(agente, paste0("      Resultado: La revisión de estructura de datos tiene observaciones. Continuar con discreción."))
        
      }
      else {
        addEventLog(agente, paste0("      Resultado: Revisión de estructura de datos satisfatoria."))
      }
      
    }
    else {
      addEventLog(agente, paste0("      Resultado: Revisión de estructura de datos satisfatoria."))
    }
  
    #layer2 ----
    addEventLog(agente, paste0("Layer 2. Validación de operaciones duplicadas."))
    eb <- layer2(agente, eb)
    
    #layer3 ----
    addEventLog(agente, paste0("Layer 3. Validación de entre BD01/BD02A, BD03A/BD03B."))
    eb <- layer3(agente, eb)
    
    #layer4 ----
    addEventLog(agente, paste0("Layer 4. Validación de campos indviduales."))
    eb <- layer4(agente, eb)

    if (nrow(eb) > 0) {
      
      if (nrow(eb %>% filter(Cod %in% c(301:708))) >0) {

        addEventLog(agente, paste0("      Resultado: La revisión errores OM 22269-2020 tiene observaciones."))
      }
      else {
        addEventLog(agente, paste0("      Resultado: Revisión de errores OM 22269-2020 fue satisfatoria."))
      }
      
    }
    else {
      addEventLog(agente, paste0("      Resultado: Revisión de errores OM 22269-2020 fue satisfatoria."))
    }
  
  eb <- eb %>% arrange(Periodo, Cod)
  return(eb)
}

closeAgent <- function(agente,
                       eb){
  agente <- agente %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(eb),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal)
  
  return(agente)
}