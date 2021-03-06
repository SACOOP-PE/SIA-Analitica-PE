createAgent  <- function(idCoopac, 
                         periodoInicial, 
                         periodoFinal, 
                         carpeta = default.carpeta, 
                         usuario = default.usuario){
  
agente <- tibble(Coopac       = idCoopac,
                 NombreCoopac = getNombreCoopacFromIdCoopac(Coopac),
                 NivelCoopac  = getNivelCoopacFrpomIdCoopac(Coopac),
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
   
  addEventLog(agente, paste0("1. M�DULO DE VALIDACI�N DE BASE DE DATOS CREDITICIAS --------------------------")) 
  addEventLog(agente, paste0("Inicio del interrogatorio modulo 1. PID-", agente %>% pull(IdProceso) %>% first(),"."))
  
  #layer0 ----
    addEventLog(agente, paste0("Layer 0. Revisi�n de pre-requisitos."))
    eb <- layer0(agente, eb)
  
    if (nrow(eb) > 0) {
        addEventLog(agente, paste0("      Resultado: Fin del proceso de revisi�n por errores cr�ticos 101-102."))
        return(eb)
    }
    else { 
        addEventLog(agente, paste0("      Resultado: Revisi�n de pre-requisitos satisfactoria."))
    }
  
  #layer1 ----
    addEventLog(agente, paste0("Layer 1. Revisi�n de estructura de tablas"))
    eb <- layer1(agente, eb)
  
  #layer2 ----
    addEventLog(agente, paste0("Layer 2. Validaci�n de Cruce Contable a la cartera."))
    eb <- layer2(agente, eb)
    
  #layer3 ----
    addEventLog(agente, paste0("Layer 3. Validaci�n de consistencia de operaciones."))
    eb <- layer3(agente, eb)
    
  #layer4 ----
    addEventLog(agente, paste0("Layer 4. Validaci�n de campos indviduales."))
    eb <- layer4(agente, eb)
    
    
  #Fin validaci�n ----
    eb <- eb %>% arrange(Periodo, Cod)
    return(eb)
}
interrogateAgent_mod2 <- function(agente, eb){

  addEventLog(agente, paste0("2. M�DULO DE DECTECCI�N DE ALERTAS DE BASE DE DATOS CREDITICIAS --------------------------"))
  addEventLog(agente, paste0("Inicio del interrogatorio modulo 2. PID-", agente %>% pull(IdProceso) %>% first(),"."))

  if (nrow(eb) > 0) {
    #layer0 ----
    addEventLog(agente, paste0("Layer 1. Revisi�n alertas Regulatorias"))
    eb <- layer0_Alertas(agente, eb) %>% filter(Cod %in% c(1000:2026)) %>% arrange(Periodo, Cod)
    return(eb)
  }
  return(eb)
}
interrogateAgent_mod3 <- function(agente, eb){
  
  addEventLog(agente, paste0("3. M�DULO DE AN�LISIS A LA CARTERA DE CR�DITOS (BD01) --------------------------")) 
  
  addEventLog(agente, paste0("Inicio del interrogatorio modulo 3. PID-", agente %>% pull(IdProceso) %>% first(),"."))
    
  #layer0 ----
    addEventLog(agente, paste0("Layer 1. An�lisis Cartera y Cartera Cancelada."))
    resultado <- layer0_Analisis(agente) %>% 
      return()
}

closeAgent <- function(agente,
                       eb){
  agente <- agente %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(eb),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NivelCoopac,NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal)
  
  return(agente)
}