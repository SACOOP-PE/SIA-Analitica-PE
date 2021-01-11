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
  
  addEventLog(agent, paste0("Se da inicio al proceso ", agent %>% pull(IdProceso) %>% first(),". ~ Validador SIA 1.2.2021"), "I", "B")
  return(agent)
}

createBucket <- function(agent){
  tibble(CodCoopac     = agent %>% pull(Coopac) %>% first(),
         IdProceso  = agent %>% pull(IdProceso) %>% first(), 
         Cod         = 100,
         Periodo = "",
         BD = "",
         txt1 = "",
         txt2 = "",
         txt3 = "",
         num1 = 0,
         num2 = 0,
         num3 = 0) %>% return()
}

interrogateAgent   <- function(agent) {
  eb <- createBucket(agent)
  
  eb <- layer0(agent, eb) #pre-requisitos
  
  if (nrow(eb) > 0) {
    if ((eb %>% pull(Cod)) %in% c(101,102)) {
      print("Resultado final - El proceso se interrumpió debido a que no se cumplen los pre requisitos. Elaborar el PY. DE OFICIO y remitirlo al analista.")
      return(eb)
    }
  }

  #
  eb <- layer1(agent, eb) #estructura de columnas
  
  if (nrow(eb) > 0) {
    if ((eb %>% pull(Cod)) %in% c(201,202)) {
       print("Resultado final - El proceso se interrumpió debido a que hay columnas sobrantes o faltantes. Elaborar el CORREO y remitirlo al analista.")
       return(eb)
    }
  print("pass")
  
  #eb <- layer2a(agent,eb)
  }
  
 
  #eb <- layer2(agent, eb) #errores OM 22269-2020
  # eb <- layer3(agent, eb) #alertas ad-hoc 11356
  
  #Aqui estaba "closeAgent(eb)" lo saque por que solo retorna elbucket, no retorna el agent, por eso lo saco al .R test-agent.R 
  #saveResults deprecated! 
   
  return(eb)
}

closeAgent   <- function(agent, eb){
  agent <- agent %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(eb),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal) %>%
    return()
}

 

