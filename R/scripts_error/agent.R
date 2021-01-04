create_agent  <- function(idCoopac, 
                          coopacCarpeta, 
                          periodoInicial, 
                          periodoFinal, 
                          bds = c("BD01","BD02A","BD02B","BD03A","BD03B","BD04")){
  round((runif(1,0,2) * 1000000),0) %>%   
    tibble(Coopac       = idCoopac,
           NombreCoopac = initCuadreContable() %>% 
             filter(CODIGO_ENTIDAD == as.integer(idCoopac)) %>%
             pull(ENTIDAD) %>% first(),
           Carpeta      = coopacCarpeta,
           IdProceso    = .,
           InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
           PeriodoInicial = periodoInicial,
           PeriodoFinal   = periodoFinal,
           Alcance        = bds) %>% return() 
}

create_bucket <- function(agent){
  tibble(Coopac     = agent %>% pull(Coopac) %>% first(),
         NombCoopac = agent %>% pull(NombreCoopac) %>% first(),
         Carpeta    = agent %>% pull(Carpeta) %>% first(),
         IdProceso  = agent %>% pull(IdProceso) %>% first(), 
         Cod         = 100,
         Descripcion = "Lorem ipsum ... ",
         Detalle     = list(c("1", "3", "2"))) %>% return()
}

interrogate <- function(agent) {
  eb <- create_bucket(agent)
  eb <- layer0(agent, eb) #pre-requisitos
  
  if ((eb %>% pull(Cod)) %in% c(101,102)) {
    return(eb)
  }
  # 
  eb <- layer1(agent, eb) #estructura de columnas
  
  eb <- layer2(agent, eb) #errores OM 22269-2020
  #eb <- layer3(agent, eb) #alertas ad-hoc 11356 
  return(eb)
}

close_agent <- function(agent, error_bucket) {
  agent <- agent %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(error_bucket),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal) %>%
    return()
}