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

create_bucket <- function(agente){
  tibble(Coopac     = agente %>% pull(Coopac) %>% first(),
         NombCoopac = agente %>% pull(NombreCoopac) %>% first(),
         Carpeta    = agente %>% pull(Carpeta) %>% first(),
         IdProceso  = agente %>% pull(IdProceso) %>% first(), 
         Cod         = 100,
         Descripcion = "Lorem ipsum ... ",
         Detalle     = list(c("1", "3", "2"))) %>% return()
}
interrogate   <- function(agente) {
  eb <- create_bucket(agente)
  eb <- layer0(agent, eb) #pre-requisitos

  if ((eb %>% pull(Cod)) %in% c(101,102)) {
    return(eb)
  }
  #
  eb <- layer1(agente, eb) #estructura de columnas
  eb <- layer2(agente, eb) #errores OM 22269-2020
  # eb <- layer3(agent, eb) #alertas ad-hoc 11356
  return(eb)
}
close_agent   <- function(agente, errorBucket) {
  agente <- agente %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(errorBucket),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal) %>%
    return()
}

create_bucket2  <- function(errorBucket, codigoError){
  if (codigoError == 201 | codigoError == 202 | codigoError == 203) {
    tblError <- tibble(Codigo = codigoError,
                       DetalleError =(errorBucket %>% filter(Cod == Codigo) %>% pull(Detalle) %>% str_split(","))[[1]],
                       Descripcion  = errorBucket %>% filter(Cod == Codigo) %>% pull(Descripcion),
                       Periodo      = str_extract(DetalleError, paste(alcanceGeneral, collapse = '|'))) %>%
      rowwise() %>% 
      mutate(Archivo = str_extract(DetalleError,
                                   getArchivosExigibles(agente = agent))[is.na(str_extract(DetalleError,
                                                                                   getArchivosExigibles(agente = agent))) == FALSE],
             BDCC          =(basename(Archivo) %>% strsplit("_"))[[1]][2],
             Cols_Creditos = gsub("\\$","",str_split(DetalleError, pattern = "txt")[[1]][2])) %>% 
      select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos)
    
    return(tblError)
    }
  if (codigoError == 321 | codigoError == 322 | codigoError == 323 | codigoError == 467){
    tblError <- tibble(Codigo = codigoError,
                       DetalleError =  errorBucket %>% filter(Cod == Codigo) %>% pull(Detalle) %>%
                         strsplit(split = ")") %>% unlist(),
                       Descripcion   = errorBucket %>% filter(Cod == Codigo) %>% pull(Descripcion),
                       Periodo      = str_extract(DetalleError, paste(alcanceGeneral, collapse = '|'))) %>%
      rowwise() %>%
      mutate(Archivo = "",
             BDCC    = switch(toString(codigoError),
                              "321" = "BD02A",
                              "322" = "BD01",
                              "323" = "BD03A",
                              "467" = "BD03A"),
             Cols_Creditos = unlist(str_split(gsub("\\(", "",gsub(Periodo, "", DetalleError)),
                                              pattern = ","))[unlist(str_split(gsub("\\(", "",gsub(Periodo, "",DetalleError)), pattern = ","))!= ""] %>%
               toString(),
             nErrores = str_split(Cols_Creditos, pattern = ",") %>% unlist() %>% length()) %>%
      select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos, nErrores)
    
    return(tblError)
    }
  else{
    tblError <- tibble(Codigo = codigoError,
                       DetalleError =  errorBucket %>% filter(Cod == Codigo) %>% pull(Detalle) %>%
                         strsplit(split = ")") %>% unlist(),
                       Descripcion  = errorBucket %>% filter(Cod == Codigo) %>% pull(Descripcion),
                       Periodo      = str_extract(DetalleError, paste(alcanceGeneral, collapse = '|'))) %>%
      rowwise() %>%
      mutate(Archivo = str_extract(DetalleError,
                                   getArchivosExigibles(agente = agent))[is.na(str_extract(DetalleError,
                                                                                   getArchivosExigibles(agente = agent))) == FALSE] %>%
               toString(),
             BDCC     = (basename(Archivo) %>% strsplit("_"))[[1]][2],
             Cols_Creditos = unlist(str_split(gsub("\\(", "",gsub(Archivo, "", DetalleError)),
                                              pattern = ","))[unlist(str_split(gsub("\\(", "",gsub(Archivo, "",DetalleError)), pattern = ","))!= ""] %>%
               toString(),
             nErrores = str_split(Cols_Creditos, pattern = ",") %>% unlist() %>% length()) %>%
      select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos, nErrores)
    
    return(tblError)
  }
}
procesarBucket2 <- function(agente, errorBucket){
  codErrores <- errorBucket %>% pull(Cod)
  tblError   <- create_bucket2(errorBucket, codErrores[1])
  
  for (i in 1:length(codErrores)){
    tblError_i <- create_bucket2(errorBucket, codErrores[i])
    tblError   <- bind_rows(tblError, tblError_i)
    }
  tblError <- tblError %>% 
    group_by(Codigo, Descripcion, Periodo, BDCC, Archivo) %>% 
    summarise(Cols_Creditos = toString(Cols_Creditos)) %>% 
    mutate(nErrores = str_split(Cols_Creditos, ",")[[1]] %>% length()) %>% 
    ungroup() %>%
    select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos, nErrores) 
  
  tblError %>%
    write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(agente %>% pull(Coopac),
                           getIdProceso(agente),
                           agente %>% pull(PeriodoInicial),
                           agente %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_errorBucket.csv"))
  
  return(tblError)
}

ejecutarValidador <- function(agente){
  errorBucket  <- interrogate(agente)
  errorBucket2 <- procesarBucket2(agente, errorBucket)
  
  agente <- close_agent(agente, errorBucket2)
  save
}