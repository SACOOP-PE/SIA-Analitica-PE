
saveResults      <- function(header, errorBucket){
  ## header ----
  header %>% 
    write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_header.csv"))
  ## errorbucket ----
  errorBucket %>%
    mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = "; "))) %>%
    write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_errorbucket.csv"))
}

generarBucketErroresFinal1   <- function(codigoError){
  tblError <- tibble(Codigo = codigoError,
                     DetalleError =(listaErrores %>% filter(Cod == Codigo) %>%
                                      pull(Detalle) %>% str_split(","))[[1]],
                     Descripcion  = getDescError(Codigo),
                     Periodo      = str_extract(DetalleError, paste(alcanceGeneral, collapse = '|'))) %>%
    rowwise() %>% 
    mutate(Archivo = str_extract(DetalleError,
                                 getArchivosExigibles(header))[is.na(str_extract(DetalleError,
                                                                                 getArchivosExigibles(header))) == FALSE],
           BDCC          =(basename(Archivo) %>% strsplit("_"))[[1]][2],
           Cols_Creditos = gsub("\\$","",str_split(DetalleError, pattern = "txt")[[1]][2])
    ) %>% 
    select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos)
  
  return(tblError)
}
generarBucketErroresFinal2 <- function(codigoError){
  if (codigoError == 321 |
      codigoError == 322 | codigoError == 323 | codigoError == 467){
    
    tblError <- tibble(Codigo = codigoError,
                       DetalleError =  listaErrores %>% filter(Cod == Codigo) %>% pull(Detalle) %>%
                         strsplit(split = ")") %>% unlist(),
                       Descripcion   = getDescError(Codigo),
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
             nErrores = str_split(Cols_Creditos, pattern = ",") %>% unlist() %>% length()
      ) %>%
      select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos, nErrores)
    
    return(tblError)
  }
  
  tblError <- tibble(Codigo = codigoError,
                     DetalleError =  listaErrores %>% filter(Cod == Codigo) %>% pull(Detalle) %>%
                       strsplit(split = ")") %>% unlist(),
                     Descripcion   = getDescError(Codigo),
                     Periodo      = str_extract(DetalleError, paste(alcanceGeneral, collapse = '|'))) %>%
    rowwise() %>%
    mutate(Archivo = str_extract(DetalleError,
                                 getArchivosExigibles(header))[is.na(str_extract(DetalleError,
                                                                                 getArchivosExigibles(header))) == FALSE] %>%
             toString(),
           BDCC     = (basename(Archivo) %>% strsplit("_"))[[1]][2],
           Cols_Creditos = unlist(str_split(gsub("\\(", "",gsub(Archivo, "", DetalleError)),
                                            pattern = ","))[unlist(str_split(gsub("\\(", "",gsub(Archivo, "",DetalleError)), pattern = ","))!= ""] %>%
             toString(),
           nErrores = str_split(Cols_Creditos, pattern = ",") %>% unlist() %>% length()
    ) %>%
    select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos, nErrores)
  
  return(tblError)
}
procesarUlimaListaErrores <- function(listaErrores){
  codigosErroresCols <- listaErrores %>% filter(Cod %in% c(201, 202, 203)) %>% pull(Cod)
  codigosErroresCred <- listaErrores %>% pull(Cod) %>% setdiff(codigosErroresCols)
  
  tblError <- generarBucketErroresFinal1(codigosErroresCols[1])
  
  for (i in 1:length(codigosErroresCols)) {
    tblError_i <- generarBucketErroresFinal1(codigosErroresCols[i])
    tblError <- bind_rows(tblError, tblError_i)
  }
  tblError <- tblError %>% 
    group_by(Codigo, Descripcion, Periodo, BDCC, Archivo) %>% 
    summarise(Cols_Creditos = toString(Cols_Creditos)) %>% 
    mutate(nErrores = str_split(Cols_Creditos, ",")[[1]] %>% length()) %>% 
    ungroup() %>% 
    select(Codigo, Descripcion, Periodo, BDCC, Archivo, Cols_Creditos, nErrores) 
  
  for (i in 1:length(codigosErroresCred)) {
    tblError_i <- generarBucketErroresFinal2(codigosErroresCred[i])
    tblError   <- bind_rows(tblError, tblError_i)
  }
  
  tblError %>% 
    write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_listaErroresFinal.csv"))
  
  return(tblError)
}
