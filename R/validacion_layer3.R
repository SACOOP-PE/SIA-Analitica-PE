ejecutarValidacionLayer3 <- function(header, error_bucket){
  carpeta <- getCarpeta(header)
  
  # i. cuadre contable ----
  exigibles1 <- getArchivos_SinErrores(header, error_bucket, c(201,203), c("KVI","KVE","KRF","KJU"))
  exigibles1 <- exigibles1[str_detect(exigibles1, "BD01")]
  
  tb1 <- tibble(NombreArchivo = exigibles1) %>% rowwise() %>% 
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           Coopac  = as.numeric(getCoopac(Ruta)),
           Periodo = getAnoMes(Ruta),
           KVI_BC   = sum(getCapitalBC(Ruta)[[1]], na.rm=T),
           KVI_BDCC = sum(getCapitalBDCC(Ruta)[[1]], na.rm=T),
           Dif_KVI = sum(getCapitalBDCC(Ruta)[[1]], na.rm=T) - sum(getCapitalBC(Ruta)[[1]], na.rm=T),
           Dif_KVE = sum(getCapitalBDCC(Ruta)[[2]], na.rm=T) - sum(getCapitalBC(Ruta)[[2]], na.rm=T),
           Dif_KRF = sum(getCapitalBDCC(Ruta)[[3]], na.rm=T) - sum(getCapitalBC(Ruta)[[3]], na.rm=T),
           Dif_KJU = sum(getCapitalBDCC(Ruta)[[4]], na.rm=T) - sum(getCapitalBC(Ruta)[[4]], na.rm=T)) %>% 
    pivot_longer(starts_with("Dif"),names_to = "Capital", values_to = "Saldo") %>% rowwise() %>%
    mutate(Resultado   = ifelse(abs(Saldo)>100, "ERROR", ""),
           Coopac      = getCoopac(Ruta),
           Coopac_n    = getNomCoopac(Ruta),
           Carpeta     = getCarpeta(header),
           IdProceso   = error_bucket %>% pull(IdProceso) %>% first(),
           Cod         = ifelse(Resultado == "ERROR",
                                CodErrorCuadreContable(str_split(Capital, "_")[[1]][2]),0),
           Descripcion =  getDescError(Cod)) %>%
    mutate(Detalle     = list(c(NombreArchivo, str_split(Capital,"_")[[1]][2], round(Saldo, digits =2))))  
  
  error_bucket <- bind_rows(error_bucket, tb1 %>% 
                              filter(Resultado == "ERROR") %>%  
                              select(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion, Detalle))

  # ii. verificar dups y vacíos ----
  exigibles2 <- getArchivosSinErrores(header, error_bucket, c(201, 203), c("CCR", "CCR_C", "CODGR"))
  
  tb2 <- tibble(NombreArchivo = exigibles2) %>% rowwise() %>% 
    mutate(Ruta           = getRuta(carpeta, NombreArchivo),
           BDCC           = getBD(Ruta),
           Periodo        = getAnoMes(Ruta),
           CodDuplicados = operaciones_duplicadas(Ruta),
           CodVacios_n   = operaciones_vacias(Ruta))
  
  dups   <- (paste(tb2 %>% rowwise() %>% pull(CodDuplicados), collapse = ",") %>% strsplit(","))[[1]]
  vacios <- tb2 %>% filter(CodVacios_n != 0) %>% select(NombreArchivo, CodVacios_n)
  
  if(length(dups[dups != "character(0)"]) > 0){
    error_bucket <- error_bucket %>%
      addError(311, getDescError(311), (dups[dups != "character(0)"]) %>% toString())
  }
  if(nrow(vacios) >0){
    error_bucket <- error_bucket %>%
      addError(312, getDescError(312), vacios %>% apply(1, paste0 , collapse ="=") %>% toString())
  }
  
  # iii. cruces BD01/BD02A, BD03A/BD03B ----
  cruce1 <- tibble(Periodo = restriccionPeriodos(error_bucket, "BD01", "BD02A", c("CCR", "CCR_C"))) %>% rowwise() %>%
    mutate(OpFaltantes_BD01  = realizarCruce(carpeta, Periodo, "BD02A", "BD01"),
           OpFaltantes_BD02A = realizarCruce(carpeta, Periodo, "BD01", "BD02A"))
  
  cruce2 <- tibble(Periodo = restriccionPeriodos(error_bucket, "BD03A", "BD03B", "CODGR")) %>% rowwise() %>%
    mutate(GaranFaltantes_BD03A = realizarCruce(carpeta, Periodo, "BD03B", "BD03A"))
  
  f_bd01  <- (paste(cruce1 %>% rowwise() %>% pull(OpFaltantes_BD01)    , collapse = ",") %>% strsplit(","))[[1]]
  f_bd02A <- (paste(cruce1 %>% rowwise() %>% pull(OpFaltantes_BD02A)   , collapse = ",") %>% strsplit(","))[[1]]
  f_bd03A <- (paste(cruce2 %>% rowwise() %>% pull(GaranFaltantes_BD03A), collapse = ",") %>% strsplit(","))[[1]]
  
  if(length(f_bd01 [f_bd01  != "character(0)"]) > 0){
    error_bucket <- error_bucket %>%
      addError(321,getDescError(321), (f_bd01[f_bd01 != "character(0)"]) %>% toString())
  }
  if(length(f_bd02A[f_bd02A != "character(0)"]) > 0){
    error_bucket <- error_bucket %>%
      addError(322,getDescError(322), (f_bd02A[f_bd02A != "character(0)"]) %>% toString())
  }
  if(length(f_bd03A[f_bd03A != "character(0)"]) > 0){
    error_bucket <- error_bucket %>%
      addError(323,getDescError(323), (f_bd03A[f_bd03A != "character(0)"]) %>% toString())
  }
  
  print(paste0("El layer 3 terminó: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(error_bucket)
}
