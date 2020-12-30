ejecutarValidacionLayer4 <- function(header, errorBucket){
  carpeta   <- getCarpeta(header)
  exigibles <- getArchivosSinErrores(header, errorBucket, c(201, 203), c("CCR","CCR_C","CODGR"))
  tb_main <- tibble(NombreArchivo = exigibles) %>% rowwise() %>%
    mutate(Ruta    = getRuta(carpeta, NombreArchivo),
           Periodo = getAnoMes(Ruta),
           BDCC    = getBD(Ruta))

  # i. Errores tipo1 ----
  exigiblesT1   <- restriccionArchivosErroresLayer4(header, errorBucket, exigibles, "tipo1")
  errorBucket_i <- errorBucket
  
   for (i in 1:length(exigiblesT1)){
     ruta_i        <- getRuta(carpeta, exigiblesT1[i])
     errorBucket_i <- procesarErroresT1(ruta_i, errorBucket_i)
    }
   errorBucket <- errorBucket_i %>% group_by(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion) %>%
                         summarise(Detalle = toString(Detalle)) %>%
                         ungroup()
  
  # ii. Errores tipo2 ----
  tb_error2 <- tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
    mutate(vf_Saldos = procesarErrorSaldosNegativos(Ruta, errorBucket) %>% toString())
  
  error461 <- tb_error2 %>% filter(vf_Saldos != "") %>% pull(vf_Saldos)
  
  error462 <- paste(procesarErroresT2(errorBucket, exigibles, 462), sep= ",")
  error463 <- paste(procesarErroresT2(errorBucket, exigibles, 463), sep= ",")
  error464 <- paste(procesarErroresT2(errorBucket, exigibles, 464), sep= ",")
  error465 <- paste(procesarErroresT2(errorBucket, exigibles, 465), sep= ",")
  error466 <- paste(procesarErroresT2(errorBucket, exigibles, 466), sep= ",")
 
  error467 <- (tibble(Periodo = restriccionPeriodos(errorBucket, "BD01", "BD03A", "CIS")) %>% rowwise() %>%
                       mutate(vf_CodDeudor = procesarErrorcodDeudor(carpeta, Periodo, "BD03A","BD01")) %>% rowwise() %>%
                       pull(vf_CodDeudor) %>% 
                       paste(collapse = ",") %>%
                       strsplit(","))[[1]]
  
    if (length(error461) > 0){
      errorBucket <- errorBucket %>%
        addError(461, getDescError(461), error461 %>% toString())
      }
    if (length(error462[error462 != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(462, getDescError(462), (error462[error462 != "character(0)"]) %>% toString())
      }
    if (length(error463[error463 != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(463, getDescError(463), (error463[error463 != "character(0)"]) %>% toString())
      }
    if (length(error464[error464 != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(464, getDescError(464), (error464[error464 != "character(0)"]) %>% toString())
      }
    if (length(error465[error465 != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(465, getDescError(465), (error465[error465 != "character(0)"]) %>% toString())
      }
    if (length(error466[error466 != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(466, getDescError(466), (error466[error466 != "character(0)"]) %>% toString())
      }
    if (length(error467[error467 != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(467, getDescError(467), (error467[error467 != "character(0)"]) %>% toString())
      }
    
  # iii. Errores tipo3 ----
  exigiblesT3    <- restriccionArchivosErroresLayer4(header,
                                                     errorBucket, 
                                                     exigibles[str_detect(exigibles, paste(c("BD01","BD02A","BD02B","BD04"), collapse = '|'))],
                                                     "tipo3")
  errorBucket_ii <- errorBucket
    for (ii in 1:length(exigiblesT3)){
      ruta_ii        <- getRuta(carpeta, exigiblesT3[ii])
      errorBucket_ii <- procesarErroresT3(ruta_ii, errorBucket_ii)
    }
    errorBucket <- errorBucket_ii %>% group_by(Coopac, NombCoopac, Carpeta, IdProceso, Cod, Descripcion) %>% 
                          summarise(Detalle = toString(Detalle)) %>%
                          ungroup()
  
  errorFechaDesembolso <- (tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
                             mutate(vf_FechaDesembolso = generarDetalleError2(Ruta, procesarErrorFechaDesembolso(Ruta))) %>% rowwise() %>%
                             pull(vf_FechaDesembolso) %>% 
                             paste(collapse = ",") %>%
                             strsplit(","))[[1]]
  
    if (length(errorFechaDesembolso[errorFechaDesembolso != "character(0)"]) > 0){
      errorBucket <- errorBucket %>%
        addError(479, getDescError(479), (errorFechaDesembolso[errorFechaDesembolso != "character(0)"]) %>% toString())
      }
  
  print(paste0("El layer 4 terminó: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(errorBucket)
}