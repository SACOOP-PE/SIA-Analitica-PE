ejecutar_validacion_layer4 <- function(header, error_bucket){
  
  carpeta   <- getCarpeta(header)
  exigibles <- getArchivos_SinErrores(header, error_bucket, c(201, 203), c("CCR","CCR_C","CODGR"))
  tb_main <- tibble(Nombre_archivo = exigibles) %>% rowwise() %>%
    mutate(Ruta    = getRuta(carpeta, Nombre_archivo),
           Periodo = getAnoMes(Ruta),
           BDCC    = getBD(Ruta))

  # i. Errores tipo1 ----
  exigibles_tipo1 <- restriccion_archivos_ErroresLayer4(header, error_bucket, exigibles, "tipo1")
  error_bucket_i <- error_bucket
   for (i in 1:length(exigibles_tipo1)){
     ruta_i         <- getRuta(getCarpeta(header), exigibles_tipo1[i])
     error_bucket_i <- procesarErroresT1(ruta_i, error_bucket_i)
    }
   error_bucket <- error_bucket_i %>% group_by(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion) %>%
                         summarise(Detalle = toString(Detalle)) %>%
                         ungroup()
  
  # ii. Errores tipo2 ----
  tb_error2 <- tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
    mutate(vf_Saldos             = procesarErrorSaldosNegativos(Ruta) %>% toString(),
           vf_ModalidadPagoCouta = realizar_pasteCondicional_2(Ruta, procesarErrorModalidadCouta(Ruta)),
           vf_MontoOtorgado      = realizar_pasteCondicional_2(Ruta, procesarErrorMontoOtorgado(Ruta)),
           vf_DiasRetraso        = realizar_pasteCondicional_2(Ruta, procesarErrorVencJudRetraso(Ruta)))
  
  errorSaldosNegativos    <- tb_error2 %>% filter(vf_Saldos != "") %>% pull(vf_Saldos)
  errorModalidadPagoCouta <- (paste(tb_error2 %>% rowwise() %>% pull(vf_ModalidadPagoCouta), collapse = ",") %>% strsplit(","))[[1]]
  errorMontoOtorgado      <- (paste(tb_error2 %>% rowwise() %>% pull(vf_MontoOtorgado), collapse = ",") %>% strsplit(","))[[1]]
  errorDiasRetraso        <- (paste(tb_error2 %>% rowwise() %>% pull(vf_DiasRetraso), collapse = ",") %>% strsplit(","))[[1]]
  
    if (length(errorSaldosNegativos) > 0){
      error_bucket <- error_bucket %>%
        addError(461, getDescError(461), errorSaldosNegativos %>% toString())
      }
    if (length(errorModalidadPagoCouta[errorModalidadPagoCouta != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(462, getDescError(462), (errorModalidadPagoCouta[errorModalidadPagoCouta != "character(0)"]) %>% toString())
      }
    if (length(errorMontoOtorgado[errorMontoOtorgado != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(463, getDescError(463), (errorMontoOtorgado[errorMontoOtorgado != "character(0)"]) %>% toString())
      }
    if (length(errorDiasRetraso[errorDiasRetraso != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(464, getDescError(464), (errorDiasRetraso[errorDiasRetraso != "character(0)"]) %>% toString())
      }
  
  errorDocumentoIdent <- (tb_main %>% filter(BDCC %in% c("BD01","BD04")) %>% rowwise() %>%
                            mutate(vf_documento = procesarErrorDocumentoIdent(Ruta)) %>% rowwise() %>%
                            pull(vf_documento) %>% 
                            paste(collapse = ",") %>%
                            strsplit(","))[[1]]
  
    if (length(errorDocumentoIdent[errorDocumentoIdent != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(465, getDescError(465), (errorDocumentoIdent[errorDocumentoIdent != "character(0)"]) %>% toString())
      }
  
  errorCredCobertura <- (tb_main %>% filter(BDCC == "BD03A") %>% rowwise() %>%
                  mutate(vf_CredCobertura = realizar_pasteCondicional_2(Ruta, procesarErrorNumCredCobertura(Ruta))) %>% rowwise() %>%
                  pull(vf_CredCobertura) %>% 
                  paste(collapse = ",") %>%
                  strsplit(","))[[1]]
  
    if (length(errorCredCobertura[errorCredCobertura != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(466, getDescError(466), (errorCredCobertura[errorCredCobertura != "character(0)"]) %>% toString())
      }
  
  errorCodDeudor <- (tibble(Periodo = restriccion_periodos(error_bucket, "BD01", "BD03A", "CIS")) %>% rowwise() %>%
                       mutate(vf_CodDeudor = procesarErrorcodDeudor(carpeta, Periodo, "BD03A","BD01")) %>% rowwise() %>%
                       pull(vf_CodDeudor) %>% 
                       paste(collapse = ",") %>%
                       strsplit(","))[[1]]

    if (length(errorCodDeudor[errorCodDeudor != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(467, getDescError(467), (errorCodDeudor[errorCodDeudor != "character(0)"]) %>% toString())
      }
    
  # iii. Errores tipo3 ----
  exigibles_tipo3 <- restriccion_archivos_ErroresLayer4(header, error_bucket, exigibles[str_detect(exigibles, paste(c("BD01","BD02A","BD02B","BD04"), collapse = '|'))], "tipo3")
  error_bucket_ii <- error_bucket
    for (ii in 1:length(exigibles_tipo3)){
      ruta_ii         <- getRuta(getCarpeta(header), exigibles_tipo3[ii])
      error_bucket_ii <- procesarErroresT3(ruta_ii, error_bucket_ii)
    }
    error_bucket <- error_bucket_ii %>% group_by(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion) %>% 
                          summarise(Detalle = toString(Detalle)) %>%
                          ungroup()
  
  errorFechaDesembolso <- (tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
                             mutate(vf_FechaDesembolso = realizar_pasteCondicional_2(Ruta, procesarErrorFechaDesembolso(Ruta))) %>% rowwise() %>%
                             pull(vf_FechaDesembolso) %>% 
                             paste(collapse = ",") %>%
                             strsplit(","))[[1]]
  
    if (length(errorFechaDesembolso[errorFechaDesembolso != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(479, getDescError(479), (errorFechaDesembolso[errorFechaDesembolso != "character(0)"]) %>% toString())
      }
  
  print(paste0("El layer 4 terminó: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(error_bucket)
}