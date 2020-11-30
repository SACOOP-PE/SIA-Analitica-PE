ejecutar_validacion_layer4 <- function(header, error_bucket){
  
  carpeta   <- getCarpeta(header)
  exigibles <- getArchivos_SinErrores(header, error_bucket, c(201, 203), c("CCR","CCR_C","CODGR"))

  # i. Errores tipo1 ----
  exigibles_tipo1 <- restriccion_archivos_ErroresLayer4(header, error_bucket, exigibles, "tipo1")
  error_bucket_i <- error_bucket
   for (i in 1:length(exigibles_tipo1)){
    ruta_i         <- getRuta(getCarpeta(header), exigibles_tipo1[i])
    error_bucket_i <- get_errorTipo1(ruta_i, error_bucket_i)
    }
   error_bucket <- error_bucket_i %>% group_by(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion) %>%
                         summarise(Detalle = toString(Detalle)) %>%
                         ungroup()
  
  # ii. Errores tipo2 ----
  tb_main <- tibble(Nombre_archivo = exigibles) %>% rowwise() %>%
     mutate(Ruta    = getRuta(carpeta, Nombre_archivo),
            Periodo = getAnoMes(Ruta),
            BDCC    = getBD(Ruta))
  tb_error2 <- tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
    mutate(verif_saldos       = vf_saldos_negativos(Ruta) %>% toString(),
           verif_esam         = realizar_pasteCondicional_2(Ruta,vf_PerNum_esam(Ruta)),
           verif_morg         = realizar_pasteCondicional_2(Ruta,vf_MontoOtorgado(Ruta)),
           verif_dias_retraso = realizar_pasteCondicional_2(Ruta,vf_KVI_KJU_dias(Ruta)))
  
  error_saldos_negativos <- tb_error2 %>% filter(verif_saldos != "") %>% pull(verif_saldos)
  error_esam             <- (paste(tb_error2 %>% rowwise() %>% pull(verif_esam), collapse = ",") %>% strsplit(","))[[1]]
  error_morg             <- (paste(tb_error2 %>% rowwise() %>% pull(verif_morg), collapse = ",") %>% strsplit(","))[[1]]
  error_dias_retraso     <- (paste(tb_error2 %>% rowwise() %>% pull(verif_dias_retraso), collapse = ",") %>% strsplit(","))[[1]]
  
    if (length(error_saldos_negativos) >0){
      error_bucket <- error_bucket %>%
        addError(461, getDescError(461), error_saldos_negativos %>% toString())
      }
    if (length(error_esam[error_esam != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(462, getDescError(462), (error_esam[error_esam != "character(0)"]) %>% toString())
      }
    if (length(error_morg[error_morg != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(463, getDescError(463), (error_morg[error_morg != "character(0)"]) %>% toString())
      }
    if (length(error_dias_retraso[error_dias_retraso != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(464, getDescError(464), (error_dias_retraso[error_dias_retraso != "character(0)"]) %>% toString())
      }
  
  error_docInd <- (tb_main %>% filter(BDCC %in% c("BD01","BD04")) %>% rowwise() %>%
                     mutate(verif_doc = vf_docInd(Ruta)) %>% rowwise() %>%
                     pull(verif_doc) %>% 
                     paste(collapse = ",") %>%
                     strsplit(","))[[1]]
  
    if (length(error_docInd[error_docInd != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(465, getDescError(465), (error_docInd[error_docInd != "character(0)"]) %>% toString())
      }
  
  error_ncr <- (tb_main %>% filter(BDCC == "BD03A") %>% rowwise() %>%
                  mutate(verif_ncr = realizar_pasteCondicional_2(Ruta, vf_NCR(Ruta))) %>% rowwise() %>%
                  pull(verif_ncr) %>% 
                  paste(collapse = ",") %>%
                  strsplit(","))[[1]]
  
    if (length(error_ncr[error_ncr != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(466, getDescError(466), (error_ncr[error_ncr != "character(0)"]) %>% toString())
      }
  
  error_cis <- (tibble(Periodo = restriccion_periodos(error_bucket, "BD01", "BD03A", "CIS")) %>% rowwise() %>%
                  mutate(verif_cis = vf_codDeudor(carpeta, Periodo, "BD03A","BD01")) %>% rowwise() %>%
                  pull(verif_cis) %>% 
                  paste(collapse = ",") %>%
                  strsplit(","))[[1]] 

    if (length(error_cis[error_cis != "character(0)"]) > 0){
      error_bucket <- error_bucket %>%
        addError(467, getDescError(467), (error_cis[error_cis != "character(0)"]) %>% toString())
      }
    
  # iii. Errores tipo3 ----
  exigibles_tipo3 <- restriccion_archivos_ErroresLayer4(header, error_bucket, exigibles[str_detect(exigibles, paste(c("BD01","BD02A","BD02B","BD04"), collapse = '|'))], "tipo3")
  error_bucket_ii <- error_bucket
    for (ii in 1:length(exigibles_tipo3)){
      ruta_ii         <- getRuta(getCarpeta(header), exigibles_tipo3[ii])
      error_bucket_ii <- get_errorTipo3(ruta_ii, error_bucket_ii)
    }
    error_bucket <- error_bucket_ii %>% group_by(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion) %>% 
                          summarise(Detalle = toString(Detalle)) %>%
                          ungroup()
  
  error_fechaDesembolso <- tb_main %>% filter(BDCC == "BD01") %>% rowwise() %>%
    mutate(verif_fechaDesembolso = vf_fechaDesembolso(Ruta)) %>%
    filter(verif_fechaDesembolso != "") %>%
    select(Nombre_archivo, verif_fechaDesembolso)
  
    if (nrow(error_fechaDesembolso) >0){
      error_bucket <- error_bucket %>%
        addError(479, getDescError(479), error_fechaDesembolso %>% apply(1, paste0 , collapse ="$") %>% toString())
      }
  
  print(paste0("El layer 4 terminó: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(error_bucket)
}