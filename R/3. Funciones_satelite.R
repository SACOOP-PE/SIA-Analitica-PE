##### 3. Funciones satelite -----
## Archivos precargados ----
init_cuadre_contable     <- function(){
  read_delim(file_cuadre_contable, 
             "\t", escape_double = FALSE, col_types = cols(ANO = col_double(), 
                                                           CODIGO_ENTIDAD = col_double(), ENTIDAD = col_character(), 
                                                           KJU = col_double(), KRF = col_double(), 
                                                           KVE = col_double(), KVI = col_double(), 
                                                           PERIODO = col_double(), TIPOENTIDAD = col_character()), 
             trim_ws = TRUE, progress = T) %>% return()
}
init_estructura_base     <- function(){ 
 read_delim(file_estructura_base, 
            "\t", escape_double = FALSE, col_types = cols(BD = col_character(), 
                                                          CAMPO = col_character(), 
                                                          DESCRIPCION = col_character(),  
                                                          NRO = col_double(), 
                                                          TIPO = col_character()), progress = T)  %>% return()
}
init_repositorio_errores <- function(){
  read_delim(file_repositorio_errores, 
             "\t", escape_double = FALSE, col_types = cols(Cod = col_double(), 
                                                           Descripcion = col_character(), 
                                                           Tipo = col_character()),
             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, progress = T) %>% return()
}

## Archivos de inicializacion----
init_header         <- function(id_coopac, coopac_carpeta, periodo_inicial, periodo_final, bds = c("BD01","BD02A","BD02B","BD03A","BD03B","BD04")){
  round((runif(1,0,2) * 1000000),0) %>%   
    tibble(Coopac       = id_coopac,
           NombreCoopac = init_cuadre_contable() %>% 
                              filter(CODIGO_ENTIDAD == as.integer(id_coopac)) %>%
                              pull(ENTIDAD) %>% first(),
           Carpeta      = coopac_carpeta,
           IdProceso    = .,
           InicioProceso  = format(Sys.time(), "%a %b %d %X %Y"), 
           PeriodoInicial = periodo_inicial,
           PeriodoFinal   = periodo_final,
           Alcance        = bds) %>% return() 
}
init_bucket_errores <- function(header) {
  tibble(Coopac    = header %>% pull(Coopac) %>% first(),
         Coopac_n  =  header %>% pull(NombreCoopac) %>% first(),
         Carpeta   = header %>% pull(Carpeta) %>% first(),
         IdProceso = header %>% pull(IdProceso) %>% first(),
         #Informaci?n temporal
         Cod         = 999999,
         Descripcion = "Lorem ipsum ... ",
         Detalle     = list(c("1", "3", "2"))) %>% return()
}

## Gestion de errores----
getDescError <- function(codigoError){
  if ((length(init_repositorio_errores() %>% filter(Cod == codigoError) %>% pull(Descripcion)) == 0)){
    return("Descripción del error no encontrada")}
  
  init_repositorio_errores() %>% filter(Cod == codigoError) %>% pull(Descripcion) %>% first() %>% return()
} 
deleteError  <- function(error_bucket, arg_codigo) {
  error_bucket %>% filter(Cod != arg_codigo) %>% return()
}
addError     <- function(error_bucket, arg_codigo, arg_descripcion, arg_detalle){ 
  rbind(error_bucket, tibble(Coopac    = error_bucket %>% pull(Coopac) %>% first(),
                             Coopac_n  = error_bucket %>% pull(Coopac_n) %>% first(),
                             Carpeta   = error_bucket %>% pull(Carpeta) %>% first(),
                             IdProceso = error_bucket %>% pull(IdProceso) %>% first(),
                             Cod         = arg_codigo, 
                             Descripcion = arg_descripcion,
                             Detalle     = list(arg_detalle))) %>%
    deleteError(999999) %>% return()
}

finalizarProceso <- function(header, errorBucket){
  header <- header %>% 
    mutate(
      FinProceso = format(Sys.time(), "%a %b %d %X %Y"),
      NroErrores = nrow(errorBucket),
      Tramo      = paste0(PeriodoInicial, ":", PeriodoFinal)) %>% 
    select(Coopac, NombreCoopac, IdProceso, InicioProceso, FinProceso, Tramo, NroErrores, PeriodoInicial, PeriodoFinal) %>%
    return()
}
saveResults      <- function(header, error_bucket){
  ## header ----
  header %>% 
    write.csv(paste0(paste(getwd(), "resultados/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_header.csv"))
  ## errorbucket ----
  error_bucket %>%
    mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
    write.csv(paste0(paste(getwd(), "resultados/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_errorbucket.csv"))
  ## resumen_errores_periodos ----
  error_bucket %>%
    mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
    rowwise() %>%
    mutate(Periodos_error = str_extract(unlist(Detalle %>% str_split(",")),
                                        paste(alcance_general,collapse = '|')
                                        )[is.na(str_extract(unlist(Detalle %>% str_split(",")),
                                                            paste(alcance_general,collapse = '|'))) == FALSE] %>%
             unique() %>% toString()
           ) %>%
    select(Descripcion, Periodos_error) %>% 
    write.csv(paste0(paste(getwd(), "resultados/", sep = "/"),
                     paste(header %>% pull(Coopac),
                           getIdProceso(header),
                           header %>% pull(PeriodoInicial),
                           header %>% pull(PeriodoFinal),
                           sep = "_"),
                     "_resumen_errores_periodos.csv"))
}

## Obtener informacion minima----
getAno    <- function(ruta) {
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    substr(1, 4) %>% return()
}
getMes    <- function(ruta) {
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    substr(5, 6) %>% return()
}
getAnoMes <- function(ruta) {
  strsplit((basename(ruta) %>% strsplit("_"))[[1]][3], ".", fixed = TRUE)[[1]][1] %>%
    return()
}
getCoopac <- function(ruta) {
  (basename(ruta) %>% strsplit("_"))[[1]][1] %>% return()
}
getNomCoopac <- function(ruta) {
  i <- (basename(ruta) %>% strsplit("_"))[[1]][1]
  init_cuadre_contable() %>% filter(CODIGO_ENTIDAD == as.numeric(i)) %>% pull(ENTIDAD) %>% first() %>% return()
}
getBD        <- function(ruta) {
  (basename(ruta) %>% strsplit("_"))[[1]][2] %>% return()
}

## Ly1-Ly2-Ly3-Ly4----
getArchivosExigibles <- function(header){
  cod_coopac <- header %>% pull(Coopac) %>% first()
  id_bds     <- (header %>% pull(Alcance))[[1]]
  periodos   <- alcance_general
  periodo_inicio <- header %>% pull(PeriodoInicial) %>% first()
  periodo_final  <- header %>% pull(PeriodoFinal) %>% first()
  
  apply(expand.grid(cod_coopac, id_bds,
                    paste0(periodos[(periodos >= periodo_inicio) &
                                    (periodos <= periodo_final)], ".txt")),
        1, paste, collapse = "_") %>% return()
}
getCarpeta           <- function(header){ 
  header %>% pull(Carpeta) %>% first() %>% return()
}
getIdProceso         <- function(header){
  header %>% pull(IdProceso) %>% first() %>% return()
}
getRuta              <- function(carpeta, filename){
  lista_rutas <- list.files(path=carpeta, full.names = TRUE, recursive =  TRUE)
  (lista_rutas[str_detect(lista_rutas, filename)])[1]  %>% return()
}
evalFile             <- function(ruta){
  read_delim(ruta,"\t",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE,
             col_types = cols(.default = "c"), progress = T, na = "NA" ) %>% 
    na_if("") %>%
    return()
}
getNombreArchivo    <- function(ruta){
  (basename(ruta) %>% strsplit("/"))[[1]] %>% return()
}

realizar_pasteCondicional_1 <- function(ruta, error){
  paste_error <- ifelse(length(error)>0,
                        list(paste0(getNombreArchivo(ruta), "$", error, collapse=",")),
                        list(character(0)))
  return(paste_error)
}
realizar_pasteCondicional_2 <- function(ruta, error){
  paste_error <- ifelse(length(error)>0,
                        list(paste0(getNombreArchivo(ruta),"(", toString(error),")")),
                        list(character(0)))
  return(paste_error)
}
realizar_pasteCondicional_3 <- function(ruta, columna, error){
  paste_error <- ifelse(length(error)>0,
                        list(paste(getNombreArchivo(ruta), columna, paste0(toString(error), "$"), sep = "$")),
                        list(character(0)))
  return(paste_error)
}
realizar_pasteCondicional_4 <- function(periodo, error_cruce){
  paste_error <- ifelse(length(error_cruce)>0,
                        list(paste0(periodo,"(", toString(error_cruce), ")")),
                        list(character(0)))
}

## Funciones Ly1----
getDuplicados <- function(carpeta, exigibles){ 
  tibble(files = basename(list.files(path=carpeta, full.names = F, recursive =  TRUE))) %>%
    group_by(files) %>%
    filter(files %in% exigibles) %>% 
    filter(n()>1) %>% 
    pull(files) %>% 
    unique() %>%  
    return()
}
getFaltantes  <- function(carpeta, exigibles){
  setdiff(exigibles,
          basename(list.files(path=carpeta, full.names = FALSE, recursive =  TRUE,  include.dirs = FALSE))) %>%
  return()
}

## Funciones Ly2----
getColumnasOM <- function(BDCC){ 
  cols_base <- switch (BDCC,
                       BD01  = {init_estructura_base() %>% filter(BD == "BD01") %>% pull(CAMPO) %>% list()},
                       BD02A = {init_estructura_base() %>% filter(BD == "BD02A") %>% pull(CAMPO) %>% list()},
                       BD02B = {init_estructura_base() %>% filter(BD == "BD02B") %>% pull(CAMPO) %>% list()},
                       BD03A = {init_estructura_base() %>% filter(BD == "BD03A") %>% pull(CAMPO) %>% list()},
                       BD03B = {init_estructura_base() %>% filter(BD == "BD03B") %>% pull(CAMPO) %>% list()},
                       BD04  = {init_estructura_base() %>% filter(BD == "BD04") %>% pull(CAMPO) %>% list()})
  cols_base %>% return()
}
getColVacias  <- function(ruta, BD = evalFile(ruta)){
  cols_vacias <- intersect(BD[sapply(BD, function(x) all(is.na(x)))] %>% colnames(), 
                           getColumnasOM(getBD(ruta)) %>% unlist())
  
  resultado <- realizar_pasteCondicional_1(ruta, cols_vacias)
  return(resultado)
}

## Funciones Ly3----
getCapital     <- function(ruta){
  tmp <- read_delim(ruta,"\t",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE, col_types = cols(.default = "c"), progress = T) 
  
  kvi <- sum(tmp$KVI, na.rm = TRUE)
  kve <- sum(tmp$KVE, na.rm = TRUE)
  krf <- sum(tmp$KRF, na.rm = TRUE)
  kju <- sum(tmp$KJU, na.rm = TRUE) 
  kre <- sum(tmp$KRE, na.rm = TRUE) 
  
  lista_bd <- list(kvi,kve,krf,kju) %>% return() 
}
getCapitalBDCC <- function(ruta){
  tmp <- read_delim(ruta,"\t", escape_double = FALSE, col_types = cols(KJU = col_double(),
                                                                       KRF = col_double(), 
                                                                       KVE = col_double(),
                                                                       KVI = col_double(),
                                                                       .default = "c"), 
                    trim_ws = TRUE, progress = T)  
  
  c(sum(tmp$KVI,na.rm=T),sum(tmp$KVE,na.rm=T),sum(tmp$KRF,na.rm=T),sum(tmp$KJU,na.rm=T)) %>% return()
}
getCapitalBC   <- function(ruta){
  tmp <- init_cuadre_contable() %>% 
    filter(CODIGO_ENTIDAD ==  as.double(getCoopac(ruta)), PERIODO == getAnoMes(ruta))
  
  if (nrow(tmp)==1){ 
    c(sum(tmp$KVI,na.rm=T),sum(tmp$KVE,na.rm=T),sum(tmp$KRF,na.rm=T),sum(tmp$KJU,na.rm=T)) %>% return()
  }
}
CodErrorCuadreContable <- function(dif_Capital){
  codError <- switch (dif_Capital,
                      KVI = 301,
                      KVE = 302,
                      KRF = 303,
                      KJU = 304)
  return(codError)
}

operaciones_duplicadas <- function(ruta){
  if (getBD(ruta) == "BD01" | getBD(ruta) == "BD03A") {
    operaciones <- evalFile(ruta) %>% select(getCodigoBD(getBD(ruta))[1]) 
    duplicados <- operaciones[duplicated(operaciones), ] %>% 
                      unique() %>%
                      pull(getCodigoBD(getBD(ruta))[1])
    
    realizar_pasteCondicional_1(ruta, duplicados) %>% return()}
  list(character(0)) %>% return()
}
operaciones_vacias     <- function(ruta, BD = evalFile(ruta)){
  vacios <- BD %>% 
    select(getCodigoBD(getBD(ruta))[1]) %>%
    sapply(function(x) sum(is.na(x))) %>% return()
}

getInfoTotal  <- function(carpeta, periodo, name_bd){
  ruta_bd <- paste(carpeta,
                   periodo,
                   getArchivosExigibles(header)[str_detect(getArchivosExigibles(header),
                                                           paste0(name_bd, "_", periodo))], 
                   sep = "/")
  
  resultado <- ruta_bd %>% read_delim("\t",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE,
                                      col_types = cols(.default = "c"), progress = T) %>% return()
}
getCodigoBD   <- function(bd){
  campo <- case_when(bd == "BD01"  ~ "CCR",
                     bd == "BD02A" ~ "CCR",
                     bd == "BD02B" ~ "CCR_C",
                     bd == "BD03A" ~ "CODGR",
                     bd == "BD03B" ~ "CODGR",
                     bd == "BD04"  ~ "CCR_C") %>%
    return()
}
getInfoCruce  <- function(carpeta, periodo, name_bd){
  resultado <- getInfoTotal(carpeta, periodo, name_bd) %>%
    pull(getCodigoBD(name_bd)) %>% return()
}
realizarCruce <- function(carpeta, periodo, nameBD1, nameBD2){
  cruce <-  setdiff(getInfoCruce(carpeta, periodo, nameBD1), 
                    getInfoCruce(carpeta, periodo, nameBD2)) %>% unique()
  
  resultado <- realizar_pasteCondicional_4(periodo, cruce)
  return(resultado)
}

## Funciones Ly4----
# Validaciones a campos numéricos específicos de cada BD    (errores tipo1)----
DigitosBD01  <- function(campo){
  digitos <- switch (campo,
                     TID = {c(1,2,3,4,5,6,7)},
                     TCR = {c(6,7,8,9,10,11,12,13,20)},
                     CAL = {c(0,1,2,3,4)},
                     ESAM = {c(1,2,3,4,5)},
                     SEC = {c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,99)},
                     MDCR ={c(1,2,3,4,5,9)},
                     OSD = {c(1,2,3,4,5,6,7,8,9,10,99)})
  return(digitos)
}
DigitosBD02A <- function(campo){
  digitos <- switch (campo,
                     MON = {c(1,2,3)},
                     FOCAN = {c(1,2,3,4,5)})
  return(digitos)
}
DigitosBD02B <- function(campo){
  digitos <- switch (campo,
                     MON_C = {c(1,2,3)},
                     FOCAN_C = {c(1,2,3,4)})
  return(digitos)
}
DigitosBD03A <- function(campo){
  digitos <- switch (campo,
                     CGR = {c(1,2,3,4,5)},
                     COBGR = {c(1,2)},
                     MONGR = {c(1,2,3)})
  return(digitos)
}
DigitosBD03B <- function(campo){
  digitos <- switch (campo,
                     CGR = {c(1,2,3,4,5)})
  return(digitos)
}
DigitosBD04  <- function(campo){
  digitos <- switch (campo,
                     TID_C = {c(1,2,3,4,5,6,7)},
                     TCR_C = {c(6,7,8,9,10,11,12,13,20)},
                     MON_C = {c(1,2,3)},
                     CAL_C = {c(0,1,2,3,4)},
                     ESAM_C = {c(1,2,3,4,5)},
                     FOCAN_C = {c(1,2,3,4,5)},
                     MDCR_C = {c(1,2,3,4,5,9)})
  return(digitos)
}

elegirDigitos      <- function(ruta,campo){
  digitos <- switch (getBD(ruta),
                     BD01  = {DigitosBD01(campo)},
                     BD02A = {DigitosBD02A(campo)},
                     BD02B = {DigitosBD02B(campo)},
                     BD03A = {DigitosBD03A(campo)},
                     BD03B = {DigitosBD03B(campo)},
                     BD04  = {DigitosBD04(campo)})
  return(digitos)
}
ColumnasErrorTipo1 <- function(ruta){
  cols <- switch (getBD(ruta),
                  BD01  = {c("TID","TCR","CAL","ESAM","SEC","MDCR","OSD")},
                  BD02A = {c("MON","FOCAN")},
                  BD02B = {c("MON_C","FOCAN_C")},
                  BD03A = {c("CGR","COBGR")},
                  BD03B = {c("CGR")},
                  BD04  = {c("TID_C","TCR_C","MON_C","CAL_C","ESAM_C","FOCAN_C","MDCR_C")}) 
  return(cols)
}
CodigoErrorTipo1   <- function(ruta, campo){
  codError <- switch (getBD(ruta),
                      BD01  = {c(401,402,403,404,405,406,407)},
                      BD02A = {c(411,412)},
                      BD02B = {c(421,422)},
                      BD03A = {c(431,432)},
                      BD03B = {c(441)},
                      BD04  = {c(451,452,453,454,455,456,457)})
  
  cod <- tibble(col       = ColumnasErrorTipo1(ruta),
                cod_error = codError) %>% 
    filter(col == campo) %>% 
    pull(cod_error)
  return(cod)
}
procesarErroresT1  <- function(ruta, error_bucket){
  BD <- evalFile(ruta)
  tb <- tibble(Columna = depurarColsErrorT1(ruta, error_bucket)) %>% rowwise() %>%
    mutate(Verif_cols = BD %>%
                          filter((as.numeric(cgrep(BD, Columna)[[1]]) %in% elegirDigitos(ruta, Columna)) == FALSE) %>%
                          pull(getCodigoBD(getBD(ruta))) %>% unique() %>% list(),
           resultado = realizar_pasteCondicional_2(ruta, Verif_cols) %>% toString(),
           Coopac    = getCoopac(ruta),
           Coopac_n  = getNomCoopac(ruta),
           Carpeta   = getCarpeta(header),
           IdProceso = getIdProceso(header),
           Cod       = ifelse(resultado !="character(0)", CodigoErrorTipo1(ruta,Columna),0),
           Descripcion = getDescError(Cod),
           Detalle     = resultado %>% list())
  
  error_bucket <- bind_rows(error_bucket, tb %>% 
                              filter(resultado !="character(0)") %>%  
                              select(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion, Detalle)) %>% return()
}

# Validaciones condicionales relacionadas con otros campos  (errores tipo2)----
 #BD01
procesarErrorSaldosNegativos <- function(ruta, BD = evalFile(ruta)){
  saldosCols <- c("SKCR", "PCI", "KVI", "KRF", "KVE", "KJU", "SIN", "SID", "SIS", "DGR", "NCPR", "NCPA", "TPINT", "NRPRG")
  
  tb <- tibble(Columna = saldosCols) %>% rowwise() %>%
    mutate(procesarSaldos = BD %>% 
                               filter(as.numeric(cgrep(BD, Columna)[[1]]) <0) %>% pull(CCR) %>% list(),
           resultado      = realizar_pasteCondicional_3(ruta, Columna, procesarSaldos) %>% toString()) %>%
    filter(resultado != "character(0)") %>%
    pull(resultado) %>% 
    return()
}
procesarErrorModalidadCouta  <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(((as.numeric(ESAM) < 5) & (as.numeric(NCPR) == 0 | as.numeric(PCUO)  == 0)) == TRUE) %>%
    pull(CCR) %>% return()
}
procesarErrorMontoOtorgado   <- function(ruta, BD = evalFile(ruta)){
  BD %>% filter(as.numeric(MORG) < as.numeric(SKCR)) %>%
    pull(CCR) %>%
    return()
}
procesarErrorVencJudRetraso  <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter((as.numeric(KVE) > 0 & as.numeric(DAK) == 0)|(as.numeric(KJU) > 0 & as.numeric(DAK) == 0)) %>% 
    pull (CCR) %>% return()
}

 #BD01 y BD04
numeroCaracteresDoc <- function(documento){
  n_caracteres <- switch (documento,
                          "1" = "8",
                          "2" = "9",
                          "3" = "13",
                          "4" = "13",
                          "5" = "12",
                          "6" = "11")
  return(n_caracteres)
}
procesarErrorDocumentoIdent <- function(ruta, BD = evalFile(ruta)){
  vf_doc <- BD %>% rowwise() %>% 
    mutate(vf =  switch (getBD(ruta),
                         BD01 = if_else(numeroCaracteresDoc(TID) == (nchar(NID) %>% toString()), "TRUE", "FALSE"),
                         BD04 = if_else(numeroCaracteresDoc(TID_C) == (nchar(NID_C) %>% toString()), "TRUE", "FALSE"))) %>%
    filter(vf == "FALSE") %>% 
    pull(getCodigoBD(getBD(ruta))) 
  
  realizar_pasteCondicional_2(ruta, vf_doc) %>% return()
}

 #BD03A
procesarErrorNumCredCobertura <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(NCR) > 0, as.numeric(NRCL) == 0) %>% 
    pull(getCodigoBD("BD03A")) %>% 
    unique() %>% return()
}

 #BD3A y BD01
procesarErrorcodDeudor <- function(carpeta, periodo, BD03A, BD01){
  cruce <- setdiff(getInfoTotal(carpeta, periodo, BD03A) %>% pull(CIS),
                   getInfoTotal(carpeta, periodo, BD01) %>% pull(CIS)) %>% unique()
  
  resultado <- realizar_pasteCondicional_4(periodo, cruce)
  return(resultado)
}

# Validaciones de campos fechas                             (errores tipo3)----
ColumnasErrorTipo3 <- function(ruta){
  cols <- switch (getBD(ruta),
                  BD01  = {c("FOT", "FVEG", "FVEP")},
                  BD02A = {c("FVEP")},
                  BD02B = {c("FVEP_C", "FCAN_C")},
                  BD04  = {c("FOT_C", "FCAN_C")}) 
  return(cols)
}
CodigoErrorTipo3   <- function(ruta, campo){
  codError <- switch (getBD(ruta),
                      BD01  = {c(471,472,473)},
                      BD02A = {c(474)},
                      BD02B = {c(475, 476)},
                      BD04  = {c(477, 478)})
  
  cod <- tibble(col       = ColumnasErrorTipo3(ruta),
                cod_error = codError) %>% 
    filter(col == campo) %>% 
    pull(cod_error)
  return(cod)
}
procesarErroresT3  <- function(ruta, error_bucket){
  BD <- evalFile(ruta)
  tb <- tibble(Columna    = depurarColsErrorT3(ruta, error_bucket)) %>% rowwise() %>%
    mutate(validar_fechas = BD %>%
                             filter(dmy(cgrep(BD, Columna)[[1]]) %>% is.na() == TRUE) %>% 
                             pull(getCodigoBD(getBD(ruta))) %>% unique() %>% list(),
           resultado   = realizar_pasteCondicional_2(ruta, validar_fechas) %>% toString(),
           Coopac      = getCoopac(ruta),
           Coopac_n    = getNomCoopac(ruta),
           Carpeta     = getCarpeta(header),
           IdProceso   = getIdProceso(header),
           Cod         = ifelse(resultado !="character(0)", CodigoErrorTipo3(ruta, Columna), 0),
           Descripcion = getDescError(Cod),
           Detalle     = resultado %>% list())
  
  error_bucket <- bind_rows(error_bucket, tb %>%
                              filter(resultado !="character(0)") %>%
                              select(Coopac, Coopac_n, Carpeta, IdProceso, Cod, Descripcion, Detalle)) %>% return()
}

getFechaCorte      <- function(ruta){
  fecha_corte <- seq(as.Date(paste(getAno(ruta),
                                    getMes(ruta),
                                    "01",
                                    sep = "-")),
                      length=1, by="months") %>%
    ceiling_date("month") - days(1)
  return(fecha_corte)
}
procesarErrorFechaDesembolso  <- function(ruta, BD =evalFile(ruta) ){
  BD %>% filter((dmy(BD %>% pull(FOT)) > getFechaCorte(ruta)) == TRUE) %>% 
    pull(CCR) %>%
    return()
}