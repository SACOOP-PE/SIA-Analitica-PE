initRepositorioAlertas <- function(){
  read_delim(fileRepositorioAlertas, 
             "\t", escape_double = FALSE, col_types = cols(CodAlerta   = col_double(), 
                                                           Responsable = col_character(), 
                                                           Descripcion = col_character()),
             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, progress = T) %>% return()
}
initBucketAlertas      <- function(header){
  tibble(Coopac     = header %>% pull(Coopac) %>% first(),
         NombCoopac = header %>% pull(NombreCoopac) %>% first(),
         Carpeta    = header %>% pull(Carpeta) %>% first(),
         IdProceso  = header %>% pull(IdProceso) %>% first(),
         #Informaci?n temporal
         CodAlerta  = 999999,
         Responsable = "Lorem ipsum ... ",
         Descripcion = "Lorem ipsum ... ",
         Detalle     = list(c("1", "3", "2"))) %>% return()
}

getDescAlerta <- function(codigoAlerta){
  if ((length(initRepositorioAlertas() %>% filter(CodAlerta == codigoAlerta) %>% pull(Descripcion)) == 0)){
    return("Descripción de alerta no encontrada")}
  
  initRepositorioAlertas() %>% filter(CodAlerta == codigoAlerta) %>% pull(Descripcion) %>% first() %>% return()
} 
deleteAlerta  <- function(alertBucket, codigoAlerta){
  alertBucket %>% filter(CodAlerta != codigoAlerta) %>% return()
}
addAlerta     <- function(alertBucket, codigoAlerta, responsableAlerta, DescripcionAlerta, DetalleAlerta){ 
  rbind(alertBucket, tibble(Coopac     = alertBucket %>% pull(Coopac) %>% first(),
                            NombCoopac = alertBucket %>% pull(NombCoopac) %>% first(),
                            Carpeta    = alertBucket %>% pull(Carpeta) %>% first(),
                            IdProceso  = alertBucket %>% pull(IdProceso) %>% first(),
                            CodAlerta  = codigoAlerta,
                            Responsable = alertBucket %>% pull(responsableAlerta),
                            Descripcion = DescripcionAlerta,
                            Detalle     = list(DetalleAlerta))) %>%
    deleteAlerta(999999) %>% return()
}

#alertas BD01 ----
# Codigos2001 ,2002
alertMontosuperiorAgencias       <- function(ruta, BD = evalFile(ruta)){
  agenciasAltoriesgo <- c("MORG", "UAGE")
  
  alert <- tibble(Columna = agenciasAltoriesgo) %>% rowwise() %>%
    mutate(procesarAlerta = BD %>% 
             filter((as.numeric(cgrep(BD, Columna)[[1]]) > 13889) == TRUE) %>%
             pull(getCodigoBD("BD01")) %>% list())
  return(alert)
}

# Codigo 2003
alertMontosuperiorSector         <- function(ruta, BD = evalFile(ruta)){
  alert <-  BD %>% 
             filter((as.numeric(SEC) %in% c(3,6,8,9,10)) & (as.numeric(MORG) > 27778) == TRUE) %>%
             pull(getCodigoBD("BD01")) %>% list()
  return(alert)
}

# Codigo 2004
alertMontosuperiorOcupaciones    <- function(ruta, BD = evalFile(ruta)){
  alert <-  BD %>% 
    filter((as.numeric(OSD) %in% c(1,2,5,9)) & (as.numeric(MORG) > 138889) == TRUE) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}

# Codigo 2005
alertTea                         <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(TEA) < 1) %>% pull(getCodigoBD("BD01")) %>% list() %>% 
    return()
}

# Codigo 2006
alertDiasGracia                  <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(DGR) > 90) %>% pull(getCodigoBD("BD01")) %>% list() %>% 
    return() 
}

# Codigo 2007
alertMontOtorsuperiorCapitalVig  <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(MORG) >= as.numeric(SKCR)) %>% pull(getCodigoBD("BD01")) %>% list() %>%
    return() 
}

# Codigo 2008
alertRendimientoDevengado        <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(CAL %in% c(3,4) & (as.numeric(KRF) >0 | as.numeric(KJU) >0) & as.numeric(SIN) >0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return() 
}

# Codigo 2009
alertDeudorCal                   <- function(ruta, BD = evalFile(ruta)){
  cis_deudor <- BD %>% select(CIS) 
  duplicados <- cis_deudor[duplicated(cis_deudor), ] %>% unique()
  
  tibble(Deudor = duplicados) %>% rowwise() %>% 
    mutate(NumeroCalificaciones = BD %>% filter(CIS %in% Deudor) %>% 
             pull(CAL) %>% unique() %>% length()) %>% 
    filter(NumeroCalificaciones > 1) %>%
    pull(Deudor) %>%
    return()
}

# Codigo 2010
alertDiasAtrasonegativo          <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(DAKR) < 0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return() 
}

# Codigo 2011
alertEsquemaAmortizaCuotaPagadas <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((as.numeric(ESAM) %in% c(3,4)) & as.numeric(NCPR) == 1) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}

# Codigo 2012
alertDeudorInteresDevengado      <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(3,4)) & as.numeric(SIN) > 0) %>%
    pull(CIS) %>% list() %>%
    return()
}

# Codigo 2013
alertCreditoInteresDevengado     <- function(ruta, BD = evalFile(ruta)){
  BD %>%
  filter((as.numeric(KRF) >0 | as.numeric(KVE) >0 | as.numeric(KJU) >0) & as.numeric(SIN) > 0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}

# Codigo 2014
alertDeudorContableVencido       <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(0,1)) & as.numeric(KVE) > 0) %>%
    pull(CIS) %>% list() %>%
    return()
}

# Codigo 2015
alertCreditoContableVigente      <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(3,4)) & as.numeric(KVI) > 0) %>%
    pull(CIS) %>% list() %>%
    return()
}

# Codigo 2016
alertDiasAtrasoJudicial          <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(DAK) > 120 & as.numeric(KJU) == 0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}

# Codigo 2017
alertCreditoCobranzaJudicial     <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(KJU) > 0 & (CAL %in%  c(0,1,2))) %>%
    pull(CIS) %>% list() %>%
    return()
}

# Codigo 20218
alertCreditosUnicouta            <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(ESAM) %in% c(1,2) & (dmy(BD %>% pull(FVEG)) - dmy(BD %>% pull(FOT))) > 365) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
} 
# Codigo 2019
alertCreditosHipotecario         <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(TCR) != 13 & (dmy(BD %>% pull(FVEG)) - dmy(BD %>% pull(FOT))) > 3650) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
} 

# Codigo 2020
# Codigo 2021
# Codigo 2022
# Codigo 2023
# Codigo 2024

#alerta BD02A y 2B ----
# Codigos 2025, 2026
ocupacionesAltoRiesgo <- function(periodo) {
  getInfoTotal(getCarpeta(header), periodo, "BD01") %>% 
    filter(OSD %in% c(1, 2, 5, 9)) %>%
    pull(getCodigoBD("BD01")) %>%
    return()
}
alertMontosuperiorOcupaciones2  <- function(BD2, periodo){
  bd2 <- getInfoTotal(getCarpeta(header), periodo, BD2)
  credicronogramas <- switch (BD2,
                              BD02A = bd2 %>% filter(cgrep(bd2, getCodigoBD(BD2))[[1]] %in% ocupacionesAltoRiesgo(periodo) &
                                                       as.numeric(TCUO) > 27778) %>%
                                pull(getCodigoBD(BD2)),
                              BD02B = bd2 %>% filter(cgrep(bd2, getCodigoBD(BD2))[[1]] %in% ocupacionesAltoRiesgo(periodo) &
                                                       as.numeric(TCUO_C) > 27778) %>%
                                pull(getCodigoBD(BD2))) 
    return(credicronogramas %>% unique())
}
#alerta BD01 y BD02A ----
# Codigo 2027
creditosComunes <- function(periodo, BD1, BD2){
  comunes <- intersect(getInfoCruce(getCarpeta(header), periodo, BD1), 
                       getInfoCruce(getCarpeta(header), periodo, BD2)) %>%
    unique() %>%
    return()
}
alertMontOrtorgadoCronograma <- function(periodo){
  creditosComun <- creditosComunes(periodo, "BD01", "BD02A")
  
  montoOtorgado    <- getInfoTotal(getCarpeta(header), periodo, "BD01") %>% filter(CCR %in% creditosComun) %>%
    pull(MORG)
  capitalPorCobrar <- getInfoTotal(getCarpeta(header), periodo, "BD02A") %>% filter(CCR %in% creditosComun) %>%
    pull(MCUO)
  
  creditosComun <- creditosComun[montoOtorgado > capitalPorCobrar] %>% 
    return()
}
#alertas BD03A ----
# Codigo 2028
# Codigo 2029


#alertas BD04 ----
# Codigo 2030
# Codigo 2031
# Codigo 2032
alertMontosuperiorOcupaciones3 <- function(periodo){
  bd4 <- getInfoTotal(getCarpeta(header), periodo, "BD04")

  creditos <- bd4 %>% filter(cgrep(bd4, getCodigoBD("BD04"))[[1]] %in% ocupacionesAltoRiesgo(periodo) &
                   as.numeric(MCT_C) > 277778) %>%
    pull(getCodigoBD("BD04"))
  
  return(creditos %>% unique())
}
# Codigo 2033
alertFechaDesembolsoCancelacion  <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(dmy(BD %>% pull(FOT_C)) == (dmy(BD %>% pull(FCAN_C)))) %>% 
    pull(getCodigoBD("BD04")) %>%
    return()
}
# Codigo 2034
alertNumeroCanceladosyOriginales <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(NCPR_C) == as.numeric(NCPA_C)) %>%
    pull(getCodigoBD("BD04")) %>%
    return()
}

alertCreditosHipotecario("C:/Users/eroque/Desktop/Proyecto_BDCC/SIA-Analitica-PE/test/datatest/202001/01172_BD01_202001.txt") %>% view()
#una tabla donde varíe los exigibles segú el código de alerta, los exigibles cambien según el flijo de errores(cols <- error 201,203)
alertMontosuperiorOcupaciones3(202001)

intersect()


