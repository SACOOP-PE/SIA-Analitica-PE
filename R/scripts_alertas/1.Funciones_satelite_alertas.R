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

#2001 ,2002
alertMontosuperiorAgencias       <- function(ruta, BD = evalFile(ruta)){
  agenciasAltoriesgo <- c("MORG", "UAGE")
  
  alert <- tibble(Columna = agenciasAltoriesgo) %>% rowwise() %>%
    mutate(procesarAlerta = BD %>% 
             filter((as.numeric(cgrep(BD, Columna)[[1]]) > 13889) == TRUE) %>%
             pull(getCodigoBD("BD01")) %>% list())
  return(alert)
}
#2003
alertMontosuperiorSector         <- function(ruta, BD = evalFile(ruta)){
  alert <-  BD %>% 
             filter((as.numeric(SEC) %in% c(3,6,8,9,10)) & (as.numeric(MORG) > 27778) == TRUE) %>%
             pull(getCodigoBD("BD01")) %>% list()
  return(alert)
}
#2004
alertMontosuperiorOcupaciones    <- function(ruta, BD = evalFile(ruta)){
  alert <-  BD %>% 
    filter((as.numeric(OSD) %in% c(1,2,5,9)) & (as.numeric(MORG) > 138889) == TRUE) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}
#2005
alertTea                         <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(TEA) < 1) %>% pull(getCodigoBD("BD01")) %>% list() %>% 
    return()
}
#2006
alertDiasGracia                  <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(DGR) > 90) %>% pull(getCodigoBD("BD01")) %>% list() %>% 
    return() 
}
#2007
alertMontOtorsuperiorCapitalVig  <- function(ruta, BD = evalFile(ruta)){
  BD %>% 
    filter(as.numeric(MORG) >= as.numeric(SKCR)) %>% pull(getCodigoBD("BD01")) %>% list() %>%
    return() 
}
#2008
alertRendimientoDevengado        <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(CAL %in% c(3,4) & (as.numeric(KRF) >0 | as.numeric(KJU) >0) & as.numeric(SIN) >0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return() 
}
#2009
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
#2010
alertDiasAtrasonegativo          <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter(as.numeric(DAKR) < 0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return() 
}
#2011
alertEsquemaAmortizaCuotaPagadas <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((as.numeric(ESAM) %in% c(3,4)) & as.numeric(NCPA) == 1) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}
#2012
alertDeudorInteresDevengado      <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(3,4)) & as.numeric(SIN) > 0) %>%
    pull(CIS) %>% list() %>%
    return()
}
#2013
alertCreditoInteresDevengado     <- function(ruta, BD = evalFile(ruta)){
  BD %>%
  filter((as.numeric(KRF) >0 | as.numeric(KVE) >0 | as.numeric(KJU) >0) & as.numeric(SIN) > 0) %>%
    pull(getCodigoBD("BD01")) %>% list() %>%
    return()
}
#2014
alertDeudorContableVencido       <- function(ruta, BD = evalFile(ruta)){
  BD %>%
    filter((CAL %in%  c(0,1)) & as.numeric(KVE) > 0) %>%
    pull(CIS) %>% list() %>%
    return()
}
#2015
# alertCreditoContableVigente      <- function(ruta, BD = evalFile(ruta)){
#   BD %>%
#     filter((CAL %in%  c(3,4)) & as.numeric(KVI) > 0) %>%
#     pull(CIS) %>% list() %>%
#     return()
# }

alertCreditoContableVigente("C:/Users/eroque/Desktop/Proyecto_BDCC/SIA-Analitica-PE/test/datatest/202001/01172_BD01_202001.txt") %>% view()
