initRepositorioAlertas <- function(){
  read_delim(fileRepositorioAlertas, 
             "\t", escape_double = FALSE, col_types = cols(CodAlerta   = col_double(), 
                                                           Responsable = col_character(), 
                                                           Descripcion = col_character()),
             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, progress = T) %>% return()
}
initBucketAlertas <- function(header){
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
addAlerta <- function(alertBucket, codigoAlerta, responsableAlerta, DescripcionAlerta, DetalleAlerta){ 
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
alertMontosuperiorAgencias <- function(ruta, BD = evalFile(ruta)){
  agenciasAltoriesgo <- c("MORG", "UAGE")
  
  alert <- tibble(Columna = agenciasAltoriesgo) %>% rowwise() %>%
    mutate(procesarAlerta = BD %>% 
             filter((as.numeric(cgrep(BD, Columna)[[1]]) > 13889) == TRUE) %>%
             pull(getCodigoBD("BD01")) %>% list())
  return(alert)
}
alertMontosuperiorSector <- function(ruta, BD = evalFile(ruta)){
  alert <-  BD %>% 
             filter((as.numeric(SEC) %in% c(3,6,8,9,10)) & (as.numeric(MORG) > 27778) == TRUE) %>%
             pull(getCodigoBD("BD01")) %>% list()
  return(alert)
}
alertMontosuperiorOcupaciones <- function(ruta, BD = evalFile(ruta)){
  alert <-  BD %>% 
    filter((as.numeric(OSD) %in% c(1,2,5,9)) & (as.numeric(MORG) > 138889) == TRUE) %>%
    pull(getCodigoBD("BD01")) %>% list()
  return(alert)
}

# alertMontosuperiorOcupaciones("C:/Users/eroque/Desktop/Proyecto_BDCC/SIA-Analitica-PE/test/datatest/202001/01172_BD01_202001.txt") %>% view()
