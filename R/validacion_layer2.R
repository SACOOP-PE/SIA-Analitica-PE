ejecutar_validacion_layer2 <- function(header, error_bucket){
  carpeta   <- getCarpeta(header)
  exigibles <- getArchivosExigibles(header)
  
  tbl1_ctrl1 <- tibble(Nombre_archivo = exigibles) %>% rowwise() %>%
    mutate(Ruta          = getRuta(carpeta, Nombre_archivo),
           Coopac        = as.numeric(getCoopac(Ruta)),
           Periodo       = getAnoMes(Ruta),
           BDCC          = getBD(Ruta), 
           Columnas      = list(colnames(evalFile(Ruta))), 
           Columnas_OM   = getColumnasOM(BDCC),    
           Col_faltantes = ifelse(length(setdiff(Columnas_OM, Columnas))>0,
                                  list(paste(Nombre_archivo, (setdiff(Columnas_OM, Columnas)), sep ="$", collapse=",")),
                                  list(character(0))),
           Col_sobrantes = ifelse(length(setdiff(Columnas, Columnas_OM))>0,
                                  list(paste(Nombre_archivo, (setdiff(Columnas, Columnas_OM)), sep ="$", collapse=",")),
                                  list(character(0))),
           Col_vacias    = getColVacias(Ruta))
  
   cf  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(Col_faltantes), collapse = ",") %>% strsplit(","))[[1]] 
   cs  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(Col_sobrantes), collapse = ",") %>% strsplit(","))[[1]] 
   cv  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(Col_vacias), collapse = ",") %>% strsplit(","))[[1]]
  
   if(length(cf[cf != "character(0)"]) > 0){
     error_bucket <- error_bucket %>%
       addError(201,getDescError(201), (cf[cf != "character(0)"]) %>% toString())
     }
   if(length(cs[cs != "character(0)"]) > 0){
     error_bucket <- error_bucket %>%
       addError(202,getDescError(202), (cs[cs != "character(0)"]) %>% toString())
     }
   if(length(cv[cv != "character(0)"]) > 0){
     error_bucket <- error_bucket %>%
       addError(203,getDescError(203), (cv[cv != "character(0)"]) %>% toString())
     }

  print(paste0("El layer 2 terminó: ", format(Sys.time(), "%a %b %d %X %Y")))
  return(error_bucket)
}