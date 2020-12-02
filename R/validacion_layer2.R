ejecutarValidacionLayer2 <- function(header, error_bucket){
  carpeta   <- getCarpeta(header)
  exigibles <- getArchivosExigibles(header)
  
  tbl1_ctrl1 <- tibble(NombreArchivo = exigibles) %>% rowwise() %>%
    mutate(Ruta          = getRuta(carpeta, NombreArchivo),
           Coopac        = as.numeric(getCoopac(Ruta)),
           Periodo       = getAnoMes(Ruta),
           BDCC          = getBD(Ruta), 
           Columnas      = list(colnames(evalFile(Ruta))), 
           ColumnasOM   = getColumnasOM(BDCC),    
           ColFaltantes = ifelse(length(setdiff(ColumnasOM, Columnas))>0,
                                 list(paste(NombreArchivo, (setdiff(ColumnasOM, Columnas)), sep ="$", collapse=",")),
                                 list(character(0))),
           ColSobrantes = ifelse(length(setdiff(Columnas, ColumnasOM))>0,
                                 list(paste(NombreArchivo, (setdiff(Columnas, ColumnasOM)), sep ="$", collapse=",")),
                                 list(character(0))),
           ColVacias    = getColVacias(Ruta))
  
   cf  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(ColFaltantes), collapse = ",") %>% strsplit(","))[[1]] 
   cs  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(ColSobrantes), collapse = ",") %>% strsplit(","))[[1]] 
   cv  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(ColVacias), collapse = ",") %>% strsplit(","))[[1]]
  
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