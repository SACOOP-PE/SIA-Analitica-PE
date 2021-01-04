#' función principal 
#' layer1()
layer1 <- function(agent, eb){

  carpeta   <- getCarpeta(agent)
  exigibles <- getArchivosExigibles(agent)
  
  tbl1_ctrl1 <- tibble(NombreArchivo = exigibles) %>% rowwise() %>%
    mutate(Ruta          = getRuta(carpeta, NombreArchivo),
           Coopac        = as.numeric(getCoopac(Ruta)),
           Periodo       = getAnoMes(Ruta),
           BDCC          = getBD(Ruta),
           Columnas      = list(colnames(evaluarFile(Ruta))),
           ColumnasOM   = getColumnasOM(BDCC),
           ColFaltantes = ifelse(length(setdiff(ColumnasOM, Columnas))>0,
                                 list(paste(NombreArchivo, (setdiff(ColumnasOM, Columnas)), sep ="$", collapse=",")),
                                 list(character(0))),
           ColSobrantes = ifelse(length(setdiff(Columnas, ColumnasOM))>0,
                                 list(paste(NombreArchivo, (setdiff(Columnas, ColumnasOM)), sep ="$", collapse=",")),
                                 list(character(0))),
           ColVacias    = getColVacia(Ruta))
  
   cf  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(ColFaltantes), collapse = ",") %>% strsplit(","))[[1]]
   cs  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(ColSobrantes), collapse = ",") %>% strsplit(","))[[1]]
   cv  <- (paste(tbl1_ctrl1 %>% rowwise() %>% pull(ColVacias), collapse = ",") %>% strsplit(","))[[1]]
  
   if(length(cf[cf != "character(0)"]) > 0){
     eb <- eb %>%
       addError(201,getDescError(201), (cf[cf != "character(0)"]) %>% toString())
     }
   if(length(cs[cs != "character(0)"]) > 0){
     eb <- eb %>%
       addError(202,getDescError(202), (cs[cs != "character(0)"]) %>% toString())
     }
   if(length(cv[cv != "character(0)"]) > 0){
     eb <- eb %>%
       addError(203,getDescError(203), (cv[cv != "character(0)"]) %>% toString())
   }
   
   n <- eb %>% filter(Cod %in% c(201,202,203)) %>% nrow()
   if(n==1){
     print(paste0("La revisión de consistencia concluyó con ",n, " observación. (~ly1) [", format(Sys.time(), "%a %b %d %X %Y"),"]"))
   } else {
     print(paste0("La revisión de consistencia concluyó con ",n, " observaciones. (~ly1) [", format(Sys.time(), "%a %b %d %X %Y"),"]"))
   }

  return(eb)
}

#' Funciones secundarias 
#' getColumnaOM()
#' getColVacia()
#' evaluarFile()
#' 
getColumnasOM <- function(BDCC){ 
  cols_base <- switch (BDCC,
                       BD01  = {initEstructuraBase() %>% filter(BD == "BD01") %>% pull(CAMPO) %>% list()},
                       BD02A = {initEstructuraBase() %>% filter(BD == "BD02A") %>% pull(CAMPO) %>% list()},
                       BD02B = {initEstructuraBase() %>% filter(BD == "BD02B") %>% pull(CAMPO) %>% list()},
                       BD03A = {initEstructuraBase() %>% filter(BD == "BD03A") %>% pull(CAMPO) %>% list()},
                       BD03B = {initEstructuraBase() %>% filter(BD == "BD03B") %>% pull(CAMPO) %>% list()},
                       BD04  = {initEstructuraBase() %>% filter(BD == "BD04") %>% pull(CAMPO) %>% list()})
  cols_base %>% return()
}
getColVacia  <- function(ruta, BD = evaluarFile(ruta)){
  cols_vacias <- intersect(BD[sapply(BD, function(x) all(is.na(x)))] %>% colnames(), 
                           getColumnasOM(getBD(ruta)) %>% unlist())
  
  resultado <- generarDetalleError1(ruta, cols_vacias)
  return(resultado)
}
evaluarFile  <- function(ruta){
  read_delim(ruta,"\t",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE,
             col_types = cols(.default = "c"), progress = T, na = "NA" ) %>% 
    na_if("") %>%
    return()
}