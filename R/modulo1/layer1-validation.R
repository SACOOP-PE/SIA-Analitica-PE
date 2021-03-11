#' función principal 
#' layer1()
#' 
layer1 <- function(agente, eb){
  eb <- validarColumnas(agente, eb)
  return(eb)
}

#' Funciones secundarias 
#' validarColumnas()
#' 
#' getColumnaOM()
#' getColVacia()

validarColumnas <- function(agente, eb){
  exigibles <- getArchivosExigiblesFromAgent(agente) 
  
  tbl1_ctrl1 <- tibble(NombreArchivo = exigibles) %>% rowwise() %>%
    mutate(ruta      = getRuta(default.carpeta, NombreArchivo),
           CodCoopac = getCoopacFromAgent(agente),
           IdProceso = getIdProcesoFromAgent(agente),
           BD        = getBDFromRuta(ruta),
           Periodo   = getAnoMesFromRuta(toString(ruta)),
           Columnas     = list(colnames(evaluarFile(ruta))),
           ColumnasOM   = getColumnasOM(BD),
           ColFaltantes = toString(setdiff(ColumnasOM, Columnas)),
           ColSobrantes = toString(setdiff(Columnas, ColumnasOM)),
           ColVacias    = toString(getColVacia(ruta)))
  
  fal <- tbl1_ctrl1 %>% filter(ColFaltantes != "")
  sob <- tbl1_ctrl1 %>% filter(ColSobrantes != "")
  vac <- tbl1_ctrl1 %>% filter(ColVacias != "")
  
  if (nrow(fal)>0) {
    chunk_201 <- fal %>% rowwise() %>% 
      mutate(Cod = 201,
             txt1 = ColFaltantes, 
             num1 = length(str_split(txt1 ,pattern = ",")[[1]])) %>%  
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
    
    addEventLog(agente, paste0("La validación de Columnas faltantes concluyó con ", nrow(fal), " observación(es)."), "I", "B")
    eb <- addError(eb, chunk_201)
  }
  if (nrow(sob)>0) {
    chunk_202 <- sob %>% rowwise() %>% 
      mutate(Cod = 202,
             txt1 = ColSobrantes, 
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%  
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
    
    addEventLog(agente, paste0("La validación de Columnas sobrantes concluyó con ", nrow(sob), " observación(es)."), "I", "B")
    eb <- addError(eb, chunk_202)
  }
  if (nrow(vac)>0) {
    chunk_203 <- vac %>% rowwise() %>% 
      mutate(Cod = 203,
             txt1 = ColVacias, 
             num1 = length(str_split(string=txt1 ,pattern = ",")[[1]])) %>%  
      select(CodCoopac, IdProceso, Cod, Periodo, BD, txt1, num1)
    
    addEventLog(agente, paste0("La validación de Columnas vacías concluyó con ", nrow(vac), " observación(es)."), "I", "B")
    eb <- addError(eb, chunk_203)
  }
  
  n <- nrow(eb %>% filter(Cod %in% c(201, 202)))
    
  if (nrow(eb) > 0) {
    if (n >0) {
        addEventLog(agente, paste0("      Resultado: La revisión de estructura de datos tiene ",n," observaciones. Continuar con discreción."))
    }
    else {
        addEventLog(agente, paste0("      Resultado: Revisión de estructura de datos satisfatoria."))
    }
      
  }
  else {
    addEventLog(agente, paste0("      Resultado: Revisión de estructura de datos satisfatoria."))
  }
  
  return(eb)
}
  
getColumnasOM <- function(bd){ 
  cols <- initEstructuraBase() %>% filter(BD == bd) %>% pull(CAMPO) %>% list()
  return(cols)
}
getColVacia   <- function(ruta){
  BD <- evaluarFile(ruta)
  
  colsVacias <- intersect(colnames(BD[sapply(BD, function(x) all(is.na(x)))]),
                          unlist(getColumnasOM(getBDFromRuta(ruta))))

  return(colsVacias)
}