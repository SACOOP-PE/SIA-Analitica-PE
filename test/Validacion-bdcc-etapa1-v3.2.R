##### Testing -----
header        <- initHeader(idCoopac = "01172",
                             coopacCarpeta  = "test/datatest/",
                             periodoInicial = "201901",
                             periodoFinal   = "202010",
                             bds            = list(c("BD01", "BD02A", "BD02B", "BD03A", "BD03B", "BD04")))
eb            <- initBucketErrores(header)
listaErrores <- main(header, eb)

############# 
#### encontrar archivos, periodos y n_caracteres por cada errror en la listaErrores
listaErrores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(ArchivosError = str_extract(Detalle,
                                     getArchivosExigibles(header))[is.na(str_extract(Detalle,
                                                                                     getArchivosExigibles(header))) == FALSE] %>%
           unique() %>% toString(),
         PeriodosError = str_extract(unlist(str_split(Detalle, ",")),
                                     paste(alcanceGeneral, collapse = '|'))[is.na(str_extract(unlist(str_split(Detalle, ",")),
                                                                                              paste(alcanceGeneral,collapse = '|'))) == FALSE] %>%
           unique() %>% toString(),
         n_caracteres = nchar(Detalle)) %>%
  select(Cod, Descripcion, ArchivosError, PeriodosError, n_caracteres) %>% view()

#####
#guardar algunas observaciones en cvs:
saveObservacion <- function(codError){
  tb <- tibble(creditos_split = listaErrores %>% filter(Cod == 322) %>% pull(Detalle) %>% 
                                  strsplit(split = ")") %>% unlist(),
              PeriodosError  = str_extract(creditos_split, paste(alcanceGeneral, collapse = '|')) %>% 
                unique()) %>%
    rowwise() %>% 
    mutate(creditosPeriodo = str_extract(unlist(creditos_split %>% str_split(pattern = ",")),
                                         paste(getInfoTotal(getCarpeta(header), PeriodosError, "BD01") %>% pull(CCR),
                                               collapse = '|')) %>% 
             unique %>% toString(), 
           Creditos = unlist(creditosPeriodo %>% str_split(pattern = ","))[unlist(creditosPeriodo %>% str_split(pattern = ",")) != "NA"] %>%
             toString()) %>% 
    select(PeriodosError, Creditos)
  
  periodos <- tb %>% pull(PeriodosError)
  creditos <- tb %>% 
                filter(PeriodosError == periodos[1]) %>% pull(Creditos) %>%
                str_split(pattern = ",") %>% unlist() %>% 
                str_replace_all(pattern=" ", repl="")
  observacionBD <- getInfoTotal(getCarpeta(header), periodos[1], "BD01") %>%
                        filter(CCR %in% creditos) %>% mutate(Periodo = periodos[1])
  
  if(length(periodos) == 1){
    observacionBD <- observacionBD %>%
      select(Periodo, unlist(getColumnasOM("BD01")))
      return(observacionBD)
  }
  if(length(periodos) > 1){
    for (i in 1:(length(periodos)-1)){
      creditos_i <- tb %>% filter(PeriodosError == periodos[i+1]) %>% pull(creditosPeriodo) %>%
                            str_split(pattern = ",") %>% unlist() %>%
                            str_replace_all(pattern=" ", repl="")

      observacionBD_i <- getInfoTotal(getCarpeta(header), periodos[i+1], "BD01") %>%
                              filter(CCR %in% creditos_i) %>% mutate(Periodo = periodos[i+1])

      observacionBD <- bind_rows(observacionBD, observacionBD_i)
      }
    observacionBD <- observacionBD %>%
    select(Periodo, unlist(getColumnasOM("BD01")))
    return(observacionBD)
  }
}
saveObservaciones <- function(){
  codErroresActuales <- listaErrores %>% pull(Cod) %>%
    setdiff(c(201, 202, 203, 301, 302, 303, 304, 431, 432, 441, 461, 466, 467))
    
  for (i in 1:length(codErroresActuales)){
   obs <- saveObservacion(codErroresActuales[i])   
   obs %>%
     write.csv(paste0(paste(getwd(), "test/observaciones/", sep = "/"),
                      paste(header %>% pull(Coopac),
                            getIdProceso(header),
                            sep = "_"),
                      paste0("_", "Observacion", which(listaErrores$Cod == codErroresActuales[i])),
                      ".csv"))
  }
}