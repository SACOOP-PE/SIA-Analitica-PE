##### Testing -----
header        <- initHeader(idCoopac = "01172",
                             coopacCarpeta  = "test/datatest/",
                             periodoInicial = "201901",
                             periodoFinal   = "202010",
                             bds            = list(c("BD01", "BD02A", "BD02B", "BD03A", "BD03B", "BD04")))
eb            <- initBucketErrores(header)
listaErrores <- main(header, eb)

#############################3
# n_caracteres
listaErrores %>%
  rowwise() %>%
  mutate(n_caracteres = nchar(Detalle)) %>% 
  select(Cod, Descripcion, n_caracteres)

# encontrar archivos en cada errror de la listaErrores
listaErrores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(ArchivosError = str_extract(Detalle,
                                     getArchivosExigibles(header))[is.na(str_extract(Detalle,
                                                                                     getArchivosExigibles(header))) == FALSE] %>%
           toString(),
         PeriodosError = str_extract(unlist(Detalle %>%
                                              str_split(",")),
                                     paste(alcanceGeneral, collapse = '|'))[is.na(str_extract(unlist(Detalle %>% str_split(",")),
                                                                                              paste(alcanceGeneral,collapse = '|'))) == FALSE] %>%
           unique() %>% toString()
         ) %>%
  select(Cod, Descripcion, ArchivosError, PeriodosError) %>% view()

# resumen_errores_periodos
listaErrores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(PeriodosError = str_extract(unlist(Detalle %>%
                                               str_split(",")),
                                     paste(alcanceGeneral, collapse = '|'))[is.na(str_extract(unlist(Detalle %>% str_split(",")),
                                                                                              paste(alcanceGeneral,collapse = '|'))) == FALSE] %>%
    unique() %>% toString()) %>%
  select(Cod, Descripcion, PeriodosError) %>%
  view()

#####
# archivos : 301,302,303,304,311, 401- 465, 471- 479
# periodos : 322,
#periodos en un error de terminado:
saveObservacion <- function(codError){
 tb <- tibble(creditos_split = listaErrores %>% filter(Cod == codError) %>% pull(Detalle) %>% 
                                  strsplit(split = ")") %>% unlist(),
              PeriodosError  = creditos_split %>% 
                                  str_extract(paste(alcanceGeneral, collapse = '|'))) %>%
   rowwise() %>% 
   mutate(creditosPeriodo = str_extract(creditos_split %>% str_split(pattern = ",") %>% unlist(),
                                        paste(getInfoTotal(getCarpeta(header), PeriodosError, "BD01") %>% pull(CCR),
                                              collapse = '|'))[is.na(str_extract(creditos_split %>% str_split(pattern = ",") %>% unlist(),
                                                                                 paste(getInfoTotal(getCarpeta(header), PeriodosError, "BD01") %>% pull(CCR),
                                                                                       collapse = '|')))==FALSE] %>%
            unique %>% toString()) %>% 
    select(PeriodosError, creditosPeriodo)
  
  periodos <- tb %>% pull(PeriodosError)
  creditos <- tb %>% 
                filter(PeriodosError == periodos[1]) %>%
                pull(creditosPeriodo) %>%
                str_split(pattern = ",") %>% unlist() %>% 
                str_replace_all(pattern=" ", repl="")
  
  observacionBD <- getInfoTotal(getCarpeta(header), periodos[1], "BD01") %>%
    filter(CCR %in% creditos) %>% mutate(Periodo = periodos[1])
  for (i in 1:(length(periodos)-1)){
    creditos_i <- tb %>% 
      filter(PeriodosError == periodos[i+1]) %>%
      pull(creditosPeriodo) %>%
      str_split(pattern = ",") %>% unlist() %>% 
      str_replace_all(pattern=" ", repl="")
    
    observacionBD_i <- getInfoTotal(getCarpeta(header), periodos[i+1], "BD01") %>%
      filter(CCR %in% creditos_i) %>% mutate(Periodo = periodos[i+1])
     
    observacionBD <- bind_rows(observacionBD, observacionBD_i)
  }
  
  observacionBD <- observacionBD %>%
    select(Periodo, unlist(getColumnasOM("BD01")))
  
  observacionBD %>%
    writexl::write_xlsx(paste0(paste(getwd(), "test/observaciones/", sep = "/"),
                               paste(header %>% pull(Coopac),
                                     getIdProceso(header),
                                     sep = "_"),
                               paste0("_", codError),
                               ".xlsx"))
  observacionBD %>% return()
}


















