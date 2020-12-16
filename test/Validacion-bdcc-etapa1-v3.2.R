##### Testing -----
header        <- initHeader(idCoopac = "01172",
                             coopacCarpeta  = "test/datatest/",
                             periodoInicial = "201901",
                             periodoFinal   = "202002",
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
           toString()) %>%
  select(Cod, Descripcion, ArchivosError) %>% view()

# resumen_errores_periodos
listaErrores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(PeriodosError = str_extract(unlist(Detalle %>%
                                               str_split(",")),
                                      paste(alcanceGeneral, collapse = '|'))[is.na(str_extract(unlist(Detalle %>% str_split(",")),
                                                                                               paste(alcanceGeneral,collapse = '|'))) == FALSE] %>%
    unique() %>% toString()) %>%
  select(Descripcion, PeriodosError) %>% 
  write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                   paste(header %>% pull(Coopac),
                         getIdProceso(header),
                         header %>% pull(PeriodoInicial),
                         header %>% pull(PeriodoFinal),
                         sep = "_"),
                   "_resumen_periodos_error.csv"))