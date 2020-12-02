##### Testing -----
header        <- init_header(id_coopac       = "01172",
                             coopac_carpeta  = "C:/Users/eroque/Desktop/Proyecto_BDCC",
                             periodo_inicial = "201901",
                             periodo_final   = "201912",
                             bds             = list(c("BD01", "BD02A", "BD02B", "BD03A", "BD03B", "BD04")))
eb            <- init_bucket_errores(header)
listaErrores <- main(header, eb)

##########################
# n_caracteres
listaErrores %>%
  rowwise() %>%
  mutate(n_caracteres = nchar(Detalle)) %>% 
  select(Cod, Descripcion, n_caracteres)

# encontrar archivos en cada errror de la listaErrores
listaErrores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(Archivos_error = str_extract(Detalle,
                                      getArchivosExigibles(header))[is.na(str_extract(Detalle,
                                                                                      getArchivosExigibles(header))
                                                                          ) == FALSE] %>% toString()) %>%
  select(Cod, Descripcion, Archivos_error) %>% view()

# resumen_errores_periodos
listaErrores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(Periodos_error = str_extract(unlist(Detalle %>%
                                               str_split(",")),
                                      paste(alcance_general,collapse = '|'))[is.na(str_extract(unlist(Detalle %>% str_split(",")),
                      paste(alcance_general,collapse = '|'))) == FALSE] %>%
    unique() %>% toString()) %>%
  select(Descripcion, Periodos_error) %>% 
  write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                   paste(header %>% pull(Coopac),
                         getIdProceso(header),
                         header %>% pull(PeriodoInicial),
                         header %>% pull(PeriodoFinal),
                         sep = "_"),
                   "_resumen_periodos_error.csv"))