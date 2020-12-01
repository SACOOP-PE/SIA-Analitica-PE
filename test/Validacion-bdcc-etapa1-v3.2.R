##### Testing -----
header        <- init_header(id_coopac       = "01172",
                             coopac_carpeta  = "C:/Users/eroque/Desktop/Proyecto_BDCC",
                             periodo_inicial = "201901",
                             periodo_final   = "201912",
                             bds             = list(c("BD01", "BD02A", "BD02B", "BD03A", "BD03B", "BD04")))
eb            <- init_bucket_errores(header)
lista_errores <- main(header, eb)

# n_caracteres
lista_errores %>%
  rowwise() %>%
  mutate(n_caracteres = nchar(Detalle)) %>% 
  select(Cod, Descripcion, n_caracteres)

# encontrar archivos en cada errror de la lista_errores
lista_errores %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(Archivos_error = str_extract(Detalle,
                                      getArchivosExigibles(header))[is.na(str_extract(Detalle,
                                                                                      getArchivosExigibles(header))
                                                                          ) == FALSE] %>% toString()
         ) %>%
  select(Cod, Descripcion, Archivos_error) %>% view()