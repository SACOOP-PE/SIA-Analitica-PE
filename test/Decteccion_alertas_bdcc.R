##### Testing -----
ab           <- initBucketAlertas(header)
listaAlertas <- main2(header, ab)


listaAlertas %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(ArchivosAlertas = str_extract(Detalle,
                                       getArchivosExigibles(header))[is.na(str_extract(Detalle,
                                                                                       getArchivosExigibles(header))) == FALSE] %>%
           toString()) %>%
  select(CodAlerta, Descripcion, ArchivosAlertas) %>% view()