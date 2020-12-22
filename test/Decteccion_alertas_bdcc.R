##### Testing -----
ab           <- initBucketAlertas(header)
listaAlertas <- mainAlertas(header, ab)


listaAlertas %>%
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  rowwise() %>%
  mutate(ArchivosAlertas = str_extract(Detalle,
                                       getArchivosExigibles(header))[is.na(str_extract(Detalle,
                                                                                       getArchivosExigibles(header))) == FALSE] %>%
           toString()) %>%
  select(CodAlerta, Descripcion, ArchivosAlertas) %>% view()

listaAlertas %>% 
  mutate(Detalle = map_chr(Detalle, ~ .[[1]] %>% str_c(collapse = ", "))) %>%
  write.csv(paste0(paste(getwd(), "test/", sep = "/"),
                   paste(header %>% pull(Coopac),
                         getIdProceso(header),
                         header %>% pull(PeriodoInicial),
                         header %>% pull(PeriodoFinal),
                         sep = "_"),
                   "_alertbucket.csv"))