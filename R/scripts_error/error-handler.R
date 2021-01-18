## Gestion de errores----
addError <- function(eb, obj) {
  
  eb <- bind_rows(eb, obj)
  return(eb)
}

addErrorMasivo <- function(eb, parte1){
  bind_rows(eb, parte1) %>% return()
}