## Gestion de errores----
addError <- function(eb, obj) {
  
  eb <- bind_rows(eb, obj)
  return(eb)
}