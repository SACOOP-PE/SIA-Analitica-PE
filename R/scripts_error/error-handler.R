## Gestion de errores----
addErrorIndividual <- function(agente, codcoopac, errorbucket, idproceso, cod, periodo, bd, 
                               arg_txt1, arg_txt2, arg_txt3,
                               arg_num1, arg_num2, arg_num3){
  t <- tibble(CodCoopac = codcoopac,
              IdProceso  = idproceso, 
              Cod         = cod, 
              Periodo     = periodo,
              BD          = bd,
              txt1     = arg_txt1,
              txt2    = arg_txt2,
              txt3    =  arg_txt3,
              num1    = arg_num1,
              num2    = arg_num2,
              num3    = arg_num3)
  
  addEventLog(agente, paste0("Bucket de errores actualizado. PID-", agente %>% pull(IdProceso) %>% first(),"."), "I", "B")
  return(bind_rows(errorbucket,t))
} 

addErrorMasivo <- function(errorbucket, parte1){
  bind_rows(errorbucket, parte1) %>% return()
}