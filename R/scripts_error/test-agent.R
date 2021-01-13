  rm(list=ls())
  source("R/scripts_error/agent.R")
  source("R/scripts_error/error-handler.R")
  source("R/scripts_error/layer0-prerequisites.R")
  source("R/scripts_error/layer1-validation-error1.R")
  source("R/scripts_error/layer2-validation-error2.R")
  source("R/scripts_error/libraries-and-options.R")
  source("R/scripts_error/log-manager.R")
  source("R/scripts_error/reporting-manager.R")
  source("R/scripts_error/utils.R")
  
  # Inicializar agente.
  # Interrogar agente - Layer del 0 al 3. 
  # Cerrar agente.
  # Formatear bucket generado para comunicarlo por correo o por Oficio SBS.
  
  # getArchivosFromAgent(agent)
  # getArchivosNoObservados(agent,bucket)
  # getArchivosNoObservadosByCols(agent, bucket, c("MON_C", "MCUO_C", "X29"))
  # getArchivosNoObservadosByErrors(agent,bucket,c(201))
  # getArchivosObservadosFromBucket(bucket)
  
  # Nuevas funciones para error-handling
  ##### Testing -----
  agent <- createAgent(idCoopac = "01172",
                       usuarioSIA = "DPACHECO",
                       coopacCarpeta  = "test/datatest",
                       periodoInicial = "201907",
                       periodoFinal   = "201909",
                       bds            = list(c("BD01", "BD02A", "BD02B", "BD03A", "BD03B", "BD04")))
  bucket      <- interrogateAgent(agent)
  agent       <- closeAgent(agent, bucket)
  ebFormatted <- formatBucket(bucket)
 
  #Mostrar log - Edwin si puedes codea para que este log mostrado se puede guardar en un archivo PID-3424234.log
  getlog(getIdProcesoFromAgent(agent))
  
