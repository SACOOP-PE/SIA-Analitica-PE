  rm(list=ls())
  source("R/scripts_error/agent.R")
  source("R/scripts_error/error-handler.R")
  source("R/scripts_error/layer0-validation-error0.R")
  source("R/scripts_error/layer1-validation-error1.R")
  source("R/scripts_error/layer2-validation-error2.R")
  source("R/scripts_error/layer3-validation-error3.R")
  source("R/scripts_error/layer4-validation-error4.R")
  
  source("R/scripts_error/libraries-and-options.R")
  source("R/scripts_error/log-manager.R")
  source("R/scripts_error/reporting-manager.R")
  source("R/scripts_error/utils.R")
  
  # Inicializar agente.
  # Interrogar agente - Layer del 0 al 4. 
  # Cerrar agente.
  # Formatear bucket generado para comunicarlo por correo o por Oficio SBS.
  # Obtener el Log (Time - Descripción)
  # Exportar agent, ebFormatted y PIDlog
  

  ##### Testing -----
  agent <- createAgent(idCoopac = "01112",
                       periodoInicial = "201901",
                       periodoFinal   = "202012")
  
  bucket      <- interrogateAgent(agent)
  agent       <- closeAgent(agent, bucket)
  ebFormatted <- formatBucket(bucket)
  PIDlog      <- getlog(getIdProcesoFromAgent(agent))
  
  saveOutputs(agent, bucket, ebFormatted, PIDlog)
  