  rm(list=ls())
  
  # Scripts de entorno
  source("R/scripts-entorno/libraries-and-options.R")
  source("R/scripts-entorno/agent.R")
  source("R/scripts-entorno/error-handler.R")
  source("R/scripts-entorno/log-manager.R")
  source("R/scripts-entorno/reporting-manager.R")
  source("R/scripts-entorno/utils.R")
  
  # Validación de información
  source("R/scripts-errores/layer0-validation.R")
  source("R/scripts-errores/layer1-validation.R")
  source("R/scripts-errores/layer2-validation.R")
  source("R/scripts-errores/layer3-validation.R")
  source("R/scripts-errores/layer4-validation.R")
  
  # Inicializar agente.
  # Interrogar agente - Layer del 0 al 4. 
  # Cerrar agente.
  # Formatear bucket generado para comunicarlo por correo o por Oficio SBS.
  # Obtener el Log (Time - Descripción)
  # Exportar agent, ebFormatted y PIDlog
  

  ##### Testing -----
  agent <- createAgent(idCoopac = "01342",
                       carpeta = "test/datatest/01342/",
                       periodoInicial = "201901",
                       periodoFinal   = "202010")
  
  bucket      <- interrogateAgent(agent)
  agent       <- closeAgent(agent, bucket)
  ebFormatted <- formatBucket(bucket)
  PIDlog      <- getlog(getIdProcesoFromAgent(agent))
  
  saveOutputs(agent, bucket, ebFormatted, PIDlog)
  