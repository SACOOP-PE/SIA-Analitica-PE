  rm(list = ls())
  source("R/scripts_error/libraries-and-options.R")
  source("R/scripts_error/utils.R")
  source("R/scripts_error/agent.R")
  source("R/scripts_error/error-handler.R")
  source("R/scripts_error/layer0-validation-error0.R")
  source("R/scripts_error/layer1-validation-error1.R")
  source("R/scripts_error/layer2-validation-error2.R")
  source("R/scripts_error/layer3-validation-error3.R")
  source("R/scripts_error/layer4-validation-error4.R")
  
  source("R/scripts_error/log-manager.R")
  source("R/scripts_error/reporting-manager.R")
 

  ##### Testing -----
  agent <- createAgent(idCoopac = "01172", 
                       periodoInicial = "201911",
                       periodoFinal   = "202010",coopacCarpeta = "test/datatest")
  
  bucket      <- interrogateAgent(agent)
  #agent       <- closeAgent(agent, bucket)
  ebFormatted <- formatBucket(bucket)
  
  PIDlog      <- getlog(getIdProcesoFromAgent(agent))
  
  saveOutputs(agent, ebFormatted, PIDlog)
  print(ebFormatted)