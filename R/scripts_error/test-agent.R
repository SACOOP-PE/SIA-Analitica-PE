rm(list=ls()) 
source("R/scripts_error/libraries-and-options.R")
source("R/scripts_error/agent.R")
source("R/scripts_error/utils.R")
source("R/scripts_error/error-handler.R")
source("R/scripts_error/layer0-prerequisites.R")
source("R/scripts_error/layer1-validation-error1.R")
source("R/scripts_error/layer2-validation-error2.R")
##### Testing -----

# Inicializar
agent <- create_agent(idCoopac = "01172",
                      coopacCarpeta  = "test/datatest/",
                      periodoInicial = "201901",
                      periodoFinal   = "202010",
                      bds            = list(c("BD01", "BD02A", "BD02B", "BD03A", "BD03B", "BD04")))

bucket  <- interrogate(agent)
bucket2 <- procesarBucket2(agent, bucket)