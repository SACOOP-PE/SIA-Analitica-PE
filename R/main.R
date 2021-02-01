 
rm(list=ls())
 
# Scripts de entorno
source("R/scripts-entorno/libraries-and-options.R")
source("R/scripts-entorno/agent.R")
source("R/scripts-entorno/error-handler.R") 
source("R/scripts-entorno/reporting-manager.R")
source("R/scripts-entorno/utils.R")
 
# 1. Módulo de validaciones 
source("R/scripts-errores/layer0-validation.R")
source("R/scripts-errores/layer1-validation.R")
source("R/scripts-errores/layer2-validation.R")
source("R/scripts-errores/layer3-validation.R")
source("R/scripts-errores/layer4-validation.R")

# # 2. Módulo de alertas regulatorias
# source("R/scripts-alertas/layer0-alertas.R")
# source("R/scripts-alertas/layer1-alertas.R") 

# # 3. Módulo de analisis crediticio
# source("R/scripts-alertas/layer0-analisis.R")
# source("R/scripts-alertas/layer1-analisis.R") 


##### Create agent -----
agent <- createAgent(idCoopac = "01172",
                     carpeta = "test/datatest/01172/",
                     periodoInicial = "201901",
                     periodoFinal   = "201902")

##### Interrogar Modulo 1
bucket      <- interrogateAgent_mod1(agent)
 
##### Close agent -----
agent       <- closeAgent(agent, bucket)

##### Reporting -----
saveOutputs(agent, bucket, formatBucket(bucket))
