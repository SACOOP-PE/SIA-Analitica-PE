 
rm(list=ls())
 
# Scripts de entorno
source("R/scripts-entorno/libraries-and-options.R")
source("R/scripts-entorno/agent.R")
source("R/scripts-entorno/error-handler.R") 
source("R/scripts-entorno/reporting-manager.R")
source("R/scripts-entorno/utils.R")
 
# 1. Módulo de validaciones 
source("R/modulo1/layer0-validation.R")
source("R/modulo1/layer1-validation.R")
source("R/modulo1/layer2-validation.R")
source("R/modulo1/layer3-validation.R")
source("R/modulo1/layer4-validation.R")

# # 2. Módulo de alertas regulatorias
source("R/modulo2/layer0-alertas.R")
# source("R/modulo2/layer1-alertas.R") #aún en desarrollo

# # 3. Módulo de analisis crediticio
source("R/modulo3/layer0-analysis.R")

##### Create agent -----
agent <- createAgent(idCoopac = "01106",
                     periodoInicial = "201901",
                     periodoFinal   = "202011")

##### Interrogar Modulo 1
bucket <- interrogateAgent_mod1(agent)
bucket <- interrogateAgent_mod2(agent, bucket)
 
##### Close agent -----
agent  <- closeAgent(agent, bucket)

##### Reporting -----
saveOutputs(agent, formatBucket(bucket))
