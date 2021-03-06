 
rm(list=ls())
 
# Scripts de entorno
source("R/scripts-entorno/libraries-and-options.R")
source("R/scripts-entorno/agent.R")
source("R/scripts-entorno/error-handler.R") 
source("R/scripts-entorno/reporting-manager.R")
source("R/scripts-entorno/utils.R")

# Script reportes
source("R/scripts-reportes/GenerateReportSIA01.R")
 
# 1. M�dulo de validaciones 
source("R/modulo1/layer0-validation.R")
source("R/modulo1/layer1-validation.R")
source("R/modulo1/layer2-validation.R")
source("R/modulo1/layer3-validation.R")
source("R/modulo1/layer4-validation.R")

# 2. M�dulo de alertas regulatorias
source("R/modulo2/layer0-alert.R")

# 3. M�dulo de analisis crediticio
source("R/modulo3/layer0-analysis.R")

##### Create agent -----
agent <- createAgent(idCoopac       = "01328",
                     periodoInicial = "202001",
                     periodoFinal   = "202004")

##### Interrogar Modulo 1
bucketErrores <- interrogateAgent_mod1(agent)
bucketAlertas <- interrogateAgent_mod2(agent, bucketErrores)

##### Reporting -----
saveOutputs(closeAgent(agent, bucketErrores), formatBucket(bucketErrores))
generar_reporte_T1(getIdProcesoFromAgent(agent), bucketErrores)
