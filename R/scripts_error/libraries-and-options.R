##### 1. Librerias opciones ----- 
library(tidyverse)
library(stringr)
library(colr)
library(lubridate)

##### 2. Opciones ----- 

options(warn = -1)
options(scipen = 999)

##### 3. Archivos de configuración ----- 
archivo.CuadreContable     = "config/config1.txt"
archivo.EstructuraBase     = "config/config2.txt"
archivo.RepositorioErrores = "config/config3.txt"
archivo.RepositorioAlertas = "config/config4.txt"

#### 4. Parametros globales ----- 

global.alcance = c(201901:201912, 202001:202012, 202101:202112)

#### 5. Parametros por defecto ----- 

default.usuario <- "ANONYM"
default.carpeta  <- "test/datatest" 
default.bd <- c("BD01","BD02A","BD02B","BD03A","BD03B","BD04")

#####4. Inicializar archivos de configuracion

## Archivos precargados ----
initCuadreContable     <- function(){
  read_delim(archivo.CuadreContable, 
             "\t", escape_double = FALSE, col_types = cols(ANO = col_double(), 
                                                           CODIGO_ENTIDAD = col_double(), ENTIDAD = col_character(), 
                                                           KJU = col_double(), KRF = col_double(), 
                                                           KVE = col_double(), KVI = col_double(), 
                                                           PERIODO = col_double(), TIPOENTIDAD = col_character()), 
             trim_ws = TRUE, progress = F) %>% return()
}
initEstructuraBase     <- function(){ 
  read_delim(archivo.EstructuraBase, 
             "\t", escape_double = FALSE, col_types = cols(BD = col_character(), 
                                                           CAMPO = col_character(), 
                                                           DESCRIPCION = col_character(),  
                                                           NRO = col_double(), 
                                                           TIPO = col_character()), progress = F)  %>% return()
}
initRepositorioErrores <- function(){
  read_delim(archivo.RepositorioErrores, 
             "\t", escape_double = FALSE, col_types = cols(Cod = col_double(), 
                                                           Descripcion = col_character(), 
                                                           Tipo = col_character()),
             locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, progress = F) %>% return()
}