##### 1. Librerias opciones ----- 

# install.packages(setdiff(c("tidyverse", "stringr", "colr", "lubridate"), 
#                          rownames(installed.packages()))) 
library(tidyverse)
library(stringr)
library(colr)
library(lubridate) 
library(openxlsx)
##### 2. Opciones ----- 

options(warn = -1)
options(scipen = 999)

##### 3. Archivos de configuración ----- 
archivo.CuadreContable = "config/config1.txt"
archivo.EstructuraBase = "config/config2.txt"
archivo.RepositorioErrores = "config/config3.txt"
archivo.RepositorioAlertas = "config/config4.txt"

#### 4. Parametros globales ----- 
global.alcance = c(201901:201912, 202001:202012, 202101:202112)

#### 5. Parametros por defecto ----- 
default.usuario <- "ANONYM"
default.carpeta <- "test/datatest"
default.bd      <- c("BD01","BD02A","BD02B","BD03A","BD03B","BD04")

### 6. Inicializar archivos de configuracion ----

initCuadreContable <- function(){
  read_delim(archivo.CuadreContable,
             "\t", escape_double = FALSE, 
             col_types = cols(C1403 = col_double(),
                              C14090702 = col_double(), 
                              C14090701 = col_double(),
                              C14090901 = col_double(), 
                              PeriodoId_1 = col_skip()),
             trim_ws = TRUE) %>% return()
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
             "\t", escape_double = FALSE, col_types = cols(Cod = col_number(), 
                                                           Descripcion = col_character(), 
                                                           Criticidad = col_character()),
             locale = locale(encoding = "ISO-8859-1"), progress = F)  %>%
    return()
}
initRepositorioAlertas <- function(){
  read_delim(archivo.RepositorioAlertas, 
             "\t", escape_double = FALSE, col_types = cols(CodAlerta   = col_number(), 
                                                           Descripcion = col_character()),
             locale = locale(encoding = "ISO-8859-1"), progress = F)  %>%
    return()
}