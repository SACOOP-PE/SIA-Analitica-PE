# Validador de Base de Datos Crediticias | Coopac Nivel 2B y 3 
## Superintendencia Adjunta de Cooperativas
 
En el presente validador se distinguen 04 capas de validación.
 
 1. ejecutar_validacion_layer1 : i) archivos duplicados, ii) archivos faltantes 
 2. ejecutar_validacion_layer2 : i) columnas faltantes, ii) columnas sobrantes iii) columnas vacias
 3. ejecutar_validacion_layer3 : i) cuadre contable BD1: KVI,KRF,KVE,KJU (2:KRE) ii) operaciones/granatías dups o vacías iii) cuadre BD01/BD02A y cuadre BD03A/BD03B
 4. ejecutar_validacion_layer4 : i) verificación de datos (formato, dígitos) por y entre columnas en cada Base de Datos (error tipo 1,2,3)
 
El proceso se inicia con el objeto "header" que contiene la informacion minima del proceso de validacion. 
Y posteriormente inicia el objeto "error_bucket" que contiene el detalle de los errores detectados. 
 
Retorna el objeto error_bucket con la información necesaria para comunicarla efectivamente a la Coopac.
