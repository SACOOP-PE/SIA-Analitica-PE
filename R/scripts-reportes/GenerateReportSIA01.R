generar_reporte_T1 <- function(idProceso) {
  
  eb <- read_excel(paste0("test/output/resultados_", idProceso, ".xlsx"), sheet = "bucketOficio", 
                   col_types = c("text", "text", "text", "text", "text", "text", "text"))
  agente <- read_excel(paste0("test/output/resultados_", idProceso, ".xlsx"), sheet = "agente",
                       col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text","text"))
  
  resumenValidacion <- "El proceso de validación concluyó satisfactoriamente. Las observaciones deben ser sometidas a revisión por el analista SACOOP. Se identificaron {0} observaciones, de las cuales {1} tienen criticidad ALTA, ello da como resultado {2} archivos inconsistentes."
  
  # Head & cols ----
  
  myhead.left <- "SUPERINTENDENCIA ADJUNTA DE COOPERATIVAS"
  myhead.center1 <- "REPORTE DE VALIDACIÓN DE BASE DE DATOS CREDITICIAS - COOPAC N2B/N3"
  myhead.center2 <- "(Según Oficio Múltiple SBS N° 22269-2020)"
  myhead.lbl1 <- "Nombre de la Coopac:"
  myhead.lbl2 <- "Nivel modular:"
  myhead.lbl3 <- "I. Resultados de la validación:"
  myhead.lbl4 <- "Periodo Inicio:"
  myhead.lbl5 <- "Periodo Fin:"
  myhead.lbl6 <- "N° proceso:"
  myhead.lbl7 <- "Periodicidad"
  myhead.lbl8 <- "II. Detalle de las observaciones:"
  
  bucket.lbl1 <- "Periodo"
  bucket.lbl2 <- "BD"
  bucket.lbl3 <- "Código"
  bucket.lbl4 <- "Tipo"
  bucket.lbl5 <- "Descripción del error"
  bucket.lbl6 <- "Categoría"
  bucket.lbl7 <- "Criticidad"
  bucket.lbl8 <- "Detalle" 
  
  # Dinamico ----
  
  myhead.txt1 <- paste0(agente %>% pull(NombreCoopac) %>% first()," (",agente %>% pull(Coopac) %>% first(),")")
  myhead.txt2 <- paste0("Nivel ", agente %>% pull(NivelCoopac) %>% first())
  myhead.txt3 <- str_replace_all(resumenValidacion, c("\\Q{0}"  = toString(nrow(eb)),
                                                      "\\Q{1}"  = toString(nrow(eb %>% filter(Criticidad == "ALTA"))),
                                                      "\\Q{2}"  = eb %>% 
                                                                   mutate(filename = paste0(agente %>% pull(Coopac), "_",BD ,"_" ,Periodo, ".txt")) %>% 
                                                                   pull(filename) %>% unique() %>% length() %>% toString()
                                                      ))
  myhead.txt4 <- agente %>% pull(PeriodoInicial) %>% first()
  myhead.txt5 <- agente %>% pull(PeriodoFinal) %>% first()
  myhead.txt6 <- agente %>% pull(IdProceso) %>% first()
  myhead.txt7 <- "Mensual"
  
  # Estilos ----
  
  myhead.left.style <- createStyle(fontSize = 12, 
                                   fontColour = "#252850",
                                   textDecoration = c("BOLD","ITALIC"))
  
  myhead.center1.style <- createStyle(fontSize = 14, 
                                      fontColour = "#252850",
                                      textDecoration = c("BOLD"))
  myhead.center2.style <- createStyle(fontSize = 12, 
                                      #fontColour = "#0000FF",
                                      textDecoration = c("ITALIC"))
  
  myhead.lbl.style <- createStyle(fontSize = 12, 
                                  #fontColour = "#0000FF",
                                  textDecoration = c("BOLD"))
  myhead.lbl2.style <- createStyle(fontSize = 14, 
                                  #fontColour = "#0000FF",
                                  textDecoration = c("BOLD"))
  myhead.lbl3.style <- createStyle(fontSize = 11, 
                                   wrapText = T, 
                                   borderStyle = "thin",
                                   halign= "left", 
                                   valign = "center")
  
  myhead.lblresultados.style <- createStyle(fontSize = 12, 
                                            #fontColour = "#0000FF",
                                            textDecoration = c("BOLD"),
                                            halign = "left",
                                            valign = "top")
  
  myhead.resultados.style <- createStyle(fontSize = 12, 
                                         fontColour = "#FF0040",
                                         textDecoration = c("BOLD","ITALIC"),
                                         halign = "left",
                                         valign = "top",
                                         wrapText = T)
  
  bucket.head.style <- createStyle(fontSize = 12, border = "TopBottomLeftRight ", fgFill = "#bfd1e7"
                                   #bgFill = "#AAAAAA"
                                   )
  
  bucket.body.style <- createStyle(fontSize = 11, wrapText = T, borderStyle = "thin", halign= "left", valign = "center", border ="TopBottomLeftRight")
  
  # CreateWorkbook ----
  wb <- createWorkbook()
  addCreator(wb, "PROYECTO SIA SACOOP")
  addWorksheet(wb, "Reporte de Validación BD", gridLines = F)
  
  modifyBaseFont(wb, fontSize = 11, fontColour = "black", 
                 fontName = "Arial Narrow")
  
  mergeCells(wb, 1, cols = 9:10, rows = 9)
  mergeCells(wb, 1, cols = 9:10, rows = 10)
  mergeCells(wb, 1, cols = 14:15, rows = 9)
  mergeCells(wb, 1, cols = 14:15, rows = 10)
  mergeCells(wb, 1, cols = 6:17, rows = 12:13)
  mergeCells(wb, 1, cols = 3:5, rows = 12)
  
  mergeCells(wb, 1, cols = 7:14, rows = 26)
  
  map(27:(nrow(eb)+27),~ mergeCells(wb, 1, 7:14, .))
  
  # WriteData ----
  writeData(wb, 1, myhead.left, 2, 2) 
  addStyle(wb, 1, myhead.left.style, cols = 2, rows = 2)
  writeData(wb, 1, myhead.center1, 7, 6)
  addStyle(wb, 1, myhead.center1.style, cols = 7, rows = 6)
  writeData(wb, 1, myhead.center2, 9 ,7)
  addStyle(wb, 1, myhead.center2.style, cols = 9, rows = 7)
  writeData(wb, 1, myhead.lbl1, 3, 9)
  addStyle(wb, 1, myhead.lbl.style, cols = 3, rows = 9)
  writeData(wb, 1, myhead.lbl2, 3, 10)
  addStyle(wb, 1, myhead.lbl.style, cols = 3, rows = 10)
  writeData(wb, 1, myhead.lbl3, 3, 12)
  addStyle(wb, 1, myhead.lbl2.style, cols = 3, rows = 12)
  writeData(wb, 1, myhead.lbl4, 9, 9)
  addStyle(wb, 1, myhead.lbl.style, cols = 9, rows = 9)
  writeData(wb, 1, myhead.lbl5, 9, 10)
  addStyle(wb, 1, myhead.lbl.style, cols = 9, rows = 10)
  writeData(wb, 1, myhead.lbl6, 14, 9)
  addStyle(wb, 1, myhead.lbl.style, cols = 14, rows = 9)
  writeData(wb, 1, myhead.lbl7, 14, 10)
  addStyle(wb, 1, myhead.lbl.style, cols = 14, rows = 10)
  writeData(wb, 1, myhead.lbl8, 3, 24)
  addStyle(wb, 1, myhead.lbl2.style, cols = 3, rows = 24)
  
  addStyle(wb, 1, myhead.lblresultados.style, cols = 14, rows = 10)
  addStyle(wb, 1,  myhead.lbl3.style, cols =6:17, rows = 12.13, gridExpand = T)
  
  writeData(wb, 1, myhead.txt1, 5, 9)
  writeData(wb, 1, myhead.txt2, 5, 10)
  writeData(wb, 1, myhead.txt3, 6, 12)
  writeData(wb, 1, myhead.txt4, 11, 9)
  writeData(wb, 1, myhead.txt5, 11, 10)
  writeData(wb, 1, myhead.txt6, 16, 9)
  writeData(wb, 1, myhead.txt7, 16, 10)
  
  writeData(wb, 1, bucket.lbl1, 3, 26)
  writeData(wb, 1, bucket.lbl2, 4, 26) 
  writeData(wb, 1, bucket.lbl3, 5, 26)
  writeData(wb, 1, bucket.lbl4, 6, 26)
  writeData(wb, 1, bucket.lbl5, 7, 26)
  writeData(wb, 1, bucket.lbl6, 15, 26)
  writeData(wb, 1, bucket.lbl7, 16, 26)
  writeData(wb, 1, bucket.lbl8, 17, 26)
  addStyle(wb, 1,  bucket.head.style  , cols =3:17, rows = 26)
  addStyle(wb, 1, myhead.resultados.style, cols = 15, rows = 9)
  
  setRowHeights(wb, 1, 27:(nrow(eb)+27), heights = 33.7)
  writeData(wb, 1, eb %>% select(Periodo, BD, Cod, Tipo, Descripcion), 3, 27, colNames = F, rowNames = F)
  writeData(wb, 1, eb %>% select(Categoria, Criticidad), 15, 27, colNames = F, rowNames = F)
  addStyle(wb, 1,  bucket.body.style  , cols =3:17, rows = 27:(27+(nrow(eb)-1)), gridExpand = T)
  
  img <- "R/scripts-reportes/logo-sbs.png"
  insertImage(wb, 1, img, startRow = 2, startCol = 16, width = 1.95, height = 0.95)
  setColWidths(wb, 1, 1:2, widths = 5.2)
  
  # Add Multiple sheet by error ----
  numObs <- which((bucket$Cod %in% c(201:203, 301:308)) == FALSE)
  
  for (i in 1:nrow(filter(bucket,(Cod %in% c(201:203, 301:308)) == FALSE))){
    
    nombreSheet <- filter(bucket,(Cod %in% c(201:203, 301:308)) == FALSE)[i,] %>% select(BD, Cod, Periodo) %>%
      apply(1, paste, collapse = "-" )
    
    addWorksheet(wb, nombreSheet)
    
    writeData(wb, i+1, getObservaciones(agent, bucket, i), startRow = 4, colNames = T, rowNames = F)
    writeFormula(wb, i+1, startRow = 2, x= makeHyperlinkString(sheet = "Reporte de Validación BD", 
                                                               row = 1,
                                                               text = "Volver"))
    writeFormula(wb, 1, startRow = 26+numObs[i], startCol = 17, x= makeHyperlinkString(sheet = nombreSheet, 
                                                                                       row = 1,
                                                                                       text = "Ver más detalle"))
  }
  
  # Save file xlsx ----
  saveWorkbook(wb, "test/output/SIA_Report_T1.xlsx", overwrite = TRUE)
  file.show("test/output/SIA_Report_T1.xlsx")
  
}
getObservaciones   <- function(agente, eb, roweb){
  
  eb <- eb %>% filter((Cod %in% c(201:203, 301:308)) == FALSE) %>% rowwise() %>%
    mutate(filename = paste0(CodCoopac, "_",BD ,"_" ,Periodo, ".txt")) %>%
    select(Cod, filename, txt1)
  
  operaciones <- unlist(eb[roweb,] %>% pull(txt1) %>% str_split(", "))
  ruta        <- getRuta(getCarpetaFromAgent(agente), eb[roweb,] %>% pull(filename))
  
  obs <- quitarVaciosBD(ruta) %>%
    filter(cgrep(quitarVaciosBD(ruta), getCodigoBD(getBDFromRuta(ruta)))[[1]] %in% operaciones)
  
  return(obs)
}