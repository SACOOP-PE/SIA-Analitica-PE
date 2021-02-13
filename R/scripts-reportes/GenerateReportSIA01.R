generar_reporte_T1 <- function(idProceso) {
  
  eb <- read_excel(paste0("test/output/resultados_", idProceso, ".xlsx"), sheet = "bucketOficio", 
                   col_types = c("text", "text", "text", "text", "text", "text", "text"))
  agente <- read_excel(paste0("test/output/resultados_", idProceso, ".xlsx"), sheet = "agente",
                       col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text","text"))
  
  if (nrow(eb %>% filter(Cod %in% c(101,102))) >0) {
    resumenValidacion <- "El proceso de validación no concluyó satisfactoriamente. {0}, de las cuales tienen criticidad ALTA, ello da como resultado {1} archivos inconsistentes."
  }
  else{
    resumenValidacion <- "El proceso de validación concluyó satisfactoriamente. Las observaciones deben ser sometidas a revisión por el analista SACOOP. Se identificaron {0} observaciones, de las cuales {1} con Críticos, ello da como resultado {2} archivos inconsistentes."
  }
  
  
  # Head & cols ----
  
  myhead.left <- "SUPERINTENDENCIA ADJUNTA DE COOPERATIVAS"
  myhead.center1 <- "REPORTE DE VALIDACIÓN DE BASE DE DATOS CREDITICIAS - COOPAC N2B/N3"
  myhead.center2 <- "(Según Oficio Múltiple SBS N° 22269-2020)"
  myhead.lbl1 <- "Nombre de la Coopac:"
  myhead.lbl2 <- "Nivel modular:"
  myhead.lbl3 <- "I. Resultados de la validación:"
  myhead.lbl4 <- "Periodo Inicio:"
  myhead.lbl5 <- "Periodo Fin:"
  myhead.lbl6 <- "Fecha de Validación:"
  myhead.lbl7 <- "Periodicidad:"
  myhead.lbl8 <- "II. Detalle de las observaciones:"
  
  bucket.lbl1 <- "Periodo"
  bucket.lbl2 <- "BD"
  bucket.lbl3 <- "Código"
  bucket.lbl5 <- "Descripción del error"
  bucket.lbl6 <- "Categoría"
  bucket.lbl7 <- "Tipo"
  bucket.lbl8 <- "Detalle"
  
  # Dinamico ----
  
  myhead.txt1 <- paste0(agente %>% pull(NombreCoopac) %>% first()," (",agente %>% pull(Coopac) %>% first(),")")
  myhead.txt2 <- paste0("Nivel ", agente %>% pull(NivelCoopac) %>% first())
  myhead.txt3 <- if_else(nrow(eb %>% filter(Cod %in% c(101,102))) >0,
                         str_replace_all(resumenValidacion, c("\\Q{0}" = eb %>% pull(Descripcion) %>% toString(),
                                                              "\\Q{1}" = bucket %>% pull(num1) %>% sum() %>% toString())),
                         str_replace_all(resumenValidacion, c("\\Q{0}" = toString(nrow(eb)),
                                                              "\\Q{1}" = toString(nrow(eb %>% filter(Criticidad == "CRITICO"))),
                                                              "\\Q{2}" = eb %>% 
                                                                mutate(filename = paste0(agente %>% pull(Coopac), "_",BD ,"_" ,Periodo, ".txt")) %>% 
                                                                pull(filename) %>% unique() %>% length() %>% toString())))
  myhead.txt4 <- agente %>% pull(PeriodoInicial) %>% first()
  myhead.txt5 <- agente %>% pull(PeriodoFinal) %>% first()
  myhead.txt6 <- format(Sys.time(), "%d/%m/%Y")
  myhead.txt7 <- "Mensual"
  
  # Estilos ----
  
  myhead.left.style <- createStyle(fontSize = 12, 
                                   fontColour = "#252850",
                                   textDecoration = c("BOLD","ITALIC"))
  
  myhead.center1.style <- createStyle(fontSize = 18, 
                                      fontColour = "#252850",
                                      textDecoration = c("BOLD"),
                                      halign = "center")
  myhead.center2.style <- createStyle(fontSize = 12, 
                                      textDecoration = c("ITALIC"))
  
  myhead.centerGrafico.style <- createStyle(fontSize = 18, 
                                            fontColour = "#252850",
                                            textDecoration = c("BOLD", "underline"),
                                            halign = "left")
  
  myhead.lbl.style <- createStyle(fontSize = 12, 
                                  textDecoration = c("BOLD"))
  myhead.lbl2.style <- createStyle(fontSize = 14, 
                                  textDecoration = c("BOLD"))
  myhead.lbl3.style <- createStyle(fontSize = 11, 
                                   wrapText = T, 
                                   borderStyle = "thin",
                                   halign= "left", 
                                   valign = "center")
  
  myhead.lblresultados.style <- createStyle(fontSize = 12, 
                                            textDecoration = c("BOLD"),
                                            halign = "left",
                                            valign = "top")
  
  myhead.resultados.style <- createStyle(fontSize = 12, 
                                         fontColour = "#FF0040",
                                         textDecoration = c("BOLD","ITALIC"),
                                         halign = "left",
                                         valign = "top",
                                         wrapText = T)
  
  bucket.head.style <- createStyle(fontSize = 12, border = "TopBottomLeftRight ", fgFill = "#bfd1e7")
  
  bucket.body.style <- createStyle(fontSize = 11, wrapText = T, borderStyle = "thin", halign= "left", valign = "center", border ="TopBottomLeftRight")
  
  # CreateWorkbook ----
  wb <- createWorkbook()
  addCreator(wb, "PROYECTO SIA SACOOP")
  addWorksheet(wb, "Reporte de Validación BD", gridLines = F)
  
  modifyBaseFont(wb, fontSize = 11, fontColour = "black", 
                 fontName = "Arial Narrow")
  
  mergeCells(wb, 1, cols = 3:17, rows = 5:6)
  
  mergeCells(wb, 1, cols = 9:10, rows = 9)
  mergeCells(wb, 1, cols = 9:10, rows = 10)
  mergeCells(wb, 1, cols = 14:15, rows = 9)
  mergeCells(wb, 1, cols = 14:15, rows = 10)
  
  mergeCells(wb, 1, cols = 6:17, rows = 12:13)
  mergeCells(wb, 1, cols = 3:5, rows = 12)
  
  mergeCells(wb, 1, cols = 6:13, rows = 17)
  mergeCells(wb, 1, cols = 14:15, rows = 17)
  
  map(18:(nrow(eb)+18),~ mergeCells(wb, 1, 6:13, .))
  map(18:(nrow(eb)+18),~ mergeCells(wb, 1, 14:15, .))
  
  # WriteData ----
  writeData(wb, 1, myhead.left, 2, 2) 
  addStyle(wb, 1, myhead.left.style, cols = 2, rows = 2)
  writeData(wb, 1, myhead.center1, 3, 5)
  addStyle(wb, 1, myhead.center1.style, cols = 3:17, rows = 5)
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
  writeData(wb, 1, myhead.lbl8, 3, 15)
  addStyle(wb, 1, myhead.lbl2.style, cols = 3, rows = 15)
  
  addStyle(wb, 1, myhead.lblresultados.style, cols = 14, rows = 10)
  addStyle(wb, 1, myhead.lbl3.style, cols =6:17, rows = 12:13, gridExpand = T)
  addStyle(wb, 1, myhead.resultados.style, cols = 15, rows = 9)
  
  writeData(wb, 1, myhead.txt1, 5, 9)
  writeData(wb, 1, myhead.txt2, 5, 10)
  writeData(wb, 1, myhead.txt3, 6, 12)
  writeData(wb, 1, myhead.txt4, 11, 9)
  writeData(wb, 1, myhead.txt5, 11, 10)
  writeData(wb, 1, myhead.txt6, 16, 9)
  writeData(wb, 1, myhead.txt7, 16, 10)
  
  writeData(wb, 1, bucket.lbl1, 3, 17)
  writeData(wb, 1, bucket.lbl2, 4, 17) 
  writeData(wb, 1, bucket.lbl3, 5, 17)
  writeData(wb, 1, bucket.lbl5, 6, 17)
  writeData(wb, 1, bucket.lbl6, 14, 17)
  writeData(wb, 1, bucket.lbl7, 16, 17)
  writeData(wb, 1, bucket.lbl8, 17, 17)
  
  addStyle(wb, 1, bucket.head.style, cols =3:17, rows = 17)
  
  setRowHeights(wb, 1, 18:(nrow(eb)+18), heights = 33.7)
  writeData(wb, 1, eb %>% select(Periodo, BD, Cod, Descripcion), 3, 18, colNames = F, rowNames = F)
  writeData(wb, 1, eb %>% select(Categoria), 14, 18, colNames = F, rowNames = F)
  writeData(wb, 1, eb %>% select(Criticidad), 16, 18, colNames = F, rowNames = F)
  addStyle(wb, 1, bucket.body.style  , cols =3:17, rows = 18:(18+(nrow(eb)-1)), gridExpand = T)
  conditionalFormatting(wb, 1,
                        cols = 16,
                        rows = 18:(18+(nrow(eb)-1)),
                        type = "notContains",
                        rule = "NO", 
                        style = createStyle(fontColour = "#9C0006", textDecoration = "bold"))
  
  img <- "R/scripts-reportes/logo-sbs.png"
  insertImage(wb, 1, img, startRow = 1, startCol = 16, width = 1.90, height = 0.90)
  setColWidths(wb, 1, 1:2, widths = 5.2)

  if (nrow(eb %>% filter(Cod %in% c(101,102))) >0) {
    saveWorkbook(wb, "test/output/SIA_Report_T1.xlsx", overwrite = TRUE)
    file.show("test/output/SIA_Report_T1.xlsx")
    return(wb)
  }  
  
  # Add plot - detalles de errores ----
  
  addWorksheet(wb, "Estado de archivos")
  
  mergeCells(wb, 2, cols = 2:8, rows = 4)
  writeData(wb, 2, "RESUMEN DE ARCHIVOS POR CRITICIDAD", 2, 4)
  addStyle(wb, 2, myhead.centerGrafico.style, cols = 2:8, rows = 4)
  
  mergeCells(wb, 2, cols = 2:8, rows = 33)
  writeData(wb, 2, "RESUMEN DE ARCHIVOS SEGÚN BD-CRITICIDAD", 2, 33)
  addStyle(wb, 2, myhead.centerGrafico.style, cols = 2:8, rows = 33)
  
  ##plots----
  print(generar_grafico_T1(idProceso, 1))
  insertPlot(wb, "Estado de archivos", startCol = 2, startRow = 6, fileType = "png",  width = 36.20, height = 12.21 ,units = "cm")
  
  print(generar_grafico_T1(idProceso, 2))
  insertPlot(wb, "Estado de archivos", startCol = 2, startRow = 35, fileType = "png",  width = 56.20, height = 12.21 ,units = "cm")
  
  ##detalle por cada error ----
  numObs <- which((bucket$Cod %in% c(201:203, 301:308)) == FALSE)

  for (i in 1:nrow(filter(bucket,(Cod %in% c(201:203, 301:308)) == FALSE))){

    nombreSheet <- filter(bucket,(Cod %in% c(201:203, 301:308)) == FALSE)[i,] %>% select(BD, Cod, Periodo) %>%
      apply(1, paste, collapse = "-" )

    addWorksheet(wb, nombreSheet)
    writeFormula(wb, i+2, startRow = 2, x= makeHyperlinkString(sheet = "Reporte de Validación BD", row = 1, text = "Volver"))
    writeFormula(wb, 1, startRow = 17+numObs[i], startCol = 17, x= makeHyperlinkString(sheet = nombreSheet,
                                                                                       row = 1,
                                                                                       text = "Ver más detalle"))

    writeData(wb, i+2, getObservaciones(agent, bucket, i), startRow = 4, colNames = T, rowNames = F)
  }
  
  # Save file xlsx ----
  saveWorkbook(wb, "test/output/SIA_Report_T1.xlsx", overwrite = TRUE)
  file.show("test/output/SIA_Report_T1.xlsx")
}
getObservaciones   <- function(agente, eb, roweb) {
  
  eb <- eb %>% filter((Cod %in% c(201:203, 301:308)) == FALSE) %>% rowwise() %>%
    mutate(filename = paste0(CodCoopac, "_",BD ,"_" ,Periodo, ".txt")) %>%
    select(Cod, filename, txt1)
  
  operaciones <- unlist(eb[roweb,] %>% pull(txt1) %>% str_split(", "))
  ruta        <- getRuta(getCarpetaFromAgent(agente), eb[roweb,] %>% pull(filename))
  
  obs <- quitarVaciosBD(ruta) %>%
    filter(cgrep(quitarVaciosBD(ruta), getCodigoBD(getBDFromRuta(ruta)))[[1]] %in% operaciones)
  
  return(obs)
}
generar_grafico_T1 <- function(idProceso, numGraf) {
  
  eb <- read_excel(paste0("test/output/resultados_", idProceso, ".xlsx"), sheet = "bucketOficio", 
                   col_types = c("text", "text", "text", "text", "text", "text", "text")) %>% rowwise() %>%
    mutate(dateYear  = substr(Periodo, 1, 4),
           dateMonth = as.integer(substr(Periodo, 5, 6))) %>% 
    select(BD, dateYear, dateMonth, Criticidad) %>% 
    arrange(dateYear, dateMonth, BD)
  
  eb$BD <- factor(eb$BD,
                  levels=c("BD01","BD02A","BD02B","BD03A","BD03B","BD04"),
                  labels=c("BD01","BD02A","BD02B","BD03A","BD03B","BD04"),
                  ordered=TRUE)
  
  eb$Criticidad <- factor(eb$Criticidad, 
                          levels=c("CRITICO","NO CRITICO"),
                          labels=c("Errores críticos", "Errores no críticos"),
                          ordered = T)
  
  eb$dateMonth <- factor(eb$dateMonth,
                         levels=as.character(1:12),
                         labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic"),
                         ordered=TRUE)
  
  fct_rev(eb$Criticidad)
  
  colors <- c("firebrick1","dodgerblue1")
  
  if (numGraf == 1) {
    plot <- eb %>%
      ggplot(aes(dateMonth, BD, group = Criticidad)) +
      scale_fill_manual(values=colors, guide = guide_legend(reverse = FALSE)) +
      geom_tile(aes(fill = Criticidad, width=0.95, height=0.95)) +
      facet_wrap(~ dateYear) +
      labs(x = "MESES",
           y = 'BASES DE DATOS CREDITICIAS') +
      theme_bw()
    
    return(plot)
  }
  if (numGraf == 2) {
    plot <-  eb %>%
      group_by(BD, Criticidad, dateYear) %>% tally() %>%
      ggplot(aes(BD, n, fill = Criticidad)) +
      facet_wrap(~ dateYear) +
      geom_bar(position= position_stack(reverse = FALSE), stat='identity', width = 0.5) +
      labs(x = "BASES DE DATOS CREDITICIAS",
           y = 'n errores') +
      scale_fill_manual(values=colors, guide = guide_legend(reverse = FALSE)) +
      geom_text(aes(label=n), vjust= 0,
                color="black", size=4, position = position_nudge(x= 0, y =0.5)) +
      coord_flip() +
      theme_bw()
    
    return(plot)
  }
}

library(tidyquant)
library(ggplot2)
library(plyr)
library(plotly)