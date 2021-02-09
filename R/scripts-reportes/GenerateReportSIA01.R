generar_reporte_T1 <- function(eb, agente) {
  
  #Head & cols ----
  myhead.left <- "SUPERINTENDENCIA ADJUNTA DE COOPERATIVAS"
  myhead.center1 <- "REPORTE DE VALIDACIÓN DE BASE DE DATOS CREDITICIAS - COOPAC N2B/N3"
  myhead.center2 <- "(Según Oficio Múltiple SBS N° 22269-2020)"
  myhead.lbl1 <- "Nombre de la Coopac:"
  myhead.lbl2 <- "Nivel modular:"
  myhead.lbl3 <- "Observaciones:"
  myhead.lbl4 <- "Inicio:"
  myhead.lbl5 <- "Fin:"
  myhead.lbl6 <- "Resultado:" 
  
  bucket.lbl1 <- "Periodo"
  bucket.lbl2 <- "BD"
  bucket.lbl3 <- "Código"
  bucket.lbl4 <- "Tipo"
  bucket.lbl5 <- "Descripción del error"
  bucket.lbl6 <- "Categoría"
  bucket.lbl7 <- "Criticidad"
  bucket.lbl8 <- "Detalle" 
  
  #Dinámico ----
  myhead.txt1 <- paste0(agente %>% pull(NombreCoopac) %>% first()," (",agente %>% pull(Coopac) %>% first(),")")
  myhead.txt2 <- agente %>% pull(NivelCoopac) %>% first()
  myhead.txt3 <- nrow(eb)
  myhead.txt4 <- agente %>% pull(PeriodoInicial) %>% first()
  myhead.txt5 <- agente %>% pull(PeriodoFinal) %>% first()
  myhead.txt6 <- "CON OBSERVACIONES DE CRITICIDAD ALTA."
  
  #Estilos ----
  
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
  
  #CreateWorkbook ----
  wb <- createWorkbook()
  addCreator(wb, "PROYECTO SIA SACOOP")
  addWorksheet(wb, "Reporte de Validación BD", gridLines = F)
  
  modifyBaseFont(wb, fontSize = 11, fontColour = "black", 
                 fontName = "Arial Narrow")
  
  mergeCells(wb, 1, cols = 14, rows = 9:10)
  mergeCells(wb, 1, cols = 15:16, rows = 9:10)
  
  mergeCells(wb, 1, cols = 7:14, rows = 13)
  
  map(14:(nrow(eb)+14),~ mergeCells(wb, 1, 7:14, .))

  #WriteData ----
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
  writeData(wb, 1, myhead.lbl3, 3, 11)
  addStyle(wb, 1, myhead.lbl.style, cols = 3, rows = 11)
  writeData(wb, 1, myhead.lbl4, 11, 9)
  addStyle(wb, 1, myhead.lbl.style, cols = 11, rows = 9)
  writeData(wb, 1, myhead.lbl5, 11, 10)
  addStyle(wb, 1, myhead.lbl.style, cols = 11, rows = 10)
  writeData(wb, 1, myhead.lbl6, 14, 9)
  addStyle(wb, 1,  myhead.lblresultados.style , cols =14, rows = 9)
  writeData(wb, 1, myhead.txt1, 5, 9)
  writeData(wb, 1, myhead.txt2, 5, 10)
  writeData(wb, 1, myhead.txt3, 5, 11)
  writeData(wb, 1, myhead.txt4, 12, 9)
  writeData(wb, 1, myhead.txt5, 12, 10)
  
  writeData(wb, 1, bucket.lbl1, 3, 13)
  addStyle(wb, 1,  bucket.head.style  , cols =3:17, rows = 13)
  
  writeData(wb, 1, bucket.lbl2, 4, 13) 
  writeData(wb, 1, bucket.lbl3, 5, 13)
  writeData(wb, 1, bucket.lbl4, 6, 13)
  writeData(wb, 1, bucket.lbl5, 7, 13)
  writeData(wb, 1, bucket.lbl6, 15, 13)
  writeData(wb, 1, bucket.lbl7, 16, 13)
  writeData(wb, 1, bucket.lbl8, 17, 13)
  writeData(wb, 1, myhead.txt6, 15, 9)
  addStyle(wb, 1, myhead.resultados.style, cols = 15, rows = 9 )
  
  setRowHeights(wb, 1, 14:(nrow(eb)+14), heights = 33.7)
  writeData(wb, 1, eb %>% select(Periodo, BD, Cod, Tipo, Descripcion), 3, 14, colNames = F, rowNames = F)
  writeData(wb, 1, eb %>% select(Categoria, Criticidad), 15, 14, colNames = F, rowNames = F)
  addStyle(wb, 1,  bucket.body.style  , cols =3:17, rows = 14:(14+(nrow(eb)-1)), gridExpand = T)
  
  img <- "R/scripts-reportes/logo-sbs.png"
  insertImage(wb, 1, img, startRow = 2, startCol = 16, width = 1.95, height = 0.95)
  setColWidths(wb, 1, 1:2, widths = 5.2)
  saveWorkbook(wb, "test/output/SIA_Report_T1.xlsx", overwrite = TRUE)
  file.show("test/output/SIA_Report_T1.xlsx")
  
}
getObservaciones   <- function(agente, eb){
  
  eb <- eb %>% filter((Cod %in% c(201:203, 301:308)) == FALSE) %>% rowwise() %>%
    mutate(filename = paste0(CodCoopac, "_",BD ,"_" ,Periodo, ".txt")) %>%
    select(Cod, filename, txt1)
  
  operaciones <- unlist(eb[1,] %>% pull(txt1) %>% str_split(", "))
  ruta        <- getRuta(getCarpetaFromAgent(agente), eb[1,] %>% pull(filename))
  
  obs <- quitarVaciosBD(ruta) %>%
    filter(cgrep(quitarVaciosBD(ruta), getCodigoBD(getBDFromRuta(ruta)))[[1]] %in% operaciones)
  
  return(obs)
}

generar_reporte_T1(read_excel("test/output/resultados.xlsx", sheet = "bucketOficio", 
                              col_types = c("text", "text", "text", 
                                            "text", "text", "text", "text")),
                   read_excel("test/output/resultados.xlsx", sheet = "agente", 
                              col_types = c("text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text")))

########################################################
library(tidyquant)
library(ggplot2)
library(plyr)
library(plotly)


aleat <- read_excel("C:/Users/DPacheco/Desktop/aleat.xlsx", col_types = c("text", "text", "text", "text", "numeric","text"))

aleat$Inconsistencias <- factor(aleat$Inconsistencias, 
                                levels=c("BAJA","MEDIA", "ALTA"),
                                labels=c("Archivo sin errores", "Archivo con errores", "Archivo con errores graves"),
                                ordered = T)

aleat$BDCC <- factor(aleat$BDCC,
                     levels=c("BD01","BD02A","BD02B","BD03A","BD03B","BD04"),
                     labels=c("BD01","BD02A","BD02B","BD03A","BD03B","BD04"),
                     ordered=TRUE)
fct_rev(aleat$Inconsistencias)
aleat$dateMonth <- factor(aleat$dateMonth,
                          levels=as.character(1:12),
                          labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic"),
                          ordered=TRUE)

colors <- c("dodgerblue4", "dodgerblue1", "firebrick1")

# ggplot(aleat, aes(dateMonth, BDCC, fill = factor(Inconsistencias))) + 
#   geom_tile() + 
#    facet_grid(dateYear~dateMonth, scales="free") +
#   scale_fill_manual(values=colors)
#   #scale_fill_gradient() +
#   xlab("Periodo") + labs(fill = "Criticidad") 


ggplot(aleat, aes(dateMonth, BDCC, fill = factor(Inconsistencias))) + 
  geom_tile(aes(fill = factor(Inconsistencias),width=0.95, height=0.95)) + 
  facet_wrap(vars(dateYear)) +
  scale_fill_manual(values=colors, guide = guide_legend(reverse = TRUE))+
  #scale_fill_gradient() +
  xlab("") + ylab("") + labs(fill = " ") +
  theme(legend.position=("right"), legend.title = element_text(size=10)) + 
  theme(axis.text.y = element_text(hjust=0))






