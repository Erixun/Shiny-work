

library(xml2)
library(shinythemes)
library(ggplot2)
library(scales)
url = 'http://www7.slv.se/apilivsmedel/LivsmedelService.svc/Livsmedel/Naringsvarde/20190101'
data = read_xml(url)

#Namn på livsmedel
lnamn = xml_text(xml_find_all(data, "//Livsmedel/Namn"))
ordn = lnamn[order(lnamn)] #i bokstavsordning

#Noder för livsmedlens respektive näringsvärden
nar_nodes = xml_find_all(data, "//Naringsvarden")

makro = c("Energi (kcal)", "Fett", "Kolhydrater", "Socker totalt", "Protein",  "Fibrer", "Fullkorn totalt", "Alkohol", "Vatten")
mikro = unlist(strsplit("Fosfor,Järn,Kalcium,Kalium,Magnesium,Natrium,Selen,Zink,Vitamin A,Retinol,Vitamin D,Vitamin E,β-Karoten,Tiamin,Riboflavin,Vitamin C,Niacin,Niacinekvivalenter,Vitamin B12,Vitamin B6,Folat,Salt", ","))
kb = 2400 #kaloribehov
rdi = c(kb, 0.33*kb/9, 0.15*kb/4, 0.52*kb/4, 30, 10, 2000, 1)

ui = fixedPage(
  theme = shinytheme("cosmo"),
  titlePanel(strong("Din Måltid"), windowTitle = "Din Måltid"),
  
        fixedRow(
           column(4, #Tabell med näring för valt livsmedel
                 htmlOutput("valt"),
                 tableOutput("tbl")),
           column(4, #Sökfunktioner
                  h4(strong("Sökning")),
                  textInput("lm",
                    NULL),
                  uiOutput("sl_select"),
                  sliderInput("vikt", 
                    NULL, 
                    post = " g",
                    min = 0, 
                    max = 400, 
                    value = 100),
                  checkboxGroupInput("dn",
                     "Detaljnivå",
                     choices = c("Total", "Makro", "Mikro"),
                     inline = T,
                     selected = "Makro"),
          column(3),
          column(9, #Knappar för lägga till/tag bort från måltid
                  actionButton("add", "Lägg till"), 
                  actionButton("del", "Tag bort") )
      
          ), #Kolumn med tabbar för Måltid och Summerad näring:
          column(4, tabsetPanel(id="ctab", 
            tabPanel("Måltid",
                     tags$div(id="Måltid")),
            tabPanel("Summering", 
                     tableOutput("summering") ) )
        )
    ),
  em("(Datakälla: Livsmedelsverket)")
)


server = function(input, output) {
  #Valt livsmedel 
  output$valt = renderText({
    paste("<h4><b>", input$sl, "</b></h4>")
  })
  #Val för selectInput
  val = reactive({
    if(input$lm == "") {
      ordn
    } else {
      ordn[grep(input$lm, ordn)]
    }
  })
  
  output$sl_select = renderUI({
    selectInput("sl",
                "Valt livsmedel",
                choices = val() 
                )
  })
  
  dig <- reactiveValues()

  observeEvent(input$add, {
    req(input$sl)
    dig$dList <- c(isolate(dig$dList), isolate(input$add) )
    insertUI(
      selector = "#Måltid",
      ui = tableOutput("maltid")
      )
  })

  myValues <- reactiveValues()
  gram <- reactiveValues()
  nutr <- reactiveValues()
  vard <- reactiveValues()
  nutr$dList <- 0
  vard$dList <- list()
  
  observeEvent(input$add, {
    req(input$sl)
    if(input$add > 0 ) {
      myValues$dList <- c(isolate(myValues$dList), isolate(input$sl))
      gram$dList <- c(isolate(gram$dList), isolate(input$vikt))
      if(isolate(input$sl) %in% lnamn) {
        namn = lnamn[grep(isolate(input$sl), lnamn)]
        ix = which(lnamn %in% namn)[1]
      } else {
        ix = 0
      }
      
      if(ix != 0) {
        Värde = .01*isolate(input$vikt)*as.numeric(gsub(",", ".", xml_text(xml_find_all(nar_nodes[ix], ".//Varde")) ))
        nar_namn2 = xml_text(xml_find_all(nar_nodes[ix], ".//Namn"))
        vard$dList <- c(isolate(vard$dList), list(Värde) )
        nutr$dList <- isolate(nutr$dList) + Värde
      }
  } })
  
  observe({
    if(input$del > 0 && length(isolate(vard$dList)) > 0 ){
      myValues$dList <- isolate(myValues$dList[-length(myValues$dList) ])
      gram$dList <- isolate(gram$dList[-length(gram$dList) ])
      nutr$dList <- isolate(nutr$dList) - isolate(vard$dList[[length(vard$dList)]])
      vard$dList <- isolate(vard$dList[-length(vard$dList)])
    }
  })
  #Skapa tabell för din måltid
  output$maltid<-renderTable(colnames = F,{
    if(input$add > 0) {
    dataf = data.frame(Livsmedel = myValues$dList, Mängd = isolate(gram$dList), enhet = rep("g", length(isolate(gram$dList) ) ) )
    names(dataf) = c("Livsmedel", "Mängd", "")
    dataf
    }
  })
  #Skapa tabell med summerad näring för din måltid
  output$summering <- renderTable(colnames = F,{
    req(input$sl)
    if(input$add >= 0 ) {
      if(isolate(input$sl) %in% lnamn) {
        namn = lnamn[grep(isolate(input$sl), lnamn)]
        ix = which(lnamn %in% namn)[1]
      } else {
        ix = 0
      }
      
      if(ix != 0 ) {
      Nutrienter = xml_text(xml_find_all(nar_nodes[ix], ".//Namn"))
      Enheter = xml_text(xml_find_all(nar_nodes[ix], ".//Enhet"))
      Värdena = nutr$dList
      sumdf <- data.frame(Nutrient = Nutrienter, Värden = abs(Värdena), Enhet = Enheter)
      
        if("Total" %in% input$dn ) {
          sumdf
          
        } else if(input$dn == "Makro" && length(input$dn) == 1) {
          ix1 = which(sumdf$Nutrient %in% makro)
          sumdf[ix1,][c(1,7,3,8,4,6,9,2,5),]
          
        } else if(input$dn == "Mikro" && length(input$dn) == 1) {
          ix2 = which(sumdf$Nutrient %in% mikro)
          sumdf[ix2,]
          
        } else if(all(c("Makro", "Mikro") %in% input$dn) ) {
          ix1 = which(sumdf$Nutrient %in% makro)
          ix2 = which(sumdf$Nutrient %in% mikro)
          rbind.data.frame(sumdf[ix1,][c(1,7,3,8,4,6,9,2,5),], sumdf[ix2,])
        }
      }
    }
  })
  #Skapa tabell med näring för valt livsmedel
  output$tbl = renderTable(na = "--",colnames = F,{

    req(input$sl)
    if(input$sl %in% lnamn) {
      namn = lnamn[grep(input$sl, lnamn)]
      ix = which(lnamn %in% namn)[1]
    } else {
      ix = 0
    }
    
    if(ix != 0) {
      Nutrient = xml_text(xml_find_all(nar_nodes[ix], ".//Namn"))
      Värde = .01*input$vikt*as.numeric(gsub(",", ".", xml_text(xml_find_all(nar_nodes[ix], ".//Varde")) ))
      Enhet = xml_text(xml_find_all(nar_nodes[ix], ".//Enhet"))
      df = data.frame(Nutrient, Värde, Enhet)
      
      if("Total" %in% input$dn ) {
        df
        
      } else if(input$dn == "Makro" && length(input$dn) == 1) {
        ix1 = which(df$Nutrient %in% makro)
        df[ix1,][c(1,7,3,8,4,6,9,2,5),]
  
      } else if(input$dn == "Mikro" && length(input$dn) == 1) {
        ix2 = which(df$Nutrient %in% mikro)
        df[ix2,]
        
      } else if(all(c("Makro", "Mikro") %in% input$dn) ) {
        ix1 = which(df$Nutrient %in% makro)
        ix2 = which(df$Nutrient %in% mikro)
        rbind.data.frame(df[ix1,][c(1,7,3,8,4,6,9,2,5),], df[ix2,])
      }
    }
  }) 
}

shinyApp(ui = ui, server = server)

