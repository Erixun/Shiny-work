

library(shiny)
library(shinythemes)
library(xml2)
library(readr)


data <- read_xml("slv_noder.txt")
#Namn på livsmedel
lnamn = xml_text(xml_find_all(data, "//Livsmedel/Namn"))
ordn = lnamn[order(lnamn)] #i bokstavsordning

#Noder och namn för livsmedlens respektive näringsvärden
nar_nodes = xml_find_all(data, "//Naringsvarden")
Nar_namn = xml_text(xml_find_all(nar_nodes[10], ".//Namn"))
Enh = xml_text(xml_find_all(nar_nodes[10], ".//Enhet"))
makro = c("Energi (kcal)", "Fett", "Kolhydrater", "Socker totalt", "Protein",  "Fibrer", "Fullkorn totalt", "Alkohol", "Vatten")
mikro = unlist(strsplit("Fosfor,Järn,Kalcium,Kalium,Magnesium,Natrium,Selen,Zink,Vitamin A,Retinol,Vitamin D,Vitamin E,β-Karoten,Tiamin,Riboflavin,Vitamin C,Niacin,Niacinekvivalenter,Vitamin B12,Vitamin B6,Folat,Salt", ","))

ui = fixedPage(
  theme = shinytheme("cosmo"),
  titlePanel(h1(strong(em("Din Måltid 2.0") ), align="center", style={'background-color: lightblue; padding-left: 10px; height: 45px;'}), windowTitle = "Din Måltid 2.0"  ),

        fixedRow(
           column(4, #Tabell med näring för valt livsmedel
                 htmlOutput("valt"),
                 tableOutput("tbl"),
           em("(Datakälla: Livsmedelsverket)")),
           column(4, #Sökfunktioner
                  h4(strong("Sökning")),
                  textInput("lm",
                    NULL),
                  fixedRow(column(8, uiOutput("sl_select") ),
                  column(4, br(),actionButton("add", "Lägg till") ) ),
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
                     selected = "Makro")#,
          ), 
          #Kolumn med tabbar för Måltid och Summerad näring:
          column(4, tabsetPanel(id="ctab", 
            tabPanel("Måltid",
                     tags$div(id="Måltid"),
                     br(),
                     column(4, actionButton("del", "Tag bort") ),
                     column(6, downloadButton("dl", "Hämta")) ),
            tabPanel("Näring", 
                     tableOutput("summering"),
                     br(),
                     downloadButton("dl2", "Hämta")) )
        )
    )
)


server = function(input, output) {
  
# Column 1 -----------------
  #Valt livsmedel 
  output$valt = renderText({
    paste("<h4><b>", input$sl, "</b></h4>")
  })

  # Visa tabell med näring för valt livsmedel
  output$tbl = renderTable(na = "--",colnames = F,{ 
    req(input$sl)
    ix = grep(input$sl, lnamn)[1]
    Värdena = numeric(55)
    ix2 = which(Nar_namn %in% xml_text(xml_find_all(nar_nodes[ix], ".//Namn")) )
    Värdena[ix2] = .01*input$vikt*as.numeric(gsub(",", ".", xml_text(xml_find_all(nar_nodes[ix], ".//Varde")) ))
    
    if("Total" %in% input$dn ) {
      data.frame(Nutrient = Nar_namn, Värde = Värdena, Enhet = Enh)
      
    } else if(input$dn == "Makro" && length(input$dn) == 1) {
      ix1 = which(Nar_namn %in% makro)[c(1,7,3,8,4,6,9,2,5)]
      data.frame(Nutrient = Nar_namn[ix1], Värde = Värdena[ix1], Enhet = Enh[ix1])
      
    } else if(input$dn == "Mikro" && length(input$dn) == 1) {
      ix2 = which(Nar_namn %in% mikro)
      data.frame(Nutrient = Nar_namn[ix2], Värde = Värdena[ix2], Enhet = Enh[ix2])
      
    } else if(all(c("Makro", "Mikro") %in% input$dn) ) {
      ix1 = which(Nar_namn %in% makro)[c(1,7,3,8,4,6,9,2,5)]
      ix2 = which(Nar_namn %in% mikro)
      data.frame(Nutrient = Nar_namn[c(ix1,ix2)], Värde = Värdena[c(ix1,ix2)], Enhet = Enh[c(ix1,ix2)])
    }
  }) 
# ------------------- end Column 1
  
# Column 2 ----------------------
  #Val för selectInput
  val = reactive({
    if(input$lm == "") {
      ordn
    } else { ordn[grep(input$lm, ordn)] }
  })
  #UI för att välja livsmedel
  output$sl_select = renderUI({
    selectInput("sl",
                "Valt livsmedel",
                choices = val() )
  })
  
  dig <- reactiveValues(); dig$dList <- integer(100)

  observeEvent(input$add, {
    req(input$sl)
    nr = input$add
    id = paste0("input",input$add)
    dig$dList[input$add] <- input$add 
    insertUI(
      selector = "#Måltid",
      ui = tableOutput("maltid") 
      )
  })

  myChoices <- reactiveValues(); myChoices$dList <- character(100)
  vikt <- reactiveValues(); vikt$dList <- integer(100)
  nutr <- reactiveValues(); nutr$dList <- 0
  vard <- reactiveValues(); vard$dList <- vector("list", 100)
  
  observeEvent(input$add, {
    req(input$sl)
    new1 <- isolate(min(grep("^$", myChoices$dList)))
    if(input$add > 0 ) {
      myChoices$dList[new1] <- isolate(input$sl)
      vikt$dList[new1] <- isolate(input$vikt)
      if(isolate(input$sl) %in% lnamn) {
        namn = lnamn[grep(isolate(input$sl), lnamn)]
        ix = which(lnamn %in% namn)[1]
      } else {
        ix = 0
      }
      
      if(ix != 0) {
        Värde = .01*isolate(input$vikt)*as.numeric(gsub(",", ".", xml_text(xml_find_all(nar_nodes[ix], ".//Varde")) ))
        narn = xml_text(xml_find_all(nar_nodes[ix], ".//Namn"))
        if(NA %in% Värde) {
            Värde[which(is.na(Värde))] <- 0
        }
        
        Värde[22] <- Värde[23]*4.184 # kcal till kJ
        if(length(Värde) == 54 ) {
          ix2 = which( !(Nar_namn %in% narn) )
          Värde = c(Värde[1:(ix2-1)], 0, Värde[ix2:54])
        } else if(length(Värde) == 53 ) {
          ix2 = which( !(Nar_namn %in% narn) )
          Värde = c(Värde[1:(ix2[1]-1)], 0, Värde[ix2[1]:(ix2[2]-1)], 0, Värde[ix2[2]:53])
        } else if(length(Värde) == 52 ) {
          ix2 = which( !(Nar_namn %in% narn) )
          Värde = c(Värde[1:(ix2[1]-1)], 0, Värde[ix2[1]:(ix2[2]-1)], 0, Värde[ix2[2]:(ix2[3]-1)], 0, Värde[ix2[3]:52])
        }
        vard$dList[[new1]] <- Värde
        nutr$dList <- isolate(nutr$dList) + Värde
      }
  } })
# ------------------------ end Column 2

# Column 3 -----------------------
  observeEvent(input$del,{ # tag bort senaste tillägg
    if(input$del > 0 && any(sapply(isolate(vard$dList), is.numeric) ) ) {
      last1 <- max(grep("[^$]", myChoices$dList)) # sista index med icke-tom sträng
      myChoices$dList[last1] <- "" 
      vikt$dList[last1] <- as.integer(0)
      nutr$dList <- isolate(nutr$dList) - isolate(vard$dList[[last1]])
      vard$dList[[last1]] <- NULL
    }
  })
  
  df_mt <- reactive({ # Skapa df för din måltid
    ix = which(myChoices$dList != "")
    data.frame(Livsmedel = myChoices$dList[ix], Mängd = isolate(vikt$dList)[ix], enhet = rep("g", length(isolate(vikt$dList)[ix] ) ) )
  })
  # Visa tabell för din måltid
  output$maltid <- renderTable(colnames = F,{
    if(input$add > 0) { df_mt() }
  })
  
  output$dl <- downloadHandler(
    filename = function() {paste("Maltid-", Sys.Date(), ".txt", sep = "") },
    content = function(file) {
      write.table(df_mt(), file, quote = F, row.names = F, col.names = F) 
      }
  )

  df_sum <- reactive({ # skapa df med summerad näring
    if("Total" %in% input$dn ) {
      data.frame(Nutrient = Nar_namn, Värden = abs(nutr$dList), Enhet = Enh)
      
    } else if(input$dn == "Makro" && length(input$dn) == 1) {
      ix1 = which(Nar_namn %in% makro)[c(1,7,3,8,4,6,9,2,5)]
      data.frame(Nutrient = Nar_namn[ix1], Värden = abs(nutr$dList)[ix1], Enhet = Enh[ix1])
      
    } else if(input$dn == "Mikro" && length(input$dn) == 1) {
      ix2 = which(Nar_namn %in% mikro)
      data.frame(Nutrient = Nar_namn[ix2], Värden = abs(nutr$dList)[ix2], Enhet = Enh[ix2])
      
    } else if(all(c("Makro", "Mikro") %in% input$dn) ) {
      ix1 = which(Nar_namn %in% makro)[c(1,7,3,8,4,6,9,2,5)]
      ix2 = which(Nar_namn %in% mikro)
      data.frame(Nutrient = Nar_namn[c(ix1,ix2)], Värden = abs(nutr$dList)[c(ix1,ix2)], Enhet = Enh[c(ix1,ix2)])
      }
  })
  
  # Visa tabell med summerad näring
  output$summering <- renderTable(colnames = F,na = "--",{
    if(input$add >= 0 ) { df_sum() }
  })
  # Ladda ned tabell med summerad näring
  output$dl2 <- downloadHandler(
    filename = function() { paste("Summering-", Sys.Date(), ".txt", sep = "")},
    content = function(file2) {
      write.table(df_sum(), file2, row.names = F, quote = F, col.names = F)
      }
  )
  
# ------------------- end Column 3

}

shinyApp(ui = ui, server = server)

