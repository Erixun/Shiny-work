

library(xml2)
library(shinythemes)
url = 'http://www7.slv.se/apilivsmedel/LivsmedelService.svc/Livsmedel/Naringsvarde/20190101'
data = read_xml(url)

#Namn på livsmedel
lnamn = xml_text(xml_find_all(data, "//Livsmedel/Namn"))
ordn = lnamn[order(lnamn)] #i bokstavsordning

#Noder för livsmedlens respektive näringsvärden
nar_nodes = xml_find_all(data, "//Naringsvarden")

makro = c("Energi (kcal)", "Energi (kJ)", "Fett", "Kolhydrater", "Socker totalt", "Protein",  "Fibrer", "Fullkorn totalt", "Alkohol", "Vatten")
mikro = unlist(strsplit("Fosfor,Järn,Kalcium,Kalium,Magnesium,Natrium,Selen,Zink,Vitamin A,Retinol,Vitamin D,Vitamin E,β-Karoten,Tiamin,Riboflavin,Vitamin C,Niacin,Niacinekvivalenter,Vitamin B12,Vitamin B6,Folat,Salt", ","))

ui = fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel(strong("Näring i livsmedel"), windowTitle = "Näring i livsmedel"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("lm",
                "Sökning", 
                placeholder = "Abborre kokt"),
      uiOutput("sl_select"),
      checkboxGroupInput("dn",
                         "Detaljnivå",
                         choices = c("Total", "Makro", "Mikro"),
                         inline = T,
                         selected = "Makro"),
      "(Datakälla: Livsmedelsverket)"
    ),
    
    mainPanel(width = 20,
      tableOutput("tbl")
    )
  )
)

server = function(input, output) {
  
  output$sl_select = renderUI({
    selectInput("sl",
                "Livsmedel",
                choices = ordn[grep(input$lm, ordn)], 
                selected = "Abborre kokt"
                )
  })
  
  
  output$tbl = renderTable({
    if(input$lm == "") {
      namn = "Abborre kokt"
      ix = which(lnamn == namn)
    } else if(input$sl %in% lnamn) { 
      namn = lnamn[grep(input$sl, lnamn)]
      ix = which(lnamn == namn)
    } else {
      ix = 0
    }
    
    if(ix != 0) {
      Nutrient = xml_text(xml_find_all(nar_nodes[ix], ".//Namn"))
      Värde = xml_text(xml_find_all(nar_nodes[ix], ".//Varde"))
      Enhet = xml_text(xml_find_all(nar_nodes[ix], ".//Enhet"))
      df = data.frame(Nutrient, Värde, Enhet)
      
      if("Total" %in% input$dn ) {
        df
        
      } else if(input$dn == "Makro" && length(input$dn) == 1) {
        ix1 = which(df$Nutrient %in% makro)
        df[ix1,][c(2,1,8,5,4,7,9,3,6),]
  
      } else if(input$dn == "Mikro" && length(input$dn) == 1) {
        ix2 = which(df$Nutrient %in% mikro)
        df[ix2,]
        
      } else if(all(c("Makro", "Mikro") %in% input$dn) ) {
        ix1 = which(df$Nutrient %in% makro)
        ix2 = which(df$Nutrient %in% mikro)
        rbind.data.frame(df[ix1,][c(2,1,8,5,4,7,9,3,6),], df[ix2,])
      }
    }
  }, 
  width = 300) 
}

shinyApp(ui = ui, server = server)

