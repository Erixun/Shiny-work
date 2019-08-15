#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(XML)
library(tidyverse)
library(reshape2)

data = xmlParse('http://www7.slv.se/apilivsmedel/LivsmedelService.svc/Livsmedel/Naringsvarde/20190101')
#Livsmedelsnamn
Namn = as.character(sapply( data["//Livsmedel/Namn"], as, "character" ))
Namn = gsub("[<|</]*\\w+>\n*", "", Namn)
#Näringsvärdesdata
naringsvarde = "//Livsmedel/Naringsvarden"
Naringsvarde = as.character(sapply( data[naringsvarde], as, "character" ))
test = strsplit(gsub("[<|</]*\\w+>\n*", "", Naringsvarde), "  ")
test = test[test != ""]

tfl = sapply(test, "!=", "") #positions of non-empty strings
for(i in 1:length(test)) {
  test[[i]] = test[[i]][ tfl[[i]] ]
}
test = sapply(test, trimws) #remove whitespaces

Enhet = c("g","mg","kJ","kcal","µg")
for(i in 1:length(tfl)) { #extract all relevant nutrient-data
  ix2 = which(test[[i]] %in% Enhet)
  test[[i]] = test[[i]][rbind(ix2-3, ix2-1, ix2)]
}

mat = matrix(nrow = length(test), ncol = max(sapply(test, length))+1 )
mat[,1] = Namn
for(i in 1:length(Namn)) {
  mat[i,2:(length(test[[i]])+1)] = test[[i]]
}

dfm = melt(as.data.frame(mat, stringsAsFactors = F), id.vars = "V1")
dfm = dfm[order(dfm$V1),]
rownames(dfm) = seq(dim(dfm)[1])

dfm2 = dfm[seq(1,dim(dfm)[1], 3),]
dfm2 = cbind(dfm2, dfm$value[seq(2,dim(dfm)[1], 3)])
dfm2 = cbind(dfm2, dfm$value[seq(3,dim(dfm)[1], 3)])
dfm2 = dfm2[,-2]
colnames(dfm2) = c("Livsmedel", "Nutrient", "Värde", "Enhet")
#Remove rows with NA:
ix = which(is.na(dfm2$Nutrient))
dfm2 = dfm2[-ix,]
rownames(dfm2) = seq(dim(dfm2)[1])

makro = c("Energi (kcal)", "Energi (kJ)", "Fett", "Kolhydrater", "Socker totalt", "Protein",  "Fibrer", "Fullkorn totalt", "Alkohol", "Vatten")
mikro = dfm2$Nutrient[c(31:51, 54)]

ui = fluidPage(
  titlePanel(strong("En liten databas för livsmedel"), windowTitle = "En liten databas"),
  
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
    
  mainPanel(
    tableOutput("tbl")
    )
  )
)

server = function(input, output) {

  output$sl_select = renderUI({
    selectInput("sl",
                "Livsmedel",
                choices = unique(dfm2$Livsmedel[grep(input$lm, dfm2$Livsmedel)]))
  })

  output$tbl = renderTable({
    if(input$lm != "") {
        namn = dfm2[grep(input$sl, dfm2$Livsmedel)[1], 1]
    } else { namn = "Abborre kokt" }
    
    if("Total" %in% input$dn ) {
      dfm2[grep(namn, dfm2$Livsmedel),2:4 ]
    } else if(input$dn == "Makro" && length(input$dn) == 1) {
      df_out = dfm2[grep(namn, dfm2$Livsmedel),2:4 ]
      ix1 = which(df_out$Nutrient %in% makro)
      df_out = df_out[ix1,][c(2,1,8,5,4,7,9,3,6),]
      df_out
    } else if(input$dn == "Mikro" && length(input$dn) == 1) {
      df_out = dfm2[grep(namn, dfm2$Livsmedel),2:4 ]
      ix2 = which(df_out$Nutrient %in% mikro)
      df_out[ix2,]
    } else if(all(c("Makro", "Mikro") %in% input$dn) ) {
      df_out = dfm2[grep(namn, dfm2$Livsmedel),2:4 ]
      ix1 = which(df_out$Nutrient %in% makro)
      df_out2 = df_out[ix1,][c(2,1,8,5,4,7,9,3,6),]

      ix2 = which(df_out$Nutrient %in% mikro)
      df_out = rbind.data.frame(df_out2, df_out[ix2,])
      df_out
    }
  })
}

shinyApp(ui, server)
