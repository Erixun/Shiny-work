
library(shiny)
library(shinyWidgets)
library(readxl)
skattes <- read_excel("2.-skattesatser-2018-forandring-kommunvis-korrigerad-2019-01-18..xlsx", skip=7, range = "A8:B298")

#setwd("~/Documents/R stuff/ArresLöneapp2")
timmar = c(paste("0", as.character(seq(0,9)), sep="" ), as.character(seq(10,23)))
minuter = c("00", as.character(seq(15,45, by=15) ) )

dagOB4 = c("Trettondagen", "Första maj", "Nationaldagen", "Kristi himmelsfärdsdag", "Alla helgons dag")
dag1OB5 = c("Skärtorsdagen", "Nyårsafton")
dag2OB5 = c("Långfredagen", "Nyårsdagen")
dag3OB5 = c("Pingstdagen", "Midsommarafton", "Julafton")

helgd = as.matrix(read.csv("helgdagar_2020-2029.csv", stringsAsFactors = F))
colnames(helgd) = c("Nyårsdagen", "Trettondagen", "Skärtorsdagen", "Långfredagen", "Första maj", "Pingstdagen", "Nationaldagen", "Kristi Himmelsfärdsdag", "Midsommarafton", "Alla helgons dag", "Julafton", "Nyårsafton")

obMat = matrix(data = 0, nrow = 7, ncol = 24)
rownames(obMat) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
obMat[1:5, 19:24] <- 29.16
obMat[2:5, 1:6] <- 63.63
obMat[c(1,6,7), 1:6] <- 59.65

ob5Mat1 = ob5Mat2 = ob5Mat3 = ob4Mat = obMat

ob4Mat[,1:6] <- 62.30
ob5Mat1[,19:24] <- 116.64
ob5Mat2[,1:6] <- 116.64 
ob5Mat3[,1:6] <- 116.64
obList <- list(ob4Mat, ob5Mat1, ob5Mat2, ob5Mat3)

arbTim <- matrix(0, ncol = 24, nrow = 2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundColor("Honeydew"),
  tags$head( tags$meta(name = "viewport", content = "width=1100"),uiOutput("body")),
  
  # Application title
  titlePanel("Inkomst per arbetspass"),
  h4(em("SAS"), "Bagagekastare"), # SAS logo?
  
  hr(),
  fluidRow(
    # Ange passets datum och arbetstid
    column(width=2, dateInput("datum", "Datum", min = , format = "yyyy-mm-dd", weekstart = 1, width = "100%" ) ),
    # Ange timlön
    column(width=2, numericInput("lön", "Timlön", min = 80, value = 153, max = 220, width = "40%") )
    # Ange typ av anställning?/Semesterersättning?
    # Ange skattesats?
  ),
  fluidRow( # Ange start- och slut-tider
    column(width=1, selectInput("tidst", "Start (h)", choices = timmar, width = "110%" ) ),
    column(width=1, selectInput("tidsm", "(min)", choices = minuter, width = "110%" ) ),
    column(width=1, selectInput("tidslt", "Slut (h)", choices = timmar, width = "110%" ) ),
    column(width=1, selectInput("tidslm", "(min)", choices = minuter, width = "110%" ) )
  ),
  fluidRow(
    column(width=5, selectInput("sks", "Skattesats", choices = skattes$Kommun, selected = "Sollentuna kommun") )
  ),
  hr(),
  fluidRow( 
    column(width=12,
           h4(uiOutput("ink")), # Din inkomst för arbetspasset
           br(),
           downloadButton("dl", "Spara pass") # Ladda ned info
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  sttid <- reactive({ # starttid för passet
    as.numeric(input$tidst) + as.numeric(input$tidsm)/60
  })
  timmar <- reactive({ # antal jobbtimmar
    x = as.numeric(input$tidslt) + as.numeric(input$tidslm)/60 - (as.numeric(input$tidst) + as.numeric(input$tidsm)/60)
    if(x < 0) { 24+x } else { x }
  })
  sltid <- reactive({ # sluttid för passet
    sttid() + timmar()
  })
  dag <- reactive({ # veckodag då passet börjar
    format(input$datum, "%a")
  })
  datum <- reactive({ # passets start-datum
    as.character(input$datum)
  })
  datum2 <- reactive({ # dagen efter
    as.character(input$datum+1)
  })
  dats <- reactive({ # bägge datum
    c(datum(), datum2() )
  })
  datumDag <- reactive({
    if(datum() %in% helgd) {
      # vilket index har datum i helgd-matrisen?
      ixmat <- which(helgd %in% datum() )
      k <- arrayInd(ixmat, dim(helgd))
      
      # vilket kolumn-namn har detta index?
      colnames(helgd)[k[,2]]

    } else { format(input$datum, "%a %d %b") }
  })
  skats <- reactive({
    skattes[which(skattes == input$sks), 2]
  })
  
  lön <- reactive({ # beräkna lön/inkomst
    rownames(arbTim) <- c(dag(), format(input$datum+1, "%a"))
    
    if( sltid() <= 24 && sltid() != sttid() ) { # ej över midnatt
      tim=c(sttid()-floor(sttid()), rep(1, abs(ceiling(sttid()) - sltid() ) ), sltid()-floor(sltid()) )
      arbTim[1,(floor(sttid())+1):ceiling(sltid() )] <- tim[tim > 0]
    } else if( sltid() > 24 && sttid() > 0) { # över midnatt
      tim1 = c(floor(sttid()+1)-sttid(), rep(1, (24-floor(sttid()+1) ) ) )
      arbTim[1,(floor(sttid())+1):24] <- tim1[tim1 > 0]
      tim2 = c(rep(1, floor(sltid() ) - 24), sltid()-floor(sltid() ) )
      arbTim[2, 1:length(tim2[tim2 > 0 ])] <- tim2[tim2 > 0]
    }

    # är datum/datum+1 en helgd?
    if( any(dats() %in% helgd) ) {
      # vilket/vilka av dats() är i helgd?
      ixd <- which(dats() %in% helgd)

      # vilket/vilka index har datum/datum+1 i helgd-matrisen?
      ixmat <- which(helgd %in% dats()[ixd])
      k <- arrayInd(ixmat, dim(helgd))

      # i vilken/vilka kolumn(er) (helgdag) finns detta index?
      coln <- colnames(helgd)[k[,2]]

      # i vilken/vilka OB-vektor finns denna helgdag?
      ixOB4 <- which(coln %in% dagOB4)
      ix1OB5 <- which(coln %in% dag1OB5)
      ix2OB5 <- which(coln %in% dag2OB5)
      ix3OB5 <- which(coln %in% dag3OB5)
      cix <- list(ixOB4, ix1OB5, ix2OB5, ix3OB5)
      
      if(length(coln) == 2 ) { 
        obMat1 <- obList[[which(cix == 1)]]; obMat2 <- obList[[which(cix == 2)]]
      } else { if(ixd == 2) { obMat1 <- obMat; obMat2 <- obList[[which(cix==1)]] } else { obMat2 <- obMat; obMat1 <- obList[[which(cix==1)]] } }

      ixr1 <- which(rownames(obMat1) %in% rownames(arbTim)[1])
      ixr2 <- which(rownames(obMat2) %in% rownames(arbTim)[2])
      pay = sum(obMat1[ixr1, ]*arbTim[1,]) + sum(obMat2[ixr2, ]*arbTim[2,]) + sum(arbTim)*input$lön
      pay

    } else {
      ixr <- which(rownames(obMat) %in% rownames(arbTim))
      pay = sum(obMat[ixr, ]*arbTim) + sum(arbTim)*input$lön
      pay 
    }
    
  }) 
  output$ink = renderUI(HTML( # output med inkomst för arbetspasset
    paste("Under ett ", timmar(), " timmars arbetspass ", datumDag(),
          " kl ", paste(paste(input$tidst, input$tidsm, sep=":"), paste(input$tidslt, input$tidslm, sep=":"), sep="-" ), "" ," <br>tjänar du ", round(lön(), 2)," kr, före skatt.", 
          " <br> Total skattesats i ", input$sks, " är ", skats(), " %. <br> Din inkomst efter skatt blir därmed ", round(lön()*(1-skats()/100 ), digits = 2), " kr." )
  ))
  
  # Ordna så att man kan spara flera arbetspass?
  
  # Ladda ned info om arbetspasset
  output$dl = downloadHandler(
    filename = function() { paste("Arbetspass_", input$datum, ".txt", sep = "")},
    content = function(file2) {
      df_ink = data.frame(Datum = paste(input$datum, " (", dag(), ")", sep=""), Timlön = input$lön, Tid = paste(paste(input$tidst, input$tidsm, sep=":"), paste(input$tidslt, input$tidslm, sep=":"), sep="-" ), Timmar = timmar(), Inkomst = lön() , OB = lön() - 153*timmar()*1.12  )
      write.table(t(df_ink), file2, row.names = T, quote = F, col.names = F)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

