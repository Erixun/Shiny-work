#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)
# Define UI for application that draws an interest-plot
ui <- fluidPage(
  
  # Application title
  titlePanel("Hur rik ska du bli på börsen?"),
  
  # Sidebar with a slider input for... 
  sidebarLayout(
    sidebarPanel(
      sliderInput("start",
                  "Startkapital:",
                  min = 0,
                  max = 100000,
                  step = 1000,
                  sep = " ",
                  post = " kr",
                  value = 1000),
      sliderInput("mon",
                  "Månadssparande:",
                  min = 0,
                  max = 20000,
                  step = 100,
                  sep = " ",
                  post = " kr",
                  value = 1000),
      sliderInput("tid",
                  "Tidshorisont:",
                  min = 0,
                  max = 50,
                  post = " år",
                  sep = " ",
                  value = 5),
      sliderInput("ranta",
                  "Önskad/förväntad avkastning:*",
                  min = 0,
                  max = 30, 
                  post = " %",
                  value = 10),
      textOutput("rimlig"),
      tags$head(tags$style( "#rimlig{color: gray;
                                 font-size: 8px;
                                 font-style: italic;
                                 }"))
    ),
    
    # Show text and plot of values generated 
    mainPanel(
      #plotOutput("utv"),
      h4(uiOutput("avk")),
      h3(strong(textOutput("varde"))),
      plotOutput("utv")
    )
  )
)

# Define server logic required to..
server <- function(input, output) {
  
  output$rimlig = renderText({
    paste("*En rimlig förväntad avkastning är mellan 8-10 %")
  })
  
  avkastn = reactive({
    p = 1
    round(input$start*(1+input$ranta/100)**input$tid+input$mon*12*((1+(input$ranta/p)*1/100)**(p*input$tid)-1)*1/(input$ranta*1/100))
  })
  
  output$avk = renderUI(HTML(
    paste("Med ett startkapital på ", formatC(input$start, big.mark = " ", format = "f", digits = 0),
          " kr och en månatlig insättning på ", formatC(input$mon, big.mark = " ", format = "f", digits = 0), 
          " kr <br> kommer du efter", input$tid, " år, med en förväntad årsavkastning på ", input$ranta,"%, ha ", h3(strong(formatC(avkastn(),big.mark = " ", digits = 0, format = "f") ), strong(" kr!")) )
  ))
  
  output$utv = renderPlot({
    if(input$tid > 0) {
      yrs = seq(input$tid, from = 0)
    } else { yrs = 1}
    p = 1
    avkastning = c()
    if(input$tid > 0) {
      for(i in yrs) {
        avkastning = c(avkastning, input$start*(1+input$ranta/100)**i+input$mon*12*((1+(input$ranta/p)*1/100)**(p*i)-1)*1/(input$ranta*1/100) )
      } 
    } else { avkastning = input$start }
    databl = data.frame(yrs, avkastning)
    g = ggplot(databl, aes(x=yrs, y=avkastning))
    g = g + xlab("År") + ylab('')# + ggtitle("Ditt sparandes utveckling över tid")
    g = g + geom_line(color="darkgreen", size = 1.5, lineend = "round") + geom_point()
    g = g + theme(axis.line = element_line(colour = "black", arrow = arrow(length = unit(0.1, "inches"), type = "closed")), axis.title.x = element_text(size = 12, colour = "black", hjust = 1), axis.title.y = element_text(size = 15, colour = "black", hjust = 1, angle = 0), axis.text.x = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 12, face = "bold"),  plot.title = element_text(size = 20, colour = "darkblue", face = "bold", hjust = 0.5) )
    g = g + scale_x_continuous(breaks = yrs, expand = c(0,0), limits = c(0,max(yrs)+0.5) ) + scale_y_continuous(breaks = seq(0, 1.2*max(avkastning), 2*round(max(avkastning)%/%10, 1-nchar(max(avkastning)%/%10) ) ),labels = dollar_format(big.mark = " ",suffix = " kr", prefix = ""), expand = c(0,0), limits = c(0, 1.2*max(avkastning)) )
    g
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
