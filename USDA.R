
library(curl)
library(shiny)
library(jsonlite)

APIkey = "jkIbqO4LpNmGuSkS98SmD59lYpXNV5Ojps1gaoCV"
macro = c("Water","Energy","Energy","Protein","Total lipid (fat)","Ash","Carbohydrate, by difference","Fiber, total dietary","Sugars, total","Sodium, Na")
micro = c("Calcium, Ca","Iron, Fe","Magnesium, Mg","Phosphorus, P","Potassium, K","Sodium, Na","Zinc, Zn","Copper, Cu","Manganese, Mn","Selenium, Se","Fluoride, F","Vitamin C, total ascorbic acid","Thiamin","Riboflavin","Niacin","Pantothenic acid","Vitamin B-6","Folate, total","Vitamin B-12","Vitamin A, RAE","Vitamin A, IU","Vitamin E (alpha-tocopherol)","Vitamin D (D2 + D3)","Vitamin K (phylloquinone)","Caffeine","Theobromine")

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      textInput("qry", "Search term"),
      uiOutput("slct"),
      sliderInput("gr", NULL, value = 100, min = 0, max = 400),
      checkboxGroupInput("dn", "Detail", choices = c("Total", "Macro", "Micro"), selected = "Macro", inline = T )
    ),
    
    mainPanel(
      tableOutput("tbl")
    )
  )
)


server <- function(input, output) {
  
  search = reactive({
    paste("q=", input$qry, "&", sep="")
  })
  
  val = reactive({
    url_search = paste("https://api.nal.usda.gov/ndb/search/?format=json&ds=Standard Reference&", search(), "api_key=", APIkey, sep = "")
    data = fromJSON(URLencode(url_search) )
    pat = input$qry
    data$list$item$name[grep(pat, data$list$item$name)] 
  })
  
  output$slct = renderUI({
    selectInput("sl", "Food-selection", choices = val() )
  })
  
  ndbno = reactive({
    req(input$sl)
    url_search = paste("https://api.nal.usda.gov/ndb/search/?format=json&ds=Standard Reference&", search(), "api_key=", APIkey, sep = "")
    data = fromJSON(URLencode(url_search) )
    data$list$item$ndbno[grep(input$sl, data$list$item$name)]
  })
  
  output$tbl = renderTable(colnames = F,{
    req(input$sl)
    url_nutr = paste("https://api.nal.usda.gov/ndb/V2/reports?ndbno=", ndbno(), "&type=f&format=json&api_key=", APIkey, sep = "")
    nutr_data = fromJSON(url_nutr)
    #nutrition-dataframe for the selection:
    df = nutr_data$foods$food$nutrients[[1]][,c("name","value", "unit")]
    df$value = df$value*(input$gr)/100
    req(input$dn)
    if(input$dn == "Total") {
      df
    } else if(input$dn == "Macro" && length(input$dn) == 1 ) {
      df[c(1:9, 15),]
    } else if(input$dn == "Micro" && length(input$dn) == 1 ) {
      df[which(df$name %in% micro),]
    } else if (all(c("Macro", "Micro") %in% input$dn) ) {
      df[which(df$name %in% c(macro, micro)),]
    }
  })
}

shinyApp(ui = ui, server = server)