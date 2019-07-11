#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(curl)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)

#Generate table of prices ...


#Function receiving prices for a certain currency from shrimpy..
get_pricelist <- function(currency){
  
  #receive list of exchanges ...
  supported_exchanges <- jsonlite::fromJSON("https://dev-api.shrimpy.io/v1/list_exchanges")
  
  #helper function 1
  get_ticker <- function(exchangename){
    get_ticker <- jsonlite::fromJSON(paste0("https://dev-api.shrimpy.io/v1/exchanges/",exchangename,"/ticker"))
  }
  #helper function 2
  f <- unlist(function(x){all_ticker[x][[1]]$priceUsd[all_ticker[x][[1]]$name==currency]})
  
  #get data from ticker
  all_ticker <- lapply(X=supported_exchanges$exchange,FUN = get_ticker)
  
  #extract prices in usd from ticker
  get_pricelist <- cbind(supported_exchanges$exchange,unlist(lapply(1:length(supported_exchanges$exchange),f)),supported_exchanges$bestCaseFee,supported_exchanges$worstCaseFee)
}

datasetInput <- get_pricelist("Bitcoin")
colnames(datasetInput) <- c("Exchange","USDBTC","Best case fee", "Worst case fee")


ui <- fluidPage(
  theme = shinytheme("journal"),
  navbarPage("Navigation",
             tabPanel("Stats",
                  titlePanel("Bitcoin price comparison (experimental, no real data)",windowTitle = "Bitcoin - Preisvergleich" ),
                  mainPanel(
                    tableOutput("table"), width = 12
                  )
                ),
        tabPanel("more",
                 verbatimTextOutput("more")
          ),
        tags$footer("Built with RStudio shiny", align = "center")
        )
      )
          server <- function(input, output, session) {
            
            #recalculate dataset here
            datasetInput <- get_pricelist("Bitcoin")
            colnames(datasetInput) <- c("Exchange","USDBTC","Best case fee", "Worst case fee")
            
            # Table of selected dataset ----
            output$table <- renderTable(
              datasetInput
            ,width = '200%',spacing = "l")
            
            # Downloadable csv of selected dataset ----
            output$downloadData <- downloadHandler(
              filename = function() {
                paste(input$dataset, ".csv", sep = "")
              },
              content = function(file) {
                write.csv(datasetInput(), file, row.names = FALSE)
              }
            )
      }

# Run the application 
shinyApp(ui = ui, server = server)


