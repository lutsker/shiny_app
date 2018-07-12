#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Explorer v0.1"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textOutput("dsInfo", container = div),
        selectizeInput("col_x", label=NULL, choices=NULL, options = list(create =T)),
        selectizeInput("col_y", label=NULL, choices=NULL, multiple = F, options = list(create =T)),
        fileInput("fileInp", label = "Select file", multiple = F, accept = "csv", buttonLabel = "open", 
                  placeholder = "choose a file")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  df.src <- reactive({
    inFile <- input$fileInp
    if(is.null(inFile))
      return(NULL)
    df.src <- read.csv(inFile$datapath, header = T)
    nms <- names(df.src)
    updateSelectizeInput(session, 'col_x', choices = nms, server = TRUE)
    updateSelectizeInput(session, 'col_y', choices = nms, server = TRUE)
    df.src
  }
  )
 
  #output$txt <- renderText({input$column}) 
  output$dsInfo <- renderText({
    df.src <- df.src()
    if(is.null(df.src))
      return(NULL)
    paste("size: ", nrow(df.src), "X", ncol(df.src))
    }) 
  

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    df.src <- df.src()
    if(input$col_x == "" | input$col_y == "" | is.null(df.src))
      return(NULL)
    ggplot(df.src, aes_string(input$col_x, input$col_y)) + geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

