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
library(dplyr)


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Diamond Features and Price"),
   
   # Sidebar with a slider input for data selection from diamonds dataset
   sidebarLayout(
      sidebarPanel(
         sliderInput("caratInput",
                     "Carats:",
                     min = 0.2000,
                     max = 5.0100,
                     value = c(0.2,2.3000)),
      
      sliderInput("priceInput", "Price:", 326, 18823, c(2500, 6000), pre = "$"),
      radioButtons("colorInput", "Colors:",
                   choices = c("D", "E", "F", "G","H","I","J"),
                   selected = "D"),
      
      uiOutput("clarityInOut")


   ),
      # Show a plot
      mainPanel(
         plotOutput("displayPlot")
      )
   )
)

# Define server logic required to draw a a plot
server <- function(input, output) {
    output$clarityInOut <- renderUI({
        selectInput("clarityInOut", "Clarity:",
                    sort(unique(diamonds$clarity)),
                    selected = "VVS2")
    })  
    
    
    filtered <- reactive({
        if (is.null(input$clarityInOut)) {
            return(NULL)
        }    
        
        # filter diamonds dataset
        diamonds %>% filter(carat >= input$caratInput[1],
                   carat <= input$caratInput[2],
                   price >= input$priceInput[1],
                   price <= input$priceInput[2],
                   color == input$colorInput,
                   clarity == input$clarityInOut
            )
    })
    
    
   output$displayPlot <- renderPlot({
       if (is.null(filtered())) {
           return()
       }
       
       # Draw lines using ggplot
       ggplot() + geom_line(data=filtered(),aes(x=carat,y=price,color=cut,linetype=cut)) + 
           scale_color_manual(values=c("red", "blue","grey6","green","aquamarine4")) + 
           xlab("Carat Size")+ ylab("Price in $")
       
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

