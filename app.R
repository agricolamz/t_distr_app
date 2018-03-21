library(shiny)
library(tidyverse)

ui <- fluidPage(
   # Application title
   titlePanel("The t-distribution and its normal approximation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("df",
                     "degrees of freedom",
                     min = 1,
                     max = 60,
                     value = 5,
                     step = 1)),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
     data_frame(x = seq(-4, 4, 0.01),
                `t-distribution` = dt(x, df = input$df),
                `normal distribution` = dnorm(x)) %>% 
       gather(distribution, value, 2:3) %>% 
       ggplot(aes(x, value, color = distribution, fill = distribution))+ 
       geom_vline(xintercept = c(-1.959964, 1.959964), linetype = 2)+
       geom_line(size = 2)+
       theme_minimal()+
       labs(x = "",
            y = "")+
       theme(axis.text.y=element_blank(),
             legend.title = element_blank(),
             legend.position = c(0.15, 0.85),
             legend.text = element_text(size=14, 
                                        face="bold"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
