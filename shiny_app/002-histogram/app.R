####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)
#data(airquality)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Ozone level!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  runif(10, min = -1, max = 1)
  vec_m <- 0
  vec_s <- 0
  for(i in 1:500){
  data <-rnorm(i*100, mean = 2.5, sd = 1)
  #hist(data)
  vec_m[i] <- MASS::fitdistr(data, "normal")[["estimate"]][["mean"]]
  vec_s[i] <- MASS::fitdistr(data, "normal")[["estimate"]][["sd"]]
  # Choose the mean as 2.5 and standard deviation as 0.5.
  #y <- dnorm(x, mean = 2.5, sd = 0.5)
  }
  plot(vec_s)
  #print(y[["estimate"]][["mean"]])
  # Give the chart file a name.
  png(file = "dnorm.png")
  output$distPlot <- renderPlot({
    
    x    <- vec_s #airquality$Ozone
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)




