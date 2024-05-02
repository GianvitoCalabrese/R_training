####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
source("C:/Users/gcalabre/GitHub/R_training/package.R")
# setwd("C:/Users/tele1/OneDrive/Documenti/GitHub/R_training/shiny_app/001-first-app")
#setwd("C:/Users/zbmbcf/Documents/GitHub/R_training/shiny_app/001-first-app")

  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "My first app",
      tabPanel("Navbar 1",
               sidebarPanel(
                 tags$h3("Input:"),
                 numericInput("txt1", "Set the number of points:", ""),
                 numericInput("txt2", "Number of measurements per subgroup:", ""),
                 actionButton(
                    inputId = "submit", label = "Submit"
                     )
               ), # sidebarPanel
               mainPanel(
                            h1("Header 1"),
                                mainPanel(
                                  # Output: runchart ----
                                plotOutput(outputId = "distPlot")
                                      ),
                            h4("Output 1"),
                            verbatimTextOutput("txtout1"),
                            h4("Output 2"),
                            verbatimTextOutput("txtout2"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    output$txtout1 <- renderText(input$txt1)
    output$txtout2 <- renderText(input$txt2)
    j <- reactive({j <- input$txt1})
    N.sub <- reactive({N.sub <- input$txt2})
    #N.sub <- renderText(input$txt2)
    observeEvent(
      eventExpr = input[["submit"]],
      handlerExpr = {
        j<-j()
        N.sub <- N.sub()
        
        output$normalError <- renderText({
        if (j > N.sub) {
          print("ok")
        } else {
          stop(
          print('is not a number between 1 and 10')
        )
        }
        })

        
        xbar <- BinMean(n=j, every = j%%N.sub)
        S = sd(xbar)
        xdb = mean(xbar)
        num.an = sqrt(2) * gamma(N.sub/2)
        den.an = sqrt(N.sub-1) * gamma((N.sub-1)/2)
        an = num.an / den.an
        LCL = xdb - (3 * S/(an * sqrt(N.sub)))
        UCL = xdb + (3 * S/(an * sqrt(N.sub)))
        paste0('Control limits: [', round(LCL, 2),'; ', round(UCL,2), ']')
        output$distPlot <- renderPlot({
            plot(x = 1,
             type = "n",
             xlim = c(0, length(xbar)), 
             ylim = c(min(xbar, na.rm = TRUE), max(xbar, na.rm = TRUE)),
             pch = 16,
             xlab = "N", 
             ylab = "Values",
             main = "Run Chart")

             points(x= 1:length(xbar), y = xbar, pch=16, col=ifelse(xbar>230, "red", "black"))
             lines(x = 1:length(xbar) , y = xbar, type = "l")
        })
      }
    )
    
  } # server
  
       
    

  # Create Shiny object
  shinyApp(ui = ui, server = server)

 



#
## Number of points
#j <- 103
## Number of measurements per subgroup
#N.sub = 5
#
#xbar <- BinMean(j, every = j%%N.sub)
#
#
##BinMean(a, every = 10)
## Average of the 20 standard deviations
## of the 20 subgroups
#S = sd(xbar)
#
## xdb = x double bar = overall mean =
##       mean of the means
#xdb = mean(xbar)
#
#num.an = sqrt(2) * gamma(N.sub/2)
#den.an = sqrt(N.sub-1) * gamma((N.sub-1)/2)
#an = num.an / den.an
#
#LCL = xdb - (3 * S/(an * sqrt(N.sub)))
#UCL = xdb + (3 * S/(an * sqrt(N.sub)))
#paste0('Control limits: [', round(LCL, 2),
#       '; ', round(UCL,2), ']')
#
#paste0('Number > UCL: ', sum(xbar > UCL))
#paste0('Number < LCL: ', sum(xbar < LCL))
#
## Exclude the one subgroup above the UCL.
## Do this by setting it to 'NA' (missing)
#xbar[xbar > UCL] = NA
#
## Calculate the mean, removing missing
## values (ignore it).
#xdb = mean(xbar, na.rm=TRUE)
#
## 'S' will change also. If you download the
## raw data (link above), you can prove
## that the new 'S' will be:
#
#
## The 'an' and 'N.sub' will not change.
#
#LCL = xdb - (3 * S/(an * sqrt(N.sub)))
#UCL = xdb + (3 * S/(an * sqrt(N.sub)))
#paste0('Control limits: [', round(LCL, 0),
#       '; ', round(UCL,0), ']')
#
#plot(x = 1,
#     type = "n",
#     xlim = c(0, length(xbar)), 
#     ylim = c(min(xbar, na.rm = TRUE), max(xbar, na.rm = TRUE)),
#     pch = 16,
#     xlab = "N", 
#     ylab = "Values",
#     main = "Run Chart")
#
#points(x= 1:length(xbar), y = xbar, pch=16, col=ifelse(xbar>230, "red", "black"))
#lines(x = 1:length(xbar) , y = xbar, type = "l")
#