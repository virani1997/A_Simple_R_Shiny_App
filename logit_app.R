#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Logistic Regression"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("threshold",
                        "Input a Threshold for Classification",
                        min = 0,
                        max = 1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("table")
          #textOutput("accuracy")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable({
      
      library(nycflights13)
      library(dplyr)
      library(rsample)
      library(ROCR)
      library(tidyr)
      
      data("flights")
      
      flights <- flights %>%
        mutate(arr_delay  = replace_na(flights$arr_delay, 0)) %>%
        mutate(dep_delay = replace_na(flights$dep_delay, 0))
      
      flights <- flights %>%
        mutate(arrdelay_binary = ifelse(arr_delay >5, 1, 0))
      
      initial_split <- initial_split(flights, prop = 0.9)
      training <- training(initial_split)
      testing <- testing(initial_split)
      
      model_logistic <- glm(arrdelay_binary ~ dep_delay, data = training, family = "binomial")
      summary(model_logistic)
      
      training <- training %>%
        mutate(prediction = predict(model_logistic, newdata = training, type = "response"))
      
      ROCRPred <- prediction(training$prediction, training$arrdelay_binary)
      ROCRPref <- performance(ROCRPred, "tpr", "fpr")
      plot(ROCRPref, colorize = T, print.cutoffs.at = seq(0.1, by = 0.1))
      
      table_train_results <- table(Actual = training$arrdelay_binary, Predicted = training$prediction > input$threshold)
      
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    #output$accuracy <- renderText(
     # accuracy_train <- (table_train_results[1,1] + table_train_results[2,2])/
      #  sum(table_train_results))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
