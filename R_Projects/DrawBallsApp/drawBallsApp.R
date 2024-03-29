# Shikha Chamoli
# Drawing balls from boxes
# Inputs:
#   Repetitions: number of repetitions
#   Threshold: threshold to select box
#
# Outputs:
#   Frequency plot of number of blue balls

library(shiny)
library(ggplot2)
library(ggeasy)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  # titlePanel("Drawing Balls Experiment"),
  h1(id="big-heading", "Drawing Balls Experiment"),
  tags$style(HTML("#big-heading{color: #ff8c00 ;font-size: 40px;
           font-style: bold; }", 
           '#sidebar { background-color: #e6e6fa;}'
               )),
 
  # Sidebar  
  sidebarLayout(
    sidebarPanel(id="sidebar",
      sliderInput("repetitions",
                  label = "Number of repetitions:",
                  min = 1,
                  max = 5000,
                  value = 100),
      sliderInput("threshold",
                  label = "Threshold for choosing boxes:",
                  min = 0,
                  max = 1,
                  value = 0.5)
    ),
    
    # Show a plot of the relative frequencies
    mainPanel( 
      plotOutput("freqs_plot"),
      tableOutput("summary_table"),
      fluid = TRUE
      
    )
  )
)


# Define server logic required to draw the plot
server <- function(input, output) {
  
  blue_counts <- reactive({
    # boxes as character vectors 
    box1 <- c('blue', 'blue', 'red')
    box2 <- c('blue', 'blue', 'red', 'red', 'red', 'white')
    
    size <- 4
    drawn_balls <- matrix("", input$repetitions, size)
    set.seed(input$seed)
    for (r in 1:input$repetitions) {
      aux <- runif(1)
      if (aux > input$threshold) {
        drawn_balls[r, ] <- sample(box1, size, replace = TRUE)
      } else {
        drawn_balls[r,] <- sample(box2, size)
      }
    }
    
    # number of blue balls in each repetition
    blue_counts <- apply(drawn_balls, 1, function(x) sum(x == 'blue'))
    blue_counts
  })
  
  # Fill in the spot we created for a plot
  output$freqs_plot <- renderPlot({
    # progression of relative frequencies
    blue_freqs <- vector(mode = "list", length = 5)
    for (num_blue in 0:4) {
      temp_freqs <- cumsum(blue_counts() == num_blue) / (1:input$repetitions)
      blue_freqs[[num_blue + 1]] <- temp_freqs
    }
    
    dat <- data.frame(
      reps = rep(1:input$repetitions, 5),
      freqs = unlist(blue_freqs),
      number = factor(rep(0:4, each = input$repetitions))
    )
    
    ggplot(data = dat, aes(x = reps, y = freqs, group = number)) +
      geom_path(aes(color = number)) +
      ggtitle("Relative frequencies of number of blue balls")
  })
  
  output$summary_table <- renderTable({
    table(blue_counts()) / input$repetitions
  })
}



# Run the application 
shinyApp(ui = ui, server = server)



