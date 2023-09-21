library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Confidence Intervals for Alien Eye Counts"),
  
  # Sidebar with a slider input for the number of samples
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_samples",
                  "Number of samples:",
                  min = 0,
                  max = 200,
                  value = 100),
      checkboxInput("greyOut", "Grey out CIs containing the true mean", FALSE),
      actionButton("goButton", "Go")
    ),
    
    # Show a plot of the generated confidence intervals
    mainPanel(
      plotOutput("ciPlot", height = "800px"),
      textOutput("ciText")
    )
  )
)

# Define server logic required to draw the confidence intervals
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    
    # Set the true population mean and standard deviation
    true_mean <- 3
    true_sd <- 1
    
    # Set the sample size and the number of samples
    sample_size <- 30
    num_samples <- input$num_samples
    
    # Create a matrix to store the lower and upper bounds of the confidence intervals
    ci_bounds <- matrix(nrow = num_samples, ncol = 2)
    
    # Generate random samples and calculate the confidence intervals
    set.seed(123)
    for (i in 1:num_samples) {
      sample <- rnorm(sample_size, mean = true_mean, sd = true_sd)
      sample_mean <- mean(sample)
      sample_sd <- sd(sample)
      error <- qt(0.975, df = sample_size - 1) * (sample_sd / sqrt(sample_size))
      ci_bounds[i, ] <- c(sample_mean - error, sample_mean + error)
    }
    
    # Create a data frame for plotting
    ci_data <- data.frame(
      sample = factor(1:num_samples),
      lower = ci_bounds[, 1],
      upper = ci_bounds[, 2],
      mean = rowMeans(ci_bounds),
      contains_true_mean = (ci_bounds[, 1] <= true_mean & ci_bounds[, 2] >= true_mean)
    )
    
    # Define colors conditionally based on the checkbox input
    colors <- if (input$greyOut) {
      c("TRUE" = "grey", "FALSE" = "red")
    } else {
      c("TRUE" = "blue", "FALSE" = "red")
    }
    
    # Plot the confidence intervals along with the true population mean
    output$ciPlot <- renderPlot({
      ggplot(ci_data, aes(y = sample)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper, color = contains_true_mean), height = 0.2) +
        geom_point(aes(x = mean), color = "blue") +
        geom_vline(xintercept = true_mean, color = "red", linetype = "dashed") +
        scale_color_manual(values = colors) +
        labs(title = "Confidence Intervals for Alien Eye Counts",
             x = "Number of Eyes") +
        theme_minimal() +
        theme(axis.title.y = element_blank(), 
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
        )
    })
    
    
    # Calculate the number of confidence intervals that do not contain the true mean
    not_containing_true_mean <- sum(ci_bounds[, 1] > true_mean | ci_bounds[, 2] < true_mean)
    output$ciText <- renderText({
      paste("Number of confidence intervals not containing the true mean:", not_containing_true_mean)
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
