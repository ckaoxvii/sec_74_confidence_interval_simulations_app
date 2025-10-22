library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

ui <- page_sidebar(
  title = "Confidence Interval Simulation for Population Proportion",
  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),
  # Add custom CSS to ensure card headers have the correct styling
  tags$head(
    tags$style(HTML("
      .card-header {
        background-color: #A90533 !important;
        color: white !important;
        font-weight: bold;
      }
    "))
  ),
  sidebar = sidebar(
    numericInput("true_prop", 
                 "True Population Proportion:", 
                 value = 0.6, 
                 min = 0.01, 
                 max = 0.99, 
                 step = 0.01),
    
    numericInput("sample_size", 
                 "Sample Size (n):", 
                 value = 100, 
                 min = 10, 
                 max = 1000, 
                 step = 10),
    
    numericInput("conf_level", 
                 "Confidence Level (%):", 
                 value = 95, 
                 min = 80, 
                 max = 99, 
                 step = 1),
    
    numericInput("n_simulations", 
                 "Number of Simulations:", 
                 value = 50, 
                 min = 10, 
                 max = 100, 
                 step = 5),
    
    actionButton("simulate", 
                 "Run Simulation", 
                 style = "background-color: #A90533; border-color: #A90533; color: white;"),
  ),
  
  layout_columns(
    card(
      card_header("Confidence Interval Visualization"),
      plotOutput("ci_plot", height = "500px")
    ),
    card(
      card_header("Simulation Results"),
      tableOutput("results_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store simulation results
  sim_results <- reactiveVal(NULL)
  
  # Function to calculate confidence interval
  calculate_ci <- function(x, n, conf_level) {
    p_hat <- x / n
    alpha <- 1 - (conf_level / 100)
    z_score <- qnorm(1 - alpha/2)
    
    se <- sqrt(p_hat * (1 - p_hat) / n)
    margin_error <- z_score * se
    
    lower <- p_hat - margin_error
    upper <- p_hat + margin_error
    
    # Ensure bounds are within [0, 1]
    lower <- pmax(0, lower)
    upper <- pmin(1, upper)
    
    return(data.frame(
      p_hat = p_hat,
      lower = lower,
      upper = upper,
      margin_error = margin_error
    ))
  }
  
  # Run simulation when button is pressed
  observeEvent(input$simulate, {
    # Generate random samples and calculate confidence intervals
    results <- data.frame(
      simulation = 1:input$n_simulations,
      successes = rbinom(input$n_simulations, input$sample_size, input$true_prop)
    )
    
    # Calculate confidence intervals for each simulation
    ci_results <- results %>%
      rowwise() %>%
      do({
        ci_data <- calculate_ci(.$successes, input$sample_size, input$conf_level)
        data.frame(
          simulation = .$simulation,
          successes = .$successes,
          p_hat = ci_data$p_hat,
          lower = ci_data$lower,
          upper = ci_data$upper,
          captures_true = input$true_prop >= ci_data$lower & input$true_prop <= ci_data$upper
        )
      })
    
    sim_results(ci_results)
  })
  
  # Create the confidence interval plot as a number line
  output$ci_plot <- renderPlot({
    req(sim_results())
    
    data <- sim_results()
    
    # Add y position for each interval (stacked below the number line)
    data$y_pos <- -(1:nrow(data)) * 0.8
    
    # Create the plot
    p <- ggplot(data) +
      # Draw the number line at y = 0
      geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add tick marks on the number line
      geom_segment(data = data.frame(x = seq(0, 1, 0.1)), 
                   aes(x = x, xend = x, y = 0, yend = 0.03),
                   color = "black", size = 0.5) +
      
      # Add labels for tick marks
      geom_text(data = data.frame(x = seq(0, 1, 0.2), label = seq(0, 1, 0.2)), 
                aes(x = x, y = -0.07, label = label),
                size = 3.5) +
      
      # Draw confidence intervals as horizontal segments
      geom_segment(aes(x = lower, xend = upper, y = y_pos, yend = y_pos,
                      color = captures_true), 
                  size = 2, alpha = 0.8) +
      
      # Add endpoints for confidence intervals
      geom_point(aes(x = lower, y = y_pos, color = captures_true), size = 1.5) +
      geom_point(aes(x = upper, y = y_pos, color = captures_true), size = 1.5) +
      
      # Add sample proportion points
      geom_point(aes(x = p_hat, y = y_pos), 
                size = 2, color = "black", shape = 16) +
      
      # Draw the true proportion as a red vertical line
      geom_vline(xintercept = input$true_prop, 
                color = "red", 
                size = 2, 
                alpha = 0.5) +
      
      # Color scheme
      scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                        name = "Captures True Proportion",
                        labels = c("TRUE" = "Yes", "FALSE" = "No")) +
      
      # Set plot limits and labels
      xlim(0, 1) +
      ylim(min(data$y_pos) - 1, 1) +
      # Customize theme
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_line(color = "black"),
        axis.title.x = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
      )
    
    # Add simulation numbers on the left
    p <- p + geom_text(aes(x = -0.05, y = y_pos, label = simulation),
                      size = 2.5, hjust = 1)
    
    # Add title for simulation numbers
    p <- p + annotate("text", x = -0.05, y = 0.8, label = "Sim #", 
                     size = 3, fontface = "bold", hjust = 1)
    
    p
  })
  
  # Display results table with coverage summary included
  output$results_table <- renderTable({
    req(sim_results())
    
    data <- sim_results()
    
    # Calculate coverage summary
    coverage_rate <- mean(data$captures_true) * 100
    expected_coverage <- input$conf_level
    
    # Show first 15 simulations and add summary info
    table_data <- data %>%
      slice_head(n = 15) %>%
      mutate(
        `Sample Prop` = round(p_hat, 3),
        `CI Lower` = round(lower, 3),
        `CI Upper` = round(upper, 3),
        `Captures True` = ifelse(captures_true, "Yes", "No")
      ) %>%
      select(Simulation = simulation, 
             Successes = successes,
             `Sample Prop`, 
             `CI Lower`, 
             `CI Upper`, 
             `Captures True`)
    
    # Add summary row
    summary_row <- data.frame(
      Simulation = paste("Coverage:", sum(data$captures_true), "/", nrow(data)),
      Successes = "",
      `Sample Prop` = paste(round(coverage_rate, 1), "%"),
      `CI Lower` = paste("Expected:", expected_coverage, "%"),
      `CI Upper` = "",
      `Captures True` = "",
      check.names = FALSE
    )
    
    rbind(table_data, summary_row)
  }, striped = TRUE, hover = TRUE)
}

shinyApp(ui = ui, server = server)
