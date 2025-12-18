library(shiny)
library(readr)
library(rstatix)
library(dplyr)
library(ggplot2)

# Load data
data <- read_rds("paper_data.rds")

# Function to determine if a variable is numeric
is_numeric_var <- function(df, var_name) {
  var <- df[[var_name]]
  is.numeric(var) || is.integer(var)
}

server <- function(input, output, session) {
  
  # Update independent variable choices based on available columns
  # Exclude dependent variable options and keep only non-dependent variables
  observe({
    available_vars <- setdiff(
      names(data),
      c("is_code_available", "data_multi")
    )
    updateSelectInput(session, "independent_var",
                      choices = available_vars)
  })
  
  # Reactive value to track if independent variable is numeric
  output$is_numeric <- reactive({
    if (is.null(input$independent_var) || input$independent_var == "") {
      return(FALSE)
    }
    is_numeric_var(data, input$independent_var)
  })
  outputOptions(output, "is_numeric", suspendWhenHidden = FALSE)
  
  # Output variable type information
  output$var_type_info <- renderText({
    if (is.null(input$independent_var) || input$independent_var == "") {
      return("Please select an independent variable.")
    }
    
    var_type <- if (is_numeric_var(data, input$independent_var)) {
      "Numeric"
    } else {
      "Categorical"
    }
    
    paste("Variable Type:", var_type)
  })
  
  # Create histogram for numeric independent variables
  output$histogram <- renderPlot({
    if (is.null(input$independent_var) || input$independent_var == "") {
      return(NULL)
    }
    
    if (!is_numeric_var(data, input$independent_var)) {
      return(NULL)
    }
    
    # Filter out missing values
    plot_data <- data %>%
      select(all_of(c(input$dependent_var, input$independent_var))) %>%
      filter(!is.na(.data[[input$dependent_var]]),
             !is.na(.data[[input$independent_var]]))
    
    ggplot(plot_data, aes(x = .data[[input$independent_var]], fill = .data[[input$dependent_var]])) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      facet_wrap(as.formula(paste("~", input$dependent_var)), ncol = 1) +
      labs(title = paste("Distribution of", input$independent_var, "by", input$dependent_var),
           x = input$independent_var,
           y = "Frequency",
           fill = input$dependent_var) +
      theme_minimal()
  })
  
  # Create cross-tabulation table for categorical independent variables
  output$crosstab <- renderTable({
    if (is.null(input$independent_var) || input$independent_var == "") {
      return(NULL)
    }
    
    if (is_numeric_var(data, input$independent_var)) {
      return(NULL)
    }
    
    # Filter out missing values
    plot_data <- data %>%
      select(all_of(c(input$dependent_var, input$independent_var))) %>%
      filter(!is.na(.data[[input$dependent_var]]),
             !is.na(.data[[input$independent_var]]))
    
    # Create cross-tabulation
    table(plot_data[[input$independent_var]], plot_data[[input$dependent_var]])
  }, rownames = TRUE)
  
  # Perform statistical tests
  output$test_results <- renderTable({
    if (is.null(input$independent_var) || input$independent_var == "") {
      return(NULL)
    }
    
    # Filter out missing values
    test_data <- data %>%
      select(all_of(c(input$dependent_var, input$independent_var))) %>%
      filter(!is.na(.data[[input$dependent_var]]),
             !is.na(.data[[input$independent_var]]))
    
    if (is_numeric_var(data, input$independent_var)) {
      # Perform t-test
      tryCatch({
        # For t_test, we need the formula format: numeric_var ~ categorical_var
        formula_str <- paste(input$independent_var, "~", input$dependent_var)
        t_test_result <- test_data %>%
          t_test(as.formula(formula_str), detailed = TRUE)
        return(t_test_result)
      }, error = function(e) {
        return(data.frame(Error = paste("Error performing t-test:", e$message)))
      })
    } else {
      # Perform chi-squared test using rstatix
      tryCatch({
        # Use rstatix::chisq_test with formula interface
        formula_str <- paste(input$dependent_var, "~", input$independent_var)
        chi2_result <- test_data %>%
          chisq_test(as.formula(formula_str))
        return(chi2_result)
      }, error = function(e) {
        return(data.frame(Error = paste("Error performing chi-squared test:", e$message)))
      })
    }
  })
}

