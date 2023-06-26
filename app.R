# Updating all the required libraries

library(shiny)
library(shinythemes)
library(kernlab)
library(AER)
library(dplyr)
library(broom)
library(maxLik)
library(tidyr)
library(tidyverse)
library(skimr)
library(factoextra)
library(ggpubr)
library(plotly)
library(cluster)
library(DT)
library(ggforce)
library(gridExtra)
library(corrplot)
library(MASS)


# Define UI for Shiny app
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("NewChic Accessories Data Analysis"),
  
  # Sidebar with input options
  sidebarLayout(
    sidebarPanel(
      h4("Select Dataset"),
      selectInput("dataset", "Choose a dataset:",
                  choices = c("accessories")),
      selectInput("model_type", "Choose a model type:",
                  choices = c("poisson", "negbin")),
      
      h4("Select Variables"),
      checkboxGroupInput("variables", "Choose variables to display:",
                         choices = c("current_price", "raw_price", "discount",
                                     "likes_count", "discount_price"),
                         selected = c("current_price", "raw_price", "discount",
                                      "likes_count", "discount_price"))
    ),
    
    # Main panel with output
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Statistics",
                 DT::dataTableOutput("summary_table"),
                 plotlyOutput("likes_count_hist")
        ),
        
        tabPanel("Analysing the Plots and MLE",
                 h4("Scatter Plots"),
                 br(),
                 plotOutput("scatterplots"),
                 br(),
                 h4("Correlation Plot"),
                 plotOutput("correlation_plot"),
                 br(),
                 h4("Maximum likelihood Estimation"),
                 br(),
                 verbatimTextOutput("mle_normal_output")
        ),
        tabPanel("Mean vs Variance Plot",
                 plotOutput("mean_var_output")
        ),
        tabPanel("Model Summary",
                 h4("Summary of Model"),
                 br(),
                 verbatimTextOutput("model_summary"),
                 br(),
                 h4("Assumptions of Model"),
                 br(),
                 h4("Linearity"),
                 plotOutput("linearity"),
                 h4("Independence"),
                 plotOutput("independence"),
                 br(),
                 h4("Over Dispersion"),
                 verbatimTextOutput("dispersion_test")
                  ),
        tabPanel("Model Evaluation",
                 h4("Model Evaluation"),
                 verbatimTextOutput("aic_bic"),
                 br(),
                 h4("Final Result"),
                 htmlOutput("mod_summary")
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Load data
  accessory <- reactive({
    if(input$dataset == "accessories") {
      accessories <- read_csv("accessories.csv", na = c("", "NA"))
      accessories <- accessories %>% 
        mutate(discount_price = (raw_price * (100 - discount)) / 100)
      
      # Convert the subcategory and currency variables to factors
      accessories$subcategory <- as.factor(accessories$subcategory)
      accessories$currency <- as.factor(accessories$currency)
      
      # Remove any missing values or outliers from the dataset
      accessories <- accessories %>% 
        drop_na() %>%  # Remove rows with missing values
        filter(likes_count < quantile(likes_count, 0.99))  # Remove the top 1% of likes_count values
      
      return(accessories)
    }
  })
  
  # Create summary table
  output$summary_table <- DT::renderDataTable({
    if(!is.null(accessory())) {
      skimr::skim(accessory())
    }
  })
  
  # Create histogram of likes_count
  output$likes_count_hist <- renderPlotly({
    if(!is.null(accessory())) {
      ggplotly(ggplot(accessory(), aes(x = likes_count)) +
                 geom_histogram(bins = 20, fill = "lightblue", color = "black") +
                 xlab("Likes Count") +
                 ylab("Frequency") +
                 ggtitle("Distribution of Likes Count")
      )
    }
  })
  
  # Generate scatterplots for selected variables
  output$scatterplots <- renderPlot({
    if(!is.null(input$variables) && length(input$variables) > 0 && !is.null(accessory())) {
      p_list <- lapply(input$variables, function(var) {
        ggplot(accessory(), aes_string(x = var, y = "likes_count")) +
          geom_point() +
          ggtitle(paste0("Scatterplot of ", var, " vs. likes_count"))
      })
      grid.arrange(grobs = p_list)
    } else {
      NULL
    }
  })
  
  # Compute the correlation matrix for selected variables
  cor_matrix <- reactive({
    selected_vars <- accessory()[, input$variables, drop = FALSE]
    cor(selected_vars)
  })
  
  # Create correlation plot
  output$correlation_plot <- renderPlot({
    corrplot(cor_matrix(), method = "number", type = "upper", order = "hclust",
             tl.col = "black", col = colorRampPalette(c("white", "deepskyblue", 
                                                        "blue4"))(100))
  })
  
  # Maximum Likelihood Estimates for Normal Distribution
  output$mle_normal_output <- renderPrint({
    fit <- fitdistr(accessory()$likes_count, "normal")
    mean_est <- fit$estimate[1]
    sd_est <- fit$estimate[2]
    
    # Print the estimated parameter values
    cat("Maximum likelihood estimates for Normal distribution:\n")
    cat("Mean:", mean_est, "\n")
    cat("Standard deviation:", sd_est, "\n")
  })
  
  # Mean-Variance Relationship
  output$mean_var_output <- renderPlot({
    # Calculate the mean and variance of likes_count for each value of the subcategory variable
    mean_var_df <- accessory() %>% group_by(subcategory) %>% 
      summarise(mean_likes = mean(likes_count), var_likes = var(likes_count))
    
    # Plot the mean-variance relationship
    ggplot(mean_var_df, aes(x = mean_likes, y = var_likes)) +
      geom_point() +
      scale_x_continuous(name = "Mean likes count") +
      scale_y_continuous(name = "Variance of likes count") +
      ggtitle("Mean-Variance Relationship for Likes Count")
  })
  
  # Create a reactive input switch to choose between Poisson and Negative Binomial models
  model_choice <- reactive({
    if (input$model_type == "poisson") {
      return("poisson")
    } else if (input$model_type == "negbin") {
      return("negbin")
    } else {
      stop("Invalid model type specified.")
    }
  })
  
  output$linearity <- renderPlot({
    # Split data into training and testing sets
    set.seed(123)
    train_indices <- sample(nrow(accessory()), 0.7 * nrow(accessory())) # 70% for training
    train_data <- accessory()[train_indices, ]
    test_data <- accessory()[-train_indices, ]
    
    if (model_choice() == "poisson") {
      # Create Poisson regression model using training data
      poisson_model <- glm(likes_count ~ current_price + raw_price + discount, 
                           data = train_data, family = "poisson")
      # Check assumptions
      poisson_aug <- augment(poisson_model)
      
      # Linearity assumption: plot residuals vs. fitted values
      ggplot(data = poisson_aug, aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        xlab("Fitted values") +
        ylab("Residuals")
    } else if (model_choice() == "negbin") {
      # Create Negative Binomial regression model using training data
      negbin_model <- glm.nb(likes_count ~ subcategory + current_price + raw_price + discount + discount_price, 
                             data = train_data)
      # Check assumptions
      negbin_aug <- augment(negbin_model)
      
      # Linearity assumption: plot residuals vs. fitted values
      ggplot(data = negbin_aug, aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        xlab("Fitted values") +
        ylab("Residuals")
    }
  })
  
  output$independence <- renderPlot({
    # Split data into training and testing sets
    set.seed(123)
    train_indices <- sample(nrow(accessory()), 0.7 * nrow(accessory())) # 70% for training
    train_data <- accessory()[train_indices, ]
    test_data <- accessory()[-train_indices, ]
    
    if (model_choice() == "poisson") {
      # Create Poisson regression model using training data
      poisson_model <- glm(likes_count ~ current_price + raw_price + discount, 
                           data = train_data, family = "poisson")
      # Check assumptions
      poisson_aug <- augment(poisson_model)
      
      # Independence assumption: plot residuals vs. order of observations
      ggplot(data = poisson_aug, aes(x = 1:nrow(poisson_aug), y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        xlab("Observation order") +
        ylab("Residuals")
    } else if (model_choice() == "negbin") {
      # Create Negative Binomial regression model using training data
      negbin_model <- glm.nb(likes_count ~ subcategory + current_price + raw_price + discount + discount_price, 
                             data = train_data)
      # Check assumptions
      negbin_aug <- augment(negbin_model)
      
      # Independence assumption: plot residuals vs. order of observations
      ggplot(data = negbin_aug, aes(x = 1:nrow(negbin_aug), y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        xlab("Observation order") +
        ylab("Residuals")
    }
   })
  #Over dispersion test
  output$dispersion_test <- renderPrint({
    
    # Split data into training and testing sets
    set.seed(123)
    train_indices <- sample(nrow(accessory()), 0.7 * nrow(accessory())) # 70% for training
    train_data <- accessory()[train_indices, ]
    test_data <- accessory()[-train_indices, ]
    
    
    if (model_choice() == "poisson") {
      # Create Poisson regression model using training data
      poisson_model <- glm(likes_count ~ current_price + raw_price + discount, 
                           data = train_data, family = "poisson")
      # Check assumptions
      poisson_aug <- augment(poisson_model)
      
      # Calculate dispersion parameter for Poisson model
      disptest <- dispersiontest(poisson_model)
      disptest
    } else if (model_choice() == "negbin") {
      print("Not Applicable Over Dispersion Test is only for Poisson GLM models")

    } else {
      print("Invalid model choice specified.")
    }
  })
  
  # Model Summary
  output$model_summary <- renderPrint({
    
    # Split data into training and testing sets
    set.seed(123)
    train_indices <- sample(nrow(accessory()), 0.7 * nrow(accessory())) # 70% for training
    train_data <- accessory()[train_indices, ]
    test_data <- accessory()[-train_indices, ]
    
    
    if (model_choice() == "poisson") {
      # Create Poisson regression model using training data
      poisson_model <- glm(likes_count ~ current_price + raw_price + discount, 
                           data = train_data, family = "poisson")
      # Check assumptions
      poisson_aug <- augment(poisson_model)
      
      # Display summary of Poisson model
      summary(poisson_model)
    } else if (model_choice() == "negbin") {
      negbin_model <- glm.nb(likes_count ~ current_price + raw_price + discount + discount_price, 
                             data = train_data)
      # Check assumptions
      negbin_aug <- augment(negbin_model)
      # Display summary of Negative Binomial model
      summary(negbin_model)
    } else {
      print("Invalid model choice specified.")
    }
  })
  
  
  # Model Evaluation
  output$aic_bic <- renderPrint({
    # Split data into training and testing sets
    set.seed(123)
    train_indices <- sample(nrow(accessory()), 0.7 * nrow(accessory())) # 70% for training
    train_data <- accessory()[train_indices, ]
    
    # Create Poisson and Negative Binomial regression models using training data
    poisson_model <- glm(likes_count ~ current_price + raw_price + discount, 
                         data = train_data, family = "poisson")
    negbin_model <- glm.nb(likes_count ~ current_price + raw_price + discount + discount_price, 
                           data = train_data)
    
    # Calculate AIC and BIC for both models
    aic_poisson <- AIC(poisson_model)
    bic_poisson <- BIC(poisson_model)
    aic_negbin <- AIC(negbin_model)
    bic_negbin <- BIC(negbin_model)
    
    # Compare AIC and BIC values to determine which model has a better fit
    if (aic_poisson < aic_negbin & bic_poisson < bic_negbin) {
      result <- "Poisson model is the better fit."
      eqn <- "likes_count ~ current_price + raw_price + discount"
    } else if (aic_negbin < aic_poisson & bic_negbin < bic_poisson) {
      result <- "Negative Binomial model is the better fit."
      eqn <- "likes_count ~ current_price + raw_price + discount + discount_price"
    } else {
      result <- "Neither model has a significantly better fit."
      eqn <- "likes_count ~ current_price + raw_price + discount,"
    }
    
    # Output the results and model equation
    cat("AIC comparison:\n")
    print(cbind(poisson_model = aic_poisson, negbin_model = aic_negbin))
    cat("\nBIC comparison:\n")
    print(cbind(poisson_model = bic_poisson, negbin_model = bic_negbin))
    cat("\n", result, "\n")
    cat("Model equation:\n", eqn)
  })
  
  output$mod_summary <- renderText({
    "<p>Based on the given problem statement and the results obtained from the Poisson and Negative Binomial models, it can be observed that the Negative Binomial model is a better fit for predicting the dependent variable 'Likes_count'. The AIC and BIC values for the Negative Binomial model are significantly lower than the Poisson model, indicating that the former model has a better fit to the data.</p>

  <p>Furthermore, the model equation suggests that the variables 'current_price', 'raw_price', 'discount', and 'discount_price' are significant factors that affect the 'Likes_count'. It can be inferred that a decrease in these variables may lead to an increase in the 'Likes_count' and vice versa.</p>

  <p>In conclusion, the Negative Binomial model with the given variables provides a better fit for predicting the 'Likes_count' and suggests that the current_price, raw_price, discount, and discount_price are important predictors of the likes count in this scenario.</p>"
  })
  
  
}

# Run Shiny app
shinyApp(ui, server)

       
