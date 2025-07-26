library(shiny)
library(tidyverse)
library(gamlss)


bayley_gsv <- readRDS("Bayley_GSV_scores.rds")

gsv_rename <- bayley_gsv %>% rename(
  Cognitive = 'bsid_gsv_cog',
  Receptive = 'bsid_gsv_rc',
  Expressive = 'bsid_gsv_ec',
  `Fine Motor` = 'bsid_gsv_fm',
  `Gross Motor` = 'bsid_gsv_gm'
)



gsv_long <- gsv_rename %>% pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine Motor', 'Gross Motor'), names_to = 'domain', values_to = 'score')

transform_score <- function(scores) {
  z_scores <- (scores - 500)/100
  
  transformed <- z_scores * 25 + 500
  
  return(transformed)
}


new_gsv_long <- gsv_long %>% mutate(transformed_score = round(transform_score(score)),0) # Add transformed scores as a new column

new_gsv_long_rem <- new_gsv_long %>% filter(redcap_event_name != "2_month_visit_arm_1")

gsv <- new_gsv_long_rem

ui <- fluidPage(
  titlePanel("Growth Plots by Domain with Age Input"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("domainInput", "Select Domain:",
                  choices = unique(gsv$domain),
                  selected = unique(gsv$domain)[1]),
      numericInput("ageInput", "Input Age (months):", value = 12, min = 2, max = 41, step = 0.5),
      helpText("Age must be between 2 and 41 months.")
    ),
    mainPanel(
      plotOutput("growthPlot")
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive filtered dataset based on domain and visit filter
  filtered_data <- reactive({
    data <- gsv %>%
      filter(domain == input$domain)
    
    if (input$exclude_2m) {
      data <- data %>% filter(redcap_event_name != "2_month_visit_arm_1")
    }
    
    data %>% 
      filter(!is.na(bsid_age_calc), !is.na(score))
  })
  
  # Plot growth curve
  output$growthPlot <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = bsid_age_calc, y = transformed_score)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", se = FALSE) +
      labs(
        title = paste(input$domain, "Scores vs. Age"),
        x = "Age (months)",
        y = paste(input$domain, "Score")
      )
  })
  
  # Fit and print GAMLSS model summary
  output$modelSummary <- renderPrint({
    data <- filtered_data()
    if(nrow(data) < 10){
      cat("Not enough data to fit model")
      return()
    }
    
    # Transform score like in your original code
    data <- data %>% mutate(bsid_gsv_cog4 = ((gsv_score - 500)/100)*25 + 500)
    
    model <- tryCatch({
      gamlss(
        formula = bsid_gsv_cog4 ~ pb(bsid_age_calc, lambda = 5),
        sigma.formula = ~ pb(bsid_age_calc),
        nu.formula = ~1,
        tau.formula = ~1,
        data = data,
        family = BCCG()
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(model)) {
      cat("Model fitting failed.")
    } else {
      summary(model)
    }
  })
  
  output$growthPlot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) < 10){
      plot.new()
      text(0.5, 0.5, "Not enough data to plot")
      return()
    }
    

    # Fit GAMLSS model
    gamlss_gm <- tryCatch({
      gamlss(
        formula = transformed_score ~ pb(bsid_age_calc, lambda = 5),
        sigma.formula = ~ pb(bsid_age_calc),
        nu.formula = ~ 1,
        tau.formula = ~ 1,
        data = data,
        family = BCCG()
      )
    }, error = function(e) { NULL })
    
    # Prepare prediction ages
    ages <- seq(min(data$bsid_age_calc), max(data$bsid_age_calc), by = 0.5)
    newdata <- data.frame(bsid_age_calc = ages)
    
    if(!is.null(gamlss_gm)){
      params <- predictAll(gamlss_gm, newdata = newdata)
      
      centiles <- c(5, 10, 25, 50, 75, 90, 95)
      q_vals <- sapply(centiles / 100, function(p) {
        qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
      })
      
      lms_gm <- data.frame(age = ages, q_vals)
      colnames(lms_gm)[-1] <- paste0("P", centiles)
    }
    
    p <- ggplot(data, aes(x = bsid_age_calc, y = transformed_score)) +
      geom_point(alpha = 0.6) +
      labs(x = "Age (months)", y = paste(input$domain, "Transformed Score")) +
      theme_minimal()
    
    if(!is.null(gamlss_gm)){
      # Add centile curves
      for (col in colnames(lms_gm)[-1]) {
        p <- p + geom_line(data = lms_gm, aes_string(x = "age", y = col), color = "blue", linetype = "dashed")
      }
    }
    
    p
  })
  
}

shinyApp(ui, server)
