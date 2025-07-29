#########################
#Starting code for tab 4#
#########################

library(shiny)
library(tidyverse)
library(dplyr)
library(gamlss)
library(gamlss.add)
library(ggplot2)
library(zoo)
library(gridExtra)

# Load and preprocess data
new_gsv_long_rem <- readRDS("Bayley_GSV_scores.rds") %>%
  mutate(sca_condition = as.character(sca_condition)) %>%
  pivot_longer(
    !c(study_id_extraordinary, redcap_event_name, sca_condition, bayley_version, bsid_age_calc),
    names_to = "domain", values_to = "score"
  ) %>%
  mutate(domain = case_when(
    domain == "bsid_gsv_cog" ~ "Cognitive",
    domain == "bsid_gsv_rc" ~ "Receptive Communication",
    domain == "bsid_gsv_ec" ~ "Expressive Communication",
    domain == "bsid_gsv_fm" ~ "Fine Motor",
    domain == "bsid_gsv_gm" ~ "Gross Motor"
  )) %>%
  mutate(transformed_score = ((score - 500) / 100) * 25 + 500)

# UI
ui <- fluidPage(
  titlePanel("Growth Input Charts (Bayley Scores)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain_select", "Select Domain:",
                  choices = unique(new_gsv_long_rem$domain),
                  selected = "Cognitive"),
      selectInput("sct_select", "Select SCT Condition:",
                  choices = c("ALL", unique(new_gsv_long_rem$sca_condition)),
                  selected = "ALL"),
      h4("Input 4 GSV Checkup Points"),
      numericInput("age1", "Age 1 (months):", NA),
      numericInput("score1", "GSV 1:", NA),
      numericInput("age2", "Age 2 (months):", NA),
      numericInput("score2", "GSV 2:", NA),
      numericInput("age3", "Age 3 (months):", NA),
      numericInput("score3", "GSV 3:", NA),
      numericInput("age4", "Age 4 (months):", NA),
      numericInput("score4", "GSV 4:", NA),
      actionButton("save_inputs", "Save Inputs"),
      downloadButton("download_plot", "Download All Domain Plots")
    ),
    mainPanel(
      plotOutput("input_growth_plot"),
      br(),
      p("Figure caption: Shows user-entered trajectory with study population ranges, from the 10th to 90th percentile in grey.")
    )
  )
)

# Server
server <- function(input, output, session) {
  global_user_data <- reactiveValues(df = data.frame(Age = numeric(), Score = numeric(), domain = character()))
  
  observeEvent(input$save_inputs, {
    new_points <- data.frame(
      Age = c(input$age1, input$age2, input$age3, input$age4),
      Score = c(input$score1, input$score2, input$score3, input$score4)
    ) %>%
      filter(!is.na(Age), !is.na(Score)) %>%
      mutate(domain = input$domain_select)
    
    global_user_data$df <- new_points
  })
  
  output$input_growth_plot <- renderPlot({
    user_df <- global_user_data$df
    
    # Return nothing if not enough points
    if (nrow(user_df) <= 3) return(NULL)
    
    # Filtered data (local to renderPlot)
    filtered_data <- new_gsv_long_rem %>%
      filter(domain == input$domain_select) %>%
      dplyr::select(study_id_extraordinary, sca_condition, domain, bsid_age_calc, transformed_score) %>%
      filter(complete.cases(.))
    
  
    # Fit GAMLSS model
    model <- gamlss(
      formula = as.formula("transformed_score ~ pb(bsid_age_calc, lambda = 5)"),
      sigma.formula = as.formula("~ pb(bsid_age_calc)"),
      nu.formula = ~1,
      tau.formula = ~1,
      data = filtered_data,
      family = BCCG(),
      control = gamlss.control(save.data = TRUE),   
      trace = FALSE
    )
    
    # Predict percentiles
    age_seq <- seq(
      from = max(5, min(filtered_data$bsid_age_calc, na.rm = TRUE)),
      to = max(filtered_data$bsid_age_calc, na.rm = TRUE),
      length.out = 100
    )
    
    model$call$data <- filtered_data
    
    # data 
    newdata <- data.frame(bsid_age_calc = age_seq)
    
    
    # Get predicted distribution parameters from the model
    params <- predictAll(model, newdata = newdata)
    
    # Calculate centiles manually using the BCCG distribution quantile function
    centiles <- c(10, 25, 50, 75, 90)
    q_vals <- sapply(centiles / 100, function(p) {
      qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
    })
    
    # Create a dataframe of predicted centiles
    lms_mod <- data.frame(age = age_seq, q_vals)
    colnames(lms_mod)[-1] <- paste0("P", centiles)
    
    validate(
      need(!is.null(lms_mod), "Centile prediction failed.")
    )
    
    # ---- Pivot long for ggplot ----
    pred_long <- pivot_longer(lms_mod, -age,
                              names_to = "Percentile", values_to = "Score")
    
    
    p <- ggplot() +
      # 10–90 ribbon
      geom_ribbon(data = lms_mod, aes(x = age, ymin = P10, ymax = P90),
                  fill = "gray85", alpha = 0.3) +
      
      # 50th percentile: thick black solid
      geom_smooth(data = lms_mod, aes(x = age, y = P50),
                  color = "black", size = 1.6, linetype = "solid") +
      
      # 10th & 90th percentiles: blue dashed
      geom_smooth(data = lms_mod, aes(x = age, y = P10),
                  color = "slateblue1", size = 1.1, linetype = "dashed") +
      geom_smooth(data = lms_mod, aes(x = age, y = P90),
                  color = "slateblue1", size = 1.1, linetype = "dashed") +
      
      # 25th & 75th percentiles: gray, lighter lines
      geom_smooth(data = lms_mod, aes(x = age, y = P25),
                  color = "snow3", size = 0.8, linetype = "solid") +
      geom_smooth(data = lms_mod, aes(x = age, y = P75),
                  color = "snow3", size = 0.8, linetype = "solid") +
      
      # Theme and labels
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Growth Chart for", input$domain_select),
        x = "Age (months)",
        y = "Bayley-4 Score"
      )
    # Add user input points
    p <- p + geom_point(data = user_df,
                        aes(x = Age, y = Score),
                        color = "darkorange2", size = 3)
    
    # Add user input trajectory line
    p <- p + geom_smooth(data = user_df,
                       aes(x = Age, y = Score),
                       color = "darkorange2", size = 1.2)
    
    p
    }
    
)}

  

shinyApp(ui = ui, server = server)
