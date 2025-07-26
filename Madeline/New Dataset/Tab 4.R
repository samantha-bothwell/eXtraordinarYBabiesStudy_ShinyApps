#########################
#Starting code for tab 4#
#########################

library(shiny)
library(tidyverse)
library(gamlss)
library(gamlss.add)
library(ggplot2)

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

# Global storage, to save teh reac tive chart
global_user_data <- reactiveValues(df = data.frame(Age = numeric(), Score = numeric(), domain = character()))

# UI
ui <- fluidPage(
  titlePanel("Growth Input Charts (Bayley Scores)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain_select", "Select Domain:",
                  choices = unique(new_gsv_long_rem$domain),
                  selected = "Cognitive"),#The drop down for domain
      selectInput("sct_select", "Select SCT Condition:",
                  choices = c("ALL", unique(new_gsv_long_rem$sca_condition)),
                  selected = "ALL"),#Drop down for SCA condition
      
      h4("Input 4 GSV Checkup Points"), #The age and scores to be saved for the plot in server
      numericInput("age1", "Age 1 (months):", NA),
      numericInput("score1", "GSV 1:", NA),
      numericInput("age2", "Age 2 (months):", NA),
      numericInput("score2", "GSV 2:", NA),
      numericInput("age3", "Age 3 (months):", NA),
      numericInput("score3", "GSV 3:", NA),
      numericInput("age4", "Age 4 (months):", NA),
      numericInput("score4", "GSV 4:", NA),
      
      #Save the inputs to make it show up on graph & export buttons
      actionButton("save_inputs", "Save Inputs"),
      downloadButton("download_plot", "Download All Domain Plots") 
    ),
    mainPanel(
      plotOutput("input_growth_plot"),
      br(),
      p("Figure caption: Shows user-entered trajectory with study population ranges.")
    )
  )
)

# Server
server <- function(input, output, session) {
  #Makes the data reactive in the server so the data doesn't save 
  user_data <- reactiveValues(df = data.frame(Age = numeric(), Score = numeric(), domain = character()))
  #Make the inputs for age and score be in the server 
  observeEvent(input$save_inputs, {
    new_points <- data.frame(
      Age = c(input$age1, input$age2, input$age3, input$age4),
      Score = c(input$score1, input$score2, input$score3, input$score4)
    ) %>%
      filter(!is.na(Age), !is.na(Score)) %>%
      mutate(domain = input$domain_select)
    
    global_user_data$df <- new_points #Saves the new points to the reactive graph
  })
  
  output$input_growth_plot <- renderPlot({
    user_df <- global_user_data$df
    if (nrow(user_df) <= 3) return(NULL)  # Do not plot line unless 3 or more points
    
    # Filter population data
    data_filtered <- new_gsv_long_rem %>%
      filter(domain == input$domain_select) %>%
      filter(if (input$sct_select == "ALL") TRUE else sca_condition == input$sct_select)
    
    # Compute percentiles
    percentiles_df <- data_filtered %>%
      group_by(bsid_age_calc) %>%
      summarize(
        p10 = quantile(transformed_score, 0.10, na.rm = TRUE),
        p25 = quantile(transformed_score, 0.25, na.rm = TRUE),
        p50 = quantile(transformed_score, 0.50, na.rm = TRUE),
        p75 = quantile(transformed_score, 0.75, na.rm = TRUE),
        p90 = quantile(transformed_score, 0.90, na.rm = TRUE),
        .groups = "drop"
      )
    
    #Plot 
    ggplot() +
      geom_ribbon(data = percentiles_df, aes(x = bsid_age_calc, ymin = p10, ymax = p90), fill = "lightblue", alpha = 0.3) +
      geom_ribbon(data = percentiles_df, aes(x = bsid_age_calc, ymin = p25, ymax = p75), fill = "blue", alpha = 0.2) +
      geom_line(data = percentiles_df, aes(x = bsid_age_calc, y = p50), color = "black", size = 1.2) +
      geom_line(data = percentiles_df, aes(x = bsid_age_calc, y = p25), color = "black", linetype = "dashed") +
      geom_line(data = percentiles_df, aes(x = bsid_age_calc, y = p75), color = "black", linetype = "dashed") +
      geom_line(data = user_df, aes(x = Age, y = Score), color = "red", size = 1.5) +
      geom_point(data = user_df, aes(x = Age, y = Score), color = "red", size = 3) +
      labs(
        title = paste("Growth Trajectory for", input$domain_select),
        x = "Age (months)",
        y = "Transformed GSV Score"
      ) +
      theme_minimal(base_size = 14)
  })
  
  #Download the plot 
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("growth_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      domains <- unique(new_gsv_long_rem$domain)
      plots <- lapply(domains, function(dom) {
        user_df <- global_user_data$df %>% filter(domain == dom)
        data_filtered <- new_gsv_long_rem %>%
          filter(domain == dom) %>%
          filter(if (input$sct_select == "ALL") TRUE else sca_condition == input$sct_select)
        
        percentiles_df <- data_filtered %>%
          group_by(bsid_age_calc) %>%
          summarize(
            p10 = quantile(transformed_score, 0.10, na.rm = TRUE),
            p25 = quantile(transformed_score, 0.25, na.rm = TRUE),
            p50 = quantile(transformed_score, 0.50, na.rm = TRUE),
            p75 = quantile(transformed_score, 0.75, na.rm = TRUE),
            p90 = quantile(transformed_score, 0.90, na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot() +
          geom_ribbon(data = percentiles_df, aes(x = bsid_age_calc, ymin = p10, ymax = p90), fill = "lightblue", alpha = 0.3) +
          geom_ribbon(data = percentiles_df, aes(x = bsid_age_calc, ymin = p25, ymax = p75), fill = "blue", alpha = 0.2) +
          geom_smooth(data = percentiles_df, aes(x = bsid_age_calc, y = p50), color = "black", size = 1.2) +
          geom_smooth(data = user_df, aes(x = Age, y = Score), color = "red", size = 1.5) +
          geom_point(data = user_df, aes(x = Age, y = Score), color = "red", size = 3) +
          labs(title = paste("Domain:", dom), x = "Age (months)", y = "Transformed GSV Score") +
          theme_minimal(base_size = 14)
      })
      
      ggsave(file, arrangeGrob(grobs = plots, ncol = 1), width = 8, height = 4 * length(plots))
    }
  )
}

shinyApp(ui = ui, server = server)

