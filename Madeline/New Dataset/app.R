# Load libraries
library(shiny)
library(ggplot2)
library(gamlss)
library(dplyr)
library(tidyr)
library(tidyverse)

# ---- Load your dataset ----
# You must load your dataset BEFORE server starts
# Change this path as needed if the CSV is external
new_gsv_long_rem <- readRDS("Bayley_GSV_scores.rds") %>% 
  mutate(sca_condition = as.character(sca_condition)) %>% 
  pivot_longer(!c(study_id_extraordinary, redcap_event_name, sca_condition, bayley_version, bsid_age_calc), 
               names_to = "domain", values_to = "score") %>% 
  mutate(domain = case_when(domain == "bsid_gsv_cog" ~ "Cognitive", 
                            domain == "bsid_gsv_rc" ~ "Receptive Communication", 
                            domain == "bsid_gsv_ec" ~ "Expressive Communication", 
                            domain == "bsid_gsv_fm" ~ "Fine Motor", 
                            domain == "bsid_gsv_gm" ~ "Gross Motor")) %>% 
  mutate(transformed_score = ((score - 500)/100)*25 + 500)

# UI
ui <- fluidPage(
  titlePanel("Growth Charts (Bayley Scores)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain_select", "Select Domain:",
                  choices = unique(new_gsv_long_rem$domain), 
                  selected = unique(new_gsv_long_rem$domain)[1]),
      selectInput("sct_select", "Select SCT Condition:",
                  choices = c("ALL", unique(new_gsv_long_rem$sca_condition)),
                  selected = "ALL")
    ),
    mainPanel(
      plotOutput("growth_plot"),
      br(),
      p("Figure caption: see explanation in Tab 5")
    )
  )
)

# ---- Server logic ----
server <- function(input, output, session) {
  
  output$growth_plot <- renderPlot({
    
    req(input$domain_select) 
    req(input$sct_select)
    
    # Filtered data (local to renderPlot)
    filtered_data <- new_gsv_long_rem %>%
      filter(domain == input$domain_select) %>%
      dplyr::select(study_id_extraordinary, sca_condition, domain, bsid_age_calc, transformed_score) %>%
      filter(complete.cases(.))
    
    
    if (input$sct_select != "ALL") {
      filtered_data <- filtered_data %>%
        filter(sca_condition == input$sct_select)
    }
    

    validate(
      need(nrow(filtered_data) > 10, "Not enough data after filtering.")
    )
    
    
    # Fit GAMLSS model
    model <- gamlss(
        formula = as.formula("transformed_score ~ pb(bsid_age_calc, lambda = 5)"),
        sigma.formula = as.formula("~ pb(bsid_age_calc)"),
        nu.formula = ~1,
        tau.formula = ~1,
        data = filtered_data,
        family = BCCG(),
        control = gamlss.control(save.data = TRUE),   # <-- THIS FIX IS CRUCIAL
        trace = FALSE
      )
    
    # ages to predict over 
    age_seq <- seq(
      min(filtered_data$bsid_age_calc, na.rm = TRUE),
      max(filtered_data$bsid_age_calc, na.rm = TRUE),
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

    # ---- Plot ----
    ggplot(pred_long, aes(x = age, y = Score, color = Percentile)) +
      geom_line(size = 1.2) +
      geom_ribbon(
        data = data.frame(
          x = lms_mod$age,
          ymin = lms_mod$P10,
          ymax = lms_mod$P90
        ),
        aes(x = x, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        alpha = 0.2,
        fill = "gray70"
      ) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Growth Chart for", input$domain_select),
        x = "Age (months)",
        y = "Transformed Bayley Score",
        color = "Percentile"
      )
  })
}



# Run the app
shinyApp(ui = ui, server = server)

                              

