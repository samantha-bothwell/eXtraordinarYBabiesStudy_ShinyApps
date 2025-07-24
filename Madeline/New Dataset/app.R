# Load libraries
library(shiny)
library(ggplot2)
library(gamlss)
library(dplyr)
library(tidyr)

# ---- Load your dataset ----
# You must load your dataset BEFORE server starts
# Change this path as needed if the CSV is external
new_gsv_long_rem <- readRDS("new_gsv_long_rem.rds") %>% 
  mutate(sca_condition = as.character(sca_condition))

# UI
ui <- fluidPage(
  titlePanel("Growth Charts (Bayley Scores)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain_select", "Select Domain:",
                  choices = unique(new_gsv_long_rem$domain), 
                  selected = unique(new_gsv_long_rem$domain)[1]),
      selectInput("sct_select", "Select SCT Condition:",
                  choices = c("ALL", unique(as.character(new_gsv_long_rem$sca_condition))),
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
    
    filtered_data <- droplevels(filtered_data)
    
    validate(
      need(nrow(filtered_data) > 10, "Not enough data after filtering.")
    )
    
    # ---- Fit GAMLSS model (fixed with save.data = TRUE) ----
    model <- tryCatch({
      gamlss(
        formula = as.formula("transformed_score ~ pb(bsid_age_calc, lambda = 5)"),
        sigma.formula = as.formula("~ pb(bsid_age_calc)"),
        nu.formula = ~1,
        tau.formula = ~1,
        data = filtered_data,
        family = BCCG(),
        control = gamlss.control(save.data = TRUE),   # <-- THIS FIX IS CRUCIAL
        trace = FALSE
      )
    }, error = function(e) {
      print("=== ERROR IN GAMLSS MODEL ===")
      print(conditionMessage(e))
      return(NULL)
    })
    
    validate(
      need(!is.null(model), "Model failed to fit.")
    )
    
    # ---- Centiles prediction ----
    age_seq <- seq(
      min(filtered_data$bsid_age_calc, na.rm = TRUE),
      max(filtered_data$bsid_age_calc, na.rm = TRUE),
      length.out = 100
    )
    
    qvals <- tryCatch({
      centiles.pred(
        object = model,
        xname = "bsid_age_calc",
        xvalues = age_seq,
        cent = c(10, 25, 50, 75, 90),
        plot = FALSE,
        data = filtered_data  # <- This is KEY
      )
    }, error = function(e) {
      print("=== ERROR IN CENTILES.PRED ===")
      print(conditionMessage(e))
      return(NULL)
    })
    
    validate(
      need(!is.null(qvals), "Centile prediction failed.")
    )
    
    # ---- Pivot long for ggplot ----
    pred_long <- pivot_longer(as.data.frame(qvals), -x,
                              names_to = "Percentile", values_to = "Score")
    
    # ---- Plot ----
    ggplot(pred_long, aes(x = x, y = Score, color = Percentile)) +
      geom_line(size = 1.2) +
      geom_ribbon(
        data = data.frame(
          x = qvals$x,
          ymin = qvals$`10`,
          ymax = qvals$`90`
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

                              

