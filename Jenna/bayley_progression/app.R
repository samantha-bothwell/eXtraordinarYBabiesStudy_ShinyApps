library(shiny)
library(tidyverse)
library(plotly)
library(gamlss)

milestones <- readRDS("Milestones.RDS")
genpop <- readRDS("GenPop_Milestones.RDS")
indiv_percentiles <- readRDS("Individual_Percentiles.RDS")

ui <- fluidPage(
  titlePanel("Bayley"),
  
  tabsetPanel(
    tabPanel("Overall Plot",
             # Tab 1 content here - placeholder
             h3("Overall Plot"),
             p("Content for Overall Plot tab goes here.")
    ),
    
    tabPanel("Boxplot Tab",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_sca", "Select SCT Condition:",
                             choices = unique(indiv_percentiles$sca_condition)),
                 
                 selectInput("domain", label = "Domain",
                             choices = c("Language and Motor", unique(indiv_percentiles$domain)), 
                             selected = "Language and Motor"),
                 
                 sliderInput("age", "Age Range:",
                             min = 0, max = 50, value = c(0, 50)),
                 
                 h4("Enter Milestone Data"),
                 textInput("user_milestone", "Milestone Name:"),
                 numericInput("user_age", "Age (months):", value = NA, min = 0, max = 60),
                 numericInput("user_percentile", "Percentile:", value = NA, min = 0, max = 100),
                 
                 actionButton("add_point", "Add Point")
               ),
               
               mainPanel(
                 plotlyOutput("indiv_perc", height = "700px")
               )
             )
    ),
    
    tabPanel("GAMLSS Growth Plots (Just Cog)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("growth_category", "Select Category:",
                             choices = c("Cognitive", "Receptive Language", "Expressive Language", "Fine Motor", "Gross Motor"), selected = "Cognitive"),
                 selectInput("growth_sct", "Select SCT Condition:",
                             choices = unique(indiv_percentiles$sca_condition)),
                 
                 numericInput("input_age", "Enter Age (months):", value = NA, min = 0, max = 60),
                 numericInput("input_score", "Enter Score:", value = NA),
                 actionButton("plot_point", "Plot Point")
               ),
               mainPanel(
                 plotOutput("cognitive_plot")
               )
             )
    ),
    
    tabPanel("Input Growth Tab",
             # Tab 4 content here - placeholder
             h3("Input Growth Tab"),
             p("Content for Input Growth tab goes here.")
    ),
    
    tabPanel("Method/References",
             # Tab 5 content here - placeholder
             h3("Method/References"),
             p("Content for Method/References tab goes here.")
    ),
    
    tabPanel("Meet the Team",
             # Tab 6 content here - placeholder
             h3("Meet the Team"),
             p("Content for Meet the Team tab goes here.")
    )
  )
)

server <- function(input, output, session) {
  # Reactive storage for user-input points
  user_points <- reactiveVal(data.frame(
    milestone = character(),
    Age = numeric(),
    Percentile = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Add user point when button is clicked
  observeEvent(input$add_point, {
    req(input$user_milestone, input$user_age, input$user_percentile)
    
    new_point <- data.frame(
      milestone = input$user_milestone,
      Age = input$user_age,
      Percentile = input$user_percentile,
      stringsAsFactors = FALSE
    )
    
    user_points(bind_rows(user_points(), new_point))
  })
  
  output$indiv_perc <- renderPlotly({
    data <- indiv_percentiles
    
    # Filter domain
    if (input$domain == "Motor") {
      data <- data %>% filter(domain == "Motor")
    } else if (input$domain == "Language") {
      data <- data %>% filter(domain == "Language")
    }
    
    # Filter by SCT and age range
    data <- data %>%
      filter(sca_condition == input$selected_sca) %>%
      group_by(milestone) %>%
      filter(min(Age, na.rm = TRUE) >= input$age[1]) %>%
      filter(max(Age, na.rm = TRUE) <= input$age[2]) %>%
      ungroup()
    
    # Sort milestones
    data <- data %>%
      mutate(milestone = as.character(milestone)) %>%
      mutate(milestone = fct_reorder(milestone, Age, .fun = median, .desc = FALSE))
    
    # Add user points
    user_data <- user_points()
    if (nrow(user_data) > 0) {
      user_data$milestone <- factor(user_data$milestone, levels = levels(data$milestone))
    }
    
    fill_color <- case_when(
      input$selected_sca == "XXY" ~ "khaki",
      input$selected_sca == "XYY" ~ "lightblue",
      input$selected_sca == "XXX" ~ "mediumorchid1",
      TRUE ~ "grey"
    )
    
    # Main boxplot
    p <- plot_ly(data,
                 y = ~milestone,
                 x = ~Percentile,
                 type = "box",
                 boxpoints = FALSE,
                 color = I(fill_color),
                 hoverinfo = "skip")
    
    # Add user points
    if (nrow(user_data) > 0) {
      p <- p %>% add_trace(data = user_data,
                           x = ~Percentile,
                           y = ~milestone,
                           type = "scatter",
                           mode = "markers",
                           marker = list(size = 15, color = 'black', symbol = 'star'),
                           text = ~paste("Age:", Age, "<br>Percentile:", Percentile),
                           hoverinfo = "text",
                           inherit = FALSE)
    }
    
    # Add percentile lines
    p <- p %>%
      layout(boxmode = "group",
             xaxis = list(title = "Achievement Percentile"),
             yaxis = list(title = ""),
             showlegend = FALSE,
             shapes = list(
               list(type = "line", x0 = 75, x1 = 75, y0 = 0, y1 = 1,
                    xref = "x", yref = "paper",
                    line = list(color = "orange", width = 2, dash = "dot")),
               list(type = "line", x0 = 90, x1 = 90, y0 = 0, y1 = 1,
                    xref = "x", yref = "paper",
                    line = list(color = "red", width = 2, dash = "dot"))
             ),
             annotations = list(
               list(x = 75, y = -0.03, xref = "x", yref = "paper",
                    text = "75", showarrow = FALSE, font = list(size = 12)),
               list(x = 90, y = -0.03, xref = "x", yref = "paper",
                    text = "90", showarrow = FALSE, font = list(size = 12))
             ))
    
    p
  })
  
  # Store user input point
  user_cog_point <- reactiveVal(NULL)
  
  observeEvent(input$plot_point, {
    req(input$input_age, input$input_score)
    user_cog_point(data.frame(age = input$input_age, score = input$input_score))
  })
  
  output$cognitive_plot <- renderPlot({
    # Reshape for plotting
    lms_long <- pivot_longer(
      lms_hgt,
      cols = -age,
      names_to = "Percentile",
      values_to = "Score"
    )
    
    p <- ggplot(lms_long, aes(x = age, y = Score, color = Percentile)) +
      geom_smooth(size = 1) +
      labs(
        title = "Growth Chart: Bayley-4 Cognitive Scores by Age (XXY)",
        x = "Age (months)",
        y = "Bayley-4 Cogniitve Score",
        color = "Percentile"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right"
      )
    
    # Add user point if exists
    if (!is.null(user_cog_point())) {
      p <- p + 
        geom_point(data = user_cog_point(), aes(x = age, y = score),
                   color = "black", size = 3, shape = 16) + # triangle shape
        geom_text(data = user_cog_point(), aes(x = age, y = score, label = ""),
                  vjust = -1, hjust = 0.5, color = "black")
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)
