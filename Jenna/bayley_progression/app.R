library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Boxplot by sca_condition and category"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_condition", "Choose Condition:",
                  choices = NULL),
      selectInput("selected_category", "Choose Category:",
                  choices = NULL),
      numericInput("input_score", "Input Score:", value = NA, step = 1),
      selectInput("input_visit", "Input Visit (Month):",
                  choices = NULL)
    ),
    
    mainPanel(
      plotOutput("conditionBoxPlot", height = "600px")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Update dropdowns based on data
  observe({
    req(exists("GSV_copy"))
    
    updateSelectInput(session, "selected_condition",
                      choices = sort(unique(GSV_copy$sca_condition)))
    
    updateSelectInput(session, "selected_category",
                      choices = sort(unique(GSV_copy$category)))
    
    updateSelectInput(session, "input_visit",
                      choices = sort(unique(GSV_copy$redcap_event_name)))
  })
  
  # Render filtered boxplot + user point
  output$conditionBoxPlot <- renderPlot({
    req(input$selected_condition, input$selected_category)
    
    df <- GSV_copy[
      GSV_copy$sca_condition == input$selected_condition &
        GSV_copy$category == input$selected_category, ]
    
    ggplot(df, aes(y = as.factor(redcap_event_name), x = score)) +
      geom_boxplot() +
      # Add user input point if both are filled in
      {
        if (!is.na(input$input_score) && input$input_visit %in% df$redcap_event_name) {
          geom_point(data = data.frame(
            score = input$input_score,
            redcap_event_name = input$input_visit
          ), aes(x = score, y = redcap_event_name),
          color = "red", size = 3)
        } else {
          NULL
        }
      } +
      labs(
        x = "GSV Score",
        y = "Month Visit",
        title = paste("Scores by Visit for", input$selected_category, "in", input$selected_condition)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

