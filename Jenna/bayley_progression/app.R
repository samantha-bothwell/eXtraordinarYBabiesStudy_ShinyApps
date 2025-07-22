library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Boxplot by Condition and Category"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_condition", "Choose Condition:",
                  choices = NULL),
      selectInput("selected_category", "Choose Category:",
                  choices = NULL),
      numericInput("input_score", "Input GSV Score:", value = NA, step = 1),
      selectInput("input_visit", "Input Visit Month:",
                  choices = NULL),
      actionButton("submit_point", "Enter")  # <-- The new button
    ),
    
    mainPanel(
      plotOutput("conditionBoxPlot", height = "600px")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # dropdowns
  observe({
    req(exists("GSV_copy"))
    
    updateSelectInput(session, "selected_condition",
                      choices = sort(unique(GSV_copy$sca_condition)))
    
    updateSelectInput(session, "selected_category",
                      choices = sort(unique(GSV_copy$category)))
    
    updateSelectInput(session, "input_visit",
                      choices = sort(unique(GSV_copy$redcap_event_name)))
  })
  
  # Only show point when user clicks "Enter"
  user_point <- eventReactive(input$submit_point, {
    req(input$input_score, input$input_visit)
    data.frame(
      score = input$input_score,
      redcap_event_name = input$input_visit
    )
  })
  
  # make filtered boxplot with red dot (only after button click)
  output$conditionBoxPlot <- renderPlot({
    req(input$selected_condition, input$selected_category)
    
    df <- GSV_copy[
      GSV_copy$sca_condition == input$selected_condition &
        GSV_copy$category == input$selected_category, ]
    
    p <- ggplot(df, aes(y = as.factor(redcap_event_name), x = score)) +
      geom_boxplot() +
      labs(
        x = "GSV Score",
        y = "Month Visit",
        title = paste("Scores by Visit for", input$selected_category, "in", input$selected_condition)
      )
    
    #overlay point
    if (!is.null(user_point())) {
      p <- p + geom_point(
        data = user_point(),
        aes(x = score, y = redcap_event_name),
        color = "red", size = 3
      )
    }
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)

