library(shiny)
library(shinythemes)
library(tidyverse)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title and logo
  fluidPage(
    fluidRow(
      column(10, h1("eXtraordinarY Babies Study : SCT Developmental Milestones")),
      column(2, tags$img(src = "eBs_Logo.jpg", height = "100px", style = "float: right;"))
    )
  ),

  
  # Tabset with two tabs
  tabsetPanel(id = "tabs",
              tabPanel("Overview", value = "score_overview",
                       conditionalPanel(
                         condition = "input.tabs == 'score_overview'",
                         titlePanel("Score Overviews"),
                         
                         # Dropdown to select image
                         selectInput("image_choice", "Choose a Data Type:",
                                     choices = c("Composite", "Scaled", "GSV")),
                         
                         # Add this line to display the image and paragraph UI from server
                         uiOutput("image_display")
                       )
              ),
              tabPanel("Tab 2", value = "tab2",
                       h3("This is another tab"))
  )
)

server <- function(input, output, session) {
  
  output$image_display <- renderUI({
    img_file <- case_when(
      input$image_choice == "Composite" ~ "comp_plot_violin.png",
      input$image_choice == "Scaled" ~ "violin_plot_scaled.png", 
      input$image_choice == "GSV" ~ "violin_plot_gsv.png"
    )
    
    fluidRow(
      column(
        width = 6,
        tags$img(src = img_file, height = "400px", width = "100%")
      ),
      column(
        width = 6,
        tags$p(
          "This plot shows developmental milestone scores for the selected data type. 
        It is designed to help visualize patterns across groups in the eXtraordinarY Babies Study. 
        Use the dropdown to explore different scoring types."
        )
      )
    )
  })
}

shinyApp(ui, server)
