#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

### Datasets
milestones <- readRDS("Milestones.RDS")
genpop <- readRDS("GenPop_Milestones.RDS")
indiv_percentiles <- readRDS("Individual_Percentiles.RDS")

## Datasets Pt.2
composite <- readRDS("Bayley_Composite_scores.RDS")
GSV <- readRDS("Bayley_GSV_scores.RDS")
scaled <- readRDS("Bayley_Scaled_scores.RDS")


# Pull general population 90th percentile into the individual percentiles data
indiv_percentiles$norms_90th <- genpop$Q90[match(indiv_percentiles$milestone, genpop$milestone)]
# Format ID to display number and SCT
indiv_percentiles$study_id_extraordinary <- paste0(indiv_percentiles$study_id_extraordinary, " (", 
                                                   indiv_percentiles$sca_condition, ")")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Testin 1,2,3"),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("indiv_perc")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$indiv_perc <- renderPlotly({
    
    # Filter to age range 
    indiv_percentiles <- indiv_percentiles %>% 
      group_by(milestone) %>% 
      filter(min(Age, na.rm = T) >= input$age[1]) %>% 
      filter(max(Age, na.rm = T) <= input$age[2]) 
    
    
    # Explicitly reorder the milestone factor by median Age, descending
    indiv_percentiles <- indiv_percentiles %>%
      mutate(milestone = as.character(milestone)) %>% 
      mutate(milestone = fct_reorder(milestone, Age, .fun = median, .desc = FALSE))
    
    
    indiv_dat <- indiv_percentiles %>% filter(study_id_extraordinary == input$selected_id) %>% filter(!is.na(Age))
    sca = indiv_dat$sca_condition[1]
    sca_milestones <- indiv_percentiles %>% filter(sca_condition == sca)
    
    # Filter to percentile range 
    indiv_dat <- indiv_dat %>% 
      group_by(milestone) %>% 
      filter(min(Percentile >= input$percentile[1], na.rm = T) & max(Percentile <= input$percentile[2], na.rm = T))
    sca_milestones <- sca_milestones %>% filter(milestone %in% indiv_dat$milestone)
    
    
    # pick fill color
    scafill = case_when(sca == "XXY" ~ "#fdb863", 
                        sca == "XYY" ~ "cyan3",
                        sca == "XXX" ~ "#4B0082")
    
    
    # Sort individual data by percentile
    ordered_levels <- indiv_dat %>%
      group_by(milestone) %>%
      summarise(median_percentile = median(Percentile, na.rm = TRUE)) %>%
      arrange(median_percentile) %>%
      pull(milestone)
    
    # makes sure datasets are ordered  by percentiles
    sca_milestones$milestone <- factor(sca_milestones$milestone, levels = ordered_levels)
    indiv_dat$milestone <- factor(indiv_dat$milestone, levels = ordered_levels)
    
    #Assigns symbol and color per percentile
    indiv_dat <- indiv_dat %>%
      mutate(
        symbol = case_when(
          Percentile < 75 ~ "check",
          Percentile < 90 ~ "question",
          TRUE ~ "x"
        ),
        color = case_when(
          Percentile < 75 ~ "green",
          Percentile < 90 ~ "orange",
          TRUE ~ "red"
        )
      )
    
    # Overlay all points for a better comparison
    if(input$overlay == "Yes"){
      p <- plot_ly(sca_milestones, 
                   y = ~milestone, 
                   x = ~Percentile, 
                   color = I(scafill),
                   type = "box", 
                   marker = list(size = 6, color = 'white', line = list(color = 'black', width = 1)),
                   boxpoints = "all",
                   jitter = 0.6,
                   pointpos = 0,
                   hoverinfo = "skip")
      
      # plots without scatter
    }else{
      p <- plot_ly(sca_milestones, 
                   y = ~milestone, 
                   x = ~Percentile, 
                   color = I(scafill),
                   type = "box", 
                   boxpoints = FALSE,
                   hoverinfo = "skip")
    }
    
    # Fixed trace: milestone overlay uses dynamic symbols/colors
    p <- p %>% add_trace(data = indiv_dat,
                         x = ~Percentile,
                         y = ~milestone,
                         type = "scatter",
                         mode = "markers",
                         marker = list(
                           size = 15,
                           symbol = ~symbol,
                           color = ~color
                         ),
                         text = ~paste("ID:", study_id_extraordinary,
                                       "<br>Age:", round(Age, 1), "months",
                                       "<br>Percentile:", round(Percentile, 1)),
                         hoverinfo = "text",
                         inherit = FALSE) %>% 
      layout(xaxis = list(range = c(0, 100)),yaxis = list(title = ""),
             shapes = list(
               list(type = "rect", fillcolor = "rgba(255, 0, 0, 0.2)", 
                    line = list(color = "red", width = 0), x0 = 90, x1 = 100, y0 = 0, y1 = 1, xref = "x", yref = "paper")
             )
      )
    
    # Show the plot
    p <- p %>%
      layout(boxmode = "group",
             xaxis = list(title = "Achievement Percentile",
                          tickfont = list(size = 14)),  # Custom x-axis label
             yaxis = list(title = ""), 
             font = list(size = 16), 
             showlegend = FALSE)
    p
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
