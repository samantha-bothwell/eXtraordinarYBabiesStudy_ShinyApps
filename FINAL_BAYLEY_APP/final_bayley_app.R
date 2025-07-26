###################################################################################################
'
eXtraordinarY Babies Study - FINAL Bayley App
University of Colorado - Anschutz Medical Campus


App Created By:  Chloe Child, Liam Hallinan,  Jenna Jimenez, Madeline Murphy, Jack Pressier
Project Mentors/PIs: Samantha Bothwell, Samantha Roberts

final_bayley_app.R: this file displays relevant plots, and allows for inputs of Bayley scores and milestones 
                    for clinical use in the eXtraordinarY Babies Study.

Last Updated: 26 July 2025
'
###################################################################################################


###### importing libraries for processing/plotting #####
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(plotly)
library(kableExtra)
library(stringr)
library(DT)
library(gamlss)

####### importing databases for plotting #####

# Milestones data
milestones <- readRDS("Milestones.RDS")
genpop <- readRDS("GenPop_Milestones.RDS")
indiv_percentiles <- readRDS("Individual_Percentiles.RDS")

# Bayley scores data
composite <- readRDS("Bayley_Composite_scores.RDS")
GSV <- readRDS("Bayley_GSV_scores.RDS")
scaled <- readRDS("Bayley_Scaled_scores.RDS")


##### Global Code: processing of datasets for plotting #####

# Pull general population 90th percentile into the individual percentiles data
indiv_percentiles$norms_90th <- genpop$Q90[match(indiv_percentiles$milestone, genpop$milestone)]

# Format ID to display number and SCT
indiv_percentiles$study_id_extraordinary <- paste0(indiv_percentiles$study_id_extraordinary, " (", 
                                                   indiv_percentiles$sca_condition, ")")
# pulls unique milestones for input/plot
milestones_list <- c(unique(indiv_percentiles$milestone))

############################## TO DO: Update to Mean and SD ################################ 
calculate_percentile <- function(age, percentiles) {
  # percentiles must be named vector with Q25, Q50, Q75, Q90
  known_percentiles <- c(25, 50, 75, 90)
  known_ages <- unname(percentiles[c("Q25", "Q50", "Q75", "Q90")])
  
  if (is.na(age) || any(is.na(known_ages))) return(NA_real_)
  if (age <= known_ages[1]) return(10)        # Extrapolate left
  if (age >= known_ages[4]) return(95)        # Extrapolate right
  
  approx(x = known_ages, y = known_percentiles, xout = age)$y
}
############################## End TO DO #################################################### 


##### Defining UI #####
ui <- fluidPage(
  
  # Set theme of app
  theme = shinytheme("flatly"),
  
  # Title of Application
  fluidPage(
    fluidRow(
      column(10, 
             h1("eXtraordinarY Babies Study : SCT Developmental Milestones")  # Title on the left
      ),
      column(2, 
             tags$img(src = "eBs_Logo.jpg", height = "100px", style = "float: right;")  # Image on the right
      )
    ) # end titlePage section
  ),
  
  # Conditional panel for carrying across tabs
  
  # Tabset Panel that defines each of the tabs in use
  tabsetPanel(id = "tabs",
              
              # Tab 1: Welcome to the App/Overview Plot of Scaled/Composite/GSV of Study
              tabPanel("Overview Plots"),
              
              # Tab 2: GAMLSS Growth Plots, based on existing data (static images)
              tabPanel("GAMLSS Growth Plots"),
              
              # Tab 3: Allows inputs of milestone data, and plots over the general population boxplot
              tabPanel("Input Milestones",
                       sidebarLayout(
                         sidebarPanel(
                           div(h3("Input Milestones Below:"), 
                               tagList(
                                 lapply(milestones_list, function(milestone) {
                                   numericInput(
                                     inputId = paste0("AgeWhen_", gsub(" ","",milestone)), # be careful with spaces !!!!
                                     label= gsub(" ","",milestone),
                                     min = 0, max = 48, value = NA, step = 1
                                   )
                                   
                                 }),
                                 actionButton("addPoints", "Add to Graph", class = "btn btn-success"),
                                 br(), br()
                               ))),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           fluidRow(
                             column(12,plotlyOutput("milestones_barplot")
                             ),
                             column(12,
                                    DTOutput("milestones_table_output"))
                                    )
                                  )
                                  )
                       ), # end tab3 
              
              # Tab 4: GSV Scores Input, with Reactive Data Frame and Plot over study population percentile curves
              tabPanel("Input GSV Scores"),
              
              # Tab 5: Background Information and References
              tabPanel("Background/References"),
              
              # Tab 6: Meet the Team
              tabPanel("Meet the Team")
              
              ) # end tabsetPanel

) # end UI

##### Defining Server logic #####
server <- function(input, output) {
  
  ### For Tab 1 ###
  
  ### For Tab 2 ###
  
  ### For Tab 3 ###
  input_milestones_data <- reactiveVal(data.frame(# creates reactive dataFrame that takes in the user inputs of milestone values
    milestone = character(),
    months_WhenAchieved = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # creates milestones table based on user input, which can be exported
  output$milestones_table_output <- renderDT({
    
    display_data <- input_milestones_data() %>% select(-Q25, -Q50, -Q75, -Q90) # cleans up data for exportable plot
    
    datatable(display_data, extensions = "Buttons",
              options = list(pageLength = 12,
                             dom = 'Bfrtip',
                             buttons = list(
                               list(extend = 'csv', filename = 'Milestones'),# options to print
                               list(extend = 'pdf', filename = 'Milestones'),
                               list(extend = 'print', title = 'Milestones')),
                             lengthMenu = c(5, 10, 12)), 
              class = 'display'
    )
  })
  
  # When "Add Points" button is clicked, calculates percentiles and categorizes by color/symbol
  observeEvent(input$addPoints, {
    existing_milestones <- input_milestones_data()$milestone # adds new milestones
    
    new_rows <- lapply(milestones_list, function(milestone){ # creates new rows for each milestone
      input_id <- paste0("AgeWhen_", gsub(" ", "", milestone))
      valueWhenAchieved <- input[[input_id]] # inputs passed through
      
      if (!is.null(valueWhenAchieved) && !is.na(valueWhenAchieved) && !(milestone %in% existing_milestones)) { # if given something new
        data.frame(
          milestone = milestone, # add milestone
          months_WhenAchieved = valueWhenAchieved, # add inputted value
          stringsAsFactors = FALSE
        )}
    })
    
    new_rows <- Filter(Negate(is.null), new_rows) # EXCLUDES milestones without input
    if (length(new_rows) > 0) {
      combined <- do.call(rbind, new_rows)
      combined <- combined %>% left_join(genpop[, c("milestone", "Q25", "Q50", "Q75", "Q90")], by = "milestone") # adds Reference milestones
      combined <- combined %>% rowwise() %>% mutate(
        ############################################################ FIX THIS WHEN CALCULATE_PERCENTILE IS UPDATED ##################################################################
        Percentile = calculate_percentile(months_WhenAchieved, c(Q25 = Q25, Q50 = Q50, Q75 = Q75, Q90 = Q90)) # adds calculated percentile to data table, and eventually, plot
        ########################################################### #####################################################################
      ) %>% ungroup() # not entirely sure why this happens
      
      combined <- combined %>% mutate( # adds different markers to plot based on percentile calculated
        symbol = case_when(
          Percentile < 75 ~ "circle",
          Percentile < 90 ~ "diamond",
          TRUE ~ "x"
        ),
        color = case_when(
          Percentile < 75 ~ "green",
          Percentile < 90 ~ "orange",
          TRUE ~ "red"
        )
      )
      updated_data <- bind_rows(input_milestones_data(), combined) # adds everything together
      input_milestones_data(updated_data)
    } else{
      showNotification("Error: No new milestones input, or milestone has already been plotted.", type = "error") # sanity check
    }
    
    # Clears input boxes
    lapply(milestones_list, function(milestone) {
      input_id <- paste0("AgeWhen_", gsub(" ", "", milestone))
      updateNumericInput(inputId = input_id, value = NA)
      
        })
      }) # end observeEvent
  
  # plots the inputted milestones on top of the existing boxplot
  output$milestones_barplot <- renderPlotly({
    
    # Filter to age range 
    indiv_percentiles <- indiv_percentiles %>% filter(!is.na(Age))
    
    
    # Explicitly reorder the milestone factor by median Age, descending
    indiv_percentiles <- indiv_percentiles %>%
      mutate(milestone = as.character(milestone)) %>% 
      mutate(milestone = fct_reorder(milestone, Age, .fun = median, .desc = FALSE))
    
    
    indiv_dat <- indiv_percentiles %>% filter(!is.na(Age))
    # sanity check
    if (nrow(indiv_dat) == 0) {
      showNotification("No data available after filtering — nothing to plot.", type = "error")
      return(NULL)
    }
    
    sca = indiv_dat$sca_condition[1]
    sca_milestones <- indiv_percentiles #%>% filter(sca_condition == sca)
    
    # Sort individual data by percentile
    ordered_levels <- indiv_dat %>%
      group_by(milestone) %>%
      summarise(median_percentile = median(Percentile, na.rm = TRUE)) %>%
      arrange(median_percentile) %>%
      pull(milestone)
    
    # makes sure datasets are ordered  by percentiles
    sca_milestones$milestone <- factor(sca_milestones$milestone, levels = ordered_levels)
    indiv_dat$milestone <- factor(indiv_dat$milestone, levels = ordered_levels)
    
    milestone_input_plot <- plot_ly(sca_milestones, 
                 y = ~milestone, 
                 x = ~Percentile, 
                 color = I("lightblue"),
                 type = "box", 
                 boxpoints = FALSE,
                 hoverinfo = "skip")
    
    # creates list of points to plot 
    user_points <- input_milestones_data()  
    if(nrow(user_points)>0){
      # Fixed trace: milestone overlay uses dynamic symbols/colors
      milestone_input_plot <- milestone_input_plot %>% add_trace(data = user_points,
                           x = ~Percentile,
                           y = ~milestone,
                           type = "scatter",
                           mode = "markers",
                           marker = list(
                             size = 15,
                             symbol = ~symbol,
                             color = ~color
                           ),
                           text = ~paste("Input Milestone<br> Age(months):", round(months_WhenAchieved, 1),
                                         "<br>Percentile:", round(Percentile, 1)),
                           hoverinfo = "text",
                           inherit = FALSE) 
    }
    
    # Show the plot
    milestone_input_plot <- milestone_input_plot %>%
      layout(xaxis = list(title = "Achievement Percentile",
                          tickfont = list(size = 14)),  # Custom x-axis label
             yaxis = list(title = ""), 
             font = list(size = 16), 
             showlegend = FALSE)
    #milestone_input_plot
    
  })
  
  
  ### For Tab 4 ###
  
  ### For Tab 5 ###
  
  ### For Tab 6 ###
  
} # end server


##### Run the application #####
shinyApp(ui = ui, server = server)