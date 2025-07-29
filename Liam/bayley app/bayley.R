# live laugh love Shiny app

library(plotly)
library(shiny)
library(tidyverse)
library(ggplot2)
library(gamlss)


### Datasets
milestones <- readRDS("Milestones.RDS")
genpop <- readRDS("GenPop_Milestones.RDS")
indiv_percentiles <- readRDS("Individual_Percentiles.RDS")

### Datasets Pt.2
composite <- readRDS("Bayley_Composite_scores.RDS")
GSV <- readRDS("Bayley_GSV_scores.RDS")
scaled <- readRDS("Bayley_Scaled_scores.RDS")

######### TO DO #####################
'
change milestones to have spaces (in sidebar/inputs)
# Add "in months" to top of sidebar
# select SCT dropdown
#then sets the ecdf based on that population -> data_in (needs to filter indiv_percentiles$sca_id == data_in) # check naming
#Compare to: use same choice from SCT dropdown

#Round percentile to 0 decimals, in function AND in datatable
#Remove symbols and colors from datatable
Switch months_toAchieved to Months to Achieved  
Increase text size and plot size overall
Make side bar smaller, if possible
'
######### TO DO #####################

# separates unique milestones
milestones_list <- c(unique(indiv_percentiles$milestone))

# Pull general population 90th percentile into the individual percentiles data
indiv_percentiles$norms_90th <- genpop$Q90[match(indiv_percentiles$milestone, genpop$milestone)]
# Format ID to display number and SCT
indiv_percentiles$study_id_extraordinary <- paste0(indiv_percentiles$study_id_extraordinary, " (", 
                                                   indiv_percentiles$sca_condition, ")")

# LOOP: pass in ONE milestone at a time, and calculate 
calc_percentile <- function(age_in, milestone_in){
  # set seed an initialize parameters
  set.seed(2024)
  percentiles <- c(0.25, 0.5, 0.75, 0.9)

  # Pull the gen pop quantile values from the dataset
  values = c(genpop$Q25[genpop$milestone == milestone_in],
             genpop$Q50[genpop$milestone == milestone_in],
             genpop$Q75[genpop$milestone == milestone_in],
             genpop$Q90[genpop$milestone == milestone_in])
  
  # Set up the the quantile function 
  quantile_function <- approxfun(percentiles, values, method = "linear", rule = 2)
  
  n_samples = 1000
  simulated_ages <- quantile_function(runif(n_samples))
  
  ecdf_func <- ecdf(simulated_ages)
  result_percentile <- round(100*ecdf_func(age_in))

  return(result_percentile)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Milestones Dynamic Plot"),
    
    sidebarLayout(
      sidebarPanel(width=2,
        selectInput("sca_condition", label = "Select SCT",
                    choices = c("All SCTs", unique(milestones$sca_condition)), 
                    selected = "All SCTs"),
        div(h3("Input Age Milestone was Achieved: (in months)"), 
           tagList(
             lapply(milestones_list, function(milestone) {
               numericInput(
                 inputId = paste0("AgeWhen_", gsub(" ","",milestone)), # be careful with spaces version!!!!
                 label= milestone,
                 min = 0, max = 48, value = NA, step = 1
               )
               
             }),
             actionButton("addPoints", "Add to Graph", class = "btn btn-success"),
             br(), br()
      ))),

        # Show a plot of the generated distribution
      mainPanel(width=10,
           fluidRow(
             column(12,plotlyOutput("indiv_perc")
             ),
             column(12,
                    DTOutput("user_table"))
           )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  input_milestones_data <- reactiveVal(data.frame(# creates reactive dataFrame that takes in the user inputs of milestone values
    milestone = character(),
    months_WhenAchieved = numeric(),
    Percentile = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Render user-submitted table
  output$user_table <- renderDT({
    
    display_data <- input_milestones_data() %>% 
      select(-Q25, -Q50, -Q75, -Q90, -color, -symbol) %>% # gets rid of percentiles in exportable plot
      rename('Age Milestone Achieved (months)' = months_WhenAchieved) %>%
      rename("Milestone" = milestone)# rename column
    
    datatable(display_data, extensions = "Buttons",
              options = list(pageLength = 12,
                             dom = 'Bfrtip',  # B = Buttons, f = filter, r = processing, t = table, i = info, p = pagination
                             buttons = list(
                               list(extend = 'csv', filename = 'Milestones'),#options to print
                               list(extend = 'pdf', filename = 'Milestones'),
                               list(extend = 'print', title = 'Milestones')),
                             lengthMenu = c(5, 10, 12)), 
              class = 'display'
    )
  })
  
  

    
   
  
      # Observe when user clicks "Add Milestone"
      observeEvent(input$addPoints, {
        new_rows <- lapply(milestones_list, function(milestone){ # creates new rows for each milestone
          input_id <- paste0("AgeWhen_", gsub(" ", "", milestone))
          valueWhenAchieved <- input[[input_id]] # inputs passed through
          if (!is.null(valueWhenAchieved) && !is.na(valueWhenAchieved)) { # if given something
            data.frame(
              milestone = milestone, # add milestone
              months_WhenAchieved = valueWhenAchieved, # add inputted value
              stringsAsFactors = FALSE
            )}
          })
    
    new_rows <- Filter(Negate(is.null), new_rows) # EXCLUDES milestones without input
    
    if (length(new_rows) > 0) {
      combined <- do.call(rbind, new_rows)
      combined <- combined %>% 
        left_join(genpop[, c("milestone", "Q25", "Q50", "Q75", "Q90")], by = "milestone") # adds Reference milestones
      combined <- combined %>% 
        rowwise() %>% 
        mutate(Percentile = calc_percentile(months_WhenAchieved, milestone)) %>%
        ungroup() %>% # fully no clue why I do this %>% 
      mutate(
        symbol = case_when( # adds different markers to plot based on percentile calculated
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
      showNotification("Please fill in at least one milestone before plotting", type = "error") # throws error just in case
    }
    
  })

  output$indiv_perc <- renderPlotly({
    
          # Filter to age range 
          indiv_percentiles <- indiv_percentiles %>% 
                              filter(!is.na(Age))
            
          if(input$sca_condition != "All SCTs") {
            indiv_percentiles <- indiv_percentiles %>% 
              filter(sca_condition == input$sca_condition)
            }
          
          
          # Explicitly reorder the milestone factor by median Age, descending
          indiv_percentiles <- indiv_percentiles %>%
            mutate(milestone = as.character(milestone)) %>% 
            mutate(milestone = fct_reorder(milestone, Age, .fun = median, .desc = FALSE))
          
          # to NOT mutate original data
          indiv_dat <- indiv_percentiles %>% filter(!is.na(Age))
          
          ### OLD APP ###
          # sanity check
          if (nrow(indiv_dat) == 0) {
            showNotification("No data available after filtering — nothing to plot.", type = "error")
            return(NULL)
          }
          
          sca = indiv_percentiles$sca_condition[1]
          sca_milestones <- indiv_percentiles
          
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
                         hoverinfo = "skip",
                         showlegend = F)
            
          # creates list of points to plot 
          user_points <- input_milestones_data()  
          if (nrow(user_points)>0){
            # Fixed trace: milestone overlay uses dynamic symbols/colors
            milestone_input_plot <- milestone_input_plot %>%
              add_trace(data = user_points,
                        x = ~Percentile,
                        y = ~milestone,
                        type = "scatter",
                        mode = "markers",
                        marker = list(size=20,
                        symbol = ~symbol,
                        color = ~color),
                        text = ~paste("Input Milestone<br> Age(months):", round(months_WhenAchieved, 1),
                                      "<br>Achievement Percentile:", round(Percentile, 1)),
                        hoverinfo = "text",
                        inherit = FALSE) 
        }
          
        
        
        # Show the plot
        milestone_input_plot <- milestone_input_plot %>%
          layout(xaxis = list(title = "Percentile", range = c(0, 100), titlefont = list(size=15)),
                 yaxis = list(title = " "),
                 title = list(text="Individual Milestones Achieved", font = list(size = 18)),
                 shapes = list(
                   list(type = "rect", fillcolor = "rgba(255, 0, 0, 0.2)", 
                        line = list(color = "red", width = 0), x0 = 90, x1 = 100, y0 = 0, y1 = 1, xref = "x", yref = "paper")
                   )
          )
          
        #milestone_input_plot
}) # closes milestone plot

} # close server
# Run the application 
shinyApp(ui = ui, server = server)