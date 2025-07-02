
### Libraries 
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(plotly)
library(kableExtra)


### Datasets
milestones <- readRDS("Milestones.RDS")
genpop <- readRDS("GenPop_Milestones.RDS")
indiv_percentiles <- readRDS("Individual_Percentiles.RDS")


# Pull general population 90th percentile into the individual percentiles data
indiv_percentiles$norms_90th <- genpop$Q90[match(indiv_percentiles$milestone, genpop$milestone)]
# Format ID to display number and SCT
indiv_percentiles$study_id_extraordinary <- paste0(indiv_percentiles$study_id_extraordinary, " (", 
                                                   indiv_percentiles$sca_condition, ")")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Set theme
  theme = shinytheme("flatly"),
  
  # Application title
  fluidPage(
    fluidRow(
      column(10, 
             h1("eXtraordinarY Babies Study : SCT Developmental Milestones")  # Title on the left (adjust this as needed)
      ),
      column(2, 
             tags$img(src = "eBs_Logo.jpg", height = "100px", style = "float: right;")  # Image on the right
      )
    )
  ),
  
  
  # ID filter: only shown in Individual summaries
  conditionalPanel(
    condition = "input.tabs == 'Individual summaries'",
    fluidRow(
      # ID : dropdown
      column(4,
             selectInput("selected_id", "Select Individual ID",
                         choices = unique(indiv_percentiles$study_id_extraordinary))
      ),
      # Percentile : sliderbar
      column(4,
             sliderInput("percentile", "Percentile Range:",
                         min = 0, max = 100,
                         value = c(0, 100))
      ),
      # Data Points : Radio
      column(4,
             radioButtons("overlay", "Overlay All Data Points:",
                          choices = list("No", "Yes"), 
                          selected = "No")
      )
    )
  ),
  
  # SCT filter: only shown in Individual summaries
  conditionalPanel(
    condition = "input.tabs == 'SCT Comparisons'",
    fluidRow(
      # SCT : dropdown
      column(4,
             selectInput("sca_condition", label = "Select SCT",
                         choices = c("All SCTs", unique(milestones$sca_condition)), 
                         selected = "All SCTs")
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.tabs == 'SCT Comparisons' || input.tabs == 'Individual summaries'",
    fluidRow(
      # Domain : dropdown
      column(4,
             selectInput("domain", label = "Domain",
                         choices = c("Language and Motor", unique(indiv_percentiles$domain)), 
                         selected = "Language and Motor")
      ),
      # Age Range : dropdown
      column(4,
             sliderInput("age", "Age Range:",
                         min = 0, max = 50,
                         value = c(0, 50))
      )
    )
  ),
  
  
  # Tabs with ID to control visibility of filters
  tabsetPanel(id = "tabs",
              
              # Panel 1 of overview
              tabPanel("Milestones Overview",
                # Put plot on the left and text on the right using column command
                fluidRow(
                  column(
                    width = 6,
                    div(
                      style = "text-align: center;",
                      tags$img(src = "Milestones_fig.jpg", height = "800px")
                    )
                  ),
                  # Summary text formatting
                  column(
                    width = 6,
                    div(
                      style = "padding: 50px; margin-top: -70px;",
                      tags$h3("Understanding the Milestones", style = "text-align: center;"),
                      p("Sex Chromosome Trisomy (SCT) milestones were collected from our study cohort are compared with existing published norms. Normative milestone achievement data for the 25th, 50th, 75th, and 90th percentiles were taken from the Denver II Scales, the World Health Organization (WHO) Motor Development Study, and the Primitive Reflex Profile (PRP). We used the Denver II whenever possible (sitting, walking, running, jumping, cooing, babbling, single words, 2-word phrases). WHO was used for crawling and cruising and the PRP was used for rolling front to back and rolling back to front. Percentiles for the PRP were estimated theoretically under the assumption of a normal distribution from the provided mean and standard deviations."),
                      p("To evaluate if each SCT condition was significantly delayed compared to the general population, a simulated dataset of 1000 samples was generated based on the provided percentiles under the assumption of a non-normal distribution using the approxfun function in R."),
                      p("The current number of participants included for analysis are as follows : "),
                      uiOutput("counts"),
                      p("This figure supports the following paper :"),
                      p("Thompson T, Bothwell S, Janusz J, Wilson R, Howell S, Davis S, Swenson K, Martin S, Kowal K, Ikomi C, Despradel M, Ross J, Tartaglia N. Quantifying the Spectrum of Early Motor and Language Milestones in Sex Chromosome Trisomy. medRxiv [Preprint]. 2024 Aug 19:2024.08.16.24312065. doi: 10.1101/2024.08.16.24312065. PMID: 39228733; PMCID: PMC11370534.")
                    )
                  )
                )
              ),
              
              # Panel 2 : SCT specific data
              tabPanel("SCT Comparisons",
                       fluidRow(
                         column(6, plotlyOutput("scaPlot", height = "650px", width = "100%")),
                         column(6, div(
                           style = "height: 750px; overflow-y: auto; font-size: 20px;",
                           uiOutput("summary")
                         ))
                       ),
                       fluidRow(
                         column(12,
                                div(
                                  plotlyOutput("over90plot", height = "750px"),
                                  style = "padding-bottom: 30px;"
                                )
                         )
                       )
              ),
              
              # Panel 3 : Individual summary data
              tabPanel("Individual summaries",
                       fluidPage(
                         column(6, plotlyOutput("indiv", height = "650px", width = "100%")),
                         column(6, plotlyOutput("indiv_perc", height = "650px", width = "100%"))
                       )
              )
  )
)

# Define server logic
server <- function(input, output) {
  
    output$counts <- renderUI({
      
      tbl_counts <- data.frame(`Total` = length(unique(indiv_percentiles$study_id_extraordinary)),
                               `XXY` = length(unique(indiv_percentiles$study_id_extraordinary[indiv_percentiles$sca_condition == "XXY"])),
                               `XYY` = length(unique(indiv_percentiles$study_id_extraordinary[indiv_percentiles$sca_condition == "XYY"])),
                               `XXX` = length(unique(indiv_percentiles$study_id_extraordinary[indiv_percentiles$sca_condition == "XXX"])))
      
      HTML(
        kable(tbl_counts, "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
      )
      
    })

    output$scaPlot <- renderPlotly({
      
      #Subset domain
      if(input$domain == "Motor"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Motor")
      }
      
      if(input$domain == "Language"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Language")
      }
      
      # Filter to age range 
      indiv_percentiles <- indiv_percentiles %>% 
        group_by(milestone) %>% 
        filter(min(Age, na.rm = T) >= input$age[1]) %>% 
        filter(max(Age, na.rm = T) <= input$age[2]) 
        
      
      # Explicitly reorder the milestone factor by median Age, descending
      indiv_percentiles <- indiv_percentiles %>%
        mutate(milestone = as.character(milestone)) %>% 
        mutate(milestone = fct_reorder(milestone, Age, .fun = median, .desc = FALSE))
      
      # Identify outliers manually per group (you may need to adapt this to your data structure)
      outliers <- indiv_percentiles %>%
        group_by(sca_condition, milestone) %>%
        mutate(Q1 = quantile(Age, 0.25, na.rm = T),
               Q3 = quantile(Age, 0.75, na.rm = T),
               IQR = Q3 - Q1,
               is_outlier = Age < (Q1 - 1.5 * IQR) | Age > (Q3 + 1.5 * IQR)) %>%
        filter(is_outlier)
      
      
      # Calculate summary percentiles for each boxplot group
      box_summary <- indiv_percentiles %>%
        group_by(milestone, sca_condition) %>%
        summarise(
          p25 = quantile(Age, 0.25, na.rm = TRUE),
          p50 = quantile(Age, 0.50, na.rm = TRUE),
          p75 = quantile(Age, 0.75, na.rm = TRUE),
          p90 = quantile(Age, 0.90, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(hovertext = paste0(
          "25th percentile: ", round(p25, 1), " mo\n",
          "Median: ", round(p50, 1), " mo\n",
          "75th percentile: ", round(p75, 1), " mo\n",
          "90th percentile: ", round(p90, 1), " mo"
        ))
      
      indiv_percentiles <- indiv_percentiles %>%
        left_join(box_summary, by = c("milestone", "sca_condition"))
      

      # Subset data
      if(input$sca_condition == "All SCTs"){
        
        # Reorder sca_condition so that "XXY" comes first and "XXX" comes last
        indiv_percentiles$sca_condition <- fct_relevel(indiv_percentiles$sca_condition, "XXY", after = 0)
        indiv_percentiles$sca_condition <- fct_relevel(indiv_percentiles$sca_condition, "XXX", after = Inf)
        
        # Create a plotly boxplot
        p <- plot_ly(indiv_percentiles, 
                     y = ~fct_reorder(milestone, Age), 
                     x = ~Age, 
                     color = ~sca_condition,
                     colors = c("#fdb863", "cyan3", "#4B0082"),
                     type = "box", 
                     boxpoints = "outliers", 
                     text = ~paste("ID :", study_id_extraordinary, 
                                   "\nAge =", round(Age, 1), "Months", 
                                   "\nPercentile =", round(Percentile, 1)),
                     hoverinfo = "text") %>%
          layout(
            legend = list(
              x = 0.95,  # Position from the left
              y = 0.05,  # Position from the bottom
              xanchor = "right",  # Anchor legend to the right
              yanchor = "bottom",  # Anchor legend to the bottom
              traceorder = "reversed"  # Reverse the legend order
            )
          )
        
        # Show the plot
        p <- p %>%
          layout(boxmode = "group",
                 xaxis = list(title = "Age Milestone was Achieved (Months)",
                              tickfont = list(size = 14)),  # Custom x-axis label
                 yaxis = list(title = ""), 
                 font = list(size = 16))
        p
        
          
      }else{
        # pick fill color
        scafill = case_when(input$sca_condition == "XXY" ~ "#fdb863", 
                            input$sca_condition == "XYY" ~ "cyan3",
                            input$sca_condition == "XXX" ~ "#4B0082")
        
        # subset data
        sca_milestones <- indiv_percentiles %>% filter(sca_condition == input$sca_condition)
        sca_outliers <- outliers %>% filter(sca_condition == input$sca_condition)
        
        # Create a plotly boxplot
        p <- plot_ly(sca_milestones, 
                     y = ~fct_reorder(milestone, Age), 
                     x = ~Age, 
                     color = I(scafill),
                     type = "box", boxpoints = "outliers", 
                     text = ~paste("ID :", study_id_extraordinary, 
                                   "\nAge =", round(Age, 1), "Months", 
                                   "\nPercentile =", round(Percentile, 1)), 
                     hoverinfo = "text") %>%
          layout(
            xaxis = list(
              range = c(0, 50)  # Replace with your desired min and max values
            )
          )
        
        # Show the plot
        p <- p %>%
          layout(boxmode = "group",
                 xaxis = list(title = "Age Milestone was Achieved (Months)",
                              tickfont = list(size = 14)),  # Custom x-axis label
                 yaxis = list(title = ""), 
                 font = list(size = 16))
        p
      }

    })
    
    output$summary <- renderUI({
      #Subset domain
      if(input$domain == "Motor"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Motor")
      }
      
      if(input$domain == "Language"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Language")
      }
      
      # Filter to age range 
      indiv_percentiles <- indiv_percentiles %>% 
        group_by(milestone) %>% 
        filter(min(Age, na.rm = T) >= input$age[1]) %>% 
        filter(max(Age, na.rm = T) <= input$age[2])  
      
      
      # Explicitly reorder the milestone factor by median Age, descending
      indiv_percentiles <- indiv_percentiles %>%
        mutate(milestone = as.character(milestone)) %>% 
        mutate(milestone = fct_reorder(milestone, Age, .fun = median, .desc = FALSE))
      
      # Treat all SCAs differently 
      if(input$sca_condition == "All SCTs"){
        tbl <- indiv_percentiles %>% 
          filter(!is.na(Age)) %>% 
          group_by(milestone) %>% 
          summarise(`Total N` = n(), 
                    `25th` = round(quantile(Age, 0.25, na.rm = T), 1), 
                    `50th` = round(quantile(Age, 0.5, na.rm = T), 1),
                    `75th` = round(quantile(Age, 0.75, na.rm = T), 1),
                    `90th` = round(quantile(Age, 0.9, na.rm = T), 1)) %>% 
          rename(Milestone = milestone) %>% 
          arrange(desc(`75th`))
        
      }else{
        tbl <- indiv_percentiles %>% 
          filter(sca_condition == input$sca_condition) %>% 
          filter(!is.na(Age)) %>% 
          group_by(milestone) %>% 
          summarise(`Total N` = n(), 
                    `25th` = round(quantile(Age, 0.25, na.rm = T), 1), 
                    `50th` = round(quantile(Age, 0.5, na.rm = T), 1),
                    `75th` = round(quantile(Age, 0.75, na.rm = T), 1),
                    `90th` = round(quantile(Age, 0.9, na.rm = T), 1)) %>% 
          rename(Milestone = milestone) %>% 
          arrange(desc(`75th`))
      }
      
      HTML(
        kable(tbl, "html") %>%
          kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
      )
    })
    
    
    # Small multiples view
    output$over90plot <- renderPlotly({
      #Subset domain
      if(input$domain == "Motor"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Motor")
      }
      
      if(input$domain == "Language"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Language")
      }
      
      # Filter to age range 
      indiv_percentiles <- indiv_percentiles %>% 
        group_by(milestone) %>% 
        filter(min(Age, na.rm = T) >= input$age[1]) %>% 
        filter(max(Age, na.rm = T) <= input$age[2])  
      
      
      plot_dat = indiv_percentiles %>% filter(Age > norms_90th)
      
      if(input$sca_condition != "All SCTs"){
        plot_dat <- plot_dat %>% filter(sca_condition == input$sca_condition)
      }
      
      # pick fill color
      scafill = case_when(input$sca_condition == "All SCTs" ~ "darkseagreen3", 
                          input$sca_condition == "XXY" ~ "#fdb863", 
                          input$sca_condition == "XYY" ~ "cyan3",
                          input$sca_condition == "XXX" ~ "#4B0082")
      
      
      # Manually bin data & aggregate IDs for hover text
      df_binned <- plot_dat %>%
        group_by(milestone) %>% 
        mutate(bin_width = (max(Age) - min(Age))/10) %>% 
        mutate(bin = cut(Age, breaks = seq(floor(min(Age)), ceiling(max(Age)), by = unique(bin_width)), include.lowest = TRUE)) %>%
        ungroup() %>% 
        group_by(milestone, bin) %>%
        summarise(
          bin_width = bin_width,
          count = n(),
          ids = paste(study_id_extraordinary, collapse = "<br>"),  # Use <br> for line breaks between IDs
          bin_center = (as.numeric(sub("\\((.+),.*", "\\1", bin)) + 
                          as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", bin))) / 2,
          bin_center = ifelse(is.na(bin_center), 
                              (as.numeric(sub("\\[(.+),.*", "\\1", bin)) + 
                                 as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", bin))) / 2, bin_center)
        ) %>% 
        group_by(milestone, bin) %>% 
        slice(1) %>% 
        filter(!is.na(bin))
      
      # Create a full data frame with all bins (including empty ones) for each milestone
      all_bins <- plot_dat %>%
        group_by(milestone) %>%
        summarise(
          bins = list(seq(floor(min(Age)), ceiling(max(Age)), length.out = 11))  # 10 bins requires 11 breaks
        ) %>%
        ungroup() %>%
        unnest(cols = c(bins)) %>%
        mutate(
          bin = cut(bins, breaks = unique(seq(floor(min(plot_dat$Age)), ceiling(max(plot_dat$Age)), length.out = 11)), include.lowest = TRUE)  # Cut based on the actual range of bins
        )
      
      # Merge the original binned data with all possible bins (to include missing ones)
      df_binned$bin = as.character(df_binned$bin)
      all_bins$bin = as.character(all_bins$bin)
      df_binned_complete <- df_binned %>%
        full_join(all_bins, by = c("milestone", "bin")) %>%
        # Replace missing counts with 0
        replace_na(list(count = 0)) %>% 
        mutate(bin_center = (as.numeric(sub("\\((.+),.*", "\\1", bin)) + 
                               as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", bin))) / 2) %>% 
        group_by(milestone, bin) %>% slice(1) %>% ungroup() %>% 
        group_by(milestone) %>% mutate(mean = mean(bin_center, na.rm = T)) %>% ungroup()
      
      # Reverse the order of the facets by reversing the factor levels of 'milestone'
      df_binned$milestone <- factor(df_binned$milestone, levels = c("Jumping", "2 Word Phrases", "Running", "Walking",
                                                                    "First Words", "Cruising", "Crawling", "Babbling",
                                                                    "Sitting", "Rolling Back to Front", 
                                                                    "Rolling Front to Back", "Cooing"))
      
      
      # Create ggplot with facets
      genpop$placement = df_binned_complete$mean[match(genpop$milestone, df_binned_complete$milestone)]
      textplacement <- max(df_binned$count)
      p <- ggplot(df_binned, aes(x = bin_center, y = count, text = paste("IDs:", ids))) +
        geom_bar(stat = "identity", fill = scafill, color = "black", position = "dodge", 
                 aes(width = bin_width)) +
        geom_text(data = genpop, 
                  aes(x = placement, y = textplacement, label = paste0("GP 90th = ", round(Q90, 1))), 
                  inherit.aes = FALSE, size = 4) +
        facet_wrap(~ milestone, scales = "free_x") + theme_bw() + 
        ggtitle("Ages Over the General Population 90th Percentile") + 
        theme(plot.title = element_text(face="bold", size = 16, hjust = 0.5), 
              text = element_text(size = 14)) + ylim(0, textplacement+1) + 
        labs(x = "Age of Milestone Achievement (Months)", y = "Count")
      
      # Convert ggplot to plotly
      p_plotly <- ggplotly(p, tooltip = "text")
      
      # Display the plot
      p_plotly  %>%
        layout(
          margin = list(b = 50, l = 50) # to fully display the x and y axis labels
        )
      
    })
    
    ############ Individual data 
    output$indiv <- renderPlotly({
      
      #Subset domain
      if(input$domain == "Motor"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Motor")
      }
      
      if(input$domain == "Language"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Language")
      }
      
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
      
      if(input$overlay == "Yes"){
        p <- plot_ly(sca_milestones, 
                     y = ~fct_reorder(milestone, Age), 
                     x = ~Age, 
                     color = I(scafill),
                     type = "box", 
                     marker = list(size = 6, color = 'white', line = list(color = 'black', width = 1)),
                     boxpoints = "all",
                     jitter = 0.6,
                     pointpos = 0,
                     hoverinfo = "skip")
      }else{
        # Create a plotly boxplot
        p <- plot_ly(sca_milestones, 
                     y = ~fct_reorder(milestone, Age), 
                     x = ~Age, 
                     color = I(scafill),
                     type = "box", boxpoints = FALSE, 
                     hoverinfo = "skip")
      }
      
      # Overlay custom points from indiv_dat
      p <- p %>% add_trace(data = indiv_dat,
                  x = ~Age,
                  y = ~fct_reorder(milestone, Age),
                  type = "scatter",
                  mode = "markers",
                  marker = list(size = 15, color = 'red', symbol = 'x'),
                  text = ~paste("ID:", study_id_extraordinary,
                                "<br>Age:", round(Age, 1), "months",
                                "<br>Percentile:", round(Percentile, 1)),
                  hoverinfo = "text",
                  inherit = FALSE
        ) %>% 
        layout(
          xaxis = list(
            range = c(0, 50)  # Replace with your desired min and max values
          )
        )
      
      # Show the plot
      p <- p %>%
        layout(boxmode = "group",
               xaxis = list(title = "Age Milestone was Achieved (Months)",
                            tickfont = list(size = 14)),  # Custom x-axis label
               yaxis = list(title = ""), 
               font = list(size = 16), 
               showlegend = FALSE)
      p
      
    })
    
    
    output$indiv_perc <- renderPlotly({
      
      #Subset domain
      if(input$domain == "Motor"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Motor")
      }
      
      if(input$domain == "Language"){
        indiv_percentiles <- indiv_percentiles %>% filter(domain == "Language")
      }
      
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
      
      
      ordered_levels <- indiv_dat %>%
        group_by(milestone) %>%
        summarise(median_percentile = median(Percentile, na.rm = TRUE)) %>%
        arrange(median_percentile) %>%
        pull(milestone)
      
      sca_milestones$milestone <- factor(sca_milestones$milestone, levels = ordered_levels)
      indiv_dat$milestone <- factor(indiv_dat$milestone, levels = ordered_levels)
      
      sca_milestones <- sca_milestones %>%
        mutate(milestone_num = as.numeric(milestone),
               jittered_y = milestone_num + runif(n(), -0.2, 0.2))  # adjust jitter as needed
      
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
      }else{
        p <- plot_ly(sca_milestones, 
                     y = ~milestone, 
                     x = ~Percentile, 
                     color = I(scafill),
                     type = "box", 
                     boxpoints = FALSE,
                     hoverinfo = "skip")
      }
      
      p <- p %>% add_trace(data = indiv_dat,
                  x = ~Percentile,
                  y = ~milestone,
                  type = "scatter",
                  mode = "markers",
                  marker = list(size = 15, color = 'red', symbol = 'x'),
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
