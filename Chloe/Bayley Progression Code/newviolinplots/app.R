library(shiny)
library(shinythemes)
library(tidyverse)
library(forcats)
library(ggpubr)

# Assuming these datasets are loaded in global environment or inside server
# composite_long_filtered, scaled_long_filtered, etc.

composite_long_filtered <- readRDS("/Users/craft/OneDrive/Documents/GitHub/eXtraordinarYBabiesStudy_ShinyApps/Chloe/Bayley Progression Code/Bayley_Composite_scores.rds") %>% 
  rename(
  Cognitive = 'bsid_cog_composite',
  Language = 'bsid_lang_composite',
  Motor = 'bsid_mot_composite') %>% 
  pivot_longer(cols=c('Cognitive', 'Language', 'Motor'), names_to = 'domain', values_to = 'score') %>% 
  filter(score < 777)


new_gsv_long_rem <- readRDS("/Users/craft/OneDrive/Documents/GitHub/eXtraordinarYBabiesStudy_ShinyApps/Chloe/Bayley Progression Code/Bayley_GSV_scores.rds") %>% 
  rename(
    Cognitive = 'bsid_gsv_cog',
    Receptive = 'bsid_gsv_rc',
    Expressive = 'bsid_gsv_ec',
    `Fine Motor` = 'bsid_gsv_fm',
    `Gross Motor` = 'bsid_gsv_gm'
  ) %>% 
  pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine Motor', 'Gross Motor'), names_to = 'domain', values_to = 'score') %>%
  filter(redcap_event_name != "2_month_visit_arm_1")

scaled_long_filtered <- readRDS("/Users/craft/OneDrive/Documents/GitHub/eXtraordinarYBabiesStudy_ShinyApps/Chloe/Bayley Progression Code/Bayley_Scaled_scores.rds") %>% 
  rename(
  Cognitive = "bsid_cog_scaled",
  Receptive = "bsid_rc_scaled",
  Expressive = "bsid_ec_scaled",
  Fine_Motor = "bsid_fm_scaled",
  Gross_Motor = "bsid_gm_scaled") %>% 
  pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine_Motor', 'Gross_Motor'), names_to = 'domain', values_to = 'score') %>% 
  filter(score < 777)
  

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  fluidRow(
    column(10, h1("eXtraordinarY Babies Study : SCT Developmental Milestones")),
    column(2, tags$img(src = "eBs_Logo.jpg", height = "100px", style = "float: right;"))
  ),
  
  selectInput("plot_choice", "Choose a Data Type:",
              choices = c("Composite", "Scaled", "GSV")),
  
  plotOutput("dynamic_violin_plot"),
  
  tags$p(
    "This plot provides an overview of bayley scores for the overall eXtrodinarY babies study at CU Anschutz. This study conducts clinical research on X&Y chromosome variations to track their influence on developmental milestones. These plots demonstrate the distribution of Bayley 4 scores, subsetted into SCA conditions, across age and domains. Boxplots overlayed on the plots demonstrate general population mean and standard deviations. For more information on the eXtraordinarY babies study and CU Anschutz research",
    tags$a(href = "https://medschool.cuanschutz.edu/pediatrics/sections/developmental-pediatrics/extraordinary-kids-program/our-research", "click here", target = "_blank"),
    "."))
  

server <- function(input, output, session) {
  
  output$dynamic_violin_plot <- renderPlot({
    
    if (input$plot_choice == "Composite") {
      
      comp_ref_mean <- 100
      comp_ref_sd <- 15
      ref_box_data_composite <- data.frame(
        group = unique(composite_long_filtered$sca_condition),
        ymin = comp_ref_mean - comp_ref_sd,
        lower = comp_ref_mean - comp_ref_sd / 2,
        middle = comp_ref_mean,
        upper = comp_ref_mean + comp_ref_sd / 2,
        ymax = comp_ref_mean + comp_ref_sd
      )
      
      ggplot(composite_long_filtered, aes(
        x = fct_reorder(sca_condition, score),
        y = score,
        fill = sca_condition
      )) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        geom_jitter(aes(color = bsid_age_calc), width = 0.2, height = 0.1, size = 2, alpha = 0.7) +
        geom_boxplot(
          data = ref_box_data_composite,
          aes(
            x = group,
            ymin = ymin,
            lower = lower,
            middle = middle,
            upper = upper,
            ymax = ymax
          ),
          stat = "identity",
          inherit.aes = FALSE,
          width = 0.2,
          fill = "orange",
          color = "black",
          alpha = 0.5
        ) +
        scale_color_gradient(low = "skyblue", high = "navy") +
        facet_wrap(~domain, ncol = 5) +
        labs(x = NULL, y = "Score", fill = "SCA Condition", color = "Age at Assessment") +
        theme_bw(base_size = 16) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )
      
    } else if (input$plot_choice == "Scaled") {
      
      scale_ref_mean <- 10
      scale_ref_sd <- 3
      ref_box_data_scaled <- data.frame(
        group = unique(scaled_long_filtered$sca_condition),
        ymin = scale_ref_mean - scale_ref_sd,
        lower = scale_ref_mean - scale_ref_sd / 2,
        middle = scale_ref_mean,
        upper = scale_ref_mean + scale_ref_sd / 2,
        ymax = scale_ref_mean + scale_ref_sd
      )
      
      ggplot(scaled_long_filtered, aes(
        x = fct_reorder(sca_condition, score),
        y = score,
        fill = sca_condition
      )) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        geom_jitter(aes(color = bsid_age_calc), width = 0.2, height = 0.1, size = 2, alpha = 0.7) +
        geom_boxplot(
          data = ref_box_data_scaled,
          aes(
            x = group,
            ymin = ymin,
            lower = lower,
            middle = middle,
            upper = upper,
            ymax = ymax
          ),
          stat = "identity",
          inherit.aes = FALSE,
          width = 0.2,
          fill = "orange",
          color = "black",
          alpha = 0.5
        ) +
        # scale_color_gradient(low = "skyblue", high = "navy") + # Optional: uncomment if desired
        facet_wrap(~domain, ncol = 5) +
        labs(x = NULL, y = "Score", fill = "SCA Condition", color = "Age at Assessment") +
        theme_bw(base_size = 16) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )
      
    } else if (input$plot_choice == "GSV") {
      
      gsv_ref_mean <- 500
      gsv_ref_sd <- 25
      
      ref_box_data_gsv <- data.frame(
        group = unique(new_gsv_long_rem$sca_condition),
        ymin = gsv_ref_mean - gsv_ref_sd,
        lower = gsv_ref_mean - gsv_ref_sd / 2,
        middle = gsv_ref_mean,
        upper = gsv_ref_mean + gsv_ref_sd / 2,
        ymax = gsv_ref_mean + gsv_ref_sd
      )
      
      ggplot(new_gsv_long_rem, aes(
        x = fct_reorder(sca_condition, score),
        y = score,
        fill = sca_condition
      )) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        geom_jitter(aes(color = bsid_age_calc), width = 0.2, height = 0.1, size = 2, alpha = 0.7) +
        geom_boxplot(
          data = ref_box_data_gsv,
          aes(
            x = group,
            ymin = ymin,
            lower = lower,
            middle = middle,
            upper = upper,
            ymax = ymax
          ),
          stat = "identity",
          inherit.aes = FALSE,
          width = 0.2,
          fill = "orange",
          color = "black",
          alpha = 0.5
        ) +
        scale_color_gradient(low = "skyblue", high = "navy") +
        facet_wrap(~domain, ncol = 5) +
        labs(x = NULL, y = "Score", fill = "SCA Condition", color = "Age at Assessment") +
        theme_bw(base_size = 16) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)
        )
      
    }
  }
  )
  
  
}

shinyApp(ui, server)