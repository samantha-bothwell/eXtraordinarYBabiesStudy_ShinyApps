library(shiny)
library(shinythemes)
library(tidyverse)
library(forcats)
library(ggpubr)
library(ggnewscale)
library(patchwork)
library(cowplot)

# Assuming these datasets are loaded in global environment or inside server
# composite_long_filtered, scaled_long_filtered, etc.

composite_long_filtered <- readRDS("/Users/craft/OneDrive/Documents/GitHub/eXtraordinarYBabiesStudy_ShinyApps/Chloe/Bayley Progression Code/Bayley_Composite_scores.rds") %>% 
  rename(
  Cognitive = 'bsid_cog_composite',
  Language = 'bsid_lang_composite',
  Motor = 'bsid_mot_composite') %>% 
  pivot_longer(cols=c('Cognitive', 'Language', 'Motor'), names_to = 'domain', values_to = 'score') %>% 
  filter(score < 777) %>% 
  mutate(
    domain = factor(domain, levels = c("Cognitive", "Language", "Motor"))
  )


new_gsv_long_rem <- readRDS("/Users/craft/OneDrive/Documents/GitHub/eXtraordinarYBabiesStudy_ShinyApps/Chloe/Bayley Progression Code/Bayley_GSV_scores.rds") %>% 
  rename(
    Cognitive = 'bsid_gsv_cog',
    Receptive = 'bsid_gsv_rc',
    Expressive = 'bsid_gsv_ec',
    `Fine Motor` = 'bsid_gsv_fm',
    `Gross Motor` = 'bsid_gsv_gm'
  ) %>% 
  pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine Motor', 'Gross Motor'), names_to = 'domain', values_to = 'score') %>%
  filter(redcap_event_name != "2_month_visit_arm_1") %>%
  mutate(domain = factor(domain, levels = c(
    "Cognitive", "Receptive", "Expressive", "Fine Motor", "Gross Motor"
  ))
  )


scaled_long_filtered <- readRDS("/Users/craft/OneDrive/Documents/GitHub/eXtraordinarYBabiesStudy_ShinyApps/Chloe/Bayley Progression Code/Bayley_Scaled_scores.rds") %>% 
  rename(
  Cognitive = "bsid_cog_scaled",
  Receptive = "bsid_rc_scaled",
  Expressive = "bsid_ec_scaled",
  `Fine Motor` = "bsid_fm_scaled",
  `Gross Motor` = "bsid_gm_scaled") %>% 
  pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine Motor', 'Gross Motor'), names_to = 'domain', values_to = 'score') %>% 
  filter(score < 777) %>%
  mutate(domain = factor(domain, levels = c(
    "Cognitive", "Receptive", "Expressive", "Fine Motor", "Gross Motor"
  ))
  )

ref_line_types <- c("Pop Mean" = "solid", "Pop IQR" = "dashed")
  

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  fluidRow(
    column(10, h1("eXtraordinarY Babies Study : SCT Developmental Milestones")),
    column(2, tags$img(src = "eBs_Logo.jpg", height = "100px", style = "float: right;"))
  ),
  
  fluidRow(
    column(4,
           selectInput("plot_choice", "Choose a Data Type:",
              choices = c("Composite", "Scaled", "GSV"))),
  column(4,
         radioButtons("show_reference", "Show Population Reference Lines:",
                      choices = list("No", "Yes"),
                      selected = "No",
                      inline = TRUE)),
  column(4,
         radioButtons("overlay", "Overlay All Data Points:",
                      choices = list("No", "Yes"), 
                      selected = "No",
                      inline = TRUE))),
  
  plotOutput("dynamic_violin_plot", height = "1000px", width = "75%")) 
  
 # tags$p(
    # "This plot provides an overview of bayley scores for the overall eXtrodinarY babies study at CU Anschutz. This study conducts clinical research on X&Y chromosome variations to track their influence on developmental milestones. These plots demonstrate the distribution of Bayley 4 scores, subsetted into SCA conditions, across age and domains. Boxplots overlayed on the plots demonstrate general population mean and standard deviations. For more information on the eXtraordinarY babies study and CU Anschutz research",
    # tags$a(href = "https://medschool.cuanschutz.edu/pediatrics/sections/developmental-pediatrics/extraordinary-kids-program/our-research", "click here", target = "_blank"),
    # "."))


  
server <- function(input, output, session) {
  
  output$dynamic_violin_plot <- renderPlot({
    
    plot_base <- function(data) {
      ggplot(data, aes(x = sca_condition, y = score, fill = sca_condition)) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        scale_fill_manual(name = "SCA Condition", values = c("XXY" = "#fdb863", "XYY" = "cyan3", "XXX" = "#4B0082")) +
        ggnewscale::new_scale_fill() +
        labs(x = NULL, y = "Score") +
        theme_bw(base_size = 18) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.box = "vertical",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15),
          legend.box.spacing = unit(0.2, "cm"),
          legend.margin = margin(t = 10, r = 20, b = 10, l = 20)
        ) + 
        guides(
          fill = guide_legend(order = 1, title.position = "top"),
          linetype = guide_legend(order = 2, title.position = "top")
        ) 
    }
    
    # Ordered factor levels for each dataset
    ordered_groups_comp <- composite_long_filtered %>%
      mutate(sca_condition = fct_reorder(sca_condition, score)) %>%
      pull(sca_condition) %>%
      unique() %>%
      as.character()
    
    ordered_groups_scale <- scaled_long_filtered %>%
      mutate(sca_condition = fct_reorder(sca_condition, score)) %>%
      pull(sca_condition) %>%
      unique() %>%
      as.character()
    
    ordered_groups_gsv <- new_gsv_long_rem %>%
      mutate(sca_condition = fct_reorder(sca_condition, score)) %>%
      pull(sca_condition) %>%
      unique() %>%
      as.character()
    
    # x_maps
    x_map_comp <- data.frame(
      group = ordered_groups_comp,
      x_numeric = seq_along(ordered_groups_comp)
    )
    
    x_map_scale <- data.frame(
      group = ordered_groups_scale,
      x_numeric = seq_along(ordered_groups_scale)
    )
    
    x_map_gsv <- data.frame(
      group = ordered_groups_gsv,
      x_numeric = seq_along(ordered_groups_gsv)
    )
    
    if (input$plot_choice == "Composite") {
      
      comp_ref_mean <- 100
      comp_ref_sd <- 15
      
      ref_box_data_composite <- x_map_comp %>%
        mutate(
          lower = comp_ref_mean - comp_ref_sd / 2,
          middle = comp_ref_mean,
          upper = comp_ref_mean + comp_ref_sd / 2
        )
      
      p1 <- composite_long_filtered %>%
        filter(domain == "Cognitive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_comp)) %>%
        plot_base() +
        labs(title = "Cognitive")
      
      p2 <- composite_long_filtered %>%
        filter(domain == "Language") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_comp)) %>%
        plot_base() +
        labs(title = "Language") +
        theme(legend.position = "none")
      
      p3 <- composite_long_filtered %>%
        filter(domain == "Motor") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_comp)) %>%
        plot_base() +
        labs(title = "Motor") +
        theme(legend.position = "none")
      
      if (input$show_reference == "Yes") {
        ref_geoms <- list(
          geom_segment(data = ref_box_data_composite,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = middle, yend = middle,
                           linetype = "Pop Mean"),
                       inherit.aes = FALSE, color = "black", size = .75),
          
          geom_segment(data = ref_box_data_composite,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = lower, yend = lower,
                           linetype = "Pop IQR"),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          geom_segment(data = ref_box_data_composite,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = upper, yend = upper,
                           linetype = "Pop IQR"),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          scale_linetype_manual(
            name = paste0("Reference Lines (Mean = ", comp_ref_mean, ")"),
            values = ref_line_types
          )
        )
        p1 <- p1 + ref_geoms
        p2 <- p2 + ref_geoms
        p3 <- p3 + ref_geoms
      }
      
      if (input$overlay == "Yes") {
        overlay_geom <- list(
          geom_jitter(aes(fill = as.numeric(bsid_age_calc)),
                      shape = 21, 
                      color = "black", 
                      size = 2, 
                      alpha = 0.5),
          scale_fill_gradient(
            name = "Age at Assessment (months)",
            low = "white",
            high = "black"
          )
        )
        
        p1 <- p1 + ggnewscale::new_scale_fill() + overlay_geom
        p2 <- p2 + ggnewscale::new_scale_fill() + overlay_geom
        p3 <- p3 + ggnewscale::new_scale_fill() + overlay_geom
      }
      
      
      
      # Extract legend from p1
      legend <- cowplot::get_legend(p1)
      
      # Remove legend from p1 plot itself to avoid duplication
      p1 <- p1 + theme(legend.position = "none")
      
      top_row <- cowplot::plot_grid(p1, legend, ncol = 2, rel_widths = c(1, 0.25))


      bottom_row <- cowplot::plot_grid(p2, p3, ncol = 2)
      
      final_plot <- cowplot::plot_grid(
        top_row,            # p1 + legend
        bottom_row,         # p2 + p3
        ncol = 1,
        rel_heights = c(1, 1, 1)  # Make all rows equal height
      )
      
      print(final_plot)
      
    } else if (input$plot_choice == "Scaled") {
      
      scale_ref_mean <- 10
      scale_ref_sd <- 3
      
      ref_box_data_scaled <- x_map_scale %>%
        mutate(
          lower = scale_ref_mean - scale_ref_sd / 2,
          middle = scale_ref_mean,
          upper = scale_ref_mean + scale_ref_sd / 2
        )
      
      p1 <- scaled_long_filtered %>%
        filter(domain == "Cognitive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() + labs(title = "Cognitive")
      
      p2 <- scaled_long_filtered %>%
        filter(domain == "Receptive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() + labs(title = "Receptive") +
        theme(legend.position = "none")
      
      p3 <- scaled_long_filtered %>%
        filter(domain == "Expressive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() + 
        labs(title = "Expressive") + 
        theme(legend.position = "none")
      
      p4 <- scaled_long_filtered %>%
        filter(domain == "Fine Motor") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() +
        labs(title = "Fine Motor") +
        theme(legend.position = "none")
      
      p5 <- scaled_long_filtered %>%
        filter(domain == "Gross Motor") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() +
        labs(title = "Gross Motor") +
        theme(legend.position = "none")
      
      if (input$show_reference == "Yes") {
        ref_geoms <- list(
          geom_segment(data = ref_box_data_scaled,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = middle, yend = middle,
                           linetype = "Pop Mean"),
                       inherit.aes = FALSE, color = "black", size = .75),
          
          geom_segment(data = ref_box_data_scaled,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = lower, yend = lower,
                           linetype = "Pop IQR"),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          geom_segment(data = ref_box_data_scaled,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = upper, yend = upper,
                           linetype = "Pop IQR"),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          scale_linetype_manual(
            name = paste0("Reference Lines (Mean = ", scale_ref_mean, ")"),
            values = ref_line_types
          )
        )
        
        p1 <- p1 + ref_geoms
        p2 <- p2 + ref_geoms
        p3 <- p3 + ref_geoms
        p4 <- p4 + ref_geoms
        p5 <- p5 + ref_geoms
      }
      
      
      if (input$overlay == "Yes") {
        overlay_geom <- list(
          geom_jitter(aes(name = "Age at Assessment (months)", fill = bsid_age_calc),
                      shape = 21, color = "black", size = 2, alpha = 0.5),
          scale_fill_gradient(low = "white", high = "black")
        )
        p1 <- p1 + ggnewscale::new_scale_fill() + overlay_geom
        p2 <- p2 + ggnewscale::new_scale_fill() + overlay_geom
        p3 <- p3 + ggnewscale::new_scale_fill() + overlay_geom
        p4 <- p4 + ggnewscale::new_scale_fill() + overlay_geom
        p5 <- p5 + ggnewscale::new_scale_fill() + overlay_geom
      }
      
      
      
      # Extract legend from p1
      legend <- cowplot::get_legend(p1)
      
      # Remove legend from p1 plot itself to avoid duplication
      p1 <- p1 + theme(legend.position = "none")
      
      top_row <- cowplot::plot_grid(p1, legend, ncol = 2, rel_widths = c(1, 0.25))


      middle_row <- cowplot::plot_grid(p2, p3, ncol = 2)
      bottom_row <- cowplot::plot_grid(p4, p5, ncol = 2)
      
      final_plot <- final_plot <- cowplot::plot_grid(
        top_row,            # p1 + legend
        middle_row,         # p2 + p3
        bottom_row,         # p4 + p5
        ncol = 1,
        rel_heights = c(1, 1, 1)  # Make all rows equal height
      )
      
      print(final_plot)
      
    } else if (input$plot_choice == "GSV") {
        
        gsv_ref_mean <- 500
        gsv_ref_sd <- 25
        
        ref_box_data_gsv <- x_map_gsv %>%
          mutate(
            lower = gsv_ref_mean - gsv_ref_sd / 2,
            middle = gsv_ref_mean,
            upper = gsv_ref_mean + gsv_ref_sd / 2
          )
        
        p1 <- new_gsv_long_rem %>%
          filter(domain == "Cognitive") %>%
          mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
          plot_base() +
          labs(title = "Cognitive")
        
        p2 <- new_gsv_long_rem %>%
          filter(domain == "Receptive") %>%
          mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
          plot_base() +
          labs(title = "Receptive") +
          theme(legend.position = "none")
        
        p3 <- new_gsv_long_rem %>%
          filter(domain == "Expressive") %>%
          mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
          plot_base() +
          labs(title = "Expressive") +
          theme(legend.position = "none")
        
        p4 <- new_gsv_long_rem %>%
          filter(domain == "Fine Motor") %>%
          mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
          plot_base() +
          labs(title = "Fine Motor") +
          theme(legend.position = "none")
        
        p5 <- new_gsv_long_rem %>%
          filter(domain == "Gross Motor") %>%
          mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
          plot_base() +
          labs(title = "Gross Motor") +
          theme(legend.position = "none")
        
        if (input$show_reference == "Yes") {
          ref_geoms <- list(
            geom_segment(data = ref_box_data_gsv,
                         aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                             y = middle, yend = middle,
                             linetype = "Pop Mean"),
                         inherit.aes = FALSE, color = "black", size = .75),
            
            geom_segment(data = ref_box_data_gsv,
                         aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                             y = lower, yend = lower,
                             linetype = "Pop IQR"),
                         inherit.aes = FALSE, color = "black", size = .5),
            
            geom_segment(data = ref_box_data_gsv,
                         aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                             y = upper, yend = upper,
                             linetype = "Pop IQR"),
                         inherit.aes = FALSE, color = "black", size = .5),
            
            scale_linetype_manual(
              name = paste0("Reference Lines (Mean = ", gsv_ref_mean, ")"),
              values = ref_line_types
            )
          )
          p1 <- p1 + ref_geoms
          p2 <- p2 + ref_geoms
          p3 <- p3 + ref_geoms
          p4 <- p4 + ref_geoms
          p5 <- p5 + ref_geoms
        }
        
        
        if (input$overlay == "Yes") {
          overlay_geom <- list(
            geom_jitter(aes(name = "Age at Assessment (months)",fill = bsid_age_calc),
                        shape = 21, color = "black", size = 2, alpha = 0.5),
            scale_fill_gradient(name = "Age at Assessment (months)", low = "white", high = "black"))
          
          p1 <- p1 + ggnewscale::new_scale_fill() + overlay_geom
          p2 <- p2 + ggnewscale::new_scale_fill() + overlay_geom
          p3 <- p3 + ggnewscale::new_scale_fill() + overlay_geom
          p4 <- p4 + ggnewscale::new_scale_fill() + overlay_geom
          p5 <- p5 + ggnewscale::new_scale_fill() + overlay_geom
        }
        
        
        
        # Extract legend from p1
        legend <- cowplot::get_legend(p1)
        
        # Remove legend from p1 plot itself to avoid duplication
        p1 <- p1 + theme(legend.position = "none")
        
        top_row <- cowplot::plot_grid(p1, legend, ncol = 2, rel_widths = c(1, 0.25))
        
        middle_row <- cowplot::plot_grid(p2, p3, ncol = 2)
        bottom_row <- cowplot::plot_grid(p4, p5, ncol = 2)
        
        final_plot <- cowplot::plot_grid(
          top_row,            # p1 + legend
          middle_row,         # p2 + p3
          bottom_row,         # p4 + p5
          ncol = 1,
          rel_heights = c(1, 1, 1)  # Make all rows equal height
        )
        
        print(final_plot) 
    }
  })
}





shinyApp(ui, server)