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
library(gamlss.add)
library(zoo)
library(gridExtra)

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

# edits GSV data for plotting
new_gsv_long_rem <- readRDS("Bayley_GSV_scores.rds") %>% 
  mutate(sca_condition = as.character(sca_condition)) %>% 
  pivot_longer(!c(study_id_extraordinary, redcap_event_name, sca_condition, bayley_version, bsid_age_calc), 
               names_to = "domain", values_to = "score") %>% 
  mutate(domain = case_when(domain == "bsid_gsv_cog" ~ "Cognitive", 
                            domain == "bsid_gsv_rc" ~ "Receptive Communication", 
                            domain == "bsid_gsv_ec" ~ "Expressive Communication", 
                            domain == "bsid_gsv_fm" ~ "Fine Motor", 
                            domain == "bsid_gsv_gm" ~ "Gross Motor")) %>% 
  mutate(transformed_score = ((score - 500)/100)*25 + 500)

# Data for Violin Plots/tab1 Specifically

composite_long_filtered <- composite %>% 
  rename(
    Cognitive = 'bsid_cog_composite',
    Language = 'bsid_lang_composite',
    Motor = 'bsid_mot_composite') %>% 
  pivot_longer(cols=c('Cognitive', 'Language', 'Motor'), names_to = 'domain', values_to = 'score') %>% 
  filter(score < 777)

new_gsv_long_rem <- GSV %>% 
  rename(
    Cognitive = 'bsid_gsv_cog',
    Receptive = 'bsid_gsv_rc',
    Expressive = 'bsid_gsv_ec',
    `Fine Motor` = 'bsid_gsv_fm',
    `Gross Motor` = 'bsid_gsv_gm'
  ) %>% 
  pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine Motor', 'Gross Motor'), names_to = 'domain', values_to = 'score') %>%
  filter(redcap_event_name != "2_month_visit_arm_1")

scaled_long_filtered <- scaled %>% 
  rename(
    Cognitive = "bsid_cog_scaled",
    Receptive = "bsid_rc_scaled",
    Expressive = "bsid_ec_scaled",
    Fine_Motor = "bsid_fm_scaled",
    Gross_Motor = "bsid_gm_scaled") %>% 
  pivot_longer(cols = c('Cognitive', 'Receptive', 'Expressive', 'Fine_Motor', 'Gross_Motor'), names_to = 'domain', values_to = 'score') %>% 
  filter(score < 777)


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
              tabPanel("Overview Plots", 
                        selectInput("plot_choice", "Choose a Data Type:",
                                    choices = c("Composite", "Scaled", "GSV")),
                        
                        plotOutput("dynamic_violin_plot"),
                        
                        tags$p(
                          "This plot provides an overview of bayley scores for the overall eXtrodinarY babies study at CU Anschutz. This study conducts clinical research on X&Y chromosome variations to track their influence on developmental milestones. These plots demonstrate the distribution of Bayley 4 scores, subsetted into SCA conditions, across age and domains. Boxplots overlayed on the plots demonstrate general population mean and standard deviations. For more information on the eXtraordinarY babies study and CU Anschutz research",
                          tags$a(href = "https://medschool.cuanschutz.edu/pediatrics/sections/developmental-pediatrics/extraordinary-kids-program/our-research", "click here", target = "_blank"),
                          ".")),
              
              # Tab 2: GAMLSS Growth Plots, based on existing data (static images)
              tabPanel("GAMLSS Growth Plots",
                       # user choices of GAMLSS plots
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("domain_select", "Select Domain:",
                                       choices = unique(new_gsv_long_rem$domain), 
                                       selected = unique(new_gsv_long_rem$domain)[1]),
                           selectInput("sct_select", "Select SCT Condition:",
                                       choices = c("ALL", unique(new_gsv_long_rem$sca_condition)),
                                       selected = "ALL")
                         ),
                         # displays the GAMLSS plot based on user choices
                         mainPanel(
                           plotOutput("growth_plot"),
                           br(),
                           p("Figure caption: see explanation in Background/References")
                         )
                       )), # end tab2
              
              # Tab 3: Allows inputs of milestone data, and plots over the general population boxplot
              tabPanel("Input Milestones",
                       sidebarLayout(
                         sidebarPanel(
                           # user input of milestones
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
                           ),
                           fluidRow(
                             column(4, tags$img(src = "milestones_legend.jpg", height = "100px")),
                             column(8, h5("Figure 3: Individual Milestones plotted atop the general population data"))),
                           fluidRow(
                             h3("Data:")
                           ),
                           fluidRow(
                             column(12,
                                    DTOutput("milestones_table_output"))
                                    )
                                  )
                                  )
                       ), # end tab3 
              
              # Tab 4: GSV Scores Input, with Reactive Data Frame and Plot over study population percentile curves
              tabPanel("Input GSV Scores"),
              
              # Tab 5: Background Information and References
              tabPanel("Background/References",
                       titlePanel("Methodology"),
                       fluidRow(
                         column(
                           width = 4,
                           style = "border-right: 1px solid #ddd; padding-right:
      15px;",
                           h4("GAMLSS"),
                           p("GAMLSS enables the creation of distributional regression models using Generalized Additive Models. By adding location (mean), scale (variance), 
             and shape (skew and kurtosis) components to the GAM structure, GAMLSS supports the construction of distributional non-linear curves. A common use of GAMLSS 
             is for growth curve construction, where whole distributions (10th, 25th, 50th, 75th, 90th) percentiles can be visualized."),
                           p("Bayley GSV (Growth Scale Values) scores, 
             taken from the 'eXtraordinarY Babies' study, can be modeled similarly to growth parameters like height and weight. We used the GAMLSS R-package to create percentile curves for the 
             Bayley-IV GSV scores collected at regularly scheduled study visits."),
                           
                           tabPanel("Gross Motor",
                                    tags$img(src = "grossmotor.jpg",
                                             width = "100%", height = "auto" , alt = "Example GAMLSS"))
                         )
                         ,
                         column(
                           width = 4, style = "border-right: 1px solid #ddd; padding-right:
      15px;",
                           h4("Bayley Scores of Infant and Toddler Development"),
                           p("The National Library of Medicine estimates that one in six children experience developmental delays, (Balasundaram et al. 2022).  Bayley Scores of Infant and Toddler Development 
             (BSID) is the most widespread tool used to assess developmental growth in infant and toddlers, up to 24 months in age in discovering developmental delays. Cognition, language (expressive
                                                                                                                                                                                             and receptive), motor skills (fine and gross), socio-emotional behavior, and adaptive behavior. The steps of administering BSID are early identification, monitoring progress, research, intervention
             planning, and outcome measurement. The administrator evaluates the growth of a child through specified milestones during an observational study."),
                           
                           p("The GSV scores used in the 'eXtraordinarY' Babies Study come from the Third Edition and assume a Normal distribution with a mean of 500 and standard deviation of 100. In 2019, BSID 
             switched to the Fourth Edition; for the sake of consistency, we converted our Bayley III scores to Bayley IV scores, which similarly assume a normal distribution with a mean of 500 
             but a standard deviation of 25.")
                         ),
                         column(title = "'eXtraorindarY' Babies",
                                width = 4,
                                h4("'eXtraordinarY' Babies Study"),
                                p("A sex chromosome trisomy (SCT) is the presence of an additional sex chromosome—XXX, XXY, or XYY—rather than XX (female) and XY (male). Roughly 1 out of 500 live births result in SCTs
             (Nielsen & Wohlert, 1991). Children born with SCTs experience developmental delays at a higher rate than those without SCTs. Additionally, a recent study by Thompson et al, 20205, found 
             higher variation in the age of completion for milestones compared to the general pediatric population. As of July 2025, 298 children with an SCT enrolled in the eXtraordinarY Babies 
             Study between the ages of 2 and 12 months and subsequently attended evaluations for the Bayley Scores of Infants and toddler Development (BSID), at the Children’s Hospital of Colorado 
             and at Nemours Children’s Hospital of Thomas Jefferson University. "),
                                p("Evaluations were conducted at 2, 6, 12, 24, and 36 months, evaluating cognition, motor skills (fine and gross), and language (expressive and receptive) abilities. Observations at 2 months
             were removed for GSV growth curve estimation due to the small sample size. Future analysis with more data points should include the 2 month period for more accurate estimation. "),
                                tags$hr(style = "border-top: 2px dashed #ccc; margin: 15px 0;"),
                                p("Citation for Nielsen and Wohlert : Nielsen, J. & Wohlert, M. (1991). Chromosome abnormalities found among 34,910 newborn children: results from a 13‑year incidence study in Århus, Denmark.Human Genetics, 87, 81–83."),
                                p("Citation for Thompson, et al : Thompson T, Bothwell S, Janusz J, Wilson R, Howell S, Davis S, Swenson K, Martin S, Kowal K, Ikomi C, Despradel M, Ross J, Tartaglia N. Quantifying the Spectrum of Early Motor and Language
             Milestones in Sex Chromosome Trisomy. Pediatrics. 2025 Jul 24:e2024068773. doi: 10.1542/peds.2024-068773. Epub ahead of print. PMID: 40701561."),
                                tabPanel("EBS",
                                         tags$img(src = "eBs_Logo.jpg",
                                                  width = "100%", height = "auto" , alt = "Example GAMLSS"))
                                
                         ) 
                       )),
              
              # Tab 6: Meet the Team
              tabPanel("Meet the Team",
                       # Group image with caption at top
                       fluidRow(
                         column(12,
                           div(style = "text-align: center;",
                             tags$img(src = "Group_pic.jpg", height = "300px", style = "max-width: 100%; border-radius: 10px;"),
                             tags$figcaption("The team, from left to right:.", 
                                             style = "font-size: 14px; color: #555; margin-top: 10px;")
                           )
                         )
                       ),
                       br(),

                       # SAMANTHAS
                       fluidRow(
                         column(6,
                           div(style = "text-align:center;", img(src = "lead1.jpg", height = "200px"), h3("Samantha Bothwell"), p("Group Lead")
                           )
                         ),
                         column(6,
                           div(style = "text-align:center;", img(src = "lead2.jpg", height = "200px"), h3("Samantha Roberts"), p("Group Lead")
                           )
                         )
                       ),
                       
                       br(), hr(), br(),
                       
                       # TEAM
                       fluidRow(
                         column(3,
                           img(src = "member1.jpg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Chloe Child"), p("Hi, my name is Chloe Child! I am a rising senior at Appalachian State University in Boone, North Carolina. 
                         I study public health with minors in statistics and mathematics. I also work as a Research Assistant in Appalachian State University's 
                        Public Health Department and as a Student Associate for Blue Cross Blue Shield of North Carolina. When not in school or working, I enjoy hiking, rock climbing,
                                                        weightlifting, and drawing. I joined the Colorado Summer Institute in Biostatistics (CoSIBS) because I'm really interested
                                                        in the world of biostatistics and wanted to expand upon my ability to apply statistics and data science to public health research.")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,img(src = "member2.jpg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Liam Hallinan"), p("BLURB.")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,
                           img(src = "member3.jpg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Jenna Jimenez"), p("BLURB")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,
                           img(src = "Murphy_Madeline.jpeg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Madeline Murphy"),
                           p("Hello, my name is Madeline Murphy. I am a rising junior at Rollins College in Orlando, Florida, and I study Biology with a minor in Data Analytics.
          I was born and raised in Miami, Florida, with my older sister Sarah. I love animals, horror movies, and playing video games with my friends.
          I joined the Colorado Summer Institute in Biostatistics for the summer of 2025 to gain real experience in the field of biostatistics and to see if this is a career I wish to pursue.")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,
                           img(src = "Pressier_Jack.jpg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9,h4("John Preisser"),
                           p("My name is Jack Preisser; 
          I am a rising senior majoring in statistics at Carleton College.
          I was born and raised in Chapel Hill, North Carolina along with my twin sister Hannah. 
          I enjoy running, hiking and other physical activities. 
          I have enjoyed my time at the Colorado Summer Institute in Biostatistics (CoSIBS)
          because I have not studied public health through an in-depth manner prior to CoSIBS. ")
                         )
                       )
              
              ) # end tabsetPanel
  )

) # end UI

##### Defining Server logic #####
server <- function(input, output, session) {
  
  ### For Tab 1 ###
  
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
  
  ### For Tab 2 ###
      output$growth_plot <- renderPlot({
        
        req(input$domain_select) 
        req(input$sct_select)
        
        # Filtered data (local to renderPlot)
        filtered_data <- new_gsv_long_rem %>%
          filter(domain == input$domain_select) %>%
          dplyr::select(study_id_extraordinary, sca_condition, domain, bsid_age_calc, transformed_score) %>%
          filter(complete.cases(.))
        
        
        if (input$sct_select != "ALL") {
          filtered_data <- filtered_data %>%
            filter(sca_condition == input$sct_select)
        }
        
        
        validate(
          need(nrow(filtered_data) > 10, "Not enough data after filtering.")
        )
        
        
        # Fit GAMLSS model
        model <- gamlss(
          formula = as.formula("transformed_score ~ pb(bsid_age_calc, lambda = 5)"),
          sigma.formula = as.formula("~ pb(bsid_age_calc)"),
          nu.formula = ~1,
          tau.formula = ~1,
          data = filtered_data,
          family = BCCG(),
          control = gamlss.control(save.data = TRUE),   # <-- THIS FIX IS CRUCIAL
          trace = FALSE
        )
        
        # ages to predict over 
        age_seq <- seq(
          min(filtered_data$bsid_age_calc, na.rm = TRUE),
          max(filtered_data$bsid_age_calc, na.rm = TRUE),
          length.out = 100
        )
        
        model$call$data <- filtered_data
        
        # data 
        newdata <- data.frame(bsid_age_calc = age_seq)
        
        # Get predicted distribution parameters from the model
        params <- predictAll(model, newdata = newdata)
        
        # Calculate centiles manually using the BCCG distribution quantile function
        centiles <- c(10, 25, 50, 75, 90)
        q_vals <- sapply(centiles / 100, function(p) {
          qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
        })
        
        # Create a dataframe of predicted centiles
        lms_mod <- data.frame(age = age_seq, q_vals)
        colnames(lms_mod)[-1] <- paste0("P", centiles)
        
        validate(
          need(!is.null(lms_mod), "Centile prediction failed.")
        )
        
        # Pivot, long, for nicer ggplot
        pred_long <- pivot_longer(lms_mod, -age,
                                  names_to = "Percentile", values_to = "Score")
        
        # creates the GAMLSS plot
        ggplot(pred_long, aes(x = age, y = Score, color = Percentile)) +
          geom_line(size = 1.2) +
          geom_ribbon(
            data = data.frame(
              x = lms_mod$age,
              ymin = lms_mod$P10,
              ymax = lms_mod$P90
            ),
            aes(x = x, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE,
            alpha = 0.2,
            fill = "gray70"
          ) +
          theme_minimal(base_size = 14) +
          labs(
            title = paste("Growth Chart for", input$domain_select),
            x = "Age (months)",
            y = "Transformed Bayley Score",
            color = "Percentile"
          )
      })
  
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
                     hoverinfo = "skip",
                     showlegend = FALSE) # hides the boxplot itself from the legend
        
        # creates list of points to plot 
        user_points <- input_milestones_data()  
        if(nrow(user_points)>0){
          # Fixed trace: milestone overlay uses dynamic symbols/colors
          milestone_input_plot <- milestone_input_plot %>%
            add_trace(data = user_points,
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
                                              "<br>Achievement Percentile:", round(Percentile, 1)),
                                hoverinfo = "text",
                                inherit = FALSE) 
        }
        
        # Show the plot
        milestone_input_plot <- milestone_input_plot %>%
          layout(xaxis = list(range = c(0, 100)),
                 yaxis = list(title = " "),
                 title = "Individual Milestones Achieved",
                 shapes = list(
            list(type = "rect", fillcolor = "rgba(255, 0, 0, 0.2)", 
                 line = list(color = "red", width = 0), x0 = 90, x1 = 100, y0 = 0, y1 = 1, xref = "x", yref = "paper"))
          )
          
        
      
      })
  
  
  ### For Tab 4 ###
  
  ### For Tab 5 ###
      output$image_ui <- renderImage({
        src = file.path("grossmotor.jpg")
        
      })
      output$image_ui2 <- renderImage({
        src = file.path("eBs_Logo.jpg")
      })
  ### For Tab 6 ###
      # no server logic, all displayed outputs are static images and text
} # end server

##### Run the application #####
shinyApp(ui = ui, server = server)