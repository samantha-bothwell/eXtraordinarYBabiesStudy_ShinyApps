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
library(ggnewscale)
library(patchwork)
library(cowplot)
library(dplyr)


####### importing databases for plotting #####

  # Milestones data
  milestones <- readRDS("Milestones.RDS")
  genpop <- readRDS("GenPop_Milestones.RDS")
  indiv_percentiles <- readRDS("Individual_Percentiles.RDS")
  
  # Bayley scores data
  composite <- readRDS("Bayley_Composite_scores.rds")
  GSV <- readRDS("Bayley_GSV_scores.rds")
  scaled <- readRDS("Bayley_Scaled_scores.rds")

##### Global Code: processing of datasets for plotting #####

  # Pull general population 90th percentile into the individual percentiles data
  indiv_percentiles$norms_90th <- genpop$Q90[match(indiv_percentiles$milestone, genpop$milestone)]
  
  # Format ID to display number and SCT
  indiv_percentiles$study_id_extraordinary <- paste0(indiv_percentiles$study_id_extraordinary, " (", 
                                                     indiv_percentiles$sca_condition, ")")
  # pulls unique milestones for input/plot
  milestones_list <- c(unique(indiv_percentiles$milestone))
  
  
  new_gsv_long_rem <- readRDS("Bayley_GSV_scores.rds") %>% 
    mutate(sca_condition = as.character(sca_condition)) %>% 
    filter(redcap_event_name != "2_month_visit_arm_1") %>%
    pivot_longer(!c(study_id_extraordinary, redcap_event_name, sca_condition, bayley_version, bsid_age_calc), 
                 names_to = "domain", values_to = "score") %>% 
    mutate(domain = case_when(domain == "bsid_gsv_cog" ~ "Cognitive", 
                              domain == "bsid_gsv_rc" ~ "Receptive Communication", 
                              domain == "bsid_gsv_ec" ~ "Expressive Communication", 
                              domain == "bsid_gsv_fm" ~ "Fine Motor", 
                              domain == "bsid_gsv_gm" ~ "Gross Motor")) %>%
    filter(score < 777) %>%
    mutate(transformed_score = ((score - 500)/100)*25 + 500)
  
    # Data Filtering/Cleaning for Violin Plots - Composite Scores
    composite_long_filtered <- readRDS("Bayley_Composite_scores.rds") %>% 
      rename(
        Cognitive = 'bsid_cog_composite',
        Language = 'bsid_lang_composite',
        Motor = 'bsid_mot_composite') %>% 
      pivot_longer(cols=c('Cognitive', 'Language', 'Motor'), names_to = 'domain', values_to = 'score') %>% 
      filter(score < 777) %>% 
      mutate(
        domain = factor(domain, levels = c("Cognitive", "Language", "Motor"))
      )
    
    # Data Filtering/Cleaning for Violin Plots - GSV Scores
    scaled_long_filtered <- readRDS("Bayley_Scaled_scores.rds") %>% 
      rename(
        Cognitive = "bsid_cog_scaled",
        `Receptive Communication` = "bsid_rc_scaled",
        `Expressive Communication` = "bsid_ec_scaled",
        `Fine Motor` = "bsid_fm_scaled",
        `Gross Motor` = "bsid_gm_scaled") %>% 
      pivot_longer(cols = c('Cognitive', 'Receptive Communication', 'Expressive Communication', 'Fine Motor', 'Gross Motor'), names_to = 'domain', values_to = 'score') %>% 
      filter(score < 777) %>%
      mutate(domain = factor(domain, levels = c(
        "Cognitive", "Receptive Communication", "Expressive Communication", "Fine Motor", "Gross Motor"
      ))
      )
    
    # adds lines for reference types
    ref_line_types <- c("Population Mean" = "solid", "Population 95% Conf. Int." = "dashed")
    
    # function to calculate percentiles
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
    

##### Defining UI #####
ui <- fluidPage(
  
  # Set theme of app
  theme = shinytheme("flatly"),
  
  # Title of Application
    fluidRow(
      column(10, 
             h1("eXtraordinarY Babies Study : SCT Bayley Progression")  # Title on the left
      ),
      column(2, 
             tags$img(src = "eBs_Logo.jpg", height = "100px", style = "float: right;")  # Image on the right
      )
    ), # end titlePage section
  
  # Conditional panel for carrying across tabs
  
  # Tabset Panel that defines each of the tabs in use
  tabsetPanel(id = "tabs",
              tabPanel(
                "Welcome!",
                fluidRow(
                  column(
                    width = 7,
                    offset = 1,
                    h2("Welcome to the eXtraordinarY Babies Study Bayley and Milestone Progression App"),
                    br(),
                    p("The eXtraordinarY Kids Clinic was launched in 2007 by Founder and Director, ",
                      strong("Nicole Tartaglia, MD"), ". Dr. Tartaglia developed this unique interdisciplinary clinic team to address the medical, developmental, and psychological needs of children and adolescents with X&Y chromosome variations."),
                    br(),
                    p("This interactive application provides a tool for parents and clinicians to monitor milestone development. Milestone achievement is assessed ",
                      " through the",  strong("Bayley-III"), " and ", strong("Bayley-IV"), " assessments as well as specific developmental milestones, such as walking, running, cooing, babbling."),
                    br(),
                    p("This app was built using data obtained for the study up to ", strong("January 2025"), 
                      " and is intended for use for children with a ", strong("Sex Chromosome Trisomy"), 
                      ", between the ages of ", strong("0 and 4 years old"), "."),
                    br(),
                    p("We hope you enjoy it!"),
                    br(),
                    tags$a(
                      href = "https://medschool.cuanschutz.edu/pediatrics/sections/developmental-pediatrics/extraordinary-kids-program",
                      target = "_blank",
                      class = "btn btn-info btn-lg",
                      style = "color: white; font-weight: bold; margin-top: 10px;",
                      icon("info-circle"), " Learn more about our program!"
                    )
                  ),
                  column(
                    width = 4,
                    align = "center",
                    br(),
                    h3("Enrollment"),
                    img(
                      src = "sct_pie.jpg", 
                      width = "60%", 
                      alt = "Extraordinary Kids Clinic Team or Logo",
                      style = "margin-top: 10px;"
                    )
                  )
                )
              ),
              
              # Tab 1: Welcome to the App/Overview Plot of Scaled/Composite/GSV of Study
              tabPanel("Bayley Overview Plots",
                       
                       fluidRow(
                         column(width = 9, offset = 0.1,
                          h3(" Bayley Composite, Scaled, and GSV Score Distributions")),

                         column(4,
                                selectInput("plot_choice", "Choose a Data Type:", # select the data type to be plotted
                                            choices = c("Composite", "Scaled", "GSV"))),
                         column(4,
                                radioButtons("show_reference", "Show Population Reference Lines:", # option to reference lines of IQR and Mean
                                             choices = list("No", "Yes"),
                                             selected = "No",
                                             inline = TRUE)),
                         column(4,
                                radioButtons("overlay", "Overlay All Data Points:", # option to add data points on top of plots
                                             choices = list("No", "Yes"), 
                                             selected = "No",
                                             inline = TRUE))),
                       
                       uiOutput("dynamic_violin_ui"),# one plot that handles above inputs, and outputs a density plot based on score, by SCA condition
                       
                       fluidRow(
                         column(12,tags$p('This plot provides an overview of Bayley-IV score distribution for the overall eXtraodinarY babies study at CU Anschutz. Users can choose to view overlayed general population 95% Confidence Intervals and/or point distributions of individuals in the study.')
                       
                       ))), # end tab1
              
                      
              
              # Tab 2: GAMLSS Growth Plots, based on existing data (static images)
              tabPanel("GSV Growth Plots",
                       # user choices of GAMLSS plots
                       sidebarLayout(
                         sidebarPanel(width=3,
                           selectInput("domain_select", "Select Domain:", # to select Bayley-IV domain 
                                       choices = unique(new_gsv_long_rem$domain), 
                                       selected = unique(new_gsv_long_rem$domain)[1]),
                           selectInput("sct_select", "Select SCT Condition:", # chooses SCA condition
                                       choices = c("ALL", unique(new_gsv_long_rem$sca_condition)),
                                       selected = "ALL"),
                           checkboxInput("show_points", "Overlay Raw Data Points", value = FALSE) # option to add points over the plots
                           
                         ), # end sidebarPanel
                         mainPanel(
                           plotOutput("growth_plot", height = "600px"),
                           br(),
                           p("Growth plot made with Bayley-IV GSV Scores using GAMLSS modeling. Plots can be seperated by Bayley Domain, 
                           and SCT Condition, with the option to overlay raw data points. The lines represent the percentiles of the selected domain's milestones, 
                           with the solid black line representing the 50th percentile, the gray line for the 25th and 75th percentile, and the dashed lines for
                             the 10th and 90th percentile [for patients in the study].")
                                  ) # end mainPanel
                          ) # end sidebarLayout
                       ), # end tab2
              
              # Tab 3: Allows inputs of milestone data, and plots over the general population boxplot
              tabPanel("Input Milestones",
                       titlePanel("Milestone Input Plots (Percentiles)"),
                       sidebarLayout(
                         sidebarPanel(width=3,
                                      selectInput("sca_condition", label = "Select SCT",
                                                  choices = c("All SCTs", unique(milestones$sca_condition)), 
                                                  selected = "All SCTs"),
                                      div(h4("Input Age (Months) Milestone was Achieved : "), 
                                          tagList(
                                            lapply(milestones_list, function(milestone) {
                                              numericInput(
                                                inputId = paste0("AgeWhen_", gsub(" ","",milestone)), # handles spaces for calculations
                                                label= milestone,
                                                min = 0, max = 48, value = NA, step = 1
                                                          )
                                              
                                                    }), # end lapply function
                                            actionButton("addPoints", "View Inputs", icon = icon("plus-circle"), class = "btn btn-success"), # adds inputted milestones to plot
                                            actionButton("clear_milestones", "Clear Milestones", icon = icon("eraser"),class = "btn btn-success"),
                                            br(), br()
                                          ))),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                            fluidRow(column(12, 
                                            # Add vertical space before the plot
                                            div(style = "margin-top: 30px;"),
                                            
                                            plotlyOutput("indiv_perc", height = "550px")),
                                     
                            fluidRow(column(4, 
                                            tags$img(src = "Percentiles_legend.jpeg", height = "100px")), # add back in the plot
                                     column(1,
                                            br(),),
                                              
                                    column(7, h5("Individual Milestones plotted atop the general population data."))),
                            fluidRow(h3("   Data:")),
                            fluidRow(column(12,
                                    DTOutput("milestones_table_output"))
                                     )
                                   )
                           )
                         )
                       ), # end tab3 
         
              
              # Tab 4: GSV Scores Input, with Reactive Data Frame and Plot over study population percentile curves
              tabPanel("Input GSV Scores",
                        titlePanel("Growth Input Charts (Bayley-IV Scores)"),
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                    selectInput("domain_select", "Select Domain:",
                                                choices = unique(new_gsv_long_rem$domain),
                                                selected = "Cognitive"),
                                    selectInput("sct_select", "Select SCT Condition:",
                                                choices = c("ALL", unique(new_gsv_long_rem$sca_condition)),
                                                selected = "ALL"),
                                    h4("Input 4 GSV Checkup Points"),
                                    numericInput("age1", "Age 1 (months):", NA),
                                    numericInput("score1", "GSV 1:", NA),
                                    numericInput("age2", "Age 2 (months):", NA),
                                    numericInput("score2", "GSV 2:", NA),
                                    numericInput("age3", "Age 3 (months):", NA),
                                    numericInput("score3", "GSV 3:", NA),
                                    numericInput("age4", "Age 4 (months):", NA),
                                    numericInput("score4", "GSV 4:", NA),
                                    actionButton("save_inputs", "View Inputs", icon = icon("plus-circle"),class = "btn btn-success"),
                                    actionButton("clear_inputs", "Clear Data", icon = icon("eraser"),class = "btn btn-success"),
                                    downloadButton("download_plot", "Download All Plots", class = "btn btn-success")
                                  ),
                          mainPanel(
                            fluidRow(column(12, 
                                            # Add vertical space before the plot
                                            div(style = "margin-top: 30px;"),
                                            
                                            plotOutput("input_growth_plot")),
                                     
                                     fluidRow(column(12, 
                                     h5("Generalized Additive Model with Linearity, Shape,
                                          and Size (GAMLSS) plot with user-generated (pink) line with
                                          GSV scores, plotted over-top study population ranges, with
                                          10th-90th percentiles represented by purple-dashed lines and
                                          50th percentile represented by solid-black line."))),
                                     fluidRow(h3("Data:")),
                                     fluidRow(column(12,
                                                     DTOutput("GAMLSS_table"))
                                     )
                            )
                            # plotOutput("input_growth_plot"),
                            # br(),
                            # p("Figure caption: Shows user-entered trajectory with study population ranges, from the 10th to 90th percentile in grey."),
                            # br(),
                            # DTOutput("GAMLSS_table")
                          ) # end mainPanel
                        ) # end SidebarLayout
                       ), # end tab4
              
              # Tab 5: Background Information and References
              tabPanel("Background/References",
                       titlePanel("Methodology"),
                       fluidRow(
                         column(
                           width = 4,
                           style = "border-right: 1px solid #ddd; padding-right: 15px;",
                           h4("GAMLSS"),
# <<<<<<< Updated upstream
                           
                           p("GAMLSS enables the creation of distributional regression models using Generalized Additive Models. By adding 
                           location (mean), scale (variance), and shape (skew and kurtosis) components to the GAM structure, 
                             GAMLSS supports the construction of distributional non-linear curves. A common use of GAMLSS is for growth curve construction, 
                             where whole distributions (10th, 25th, 50th, 75th, 90th) percentiles can be visualized."),
                           
                           p("Bayley GSV (Growth Scale Values) scores, taken from the 'eXtraordinarY Babies' study, can be modeled similarly 
                           to growth parameters like height and weight. We used the GAMLSS R-package to create percentile curves for the 
                             Bayley-IV GSV scores collected at regularly scheduled study visits."),
# =======
                           p("GAMLSS enables the creation of distributional regression models using Generalized Additive Models (GAM). By adding location (mean), scale (variance), 
             and shape (skew and kurtosis) components to the GAM structure, GAMLSS supports the construction of distributional non-linear curves. A common use of GAMLSS 
             is for growth curve construction, where whole distributions (10th, 25th, 50th, 75th, and 90th) percentiles can be visualized."),
                           p("Bayley GSV (Growth Scale Values) scores, 
             taken from the eXtraordinarY Babies Study, can be modeled similarly to growth parameters like height and weight. We used the GAMLSS R-package to create percentile curves for the 
             Bayley-IV GSV scores collected at regularly scheduled study visits."),
# >>>>>> Stashed changes
                           
                           tabPanel("Gross Motor",
                                    tags$img(src = "grossmotor.jpg",
                                             width = "100%", height = "auto" , alt = "Example GAMLSS"))
                              ),
                         
                         column(
                           width = 4, style = "border-right: 1px solid #ddd; padding-right: 15px;",
                           h4("Bayley Scores of Infant and Toddler Development"),
# <<<<<<< Updated upstream
                           
                           p("The National Library of Medicine estimates that one in six children experience developmental delays, (Balasundaram et al. 2022).  
                             Bayley Scores of Infant and Toddler Development (BSID) is the most widespread tool used to assess developmental growth in infant and toddlers, 
                             up to 24 months in age in discovering developmental delays. Cognition, language (expressive and receptive), motor skills (fine and gross), 
                             socio-emotional behavior, and adaptive behavior. The steps of administering BSID are early identification, monitoring progress, research, 
                             intervention planning, and outcome measurement. The administrator evaluates the growth of a child through specified milestones during an observational study."),
                           
                           p("The GSV scores used in the 'eXtraordinarY' Babies Study come from the Third Edition and assume a Normal distribution with a mean of 500 and standard deviation of 100. 
                           In 2019, BSID switched to the Fourth Edition; for the sake of consistency, we converted our Bayley III scores to Bayley IV scores, which similarly assume a normal distribution 
                           with a mean of 500 but a standard deviation of 25.")
                                ),
                         
                         column(title = "eXtraorindarY Babies",
                                width = 4,
                                h4("eXtraordinarY Babies Study"),
                                p("A sex chromosome trisomy (SCT) is the presence of an additional sex chromosome—XXX, XXY, or XYY—rather than XX (female) and XY (male). 
                                  Roughly 1 out of 500 live births result in SCTs (Nielsen & Wohlert, 1991). Children born with SCTs experience developmental delays at a higher 
                                  rate than those without SCTs. Additionally, a recent study by Thompson et al, 20205, found  higher variation in the age of completion for milestones compared to 
                                  the general pediatric population. As of July 2025, 298 children with an SCT enrolled in the eXtraordinarY Babies Study between the ages of 2 and 12 months,
                                  and subsequently attended evaluations for the Bayley Scores of Infants and toddler Development (BSID), at the Children’s Hospital of Colorado,
                                  and at Nemours Children’s Hospital of Thomas Jefferson University. "),
                                
                                p("Evaluations were conducted at 2, 6, 12, 24, and 36 months, evaluating cognition, motor skills (fine and gross), and language (expressive and receptive) abilities. 
                                  Observations at 2 months were removed for GSV growth curve estimation due to the small sample size. Future analysis with more data points should include the 2 month period 
                                  for more accurate estimation. "),
                                
# =======
                           p("The National Library of Medicine estimates that one in six children experience developmental delays, (Balasundaram et al. 2022).  Bayley Scores of Infant and Toddler Development 
             (BSID) is the most widespread tool used to assess developmental growth in infant and toddlers, up to 24 months in age in discovering developmental delays. Cognition, language (expressive
                                                                                                                                                                                             and receptive), motor skills (fine and gross), socio-emotional behavior, and adaptive behavior scores are the fundamental components of
                                                                                                                                                                                             BSID. The steps of administering BSID are early identification, monitoring progress, research, intervention
             planning, and outcome measurement. The administrator evaluates the growth of a child through specified milestones during an observational study."),
                           
                           p("The GSV scores used in the eXtraordinarY Babies Study come from the Third Edition and assume a normal distribution with a mean of 500 and standard deviation of 100. In 2019, BSID 
             switched to the Fourth Edition; for the sake of consistency, we converted our Bayley III scores to Bayley IV scores, which similarly assume a normal distribution with a mean of 500 
             but a standard deviation of 25."),
                          tags$hr(style = "border-top: 2px dashed #ccc; margin: 15px 0;"),

                          p("Nielsen and Wohlert : Nielsen, J. & Wohlert, M. (1991). Chromosome abnormalities found among 34,910 newborn children: results from a 13‑year incidence study in Århus, Denmark.Human Genetics, 87, 81–83."),
                          p("Thompson, et al : Thompson T, Bothwell S, Janusz J, Wilson R, Howell S, Davis S, Swenson K, Martin S, Kowal K, Ikomi C, Despradel M, Ross J, Tartaglia N. 
                                                            Quantifying the Spectrum of Early Motor and Language Milestones in Sex Chromosome Trisomy. Pediatrics. 2025 Jul 24:e2024068773. doi: 10.1542/peds.2024-068773. Epub ahead of print. PMID: 40701561.")


                         )
                           ) # end fluidRow
                       ), # end tab5
              
              # Tab 6: Meet the Team
              tabPanel("Meet the Biostats Team",
                       # Group image with caption at top
                       fluidRow(
                         column(12,
                           div(style = "text-align: center;",
                             tags$img(src = "Group_pic.jpg", height = "300px", style = "max-width: 100%; border-radius: 10px;"),
                             tags$figcaption("The team, from left to right:.", 
                                             style = "font-size: 14px; color: #555; margin-top: 10px;")
                                 )
                              )
                          ), # end fluidRow
                       br(),
                       tags$hr(style = "border-top: 2px dashed #ccc; margin: 15px 0;"),
                       
                       # TEAM
                       fluidRow(
                         column(3,
                           img(src = "Child_Chloe.jpeg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Chloe Child (she/her)"), p("Chloe Child is a rising senior at Appalachian State University in Boone, North Carolina. 
                                                                  She studies Public Health with minors in Statistics and Mathematics. Chloe also works as a Research Assistant at Appalachian State University's 
                                                                  Public Health Department and as a Student Associate for Blue Cross Blue Shield of North Carolina. When not in school or working, Chloe enjoy hiking, rock climbing, crocheting,
                                                                  weightlifting, and drawing. She joined the Colorado Summer Institute in Biostatistics (CoSIBS) because of a deep interest in the world of biostatistics,
                                                                  and wanted to expand upon her ability to apply statistics and data science to public health research.")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,img(src = "Hallinan_Liam.jpeg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Liam Hallinan (he/him)"), p("Liam Hallinan is a rising senior at Colorado School of Mines. He is pursuing a degree in Quantitative Biosciences and Engineering,
                                                          with minors in Data Science, Public Affairs, and Biommedical Engineering. In school, Liam is active as a Lead Peer Mentor, helping
                                                          incoming students transition to college, and oversees the planning of school-wide orientation programming. Liam also serves as a Biology
                                                          2 Laboratory Teaching Assistant, helping students develop their foundational lab skills. Outside of academics, Liam is passionate
                                                          about hiking, baking, and spending time outdoors. He participated in the CoSIBS program to further his knowledge 
                                                          in mathematical and computational biology methods, and to develop relationships with faculty and students.")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,
                           img(src = "Jimenez_Jenna.jpg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Jenna Jimenez (they/them)"), p("Jenna Jimenez is a fifth year student, graduating May 2026, at California State University Channel Islands. They study psychology with a minor in studio art,
                                                                      while also working as a Research Assistant on the Clobes Cannabis Research Team, and starting August 2025, as a Research Assistant with Dr. Beatrice de Oca. 
                                                                      Outside of their academics, Jenna enjoys making art (including, but not limited to, painting, ceramics, and figure drawing), going to concerts, spending time outdoors, 
                                                                      and hanging out with their two cats. Jenna took part in the 2025 Colorado Summer Institute in Biostatistics (CoSIBS) to explore the field of biostatistics and to further 
                                                                      gain statistical and programming skills that they can apply to their current and future research. ")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,
                           img(src = "Murphy_Madeline.jpeg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9, h4("Madeline Murphy (she/her)"), p("Madeline Murphy is a rising junior at Rollins College in Orlando, Florida who studies Biology with a minor in Data Analytics. 
                                                                      She was born and raised in Miami, Florida, with her older sister Sarah. Madeline love animals, horror movies, and playing video games with her friends. 
                                                                      She joined the Colorado Summer Institute in Biostatistics for the summer of 2025 to gain real experience in the field of biostatistics,
                                                                      and to see if this is a career she wishes to pursue.")
                         )
                       ),
                       br(),
                       
                       fluidRow(
                         column(3,
                           img(src = "Pressier_Jack.jpg", height = "150px", style = "display:block; margin:auto;")
                         ),
                         column(9,h4("John Preisser (he/him)"), p("Jack Preisser is a rising senior majoring in statistics at Carleton College. He was born and raised in Chapel Hill, North Carolina along with 
                                                                    his twin sister, Hannah. Jack enjoys running, hiking and other physical activities. He has enjoyed his time at the Colorado Summer Institute in Biostatistics (CoSIBS)
                                                                  because he has not studied public health through an in-depth manner prior to CoSIBS. ")
                         )
                       ),
                       
                       tags$hr(style = "border-top: 2px dashed #ccc; margin: 15px 0;"),
                       
                       # SAMANTHAS
                       fluidRow(
                         column(6,
                                div(style = "text-align:center;", img(src = "Bothwell_Samantha.jpg", height = "200px"), h3("Samantha Bothwell, MS (she/her)"), 
                                    p("Group Lead; Research Scientist"), 
                                    p("Samantha Bothwell is a biostatistician in the Department of Pediatrics at the University of Colorado. She has been working with the eXtraordinarY Babies Study team since 2023. She earned her Master's degree in Biostatistics in 2021 and is currently pursuing her PhD.
                                      Outside of work and school, she enjoys rock climbing, crocheting, hiking 14ers with her dog Maizie (though she says Maizie is faster than she is), and unwinding with a healthy dose of reality TV.")
                                )
                         ),
                         column(6,
                                div(style = "text-align:center;", img(src = "Roberts_Samantha.jpg", height = "200px"), h3("Samantha Roberts, MS, MPH (she/her)"), p("Group Lead; Research Scientist"),
                                    p("Samantha Roberts is a biostatistician with the Center for Innovative Design and Analysis since 2021, first as a master's research assistant, then as a research scientist.
                                      She earned her Master's degree in Biostatistics in 2022 and her Master's degree in Public Health in 2012. When not working, she likes to read, hike and hang out with her two kids and husband.")
                                )
                         )
                       ),
                       
                       br(), hr(), br()
                ) # tend tab 6
            ) # end tabset Panel
)# end UI

##### Defining Server logic #####
server <- function(input, output, session) {
  
  ### For Tab 1 ###
  
  output$dynamic_violin_ui <- renderUI({
    # Set height/width based on plot_choice
    height <- switch(input$plot_choice,
                     "Composite" = "667px",
                     "Scaled" = "1000px",
                     "GSV" = "1000px")
    
    width <- switch(input$plot_choice,
                    "Composite" = "75%",
                    "Scaled" = "75%",
                    "GSV" = "75%")
    
    plotOutput("dynamic_violin_plot", height = height, width = width)
  })
  
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
          legend.margin = margin(t = 10, r = 20, b = 10, l = 20),
          text = element_text(family = "Arial")
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
          lower = comp_ref_mean - 1.96*comp_ref_sd,
          middle = comp_ref_mean,
          upper = comp_ref_mean + 1.96*comp_ref_sd
        )
      
      p1 <- composite_long_filtered %>%
        filter(domain == "Cognitive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_comp)) %>%
        plot_base() +
        labs(title = "Cognitive") +
        guides(fill = "none")
      
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
                           linetype = "Population Mean"),
                       inherit.aes = FALSE, color = "black", size = .75),
          
          geom_segment(data = ref_box_data_composite,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = lower, yend = lower,
                           linetype = "Population 95% Conf. Int."),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          geom_segment(data = ref_box_data_composite,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = upper, yend = upper,
                           linetype = "Population 95% Conf. Int."),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          scale_linetype_manual(
            name = paste0("Reference Lines \n(Mean = ", comp_ref_mean, ")"),
            values = ref_line_types
          )
        )
        p1 <- p1 + ref_geoms + 
          scale_y_continuous(limits = c(30, 150), breaks = seq(30, 150, 30))
        p2 <- p2 + ref_geoms + 
          scale_y_continuous(limits = c(30, 150), breaks = seq(30, 150, 30))
        p3 <- p3 + ref_geoms + 
          scale_y_continuous(limits = c(30, 150), breaks = seq(30, 150, 30))
      }
      
      if (input$overlay == "Yes") {
        overlay_geom <- list(
          geom_jitter(aes(fill = as.numeric(bsid_age_calc)),
                      shape = 21, 
                      color = "black", 
                      size = 3, 
                      alpha = 0.9),
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
      
      
      # Make a blank spacer plot 
      white_spacer <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA))
      
      # Extract legend from p1
      legend <- cowplot::get_legend(p1)
      
      # Remove legend from p1 plot itself to avoid duplication
      p1 <- p1 + theme(legend.position = "none")
    
      
      top_row <- plot_grid(
        white_spacer,  # Spacer to center p1
        p1,           # Main plot
        legend,       # Legend
        ncol = 3,
        rel_widths = c(1, 2, 1),  # Adjust to center p1 and avoid overlap
        align = "h"
      )
      
      
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
          lower = scale_ref_mean - 1.96*scale_ref_sd,
          middle = scale_ref_mean,
          upper = scale_ref_mean + 1.96*scale_ref_sd
        )
      
      p1 <- scaled_long_filtered %>%
        filter(domain == "Cognitive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() + labs(title = "Cognitive")
      
      p2 <- scaled_long_filtered %>%
        filter(domain == "Receptive Communication") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() + labs(title = "Receptive Communication") +
        theme(legend.position = "none")
      
      p3 <- scaled_long_filtered %>%
        filter(domain == "Expressive Communication") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_scale)) %>%
        plot_base() + 
        labs(title = "Expressive Communication") + 
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
                           linetype = "Population Mean"),
                       inherit.aes = FALSE, color = "black", size = .75),
          
          geom_segment(data = ref_box_data_scaled,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = lower, yend = lower,
                           linetype = "Population 95% Conf. Int."),
                       inherit.aes = FALSE, color = "black", size = .5),
          
          geom_segment(data = ref_box_data_scaled,
                       aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
                           y = upper, yend = upper,
                           linetype = "Population 95% Conf. Int."),
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
                      shape = 21, color = "black", size = 3, 
                      alpha = 0.9),
          scale_fill_gradient(name = "Age at Assessment (Months)", low = "white", high = "black")
        )
        p1 <- p1 + ggnewscale::new_scale_fill() + overlay_geom
        p2 <- p2 + ggnewscale::new_scale_fill() + overlay_geom
        p3 <- p3 + ggnewscale::new_scale_fill() + overlay_geom
        p4 <- p4 + ggnewscale::new_scale_fill() + overlay_geom
        p5 <- p5 + ggnewscale::new_scale_fill() + overlay_geom
      }
      
      
      # Make a blank spacer plot 
      white_spacer <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA))
      
      # Extract legend from p1
      legend <- cowplot::get_legend(p1)
      
      # Remove legend from p1 plot itself to avoid duplication
      p1 <- p1 + theme(legend.position = "none")
      
      top_row <- plot_grid(
        white_spacer,         # empty space for centering
        p1,                    # your main plot
        legend,                # the legend
        ncol = 3,
        rel_widths = c(1, 2, 1)  # adjust as needed to center p1
      )
      
      
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
          lower = gsv_ref_mean - 1.96*gsv_ref_sd,
          middle = gsv_ref_mean,
          upper = gsv_ref_mean + 1.96*gsv_ref_sd
        )
      
      p1 <- new_gsv_long_rem %>%
        filter(domain == "Cognitive") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
        plot_base() +
        labs(title = "Cognitive")
      
      p2 <- new_gsv_long_rem %>%
        filter(domain == "Receptive Communication") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
        plot_base() +
        labs(title = "Receptive Communication") +
        theme(legend.position = "none")
      
      p3 <- new_gsv_long_rem %>%
        filter(domain == "Expressive Communication") %>%
        mutate(sca_condition = factor(sca_condition, levels = ordered_groups_gsv)) %>%
        plot_base() +
        labs(title = "Expressive Communication") +
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
      
      #if (input$show_reference == "Yes") {
        # Going to remove because they don't make sense for GSV without age
        
        # ref_geoms <- list(
        #   geom_segment(data = ref_box_data_gsv,
        #                aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
        #                    y = middle, yend = middle,
        #                    linetype = "Population Mean"),
        #                inherit.aes = FALSE, color = "black", size = .75),
        #   
        #   geom_segment(data = ref_box_data_gsv,
        #                aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
        #                    y = lower, yend = lower,
        #                    linetype = "Population 95% Conf. Int."),
        #                inherit.aes = FALSE, color = "black", size = .5),
        #   
        #   geom_segment(data = ref_box_data_gsv,
        #                aes(x = x_numeric - 0.2, xend = x_numeric + 0.2,
        #                    y = upper, yend = upper,
        #                    linetype = "Population 95% Conf. Int."),
        #                inherit.aes = FALSE, color = "black", size = .5),
        #   
        #   scale_linetype_manual(
        #     name = paste0("Reference Lines (Mean = ", gsv_ref_mean, ")"),
        #     values = ref_line_types
        #   )
        #)
        # p1 <- p1 + ref_geoms
        # p2 <- p2 + ref_geoms
        # p3 <- p3 + ref_geoms
        # p4 <- p4 + ref_geoms
        # p5 <- p5 + ref_geoms
      #}
      
      
      if (input$overlay == "Yes") {
        overlay_geom <- list(
          geom_jitter(aes(name = "Age at Assessment (months)",fill = bsid_age_calc),
                      shape = 21, color = "black", size = 3, 
                      alpha = 0.9),
          scale_fill_gradient(name = "Age at Assessment (months)", low = "white", high = "black"))
        
        p1 <- p1 + ggnewscale::new_scale_fill() + overlay_geom
        p2 <- p2 + ggnewscale::new_scale_fill() + overlay_geom
        p3 <- p3 + ggnewscale::new_scale_fill() + overlay_geom
        p4 <- p4 + ggnewscale::new_scale_fill() + overlay_geom
        p5 <- p5 + ggnewscale::new_scale_fill() + overlay_geom
      }
      
      
      # Make a blank spacer plot 
      white_spacer <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white", color = NA))
      
      # Extract legend from p1
      legend <- cowplot::get_legend(p1)
      
      # Remove legend from p1 plot itself to avoid duplication
      p1 <- p1 + theme(legend.position = "none")
      
      # Show text explaining no reference lines
      if (input$show_reference == "Yes") {
        legend_text <- cowplot::ggdraw() + 
          draw_label("Population Norms are not computed as \nGSV scores increase non-linearly as a \nfunction of age",
                     size = 16, hjust = 0, x = 0)
        
        # Stack legend and its text
        legend_with_text <- plot_grid(legend_text, 
                                      legend_shifted <- ggdraw() +
                                        draw_grob(legend, x = 0, y = 0.05, width = 1, height = 1),  # adjust y as needed,
                                      ncol = 1,
                                      rel_heights = c(0.4, 1))  # adjust spacing as needed
        
        top_row <- plot_grid(
          white_spacer,         # empty space for centering
          p1,                    # your main plot
          legend_with_text,                # the legend
          ncol = 3,
          rel_widths = c(1, 2, 1)  # adjust as needed to center p1
        )

      }else{ 
        
        top_row <- plot_grid(
          white_spacer,         # empty space for centering
          p1,                    # your main plot
          legend,                # the legend
          ncol = 3,
          rel_widths = c(1, 2, 1)  # adjust as needed to center p1
        )
      
      }
      
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
      from = max(5, min(filtered_data$bsid_age_calc, na.rm = TRUE)),
      to = max(filtered_data$bsid_age_calc, na.rm = TRUE),
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
    
    # ---- Pivot long for ggplot ----
    pred_long <- pivot_longer(lms_mod, -age,
                              names_to = "Percentile", values_to = "Score")
    
    # ---- Plot ----
    p <- ggplot() +
      # 10–90 ribbon
      geom_ribbon(data = lms_mod, aes(x = age, ymin = P10, ymax = P90),
                  fill = "gray85", alpha = 0.3) +
      
      # 50th percentile: thick black solid
      geom_smooth(data = lms_mod, aes(x = age, y = P50),
                  color = "black", size = 1.6, linetype = "solid") +
      
      # 10th & 90th percentiles: blue dashed
      geom_smooth(data = lms_mod, aes(x = age, y = P10),
                  color = "slateblue1", size = 1.1, linetype = "dashed") +
      geom_smooth(data = lms_mod, aes(x = age, y = P90),
                  color = "slateblue1", size = 1.1, linetype = "dashed") +
      
      # 25th & 75th percentiles: gray, lighter lines
      geom_smooth(data = lms_mod, aes(x = age, y = P25),
                  color = "snow3", size = 0.8, linetype = "solid") +
      geom_smooth(data = lms_mod, aes(x = age, y = P75),
                  color = "snow3", size = 0.8, linetype = "solid") +
      
      # Theme and labels
      theme_bw(base_size = 22) +
      theme(text = element_text(family = "Arial")) +
      #theme(text = element_text(size = 20))
      labs(
        title = paste("Bayley", input$domain_select, "GSV Growth Curve"),
        x = "Age at Assessment (months)",
        y = "Bayley-4 Score"
      )
    
    if (input$show_points) {
      p <- p+geom_point(data = filtered_data,
                        aes(x = bsid_age_calc, y = transformed_score),
                        inherit.aes = FALSE,
                        alpha = 0.5, size = 3)
    }
    p
  })
  
  ### For Tab 3 ###
      input_milestones_data <- reactiveVal(data.frame(# creates reactive dataFrame that takes in the user inputs of milestone values
        milestone = character(),
        months_WhenAchieved = numeric(),
        Percentile = numeric(),
        stringsAsFactors = FALSE
      ))
      
      # Render user-submitted table
      output$milestones_table_output <- renderDT({
        
        df <- input_milestones_data()
        
        if (nrow(df) == 0) {
          empty_df <- tibble::tibble(
            Milestone = character(0),
            `Age Milestone Achieved (months)` = numeric(0),
            Percentile = numeric(0)
          )
          
          return(DT::datatable(empty_df,
                               options = list(dom = 't'),
                               rownames = FALSE
          ))
        }
        
        
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
                  editable = TRUE,
                  class = 'display'
        )
      })
      
      # Initialize container
      edited_data <- reactiveVal()
      
      # Populate once original data is available
      observe({
        req(input_milestones_data())
        edited_data(input_milestones_data())
      })
      
      # Capture edits
      observeEvent(input$milestones_table_output_cell_edit, {
        info <- input$milestones_table_output_cell_edit
        df <- edited_data()
        
        row <- info$row
        col <- info$col + 1  # convert from 0-based to 1-based
        value <- info$value
        colname <- names(df)[col]
        
        # Handle numeric conversion if needed
        if (is.numeric(df[[colname]])) {
          value <- as.numeric(value)
        }
        
        df[row, col] <- value
        edited_data(df)  # Save back
      })
      
      # Use in plot
      output$indiv_perc <- renderPlotly({
        user_points <- edited_data()
        # Continue with plot code using user_points
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
            # Make higher ages lower percentiles
            mutate(Percentile = 100 - Percentile) %>%
            ungroup() %>% # fully no clue why I do this %>% 
            mutate(
              symbol = case_when( # adds different markers to plot based on percentile calculated
                Percentile > 25 ~ "circle",
                Percentile > 10 ~ "diamond",
                TRUE ~ "x"
              ),
              color = case_when(
                Percentile > 25 ~ "green",
                Percentile > 10 ~ "orange",
                TRUE ~ "red"
              )
            )
          
          
          
          # Replace or add logic
          existing <- input_milestones_data()
          existing <- existing[!existing$milestone %in% combined$milestone, ]  # remove any matching milestone
          updated_data <- bind_rows(existing, combined)  # then add new/updated rows
          input_milestones_data(updated_data)
          
        } else{
          showNotification("Please fill in at least one milestone before plotting", type = "error") # throws error just in case
        }
        
      })
      
      observeEvent(input$clear_milestones, {
        showModal(modalDialog(
          title = "Confirm Clear",
          "Are you sure you want to clear all milestone inputs and remove them from the plot and table?",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_clear_milestones", "Yes, clear", class = "btn-danger")
          )
        ))
      })
      
      # Step 2: If user confirms, actually clear data
      observeEvent(input$confirm_clear_milestones, {
        removeModal()
        
        # Clear milestone data
        input_milestones_data(data.frame(
          milestone = character(),
          months_WhenAchieved = numeric(),
          Percentile = numeric(),
          stringsAsFactors = FALSE
        ))
        
        # Reset all milestone input boxes (AgeWhen_xxx)
        lapply(milestones_list, function(milestone) {
          input_id <- paste0("AgeWhen_", gsub(" ", "", milestone))
          if (!is.null(input[[input_id]])) {
            updateNumericInput(session, input_id, value = NA)
          }
        })
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
        
        # Make lower percentiles indicate a delay
        sca_milestones$Percentile = 100 - sca_milestones$Percentile
        indiv_dat$Percentile = 100 - indiv_dat$Percentile
        # makes sure datasets are ordered  by percentiles
        sca_milestones$milestone <- factor(sca_milestones$milestone, levels = ordered_levels)
        indiv_dat$milestone <- factor(indiv_dat$milestone, levels = ordered_levels)
        
        color = case_when(input$sca_condition == "All SCTs" ~ "lightblue", 
                          input$sca_condition == "XXY" ~ "#fdb863", 
                          input$sca_condition == "XYY" ~ "cyan3", 
                          input$sca_condition == "XXX" ~ "#4B0082")
        
        milestone_input_plot <- plot_ly(sca_milestones, 
                                        y = ~milestone, 
                                        x = ~Percentile, 
                                        color = I(color),
                                        type = "box", 
                                        boxpoints = FALSE,
                                        hoverinfo = "skip",
                                        showlegend = F) %>%
        layout(yaxis = list(tickfont = list(family = "Arial", size = 20)))
        
        # creates list of points to plot 
        user_points <- edited_data()  
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
          layout(xaxis = list(title = "Percentile", range = c(0, 100), titlefont = list(size=20)),
                 yaxis = list(title = " "),
                 title = list(text="Individual Milestones Achievement Percentiles", font = list(size = 20)),
                 margin = list(t = 40),
                 shapes = list(
                   list(type = "rect", fillcolor = "rgba(255, 0, 0, 0.2)", 
                        line = list(color = "red", width = 0), x0 = 0, x1 = 10, y0 = 0, y1 = 1, xref = "x", yref = "paper")
                 )
          )
        
      }) # closes milestone plot
  
  
  ### For Tab 4 ###
      global_user_data <- reactiveValues(df = data.frame(Age = numeric(), Score = numeric(), domain = character()))
      
      observeEvent(input$save_inputs, {
        new_points <- data.frame(
          Age = c(input$age1, input$age2, input$age3, input$age4),
          Score = c(input$score1, input$score2, input$score3, input$score4)
        ) %>%
          filter(!is.na(Age), !is.na(Score)) %>%
          mutate(Domain = input$domain_select)
        
        global_user_data$df <- new_points
      })
      
      
      
      output$GAMLSS_table <- renderDT({
        
        datatable(global_user_data$df, extensions = "Buttons",
                  options = list(pageLength = 5,
                                 dom = 'Bfrtip',  # B = Buttons, f = filter, r = processing, t = table, i = info, p = pagination
                                 buttons = list(
                                   list(extend = 'csv', filename = 'GSV_Scores'),#options to print
                                   list(extend = 'pdf', filename = 'GSV_Scores'),
                                   list(extend = 'print', title = 'GSV_Scores')),
                                 lengthMenu = c(5, 10)), 
                  class = 'display'
        )
      })
      
      observeEvent(input$clear_inputs, {
        showModal(modalDialog(
        title = "Confirm Clear",
        "Are you sure you want to clear all input data?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear", "Yes, clear", class = "btn-danger")
                        )
                      ))
        })
      
      observeEvent(input$confirm_clear, {
        removeModal()
        
        updateNumericInput(session, "age1", value = NA)
        updateNumericInput(session, "score1", value = NA)
        updateNumericInput(session, "age2", value = NA)
        updateNumericInput(session, "score2", value = NA)
        updateNumericInput(session, "age3", value = NA)
        updateNumericInput(session, "score3", value = NA)
        updateNumericInput(session, "age4", value = NA)
        updateNumericInput(session, "score4", value = NA)
        
        global_user_data$df <- data.frame(Age = numeric(), Score = numeric(), Domain = character())
      })
      
      
      output$input_growth_plot <- renderPlot({
        user_df <- global_user_data$df
        
        # Return nothing if not enough points
        #if (nrow(user_df) <= 3) return(NULL)
        
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
          from = max(5, min(filtered_data$bsid_age_calc, na.rm = TRUE)),
          to = max(filtered_data$bsid_age_calc, na.rm = TRUE),
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
        
        # ---- Pivot long for ggplot ----
        pred_long <- pivot_longer(lms_mod, -age,
                                  names_to = "Percentile", values_to = "Score")
        
        # ---- Plot ----
        p <- ggplot() +
          # 10–90 ribbon
          geom_ribbon(data = lms_mod, aes(x = age, ymin = P10, ymax = P90),
                      fill = "gray85", alpha = 0.3) +
          
          # 50th percentile: thick black solid
          geom_smooth(data = lms_mod, aes(x = age, y = P50),
                      color = "black", size = 1.6, linetype = "solid") +
          
          # 10th & 90th percentiles: blue dashed
          geom_smooth(data = lms_mod, aes(x = age, y = P10),
                      color = "slateblue1", size = 1.1, linetype = "dashed") +
          geom_smooth(data = lms_mod, aes(x = age, y = P90),
                      color = "slateblue1", size = 1.1, linetype = "dashed") +
          
          # 25th & 75th percentiles: gray, lighter lines
          geom_smooth(data = lms_mod, aes(x = age, y = P25),
                      color = "snow3", size = 0.8, linetype = "solid") +
          geom_smooth(data = lms_mod, aes(x = age, y = P75),
                      color = "snow3", size = 0.8, linetype = "solid") +
          
          # Theme and labels
          theme_bw(base_size = 22) +
          theme(text = element_text(family = "Arial")) +
          #theme(text = element_text(size = 20))
          labs(
            title = paste("Bayley", input$domain_select, "GSV Growth Curve"),
            x = "Age at Assessment (months)",
            y = "Bayley-IV Score"
          )
        
        # Add user input trajectory line
        p <- p + geom_path(data = user_df,
                           aes(x = Age, y = Score),
                           color = "maroon2", size = 1.8)+
          geom_smooth()
        
        
        #Add user input points
        p <- p + geom_point(data = user_df,
                            aes(x = Age, y = Score),
                            color = "maroon1", size = 6)

        
        
        p
        
      })
      
      
  ### For Tab 5 ###
      output$image_ui <- renderImage({
        src = file.path("grossmotor.jpg")
        
      })
     
  ### For Tab 6 ###
      # no server logic, all displayed outputs are static images and text
} # end server

##### Run the application #####
shinyApp(ui = ui, server = server)