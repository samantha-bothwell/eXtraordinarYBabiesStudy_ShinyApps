#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

compositescores <- readRDS("Bayley_Composite_scores.rds")
gsvscores <- readRDS("Bayley_GSV_scores.rds")
scaledscores <- readRDS("Bayley_Scaled_scores.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(
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
  )
)
        




# Define server logic required to draw a histogram
server <- function(input, output) {

  output$image_ui <- renderImage({
    src = file.path("grossmotor.jpg")
    
  })
  output$image_ui2 <- renderImage({
    src = file.path("eBs_Logo.jpg")
    })
 
}

#if(input$methodology_choice == "GAMLSS")  

  

# Run the application 


shinyApp(ui = ui, server = server)
