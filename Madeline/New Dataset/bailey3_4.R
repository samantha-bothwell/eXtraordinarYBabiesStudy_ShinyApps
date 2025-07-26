###############
#Cleaning data#
###############

#Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)


#Data 
composite <-readRDS("Bayley_Composite_scores.rds")
gsv<- readRDS("Bayley_GSV_scores.rds")
scale <- readRDS("Bayley_Scaled_scores.rds")

#Pivot the data frames to long format and relabel to 
#"Cognitive", "Expressive", "Receptive", "Gross Motor", and "Fine Motor".

# Check how many unique IDs are in each data set
length(unique(composite$study_id_extraordinary)) #320
length(unique(gsv$study_id_extraordinary)) #175
length(unique(scale$study_id_extraordinary)) #320

#Now pivot and make a new column 

gsv_long <- gsv %>%
  pivot_longer(
    cols = c(bsid_gsv_cog, bsid_gsv_rc, bsid_gsv_ec, bsid_gsv_fm, bsid_gsv_gm),
    names_to = "domain_short",
    values_to = "value"
  ) %>%
  mutate(domain = recode(domain_short,
                         bsid_gsv_cog = "Cognitive",
                         bsid_gsv_ec = "Expressive",
                         bsid_gsv_rc = "Receptive",
                         bsid_gsv_gm = "Gross Motor",
                         bsid_gsv_fm = "Fine Motor"))


#Change the values from Bayley-III to Bayley-IV
gsv_long <- gsv_long %>%
  mutate(Bayley_IV = round(((value - 500) / 100) * 25 + 500), 0)


#Removing the 2-month visits because there are so few and they skew the data 

#Check what the name is 
unique(gsv_long$redcap_event_name)

#Remove
gsv_long_2month_remove <- gsv_long %>%
  filter(redcap_event_name != "2_month_visit_arm_1")


#PRETTY GRAPHS

#Bayley_VI vs. Age

#Fix the order of the month visits 
visit_order <- c(
  "6_month_visit_arm_1",
  "12_month_visit_arm_1",
  "24_month_visit_arm_1",
  "36_month_visit_arm_1"
)
gsv_long_2month_remove$redcap_event_name <- factor(
  gsv_long_2month_remove$redcap_event_name,
  levels = visit_order
)

ggplot(gsv_long_2month_remove, aes(x = redcap_event_name, y = Bayley_IV, color = sca_condition)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = sca_condition)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Bayley-IV Scores by Visit and SCA Condition",
    x = "Visit Time",
    y = "Bayley-IV Score",
    color = "SCA Condition"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )



#A Box plot of the same data 
ggplot(gsv_long_2month_remove, aes(x = redcap_event_name, y = Bayley_IV)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Bayley-IV Scores by Visit",
    x = "Visit Time",
    y = "Bayley-IV Score"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

#SCA condition vs. Bayley VI
ggplot(gsv_long_2month_remove, aes(x = sca_condition, y = Bayley_IV, fill = sca_condition)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.8) +  
  scale_fill_brewer(palette = "Pastel1") +  # softer colors
  labs(
    title = "Bayley IV Scores by SCA Condition",
    x = "SCA Condition",
    y = "Bayley IV Score",
    fill = "Condition"
  ) +
  theme_minimal(base_size = 13) +  # clean theme
  theme(
    legend.position = "none",  
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0)
  )


#SCA condition 

