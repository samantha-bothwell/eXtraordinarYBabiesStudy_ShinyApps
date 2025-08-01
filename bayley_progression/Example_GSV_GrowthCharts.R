
#######################
###### Running GAMLSS 
#######################

rm(list = ls())

library(tidyverse)


gsv <- readRDS("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/eXtraordinarYBabiesStudy_ShinyApps/bayley_progression/Bayley_GSV_scores.rds")

## Plot it
ggplot(gsv, aes(x = bsid_age_calc, y = bsid_gsv_cog)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  theme_bw(base_size = 18) + 
  labs(x = "Age (Months)", y = "Bayley Cognition GSV")

## Remove NA values 
gsv_nona_cog <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_cog)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_cog) %>% 
  mutate(bsid_gsv_cog4 = ((bsid_gsv_cog - 500)/100)*25 + 500)


scaled_lang <- readRDS("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/eXtraordinarYBabiesStudy_ShinyApps/bayley_progression/Bayley_Scaled_scores.rds") %>% 
  dplyr::select(bsid_rc_scaled, bsid_ec_scaled) %>% 
  pivot_longer(everything(.), names_to = "Domain", values_to = "score") %>% 
  mutate(score = ifelse(score > 20, NA, score), 
         Domain = ifelse(Domain == "bsid_rc_scaled", "Receptive\nLanguage", "Expressive\nLanguage"))

ggplot(scaled_lang, aes(x = Domain, y = score, fill = Domain)) + 
  geom_hline(yintercept = 7, linetype = "dashed", color = "red3", size = 2) + 
  geom_violin(alpha = 0.7) + 
  theme_bw(base_size = 18) + theme(legend.position = "none") + labs(x = "", y = "Scaled Score") + 
  scale_y_continuous(limits = c(1, 18), breaks = seq(3, 18, 3))


comp_lang <- readRDS("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/eXtraordinarYBabiesStudy_ShinyApps/bayley_progression/Bayley_Composite_scores.rds")

ggplot(comp_lang[comp_lang$bsid_lang_composite < 200,], aes(x = "", y = bsid_lang_composite)) + 
  geom_hline(yintercept = 85, linetype = "dashed", color = "red3", size = 2) + 
  geom_violin(alpha = 0.7, fill = "orchid3") + 
  theme_bw(base_size = 18) + theme(legend.position = "none") + labs(x = "Language", y = "Composite Score") 

## Run gamlss
library(gamlss)

gamlss_cog <- gamlss(
  formula = bsid_gsv_cog4 ~ pb(bsid_age_calc, lambda = 5),
  sigma.formula = ~ pb(bsid_age_calc),
  nu.formula = ~1,
  tau.formula = ~1,
  data = gsv_nona_cog,
  family = BCCG() 
)


# 1. Prediction ages
ages <- seq(2, 41, by = 0.5)

# 2. Build newdata with all required covariates used in the model
newdata <- data.frame(bsid_age_calc = ages)

# 3. Get predicted distribution parameters from the model
params <- predictAll(gamlss_cog, newdata = newdata)

# 4. Calculate centiles manually using the BCCG distribution quantile function
centiles <- c(5, 10, 25, 50, 75, 90, 95)
q_vals <- sapply(centiles / 100, function(p) {
  qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
})

# 5. Create a dataframe of predicted centiles
lms_cog <- data.frame(age = ages, q_vals)
colnames(lms_cog)[-1] <- paste0("P", centiles)


## Plot it
ggplot(gsv_nona_cog, aes(x = bsid_age_calc, y = bsid_gsv_cog4)) + 
  geom_point() + 
  geom_smooth(data = lms_cog, aes(x = age, y = P5)) +  
  geom_smooth(data = lms_cog, aes(x = age, y = P25)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P50)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P75)) +  
  geom_smooth(data = lms_cog, aes(x = age, y = P95)) + 
  theme_bw(base_size = 18) + 
  labs(x = "Age (Months)", y = "Bayley Cognition GSV") + 
  annotate("text", x = 42.5, y = 530, label = "5th", size = 5, color = "blue3") + 
  annotate("text", x = 42.5, y = 536, label = "25th", size = 5, color = "blue3") + 
  annotate("text", x = 42.5, y = 539, label = "50th", size = 5, color = "blue3") + 
  annotate("text", x = 42.5, y = 542, label = "75th", size = 5, color = "blue3") + 
  annotate("text", x = 42.5, y = 545.5, label = "95th", size = 5, color = "blue3")



