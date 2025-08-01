
#######################
###### Running GAMLSS 
#######################

rm(list = ls())

library(tidyverse)


gsv <- readRDS("/Volumes/Shared/Shared Projects/Bothwell/Peds ENDO/SCA Studies/eXtraordinarYBabiesStudy_ShinyApps/bayley_progression/Bayley_GSV_scores.rds")

## Plot it
ggplot(gsv, aes(x = bsid_age_calc, y = bsid_gsv_rc)) + 
  geom_point() + 
  geom_smooth(se = F)

## Remove NA values 
gsv_nona_cog <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_cog)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_cog) %>% 
  mutate(bsid_gsv_cog4 = ((bsid_gsv_cog - 500)/100)*25 + 500)


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
  geom_smooth(data = lms_cog, aes(x = age, y = P10)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P25)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P50)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P75)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P90)) + 
  geom_smooth(data = lms_cog, aes(x = age, y = P95)) 
