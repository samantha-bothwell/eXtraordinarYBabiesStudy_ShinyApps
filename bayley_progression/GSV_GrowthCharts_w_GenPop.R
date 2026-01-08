
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
  #filter(sca_condition == "XYY") %>% 
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


gen_pop <- read.csv("/Volumes/Shared/Shared Projects/Laura/Peds Endo/Tartaglia/Growth curves/Bayley3 data for N Tartaglia.csv") %>% 
  mutate(`X5th.Percentile` = ((`X5th.Percentile` - 500)/100)*25 + 500,
         `X95th.Percentile` = ((`X95th.Percentile` - 500)/100)*25 + 500) %>% 
  mutate(
    # Split into start and end
    age_start = str_split(agebd, " to ", simplify = TRUE)[,1],
    age_end   = str_split(agebd, " to ", simplify = TRUE)[,2],
    
    # Convert Y:MM:DD to numeric months
    age_start_months = as.numeric(str_extract(age_start, "^[0-9]+")) * 12 +
      as.numeric(str_extract(age_start, "(?<=:)[0-9]+(?=:)")) +
      as.numeric(str_extract(age_start, "[0-9]+$")) / 30.44,
    
    age_end_months = as.numeric(str_extract(age_end, "^[0-9]+")) * 12 +
      as.numeric(str_extract(age_end, "(?<=:)[0-9]+(?=:)")) +
      as.numeric(str_extract(age_end, "[0-9]+$")) / 30.44,
    
    # Average of start and end
    age_months = (age_start_months + age_end_months) / 2
  )

## Run gamlss
library(gamlss)

gsv_nona_cog <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(sca_condition == "XXX") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_cog)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_cog) %>% 
  mutate(bsid_gsv_cog4 = ((bsid_gsv_cog - 500)/100)*25 + 500)
gen_cg <- gen_pop[gen_pop$subtests == "cg",]

gamlss_cog <- gamlss(
  formula = bsid_gsv_cog4 ~ pb(bsid_age_calc),
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



ggplot() +
  # 5–95 ribbon from lms_cog (existing gray)
  geom_ribbon(data = lms_cog, aes(x = age, ymin = P5, ymax = P95),
              fill = "gray85", alpha = 0.3) +
  
  # Orange ribbon from gen_cg
  geom_ribbon(data = gen_cg, aes(x = age_start_months, ymin = `X5th.Percentile`, ymax = `X95th.Percentile`),
              fill = "orange", alpha = 0.3) +
  
  # 50th percentile: thick black solid
  geom_smooth(data = lms_cog, aes(x = age, y = P50),
              color = "black", size = 1.6, linetype = "solid") +
  
  # 10th & 90th percentiles: blue dashed
  geom_smooth(data = lms_cog, aes(x = age, y = P5),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  geom_smooth(data = lms_cog, aes(x = age, y = P95),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  
  # 25th & 75th percentiles: gray, lighter lines
  geom_smooth(data = lms_cog, aes(x = age, y = P25),
              color = "snow3", size = 0.8, linetype = "solid") +
  geom_smooth(data = lms_cog, aes(x = age, y = P75),
              color = "snow3", size = 0.8, linetype = "solid") +
  
  # Theme and labels
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Bayley Cognition GSV Growth Curve \n(XXX Only)",
    x = "Age at Assessment (months)",
    y = "Bayley-4 Score"
  ) + 
  scale_x_continuous(limits = c(5.5, 39), breaks = seq(6, 36, by = 6)) + 
  scale_y_continuous(limits = c(450, 550), breaks = seq(450, 550, by = 25))




##### Expressive Communication 
## Remove NA values 
gsv_nona_ec <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(sca_condition == "XXX") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_ec)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_ec) %>% 
  mutate(bsid_gsv_ec4 = ((bsid_gsv_ec - 500)/100)*25 + 500)
gen_ec <- gen_pop[gen_pop$subtests == "ec",]

## Run gamlss
gamlss_ec <- gamlss(
  formula = bsid_gsv_ec4 ~ pb(bsid_age_calc),
  sigma.formula = ~ pb(bsid_age_calc),
  nu.formula = ~1,
  tau.formula = ~1,
  data = gsv_nona_ec,
  family = BCCG() 
)

# 3. Get predicted distribution parameters from the model
params <- predictAll(gamlss_ec, newdata = newdata)
# 4. Calculate centiles manually using the BCCG distribution quantile function
centiles <- c(5, 10, 25, 50, 75, 90, 95)
q_vals <- sapply(centiles / 100, function(p) {
  qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
})
# 5. Create a dataframe of predicted centiles
lms_ec <- data.frame(age = ages, q_vals)
colnames(lms_ec)[-1] <- paste0("P", centiles)



ggplot() +
  # 5–95 ribbon (existing gray)
  geom_ribbon(data = lms_ec, aes(x = age, ymin = P5, ymax = P95),
              fill = "gray85", alpha = 0.3) +
  
  # Orange ribbon from general population
  geom_ribbon(data = gen_ec, aes(x = age_start_months, ymin = `X5th.Percentile`, ymax = `X95th.Percentile`),
              fill = "orange", alpha = 0.3) +
  
  # 50th percentile: thick black solid
  geom_smooth(data = lms_ec, aes(x = age, y = P50),
              color = "black", size = 1.6, linetype = "solid") +
  
  # 10th & 90th percentiles: blue dashed
  geom_smooth(data = lms_ec, aes(x = age, y = P5),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  geom_smooth(data = lms_ec, aes(x = age, y = P95),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  
  # 25th & 75th percentiles: gray, lighter lines
  geom_smooth(data = lms_ec, aes(x = age, y = P25),
              color = "snow3", size = 0.8, linetype = "solid") +
  geom_smooth(data = lms_ec, aes(x = age, y = P75),
              color = "snow3", size = 0.8, linetype = "solid") +
  
  # Theme and labels
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Bayley Expressive Communication GSV Growth Curve \n(XXX Only)",
    x = "Age at Assessment (months)",
    y = "Bayley-4 Score"
  ) + 
  scale_x_continuous(limits = c(5.5, 39), breaks = seq(6, 36, by = 6))





##### Receptive Communication 
## Remove NA values 
gsv_nona_rc <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(sca_condition == "XXX") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_rc)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_rc) %>% 
  mutate(bsid_gsv_rc4 = ((bsid_gsv_rc - 500)/100)*25 + 500)
gen_rc <- gen_pop[gen_pop$subtests == "rc",]

## Run gamlss
gamlss_rc <- gamlss(
  formula = bsid_gsv_rc4 ~ pb(bsid_age_calc),
  sigma.formula = ~ pb(bsid_age_calc),
  nu.formula = ~1,
  tau.formula = ~1,
  data = gsv_nona_rc,
  family = BCCG() 
)

# 3. Get predicted distribution parameters from the model
params <- predictAll(gamlss_rc, newdata = newdata)
# 4. Calculate centiles manually using the BCCG distribution quantile function
centiles <- c(5, 10, 25, 50, 75, 90, 95)
q_vals <- sapply(centiles / 100, function(p) {
  qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
})
# 5. Create a dataframe of predicted centiles
lms_rc <- data.frame(age = ages, q_vals)
colnames(lms_rc)[-1] <- paste0("P", centiles)



ggplot() +
  # 5–95 ribbon (existing gray)
  geom_ribbon(data = lms_rc, aes(x = age, ymin = P5, ymax = P95),
              fill = "gray85", alpha = 0.3) +
  
  # Orange ribbon from general population
  geom_ribbon(data = gen_rc, aes(x = age_start_months, ymin = `X5th.Percentile`, ymax = `X95th.Percentile`),
              fill = "orange", alpha = 0.3) +
  
  # 50th percentile: thick black solid
  geom_smooth(data = lms_rc, aes(x = age, y = P50),
              color = "black", size = 1.6, linetype = "solid") +
  
  # 10th & 90th percentiles: blue dashed
  geom_smooth(data = lms_rc, aes(x = age, y = P5),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  geom_smooth(data = lms_rc, aes(x = age, y = P95),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  
  # 25th & 75th percentiles: gray, lighter lines
  geom_smooth(data = lms_rc, aes(x = age, y = P25),
              color = "snow3", size = 0.8, linetype = "solid") +
  geom_smooth(data = lms_rc, aes(x = age, y = P75),
              color = "snow3", size = 0.8, linetype = "solid") +
  
  # Theme and labels
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Bayley Receptive Communication GSV Growth Curve \n(XXX Only)",
    x = "Age at Assessment (months)",
    y = "Bayley-4 Score"
  ) + 
  scale_x_continuous(limits = c(5.5, 39), breaks = seq(6, 36, by = 6)) + 
  scale_y_continuous(limits = c(450, 555), breaks = seq(450, 550, by = 25))



##### Fine Motor
## Remove NA values 
gsv_nona_fm <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(sca_condition == "XXX") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_fm)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_fm) %>% 
  mutate(bsid_gsv_fm4 = ((bsid_gsv_fm - 500)/100)*25 + 500)
gen_fm <- gen_pop[gen_pop$subtests == "fm",]

## Run gamlss
gamlss_fm <- gamlss(
  formula = bsid_gsv_fm4 ~ pb(bsid_age_calc),
  sigma.formula = ~ pb(bsid_age_calc),
  nu.formula = ~1,
  tau.formula = ~1,
  data = gsv_nona_fm,
  family = BCCG() 
)

# 3. Get predicted distribution parameters from the model
params <- predictAll(gamlss_fm, newdata = newdata)
# 4. Calculate centiles manually using the BCCG distribution quantile function
centiles <- c(5, 10, 25, 50, 75, 90, 95)
q_vals <- sapply(centiles / 100, function(p) {
  qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
})
# 5. Create a dataframe of predicted centiles
lms_fm <- data.frame(age = ages, q_vals)
colnames(lms_fm)[-1] <- paste0("P", centiles)



ggplot() +
  # 5–95 ribbon (existing gray)
  geom_ribbon(data = lms_fm, aes(x = age, ymin = P5, ymax = P95),
              fill = "gray85", alpha = 0.3) +
  
  # Orange ribbon from general population
  geom_ribbon(data = gen_fm, aes(x = age_start_months, ymin = `X5th.Percentile`, ymax = `X95th.Percentile`),
              fill = "orange", alpha = 0.3) +
  
  # 50th percentile: thick black solid
  geom_smooth(data = lms_fm, aes(x = age, y = P50),
              color = "black", size = 1.6, linetype = "solid") +
  
  # 10th & 90th percentiles: blue dashed
  geom_smooth(data = lms_fm, aes(x = age, y = P5),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  geom_smooth(data = lms_fm, aes(x = age, y = P95),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  
  # 25th & 75th percentiles: gray, lighter lines
  geom_smooth(data = lms_fm, aes(x = age, y = P25),
              color = "snow3", size = 0.8, linetype = "solid") +
  geom_smooth(data = lms_fm, aes(x = age, y = P75),
              color = "snow3", size = 0.8, linetype = "solid") +
  
  # Theme and labels
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Bayley Fine Motor GSV Growth Curve \n(XXX Only)",
    x = "Age at Assessment (months)",
    y = "Bayley-4 Score"
  ) + 
  scale_x_continuous(limits = c(5.5, 39), breaks = seq(6, 36, by = 6))




##### Gross Motor
## Remove NA values 
gsv_nona_gm <- gsv %>% 
  filter(redcap_event_name != "2_month_visit_arm_1") %>% 
  filter(sca_condition == "XXX") %>% 
  filter(!is.na(bsid_age_calc)) %>% 
  filter(!is.na(bsid_gsv_gm)) %>% 
  dplyr::select(bsid_age_calc, bsid_gsv_gm) %>% 
  mutate(bsid_gsv_gm4 = ((bsid_gsv_gm - 500)/100)*25 + 500)
gen_gm <- gen_pop[gen_pop$subtests == "gm",]

## Run gamlss
gamlss_gm <- gamlss(
  formula = bsid_gsv_gm4 ~ pb(bsid_age_calc),
  sigma.formula = ~ pb(bsid_age_calc),
  nu.formula = ~1,
  tau.formula = ~1,
  data = gsv_nona_gm,
  family = BCCG() 
)

# 3. Get predicted distribution parameters from the model
params <- predictAll(gamlss_gm, newdata = newdata)
# 4. Calculate centiles manually using the BCCG distribution quantile function
centiles <- c(5, 10, 25, 50, 75, 90, 95)
q_vals <- sapply(centiles / 100, function(p) {
  qBCCG(p, mu = params$mu, sigma = params$sigma, nu = params$nu)
})
# 5. Create a dataframe of predicted centiles
lms_gm <- data.frame(age = ages, q_vals)
colnames(lms_gm)[-1] <- paste0("P", centiles)



ggplot() +
  # 5–95 ribbon (existing gray)
  geom_ribbon(data = lms_gm, aes(x = age, ymin = P5, ymax = P95),
              fill = "gray85", alpha = 0.3) +
  
  # Orange ribbon from general population
  geom_ribbon(data = gen_gm, aes(x = age_start_months, ymin = `X5th.Percentile`, ymax = `X95th.Percentile`),
              fill = "orange", alpha = 0.3) +
  
  # 50th percentile: thick black solid
  geom_smooth(data = lms_gm, aes(x = age, y = P50),
              color = "black", size = 1.6, linetype = "solid") +
  
  # 10th & 90th percentiles: blue dashed
  geom_smooth(data = lms_gm, aes(x = age, y = P5),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  geom_smooth(data = lms_gm, aes(x = age, y = P95),
              color = "slateblue1", size = 1.1, linetype = "dashed") +
  
  # 25th & 75th percentiles: gray, lighter lines
  geom_smooth(data = lms_gm, aes(x = age, y = P25),
              color = "snow3", size = 0.8, linetype = "solid") +
  geom_smooth(data = lms_gm, aes(x = age, y = P75),
              color = "snow3", size = 0.8, linetype = "solid") +
  
  # Theme and labels
  theme_bw(base_size = 22) +
  theme(text = element_text(family = "Arial")) +
  labs(
    title = "Bayley Gross Motor GSV Growth Curve \n(XXX Only)",
    x = "Age at Assessment (months)",
    y = "Bayley-4 Score"
  ) + 
  scale_x_continuous(limits = c(5.5, 39), breaks = seq(6, 36, by = 6))



