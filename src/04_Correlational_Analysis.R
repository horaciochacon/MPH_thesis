library(ggbiplot)
library(dplyr)
library(readxl)
library(ggplot2)
library(sf)
library(units)
library(janitor)
library(tidyr)
library(corrplot)


# Read curated datasets ---------------------------------------------------
pop_cov   <- read.csv("data/pre_processed/pop_cov.csv")
map_prov  <- read.csv("data/pre_processed/map_prov.csv")
provinces <- read.csv("output/provinces_final.csv")
rt        <- read.csv("output/rt_peak_df.csv")
migration <- read.csv("data/pre_processed/migration.csv")
inforhus  <- read.csv("data/pre_processed/inforhus.csv")
ceplan    <- read.csv("data/pre_processed/ceplan.csv")
susalud   <- read.csv("data/pre_processed/susalud.csv")

# Processing final Provincial Dataset -------------------------------------
provinces <- provinces %>% 
  left_join(migration) %>% 
  left_join(rt) %>% 
  left_join(map_prov) %>% 
  left_join(pop_cov) %>% 
  left_join(inforhus) %>% 
  left_join(ceplan) %>% 
  left_join(susalud) %>% 
  mutate(
    mig_in_perc = retor_llegaron / pop,
    mig_out_perc = retor_salieron / pop,
    log_mort_cum = log(deaths / pop),
    dens_pop = pop / area,
    log_mort1 = mort_first_peak,
    log_mort2 = mort_second_peak,
    HDI_low = ifelse(HDI < 0.42, "Low HDI", "High HDI"),
    dens_pop = as.numeric(dens_pop),
    med_ratio = medico / pop,
    enf_ratio = enfermero / pop,
    bed_ratio = beds / pop,
    med2_ratio = physicians / pop
  ) %>% 
  tibble() %>% 
  select(
    prov_cdc, log_mort_cum, log_mort1, day_first_peak, log_mort2,
    day_second_peak, n_peak, dist_peaks, rt, day_rt = day, HDI, HDI_low,
    education_years, perc_essalud, mob, mig_in_perc, mig_out_perc, dens_pop,
    perc_fem, perc_65_plus, med_ratio, enf_ratio, altitud, ide, ocup_perc,
    bed_ratio, med2_ratio
  )

# Modeling features by covariates ----------------------------------------
cov <- "~ mig_in_perc + HDI + dens_pop + perc_essalud +
          perc_fem + perc_65_plus + med_ratio + ide + altitud"

mod.mort.cum <- lm(formula(paste("log_mort_cum", cov)), data = provinces)
summary(mod.mort.cum)

mod.mort.1 <- lm(formula(paste("log_mort1", cov)), data = provinces)
summary(mod.mort.1)

mod.mort.2 <- lm(formula(paste("log_mort2", cov)), data = provinces)
summary(mod.mort.2)

mod.rt <- lm(formula(paste("rt", cov)), data = provinces)
summary(mod.rt)

# PCA covariates ----------------------------------------------------------
vars <- c("HDI", "education_years", "perc_essalud", "mig_in_perc", 
          "perc_fem", "perc_65_plus", "dens_pop", "med_ratio", "altitud")

# First Peak
provinces.cov.1 <- provinces %>% 
  select(all_of(vars))

prov.cov.pca.1 <- prcomp(provinces.cov.1, center = TRUE, scale = TRUE)
provinces.cov.1 <- provinces %>% 
  mutate(pc.1 = prov.cov.pca.1$x[,1], pc.2 = prov.cov.pca.1$x[,2])

ggbiplot(prov.cov.pca.1, obs.scale = 1, alpha = 0, ellipse = TRUE,
  groups = provinces.cov.1$HDI_low) +
  geom_point(
    aes(x = pc.1, y = pc.2, size = log_mort1),
    alpha = 0.5,
    data = provinces.cov.1) +
  theme_bw()

# Second Peak
provinces.cov.2 <- provinces %>% 
  filter(!is.na(log_mort2)) %>% 
  select(all_of(vars)) 

prov.cov.pca.2 <- prcomp(provinces.cov.2, center = TRUE, scale = TRUE)
provinces.cov.2 <- provinces %>% 
  select(all_of(vars), log_mort2, HDI_low) %>% 
  na.omit() %>% 
  mutate(
    x = prov.cov.pca.2$x[,1],
    y = prov.cov.pca.2$x[,2]
  )

ggbiplot(prov.cov.pca.2, obs.scale = 1, alpha = 0, ellipse = TRUE,
  groups = provinces.cov.2$HDI_low) +
  geom_point(
    aes(x = x, y = y, size = log_mort2),
    alpha = 0.5,
    data = provinces.cov.2
  ) +
  theme_bw()

# RT
provinces.cov.3 <- provinces %>% 
  filter(!is.na(rt)) %>% 
  select(all_of(vars))

prov.cov.pca.3 <- prcomp(provinces.cov.3, center = TRUE, scale. = TRUE)
provinces.cov.3 <- provinces %>% 
  select(all_of(vars), HDI_low, rt) %>% 
  filter(!is.na(rt)) %>% 
  mutate(
    x = prov.cov.pca.3$x[,1],
    y = prov.cov.pca.3$x[,2]
  )

ggbiplot(prov.cov.pca.3, obs.scale = 1, alpha = 0, ellipse = TRUE,
         groups = provinces.cov.3$HDI_low) +
  geom_point(
    aes(x = x, y = y, size = rt),
    alpha = 0.5,
    data = provinces.cov.3
  ) +
  theme_bw()

# Plots -------------------------------------------------------------------

#Correlation Matrix
provinces %>% 
  select(
    log_mort_cum:day_second_peak, day_rt:HDI, education_years:perc_essalud,
    mig_in_perc,mig_out_perc, perc_fem:perc_65_plus, med_ratio:ocup_perc,
    bed_ratio
    ) %>%
  cor(use =  "complete.obs") %>% 
  corrplot(type = "upper", method = "square", diag = FALSE, insig='blank',
           addCoef.col ='black', number.cex = 0.8, order = 'AOE')

# log_mort1 vs pc1
provinces.cov.1 %>% 
  ggplot(aes(x = pc.1, y = log_mort1)) +
  geom_point() +
  geom_smooth(method = "lm")

# log_mort1 vs pc2
provinces.cov.1 %>%
  ggplot(aes(x = pc.2, y = log_mort1, col = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# log_mort2 vs pc1
provinces.cov.1 %>% 
  ggplot(aes(x = pc.1, y = log_mort2)) +
  geom_point() +
  geom_smooth(method = "lm")

# log_mort2 vs pc2
provinces.cov.1 %>% 
  ggplot(aes(x = pc.2, y = log_mort2)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cumulative vs Migration in | HDI
provinces %>% 
  ggplot(aes(x = mig_in_perc, y = log_mort_cum, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cumulative vs mobility | HDI
provinces %>% 
  ggplot(aes(x = mob, y = log_mort_cum, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cumulative vs EsSalud | HDI
provinces %>% 
  ggplot(aes(x = perc_essalud, y = log_mort_cum, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cumulative vs Pop density | HDI
provinces %>% 
  ggplot(aes(x = log(dens_pop), y = log_mort_cum, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cumulative vs Pop density | HDI
provinces %>% 
  ggplot(aes(x = altitud, y = log_mort_cum, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs Migration | HDI
provinces %>% 
  ggplot(aes(x = mig_in_perc, y = log_mort1, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs mobility | HDI
provinces %>% 
  ggplot(aes(x = mob, y = log_mort1, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs EsSalud | HDI
provinces %>% 
  ggplot(
    aes(
      x = perc_essalud, 
      y = log_mort1,
      color = HDI_low
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs Pop density | HDI
provinces %>% 
  ggplot(aes(x = log(dens_pop), y = log_mort1,color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs migration | HDI
provinces %>% 
  ggplot(aes(x = mig_in_perc, y = log_mort2,color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs mobility | HDI
provinces %>% 
  ggplot(aes(x = mob, y = log_mort2, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs EsSalud | HDI
provinces %>% 
  ggplot(aes(x = perc_essalud, y = log_mort2, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs Pop density | HDI
provinces %>% 
  ggplot(aes(x = log(dens_pop), y = log_mort2, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs Physician ratio | HDI
provinces %>% 
  ggplot(aes(x = med_ratio, y = log_mort2, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Day first peak vs Migration | HDI
provinces %>% 
  ggplot(aes(x = mig_in_perc, y = day_first_peak, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Day second peak vs Migration  | HDI
provinces %>% 
  ggplot(aes(x = mig_in_perc, y = day_second_peak,color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Distance between peaks vs Mortality second peak | HDI
provinces %>% 
  ggplot(aes(x = log_mort2, y = dist_peaks, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")

# Rt vs EsSalud | HDI
provinces %>% 
  ggplot(aes(x = perc_essalud, y = rt, color = HDI_low)) +
  geom_point() +
  geom_smooth(method = "lm")


