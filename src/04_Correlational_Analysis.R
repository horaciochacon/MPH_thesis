library(ggbiplot)
library(dplyr)
library(readxl)
library(ggplot2)
library(sf)
library(units)
library(janitor)
library(tidyr)


# Read curated datasets ---------------------------------------------------
pop_sex <- read.csv("data/TB_POBLACION_INEI.csv") %>% 
  clean_names() %>% 
  group_by(provincia, sexo) %>% 
  summarise(pop = sum(cantidad)) %>% 
  pivot_wider(names_from = sexo, values_from = pop) %>% 
  mutate(porc_fem = `F` / (M + `F`)) %>% 
  select(prov_cdc = provincia, porc_fem)

pop_65_plus <- read.csv("data/TB_POBLACION_INEI.csv") %>% 
  clean_names() %>% 
  mutate(edad_anio = ifelse(edad_anio %in% 0:19, "0-19", edad_anio)) %>% 
  group_by(provincia, edad_anio) %>% 
  summarise(pop = sum(cantidad)) %>% 
  pivot_wider(names_from = edad_anio, values_from = pop) %>% 
  rowwise() %>% 
  mutate(
    porc_65_plus = sum(across(`65-69`:`80  +`)) / sum(across(`0-19`:`80  +`))
    ) %>% 
  select(prov_cdc = provincia, porc_65_plus)

sf_use_s2(FALSE)
map_prov <- read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc)
  ) %>% 
  mutate(
    prov_cdc = ifelse(prov_cdc == "NASCA", "NAZCA", prov_cdc),
    area = st_area(geometry)
  ) %>% 
  select(prov_cdc, area) %>% 
  st_drop_geometry()


provinces <- read.csv("output/provinces_final.csv")
rt <- read.csv("output/rt_peak_df.csv")
migration <- read_excel("data/RetProyProv.xls") %>% 
  rename(prov_cdc = nomprov) %>% 
  mutate(
    prov_cdc = case_when(
    prov_cdc == "NASCA" ~ "NAZCA",
    prov_cdc == "ANTONIO RAYMONDI" ~ "ANTONIO RAIMONDI",
    TRUE ~ prov_cdc
    )
  )

provinces <- provinces %>% 
  left_join(migration) %>% 
  left_join(rt) %>% 
  left_join(map_prov) %>% 
  left_join(pop_sex) %>% 
  left_join(pop_65_plus) %>% 
  mutate(
    mig_arrival_perc = retor_llegaron / pop,
    log_mort_cum = log(deaths / pop),
    dens_pop = pop / area,
    log_mort1 = mort_first_peak,
    log_mort2 = mort_second_peak,
    idh_low = idh < 0.42
  ) %>% 
  tibble() %>% 
  select(
    prov_cdc, log_mort_cum, log_mort1, day_first_peak, log_mort2, day_second_peak,
    n_peak, dist_peaks, rt, day_rt = day, idh, idh_low, education_years, 
    porc_essalud, mob, mig_arrival_perc, dens_pop, porc_fem, porc_65_plus
  )
  

# Modeling features by covariates ----------------------------------------
provinces.na <- provinces[,c(2,3,5,9)] %>% 
  na.omit()

prov.pca <- prcomp(provinces.na, center = TRUE, scale. = TRUE)
 
summary(prov.pca)

provinces.na <- provinces %>% 
  filter(
    prov_cdc %in%  (provinces[,c(1,2,3,5,9)] %>% na.omit() %>% .$prov_cdc),
    ) %>% 
  mutate(pca = prov.pca$x[,1])
  

mod1 <- lm(
  pca ~ mig_arrival_perc + idh + dens_pop + porc_essalud + porc_fem + porc_65_plus, 
  data = provinces.na
  )

summary(mod1)

mod.mort.cum <- lm(
  log_mort_cum ~ mig_arrival_perc + idh + dens_pop + porc_essalud + porc_fem + porc_65_plus, 
  data = provinces.na
)

summary(mod.mort.cum)

mod.mort.1 <- lm(
  log_mort1 ~ mig_arrival_perc + idh + dens_pop + porc_essalud + porc_fem + porc_65_plus, 
  data = provinces.na
)

summary(mod.mort.1)

mod.mort.2 <- lm(
  log_mort2 ~ mig_arrival_perc + idh + dens_pop + porc_essalud + porc_fem + porc_65_plus, 
  data = provinces.na
)

summary(mod.mort.2)

mod.rt <- lm(
  rt ~  mig_arrival_perc + idh_low + porc_essalud + porc_fem + porc_65_plus, 
  data = provinces.na
)

summary(mod.rt)


# PCA covariates ----------------------------------------------------------

# First Peak
provinces.cov.1 <- provinces %>% 
  select(idh, education_years, porc_essalud, mig_arrival_perc,
         porc_fem, porc_65_plus) %>% 
  na.omit()

prov.cov.pca.1 <- prcomp(provinces.cov.1, center = TRUE, scale. = TRUE)

summary(prov.cov.pca.1)

provinces.cov.1 <- provinces %>% 
  select(idh, education_years, porc_essalud, mig_arrival_perc,
         porc_fem, porc_65_plus, log_mort1, log_mort_cum, idh_low) %>% 
  na.omit() %>% 
  mutate(
    x = prov.cov.pca.1$x[,1],
    y = prov.cov.pca.1$x[,2]
  )

ggbiplot(
  prov.cov.pca.1, 
  obs.scale = 1, 
  alpha = 0,
  ellipse = TRUE,
  groups = provinces.cov.1$idh_low
  ) +
  geom_point(
    aes(x = x, y = y, size = log_mort1),
    alpha = 0.5,
    data = provinces.cov.1
    ) +
  theme_bw()

# Second Peak
provinces.cov.2 <- provinces %>% 
  filter(!is.na(log_mort2)) %>% 
  select(idh, education_years, porc_essalud, mig_arrival_perc,
         porc_fem, porc_65_plus) %>% 
  na.omit()

prov.cov.pca.2 <- prcomp(provinces.cov.2, center = TRUE, scale. = TRUE)

summary(prov.cov.pca.2)

provinces.cov.2 <- provinces %>% 
  select(idh, education_years, porc_essalud, mig_arrival_perc,
         porc_fem, porc_65_plus, log_mort2, log_mort_cum, idh_low) %>% 
  na.omit() %>% 
  filter(!is.na(log_mort2)) %>% 
  mutate(
    x = prov.cov.pca.2$x[,1],
    y = prov.cov.pca.2$x[,2]
  )

ggbiplot(
  prov.cov.pca.2, 
  obs.scale = 1, 
  alpha = 0,
  ellipse = TRUE,
  groups = provinces.cov.2$idh_low
  ) +
  geom_point(
    aes(x = x, y = y, size = log_mort2),
    alpha = 0.5,
    data = provinces.cov.2
  ) +
  theme_bw()

# RT
provinces.cov.3 <- provinces %>% 
  filter(!is.na(rt)) %>% 
  select(idh, education_years, porc_essalud, mig_arrival_perc,
         porc_fem, porc_65_plus) %>% 
  na.omit()

prov.cov.pca.3 <- prcomp(provinces.cov.3, center = TRUE, scale. = TRUE)

summary(prov.cov.pca.3)

provinces.cov.3 <- provinces %>% 
  select(idh, education_years, porc_essalud, mig_arrival_perc,
         porc_fem, porc_65_plus, rt, log_mort_cum, idh_low) %>% 
  na.omit() %>% 
  filter(!is.na(rt)) %>% 
  mutate(
    x = prov.cov.pca.3$x[,1],
    y = prov.cov.pca.3$x[,2]
  )

ggbiplot(
  prov.cov.pca.3, 
  obs.scale = 1, 
  alpha = 0,
  ellipse = TRUE,
  groups = provinces.cov.3$idh_low
) +
  geom_point(
    aes(x = x, y = y, size = rt),
    alpha = 0.5,
    data = provinces.cov.3
  ) +
  theme_bw()

# Plots -------------------------------------------------------------------

#Correlation Matrix
provinces.na %>% 
  select(
    log_mort_cum:day_second_peak, day_rt:idh, education_years:porc_essalud,
    mig_arrival_perc,porc_fem:porc_65_plus
    ) %>% cor() %>% 
corrplot(type = "upper", method = "square", diag = FALSE, insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE')


provinces %>% 
  ggplot(aes(x = mig_arrival_perc, y = log_mort1)) +
  geom_point() +
  geom_smooth(method = "lm")

# pca vs Migration | idh
provinces.na %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = pca,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# pca vs mobility | idh
provinces.na %>% 
  ggplot(
    aes(
      x = mob, 
      y = pca,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# pca vs essalud | idh
provinces.na %>% 
  ggplot(
    aes(
      x = porc_essalud, 
      y = pca,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# pca vs 65 plus | idh
provinces.na %>% 
  ggplot(
    aes(
      x = porc_65_plus, 
      y = pca,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")


# Mortality cummulative vs Migration | idh
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = log_mort_cum,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cummulative vs mobility | idh
provinces %>% 
  ggplot(
    aes(
      x = mob, 
      y = log_mort_cum,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cummulative vs essalud | idh
provinces %>% 
  ggplot(
    aes(
      x = porc_essalud, 
      y = log_mort_cum,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality cummulative vs Pop density | idh
provinces %>% 
  ggplot(
    aes(
      x = log(dens_pop), 
      y = log_mort_cum,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs Migration | idh
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = log_mort1,
      color = idh < 0.42
      )
    ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs mobility | idh
provinces %>% 
  ggplot(
    aes(
      x = mob, 
      y = log_mort1,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs essalud | idh
provinces %>% 
  ggplot(
    aes(
      x = porc_essalud, 
      y = log_mort1,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak vs Pop density | idh
provinces %>% 
  ggplot(
    aes(
      x = log(dens_pop), 
      y = log_mort1,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs migration | idh
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = log_mort2,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs mobility | idh
provinces %>% 
  ggplot(
    aes(
      x = mob, 
      y = log_mort2,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs essalud | idh
provinces %>% 
  ggplot(
    aes(
      x = porc_essalud, 
      y = log_mort2,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak vs Pop density | idh
provinces %>% 
  ggplot(
    aes(
      x = log(dens_pop), 
      y = log_mort2,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Day first peak vs Migration | idh
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = day_first_peak,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Day second peak vs Migration  | idh
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = day_second_peak,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Dist Peaks
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = dist_peaks,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Rt vs essalud | idh
provinces %>% 
  ggplot(
    aes(
      x = porc_essalud, 
      y = rt,
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")


