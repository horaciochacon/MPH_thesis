library(dplyr)
library(readxl)
library(stringi)
library(stringr)
library(sf)
library(janitor)
library(lubridate)


# Population provinces ----------------------------------------------------
read.csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(dpt_cdc = DEPARTAMENTO,prov_cdc = PROVINCIA) %>% 
  summarise(pob = sum(POBLACION)) %>% ungroup() %>%  select(prov_cdc, pob) %>% 
  write.csv("data/pre_processed/poblacion_prov.csv", row.names = FALSE)

# EsSalud insurance -------------------------------------------------------
read_excel("data/aseguramiento_essalud.xlsx", sheet = 1) %>% 
  mutate(
    prov_cdc = str_to_upper(stri_trans_general(prov_cdc, id = "Latin-ASCII")),
    prov_cdc = case_when(
      prov_cdc == "ANTONIO RAYMONDI" ~ "ANTONIO RAIMONDI",
      prov_cdc == "MARANON" ~ "MARAÑON",
      prov_cdc == "FERRENAFE" ~ "FERREÑAFE",
      prov_cdc == "CANETE" ~ "CAÑETE",
      prov_cdc == "DATEM DEL MARANON" ~ "DATEM DEL MARAÑON",
      prov_cdc == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
      TRUE ~ prov_cdc
    )
  ) %>% 
  select(prov_cdc, porc_essalud) %>% 
  write.csv("data/pre_processed/essalud.csv", row.names = FALSE)

# Provincial (Adm2) maps --------------------------------------------------
read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc)
  ) %>% 
  mutate(prov_cdc = ifelse(prov_cdc == "NASCA", "NAZCA", prov_cdc)) %>% 
  saveRDS("data/pre_processed/map_prov.RDS")

# HDI United nations ------------------------------------------------------
read_excel("data/IDH%202019.xlsx", sheet = 3, skip = 2, n_max = 200) %>% 
  clean_names() %>% 
  select(
    prov_cdc = x3, 
    pop = poblacion, 
    HDI = indice_de_desarrollo_humano,
    life_exp = esperanza_de_vida_al_nacer,
    perc_secondary = con_educacion_secundaria_completa_poblac_18_anos,
    education_years = anos_de_educacion_poblac_25_y_mas,
    household_per_capita_income = ingreso_familiar_per_capita
  ) %>% 
  na.omit() %>% 
  mutate(
    prov_cdc = str_to_upper(
      stri_trans_general(prov_cdc, id = "Latin-ASCII")
    )
  ) %>% 
  mutate(
    prov_cdc = case_when(
      prov_cdc == "ANTONIO RAYMONDI" ~ "ANTONIO RAIMONDI",
      prov_cdc == "MARANON" ~ "MARAÑON",
      prov_cdc == "FERRENAFE" ~ "FERREÑAFE",
      prov_cdc == "CANETE" ~ "CAÑETE",
      prov_cdc == "DATEM DEL MARANON" ~ "DATEM DEL MARAÑON",
      prov_cdc == "DANIEL A. CARRION" ~ "DANIEL ALCIDES CARRION",
      TRUE ~ prov_cdc
    )
  ) %>% 
  mutate_at(vars(2:7), ~as.numeric(.)) %>% 
  write.csv("data/pre_processed/HDI.csv", row.names = FALSE)
  
# Predicted daily mortality time series  ----------------------------------
read.csv("data/pred_prov_time_series.csv") %>% 
  mutate(day = round(x1 * 7, digits = 1), .after = x1) %>% 
  filter(day %in% 0:560) %>% 
  distinct(day, prov_cdc, .keep_all = TRUE) %>% 
  tibble() %>% 
  select(day, prov_cdc, dpt_cdc, pred, mortality) %>% 
  mutate(date = as_date(day + 18323), .before = day) %>% 
  filter(between(date, as_date("2020-03-02"), as_date("2021-06-30"))) %>% 
  write.csv("data/pre_processed/prov_pred_daily_mort.csv", row.names = FALSE)
  