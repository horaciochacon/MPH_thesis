library(dplyr)
library(readxl)
library(stringi)
library(stringr)
library(sf)
library(janitor)
library(lubridate)
library(tidyr)


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
  select(prov_cdc, perc_essalud = porc_essalud) %>% 
  write.csv("data/pre_processed/essalud.csv", row.names = FALSE)

# Provincial (Adm2) maps --------------------------------------------------
sf_use_s2(FALSE)
read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc),
    area = st_area(geometry)
  ) %>% 
  mutate(prov_cdc = ifelse(prov_cdc == "NASCA", "NAZCA", prov_cdc)) %>% 
  saveRDS("data/pre_processed/map_prov.RDS") 

read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc),
    area = st_area(geometry)
  ) %>% 
  mutate(prov_cdc = ifelse(prov_cdc == "NASCA", "NAZCA", prov_cdc)) %>% 
  select(prov_cdc, area) %>% 
  st_drop_geometry() %>% 
  write.csv("data/pre_processed/map_prov.csv", row.names = FALSE) 

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
  
# Time-varying IFR --------------------------------------------------------
read.csv("data/peru_ifr.csv") %>% 
  as_tibble() %>%  mutate(date = as_date(date)) %>% 
  filter(between(date, as_date("2020-03-02"), as_date("2021-06-30"))) %>% 
  mutate(mean = ifelse(is.infinite(mean), NA, ifelse(mean==0,0.0161,mean))) %>% 
  fill(mean, .direction = "up") %>% 
  select(date, ifr = mean) %>% 
  write.csv("data/pre_processed/peru_ifr_preprocessed.csv", row.names = FALSE)

# Mobility reports --------------------------------------------------------
read.csv("data/2021_PE_Region_Mobility_Report.csv") %>% 
  bind_rows(read.csv("data/2020_PE_Region_Mobility_Report.csv")) %>% 
  filter(sub_region_2 != "") %>% 
  select(prov_cdc = sub_region_2, date,
         "retail_and_recreation_percent_change_from_baseline":
           "residential_percent_change_from_baseline") %>% 
  mutate(
    prov_cdc = str_squish(
      str_remove(str_remove_all(prov_cdc, "Province"), " of")
    )
  ) %>% 
  mutate(
    prov_cdc = str_to_upper(
      stri_trans_general(prov_cdc, id = "Latin-ASCII")
    ),
    date = as_date(date)
  ) %>% 
  rowwise() %>% 
  mutate(mob = mean(
    c_across("retail_and_recreation_percent_change_from_baseline":
               "residential_percent_change_from_baseline"), na.rm = TRUE),
    prov_cdc = case_when(
      prov_cdc == "FERRENAFE" ~ "FERREÑAFE",
      prov_cdc == "CANETE" ~ "CAÑETE",
      prov_cdc == "CONSTITUTIONAL CALLAO" ~ "CALLAO",
      TRUE ~ prov_cdc
    )
  ) %>% 
  select(prov_cdc, date, mob) %>% 
  filter(between(date, as.Date("2020-03-02"), as.Date("2021-06-30"))) %>% 
  arrange(prov_cdc, date) %>% 
  write.csv("data/pre_processed/mobility.csv", row.names = FALSE)

# Province-level population covariates ------------------------------------
pop_sex <- read.csv("data/TB_POBLACION_INEI.csv") %>% 
  clean_names() %>% 
  group_by(provincia, sexo) %>% 
  summarise(pop = sum(cantidad)) %>% 
  pivot_wider(names_from = sexo, values_from = pop) %>% 
  mutate(perc_fem = `F` / (M + `F`)) %>% 
  select(prov_cdc = provincia, perc_fem)

pop_65_plus <- read.csv("data/TB_POBLACION_INEI.csv") %>% 
  clean_names() %>% 
  mutate(edad_anio = ifelse(edad_anio %in% 0:19, "0-19", edad_anio)) %>% 
  group_by(provincia, edad_anio) %>% 
  summarise(pop = sum(cantidad)) %>% 
  pivot_wider(names_from = edad_anio, values_from = pop) %>% 
  rowwise() %>% 
  mutate(
    perc_65_plus = sum(across(`65-69`:`80  +`)) / sum(across(`0-19`:`80  +`))
  ) %>% 
  select(prov_cdc = provincia, perc_65_plus)

pop_sex %>% 
  left_join(pop_65_plus) %>% 
  write.csv("data/pre_processed/pop_cov.csv", row.names = FALSE)

# Internal migration ------------------------------------------------------
read_excel("data/RetProyProv.xls") %>% 
  rename(prov_cdc = nomprov) %>% 
  mutate(
    prov_cdc = case_when(
      prov_cdc == "NASCA" ~ "NAZCA",
      prov_cdc == "ANTONIO RAYMONDI" ~ "ANTONIO RAIMONDI",
      TRUE ~ prov_cdc
    )
  ) %>% 
  write.csv("data/pre_processed/migration.csv", row.names = FALSE)
