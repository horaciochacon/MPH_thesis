library(readxl)

# Human Development Index
idh <- read_excel("data/IDH%202019.xlsx", sheet = 3, skip = 2, n_max = 199) %>% 
  clean_names() %>% 
  select(
    ubigeo,
    provincia = x3, 
    pop = poblacion, 
    idh = indice_de_desarrollo_humano,
    life_exp = esperanza_de_vida_al_nacer,
    perc_secondary = con_educacion_secundaria_completa_poblac_18_anos,
    education_years = anos_de_educacion_poblac_25_y_mas,
    household_per_capita_income = ingreso_familiar_per_capita
  ) %>% 
  na.omit() %>% 
  mutate(
    provincia = str_to_upper(
      stri_trans_general(provincia, id = "Latin-ASCII")
    )
  ) %>% 
  mutate_at(vars(3:8), ~as.numeric(.))