library(dplyr)
source("R/functions.R")
library(purrr)
library(ggrepel)
library(sf)
library(janitor)
library(stringr)
library(stringi)
library(readxl)

# Provincial (Adm2) maps
map_prov <- read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc)
  ) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "NASCA", "NAZCA", prov_cdc)
  )

# HDI United nations

idh <- read_excel("data/IDH%202019.xlsx", sheet = 3, skip = 2, n_max = 199) %>% 
  clean_names() %>% 
  select(
    prov_cdc = x3, 
    pop = poblacion, 
    idh = indice_de_desarrollo_humano,
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
  mutate_at(vars(2:7), ~as.numeric(.))

# Read predicted mortality time series ------------------------------------

provinces <- read.csv("data/pred_prov_time_series.csv") %>% 
  mutate(day = round(x1 * 7, digits = 1), .after = x1) %>% 
  filter(day %in% 1:560) %>% 
  distinct(day, prov_cdc, .keep_all = TRUE) %>% 
  tibble() %>% 
  select(day, prov_cdc, dpt_cdc, pred, mortality)
  
prov_preds_complete <- provinces %>% 
  split(.$prov_cdc)

provinces_list <- provinces %>% 
  distinct(dpt_cdc, prov_cdc)

# Split into province independent list of dataframes
prov_preds <- prov_preds_complete %>%
  map(. %>% mutate(peak = get_peak_province(mortality, 5e-5, 4e-5)))
      

# Extract first peak
first_peak <- prov_preds %>% 
  map(
    . %>% 
      filter(peak) %>%
      .[1,] %>% 
      select(
        day_first_peak = day,
        mort_first_peak = mortality, 
        dpt_cdc, prov_cdc)
    ) %>% 
  bind_rows() %>% 
  na.omit()

# Extract second peak
second_peak <- prov_preds %>% 
  map(
    . %>% 
      filter(peak) %>%
      .[2,] %>% 
      select(
        day_second_peak = day,
        mort_second_peak = mortality, 
        dpt_cdc, prov_cdc)
  ) %>% 
  bind_rows() %>% 
  na.omit()

# Extract number of peaks
n_peak <- prov_preds %>% 
  map(
    . %>% 
      filter(peak) %>%
      mutate(n_peak = nrow(.)) %>% 
      .[1,] %>% 
      select(n_peak,dpt_cdc, prov_cdc)
  ) %>% 
  bind_rows() %>% 
  na.omit()

provinces_list <- provinces_list %>% 
  left_join(first_peak) %>% 
  left_join(second_peak) %>% 
  left_join(n_peak) %>% 
  mutate(dist_peaks = day_second_peak - day_first_peak) %>% 
  left_join(idh)


# Plots -------------------------------------------------------------------

library(GGally)

provinces_list %>% 
  select(day_first_peak:household_per_capita_income) %>% 
  ggpairs(
    lower = list(
      continuous = wrap("points", size = 0.5, alpha = 0.5),
      combo = wrap("dot_no_facet", alpha = 0.4)
      )
    )

provinces_list %>% 
  select(day_first_peak:household_per_capita_income) %>% 
  ggpairs(
    aes(color = day_first_peak < 250),
    lower = list(
      continuous = wrap("points", size = 0.5, alpha = 0.5)
      )
    )

provinces_list %>% 
  select(day_first_peak:dist_peaks) %>% 
  ggpairs(aes(color = as.factor(n_peak)))








# Map 196 provinces peaks
for (i in 1:10) {
  g <- ggplot() +
    geom_line(
      aes(x = day, y = mortality * 100000),
      data = prov_preds[[i]], 
    ) +
    labs(
      title = paste(
        prov_preds[[i]]$dpt_cdc[1],
        prov_preds[[i]]$prov_cdc[1],
        sep = " - "
      ),
      x = "Weeks",
      y = "Mortality rate"
    ) +
    theme_bw() +
    ylim(c(0, 80)) 
  
  if (sum(prov_preds[[i]]$peak) > 0) {
    g <-  g +
      geom_label_repel(
        aes(x = day, y = mortality * 100000, label = "peak"),
        nudge_y = 10,
        data =  prov_preds[[i]] %>% filter(peak),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  print(g)
  
  # ggsave(
  #   filename = paste0(
  #     "plots/prov_peaks/",
  #     tolower(prov_preds[[i]]$dpt_cdc[1]),
  #     "_",
  #     tolower(prov_preds[[i]]$prov_cdc[1]),
  #     ".pdf"),
  #   device = "pdf",
  #   plot = g
  # )
}


map_first_peak <- first_peak %>% 
  left_join(map_prov) %>% 
  ggplot() +
  geom_sf(
    data = map_prov,
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = x1, geometry = geometry), 
    size = 0.05, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    palette = "Reds"
  ) +
  theme_bw()

map_value_first_peak <- first_peak %>% 
  left_join(map_prov) %>% 
  ggplot() +
  geom_sf(
    data = map_prov,
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = mortality * 100000, geometry = geometry), 
    size = 0.05, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    direction = 1,
    trans = "log",
    breaks = c(0, 5, 20, 30, 50, 100)
  ) +
  theme_bw()

first_peak %>% 
  # filter(x1 < 30) %>% 
  ggplot(aes(x = x1, y = mortality)) +
  geom_point() +
  geom_smooth(method = "lm")

 plot( first_peak$x1, first_peak$mortality)

