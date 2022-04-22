library(dplyr)
source("R/functions.R")
library(purrr)
library(ggrepel)
library(sf)

# Provincial (Adm2) maps
map_prov <- read_sf("data/provincias/PROVINCIAS.shp") %>% 
  rename(prov_cdc = PROVINCIA) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "ANTONIO RAYMONDI", "ANTONIO RAIMONDI", prov_cdc)
  ) %>% 
  mutate(prov_cdc = ifelse(
    prov_cdc == "NASCA", "NAZCA", prov_cdc)
  )



# Read predicted mortality time series ------------------------------------

prov_preds <- read.csv("data/pred_prov_time_series.csv") %>% 
  tibble() %>% 
  split(.$prov_cdc)

# Split into province independent list of dataframes
prov_preds <- prov_preds %>% 
  map(
    . %>% 
      mutate(peak = get_peak_province(mortality, 5e-5))
  )


# Map 196 provinces peaks
for (i in 1:196) {
  g <- ggplot() +
    geom_line(
      aes(x = x1, y = mortality * 100000),
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
  
  if (sum(!is.na(prov_preds[[i]]$peak)) > 0) {
    g <-  g +
      geom_label_repel(
        aes(x = x1, y = mortality * 100000, label = "peak"),
        nudge_y = 10,
        data =  prov_preds[[i]] %>% filter(peak),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  print(g)
  
  ggsave(
    filename = paste0(
      "plots/prov_peaks/",
      tolower(prov_preds[[i]]$dpt_cdc[1]),
      "_",
      tolower(prov_preds[[i]]$prov_cdc[1]),
      ".pdf"),
    device = "pdf",
    plot = g
  )
}

# Extract first peak

first_peak <- prov_preds %>% 
  map(
    . %>%
      filter(peak) %>% 
      .[1,]
  ) %>% 
  bind_rows() %>% 
  na.omit()

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
  filter(x1 < 30) %>% 
  ggplot(aes(x = x1, y = mortality)) +
  geom_point() +
  geom_smooth(method = "lm")

 plot( first_peak$x1, first_peak$mortality)

