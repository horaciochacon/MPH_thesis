library(dplyr)
source("R/functions.R")
library(purrr)
library(ggrepel)

# Read relevant data sources ----------------------------------------------

# Population provinces
poblacion_prov <- read.csv("data/pre_processed/poblacion_prov.csv") 

# EsSalud insurance
essalud <- read.csv("data/pre_processed/essalud.csv")

# Provincial (Adm2) maps
map_prov <- readRDS("data/pre_processed/map_prov.RDS")

# HDI United nations
HDI <- read.csv("data/pre_processed/HDI.csv")

# Read predicted mortality time series 
prov_pred_daily_mort <- read.csv("data/pre_processed/prov_pred_daily_mort.csv")
  
prov_preds_complete <- prov_pred_daily_mort %>% 
  split(.$prov_cdc)

provinces_list <- prov_pred_daily_mort %>% 
  distinct(dpt_cdc, prov_cdc)


# Feature extraction and integration --------------------------------------

# Split into province independent list of dataframes
prov_preds_peaks <- prov_preds_complete %>%
  map(. %>% mutate(peak = get_peak_province(mortality, 2e-5, 2e-5)))
      

# Extract first peak
first_peak <- prov_preds_peaks %>% 
  map(
    . %>% 
      filter(peak & day <= 300) %>% 
      .[which.max(.$mortality),] %>% 
      select(
        date_first_peak = date,
        day_first_peak = day,
        mort_first_peak = mortality, 
        dpt_cdc, prov_cdc)
    ) %>% 
  bind_rows() %>% 
  na.omit()

# Extract second peak
second_peak <- prov_preds_peaks %>% 
  map(
    . %>% 
      filter(peak & day > 300) %>% 
      .[which.max(.$mortality),] %>% 
      select(
        date_second_peak = date,
        day_second_peak = day,
        mort_second_peak = mortality, 
        dpt_cdc, prov_cdc)
  ) %>% 
  bind_rows() %>% 
  na.omit()

# Extract number of peaks
n_peak <- prov_preds_peaks %>% 
  map(
    . %>% 
      filter(peak) %>%
      mutate(n_peak = nrow(.)) %>% 
      .[1,] %>% 
      select(n_peak,dpt_cdc, prov_cdc)
  ) %>% 
  bind_rows() %>% 
  na.omit()

# Extract cumulative deaths
deaths <- prov_preds_peaks %>% 
  map(
    ~tibble(
      prov_cdc = .x %>% pull(prov_cdc) %>% unique(),
      deaths = .x %>% 
        left_join(poblacion_prov) %>% 
        mutate(deaths = pob * mortality) %>% 
        pull(deaths) %>% sum()/7
    )
  ) %>% 
  bind_rows()


# Merging in final features dataset
provinces_final <- provinces_list %>%
  left_join(essalud) %>% 
  left_join(deaths) %>% 
  left_join(first_peak) %>% 
  left_join(second_peak) %>% 
  left_join(n_peak) %>% 
  mutate(dist_peaks = day_second_peak - day_first_peak) %>% 
  left_join(HDI) 

write.csv(provinces_final, "output/provinces_final.csv", row.names = FALSE)

# Plots -------------------------------------------------------------------

library(GGally)

provinces_final %>% 
  # filter(!(prov_cdc %in% c("LIMA"))) %>% 
  select(porc_essalud, day_first_peak, mort_first_peak, day_second_peak,
         mort_second_peak, HDI) %>% 
  ggpairs(
    lower = list(
      continuous = wrap("points", size = 0.5, alpha = 0.5),
      combo = wrap("dot_no_facet", alpha = 0.4)
      )
    )

# provinces_final %>% 
#   select(porc_essalud, day_first_peak, mort_first_peak, day_second_peak,
#          mort_second_peak, HDI) %>% 
#   # select(day_first_peak:household_per_capita_income) %>% 
#   ggpairs(
#     aes(color = day_first_peak < 250),
#     lower = list(
#       continuous = wrap("points", size = 0.5, alpha = 0.5)
#       )
#     )

provinces_final %>% 
  select(day_first_peak:dist_peaks) %>% 
  ggpairs(aes(color = as.factor(n_peak)))

# Map 196 provinces peaks
for (i in 1:100) {
  g <- ggplot() +
    geom_line(
      aes(x = day, y = mortality * 100000),
      data = prov_preds_peaks[[i]], 
    ) +
    labs(
      title = paste(
        prov_preds_peaks[[i]]$dpt_cdc[1],
        prov_preds_peaks[[i]]$prov_cdc[1],
        sep = " - "
      ),
      x = "Weeks",
      y = "Mortality rate"
    ) +
    theme_bw()
  
  if (sum(prov_preds_peaks[[i]]$peak) > 0) {
    g <-  g +
      geom_label_repel(
        aes(x = day_first_peak, y = mort_first_peak * 100000, label = "peak"),
        nudge_y = 10,
        data =  first_peak %>%
          filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  
  if (second_peak %>%
      filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)) %>% nrow() > 0) {
   g <- g +
      geom_label_repel(
        aes(x = day_second_peak, y = mort_second_peak * 100000, label = "peak"),
        nudge_y = 10,
        data =  second_peak %>%
          filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  
  print(g)
  
  # ggsave(
  #   filename = paste0(
  #     "plots/prov_peaks/",
  #     tolower(prov_preds_peaks[[i]]$dpt_cdc[1]),
  #     "_",
  #     tolower(prov_preds_peaks[[i]]$prov_cdc[1]),
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
    aes(fill = day_first_peak, geometry = geometry), 
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
    aes(fill = mort_first_peak * 100000, geometry = geometry), 
    size = 0.05, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    direction = 1,
    trans = "log",
    breaks = c(0, 5, 20, 30, 50, 100)
  ) +
  theme_bw()

map_second_peak <- second_peak %>% 
  left_join(map_prov) %>% 
  ggplot() +
  geom_sf(
    data = map_prov,
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = day_second_peak, geometry = geometry), 
    size = 0.05, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    palette = "Reds"
  ) +
  theme_bw()

map_value_first_peak <- second_peak %>% 
  left_join(map_prov) %>% 
  ggplot() +
  geom_sf(
    data = map_prov,
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = mort_second_peak * 100000, geometry = geometry), 
    size = 0.05, color = "grey40", alpha = 1
  ) +
  scale_fill_distiller(
    direction = 1,
    trans = "log",
    breaks = c(0, 5, 20, 30, 50, 100)
  ) +
  theme_bw()

