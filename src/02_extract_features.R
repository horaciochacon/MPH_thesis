library(dplyr)
library(readr)
library(purrr)
library(ggrepel)
source("R/functions.R")

# Read relevant data sources ----------------------------------------------

# Population provinces
poblacion_prov <- read_csv("data/pre_processed/poblacion_prov.csv") 

# EsSalud insurance
essalud <- read_csv("data/pre_processed/essalud.csv")

# Provincial (Adm2) maps
map_prov <- readRDS("data/pre_processed/map_prov.RDS")

# HDI United nations
HDI <- read_csv("data/pre_processed/HDI.csv")

# Read predicted mortality time series 
prov_pred_daily_mort <- read_csv("data/pre_processed/prov_pred_daily_mort.csv")

# Turn time series into province-specific list  
prov_preds_complete <- prov_pred_daily_mort %>% 
  split(.$prov_cdc)

# Get province data frame for reference
provinces_list <- prov_pred_daily_mort %>% 
  distinct(dpt_cdc, prov_cdc)


# Feature extraction and integration --------------------------------------

# Split into province-specific list of dataframes with logical peak identifier
# First argument of the peak identifier is the minimal value threshold
# The second argument of the peak identifier is the variation threshold
prov_preds_peaks <- prov_preds_complete %>%
  map(. %>% mutate(peak = get_peak_province(mortality, 2e-5, 2e-5)))
      

# Extract highest peak of first wave per province
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
  bind_rows() 

# Extract highest peak of second wave per province
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
  bind_rows()

# Extract number of peaks
n_peak <-  prov_preds_peaks %>% 
  map(
    . %>% 
      filter(peak) %>%
      mutate(n_peak = nrow(.)) %>% 
      .[1,] %>% 
      select(n_peak,dpt_cdc, prov_cdc)
  ) %>% 
  bind_rows()

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

# Extract cumulative deaths first wave
deaths_first_wave <- prov_preds_peaks %>% 
  map(
    ~tibble(
      prov_cdc = .x %>% pull(prov_cdc) %>% unique(),
      deaths_first_wave = .x %>% 
        filter(day <= 300) %>%
        left_join(poblacion_prov) %>% 
        mutate(deaths = pob * mortality) %>% 
        pull(deaths) %>% sum()/7
    )
  ) %>% 
  bind_rows()

# Extract cumulative deaths second wave
deaths_second_wave<- prov_preds_peaks %>% 
  map(
    ~tibble(
      prov_cdc = .x %>% pull(prov_cdc) %>% unique(),
      deaths_second_wave = .x %>% 
        filter(day > 300) %>%
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
  left_join(deaths_first_wave) %>% 
  left_join(deaths_second_wave) %>% 
  left_join(first_peak) %>% 
  left_join(second_peak) %>% 
  left_join(n_peak) %>% 
  mutate(dist_peaks = day_second_peak - day_first_peak) %>% 
  left_join(HDI) 

write.csv(provinces_final, "output/provinces_final.csv", row.names = FALSE)

# Plots -------------------------------------------------------------------

data <- read_csv("data/pre_processed/data_prov.csv") %>%
  filter(x1 <= 68) %>% 
  split(.$prov_cdc)

# Map 196 provinces peaks
for (i in 1:196) {
  g <- ggplot() +
    geom_line(
      aes(x = day, y = mortality * 1e5),
      data = prov_preds_peaks[[i]], 
      ) +
    geom_point(
      aes(x = x1 * 7, y = y1 * 1e5), alpha = 0.8, size = 1, data = data[[i]]
      ) +
    labs(
      title = paste(
        prov_preds_peaks[[i]]$dpt_cdc[1],
        prov_preds_peaks[[i]]$prov_cdc[1],
        sep = " - "
      ),
      x = "Weeks",
      y = "Mortality rate (per 100,000)"
    ) +
    ylim(c(0, max(prov_preds_peaks[[i]]$mortality * 1e5 *1.05))) +
    theme_bw()
  
  if (sum(prov_preds_peaks[[i]]$peak[1:300]) > 0) {
    g <-  g +
      geom_label_repel(
        aes(x = day_first_peak, y = mort_first_peak * 1e5, label = "peak"),
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
        aes(x = day_second_peak, y = mort_second_peak * 1e5, label = "peak"),
        nudge_y = 10,
        data =  second_peak %>%
          filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  
  print(g)
  
  ggsave(
    filename = paste0(
      "plots/prov_peaks/",
      tolower(prov_preds_peaks[[i]]$dpt_cdc[1]),
      "_",
      tolower(prov_preds_peaks[[i]]$prov_cdc[1]),
      ".pdf"),
    device = "pdf",
    plot = g
  )
}


