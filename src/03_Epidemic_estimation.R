library(dplyr)
source("R/functions.R")
library(purrr)
library(ggrepel)
library(EpiEstim)
library(incidence)
library(lubridate)

# Reading the necessary data -----------------------------------------------

# Population provinces
poblacion_prov <- read.csv("data/pre_processed/poblacion_prov.csv") 

# Time-varying IFR
peru_ifr <- read.csv("data/pre_processed/peru_ifr_preprocessed.csv") %>% 
  mutate(date = as.Date(date))

# Mobility Reports
mobility <- read.csv("data/pre_processed/mobility.csv") %>% 
  mutate(date = as.Date(date))

# Read predicted mortality time series 
prov_preds <- read.csv("data/pred_prov_time_series.csv") %>% 
  mutate(day = round(x1 * 7, digits = 1), .after = x1) %>% 
  filter(day %in% 0:560) %>% 
  distinct(day, prov_cdc, .keep_all = TRUE) %>% 
  tibble() %>% 
  select(day, prov_cdc, dpt_cdc, pred, mortality) %>% 
  mutate(date = as_date(day + 18323), .before = day) %>% 
  filter(between(date, as.Date("2020-03-02"), as.Date("2021-06-30"))) %>%
  left_join(peru_ifr) %>% 
  left_join(poblacion_prov) %>% 
  mutate(
    D = mortality * pob / 7,
    I = D / ifr
  ) %>% 
  select(date, day, I, prov_cdc) %>%
  left_join(mobility) %>%
  split(.$prov_cdc)  %>% 
  map(
    ~.x %>% fill(mob, .direction = "up")
  )

# Estimating Rt -----------------------------------------------------------

rt_list <- prov_preds %>% 
  map(
    ~estimate_R(
      .x[c(1:3)], "parametric_si", 
      config = make_config(list(mean_si = 5.4, std_si = 0.11)))
  )

rt_peak <- rt_list %>% 
  imap(
    ~.x$R %>% 
      filter(`Quantile.0.025(R)` > 1) %>% 
      filter(`Quantile.0.975(R)` - `Quantile.0.025(R)` < 0.5 ) %>% 
      filter(`Quantile.0.025(R)` == max(`Quantile.0.025(R)`)) %>% 
      mutate(prov_cdc = .y, .before = t_start) %>% 
      select(1,3,4,6)
  ) %>% 
  map2(
    rt_list,
    ~.x %>% 
      mutate(I_peak = .y$I[.x$t_end])
  )

rt_peak_df <- rt_peak %>% 
  map2(
    prov_preds,
    ~.x %>% 
      mutate(infection_day = t_end - 7) %>% 
      left_join(.y, by = c("infection_day" = "day", "prov_cdc" = "prov_cdc"))
  ) %>% 
  bind_rows() %>% 
  select(prov_cdc, day = t_end, date, infection_day, rt = `Mean(R)`, I_peak, mob)

write.csv(rt_peak_df, "output/rt_peak_df.csv", row.names = FALSE)

# Plots -------------------------------------------------------------------

map2(
  rt_list, 
  rt_peak,
  .f = 
    ~estimate_R_plots(.x, what = "R") + 
    geom_hline(yintercept = 1) +
    labs(title = .y$prov_cdc) +
    geom_vline(xintercept = .y$t_end)
)

rt_peak_df %>% 
  ggplot(aes(x = mob, y = rt, color = day < 200 )) +
  geom_point() +
  geom_smooth(method = "lm")

rt_peak_df %>% 
  ggplot(aes(x = mob, y = rt)) +
  geom_point() +
  geom_smooth(method = "lm")



