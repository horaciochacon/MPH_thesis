library(dplyr)
source("R/functions.R")
library(purrr)
library(ggrepel)
library(EpiEstim)
library(incidence)
library(lubridate)
library(tidyr)
library(stringr)
library(stringi)
library(janitor)


# Reading the necessary data -----------------------------------------------

# Province level population
poblacion_prov <- read.csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(prov_cdc = PROVINCIA) %>% 
  summarise(pob = sum(POBLACION)) 

# Read Time-varying 
peru_ifr <- read.csv("data/peru_ifr.csv") %>% 
  as_tibble() %>%  mutate(date = as_date(date)) %>% 
  filter(between(date, as_date("2020-03-02"), as_date("2021-09-13"))) %>% 
  mutate(mean = ifelse(is.infinite(mean), NA, ifelse(mean==0,0.0161,mean))) %>% 
  fill(mean, .direction = "up") %>% 
  select(date, ifr = mean)

# Mobility Reports
mobility <- read.csv("data/2021_PE_Region_Mobility_Report.csv") %>% 
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
  filter(between(date, as.Date("2020-03-02"), as.Date("2021-09-13"))) %>% 
  arrange(prov_cdc, date)

# Read predicted mortality time series 
prov_preds <- read.csv("data/pred_prov_time_series.csv") %>% 
  mutate(day = round(x1 * 7, digits = 1), .after = x1) %>% 
  filter(day %in% 0:560) %>% 
  distinct(day, prov_cdc, .keep_all = TRUE) %>% 
  tibble() %>% 
  select(day, prov_cdc, dpt_cdc, pred, mortality) %>% 
  mutate(date = as_date(day + 18323), .before = day) %>% 
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



