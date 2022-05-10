library(dplyr)
library(readxl)
library(ggplot2)


# Read curated datasets ---------------------------------------------------

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
  mutate(
    mig_arrival_perc = retor_llegaron / pop,
    log_mort1 = mort_first_peak,
    log_mort2 = mort_second_peak
  ) %>% 
  tibble() %>% 
  select(
    prov_cdc, log_mort1, day_first_peak, log_mort2, day_second_peak,
    n_peak, dist_peaks, rt, day_rt = day, idh, porc_essalud, mob, 
    mig_arrival_perc
  )
  

# Plots -------------------------------------------------------------------

provinces %>% 
  ggplot(aes(x = mig_arrival_perc, y = log(mort_first_peak))) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality first peak
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = log(mort_first_peak),
      color = idh < 0.42
      )
    ) +
  geom_point() +
  geom_smooth(method = "lm")

# Mortality second peak
provinces %>% 
  ggplot(
    aes(
      x = mig_arrival_perc, 
      y = log(mort_second_peak),
      color = idh < 0.42
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm")

# Day first peak
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

# Day second peak
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

# Day second peak
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


plot(provinces$mig_arrival_perc, log(provinces$mort_second_peak))
