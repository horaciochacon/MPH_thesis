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
    mig_arrival_perc = retor_llegaron / pop
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


plot(provinces$mig_arrival_perc, log(provinces$mort_second_peak))
