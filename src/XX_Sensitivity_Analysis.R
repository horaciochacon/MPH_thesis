# Load Packages -----------------------------------------------------------

library(lubridate)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(data.table)
library(purrr)
library(tidyr)
library(dplyr)
library(scales)
library(viridisLite)
config <- config::get(config = "sensitivity")


# Read Sensitivity files ----------------------------------------------------------------------

# Set path to folder
path <- "sensitivity/output/"

# List all files in the folder
file_list <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Read all files in the folder
data <- rbindlist(lapply(file_list, fread))

# Read Auxiliary Files
data_prov <- readRDS(paste0(config$sbatch$wd, "sensitivity/data_prov.rds"))
pob <- fread("data/pre_processed/poblacion_prov.csv")[,1:3]

# Calculate Error, Weight and Weighted Error --------------------------------------------------

# Waves
waves <- copy(data_prov)
waves <- waves[, k := ifelse(x1 < 43, 1, 2)]
waves <- waves[, .(y_obs = sum(n, na.rm = TRUE)), by = .(dpt_cdc, prov_cdc, k)]
waves <- waves[pob, on = .(prov_cdc, dpt_cdc), nomatch = NA]
waves <- waves[, w_k := ifelse(
  k == 1, y_obs / sum(waves$y_obs[waves[,k == 1]]),
  y_obs / sum(waves$y_obs[waves[,k == 2]]))
  ]
waves

# Predicted 
data_sens <- copy(data)

data_sens <- (
  data_sens
  [, k := ifelse(x1 < 43, 1, 2)]
  [waves, on = .(prov_cdc, dpt_cdc, k), nomatch = NA]
  [, e := (exp(y_obs) * 1e6 - exp(y_hat)*1e6)^2]
  [, we := e * w_k]
)


# Saving the Predicted theta-specific Weighted Errors -----------------------------------------

df_we_k     <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov,k)]
df_we_l     <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov,l = x1)]
df_we_ki    <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc)]
df_we_kij   <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc, prov_cdc)]

df_we_kij   <- data_sens[,
                         .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc, prov_cdc)
                         ]

data <- df_we_kij %>% 
  group_by(prov_cdc, k) %>% 
  mutate(we_k = ifelse(we_k >= 4e4, NA, we_k)) %>% 
  summarise(med = median(we_k, na.rm = TRUE))

df_we_kij_avg <-  df_we_kij %>% 
  left_join(data) %>% 
  mutate(we_k_avg = we_k / med)

fwrite(df_we_k, paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_k.csv")) 
fwrite(df_we_l, paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_l.csv"))
fwrite(df_we_ki, paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_ki.csv"))
fwrite(df_we_kij,  paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_kij.csv"))


# Plots ---------------------------------------------------------------------------------------

# By Wave
df_we_k %>% 
  filter(we_k < 40000) %>%  # Had some weird outliers when high thetas, very few
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno", direction = -1
    # trans = "log",
    # limits = c(3.94,15)
    ) +
  facet_wrap(. ~ k) +
  theme_minimal()

# Wave 1
df_we_k %>% 
  filter(we_k < 40000, k == 1) %>%  
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno", 
    direction = -1
    # limits = c(3.94,15)
    ) +
  facet_wrap(. ~ k) +
  theme_minimal()

# Wave 2
df_we_k %>% 
  filter(we_k < 40000, k == 2) %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno", 
    direction = -1,
    limits = c(3.94,4)
  ) +
  facet_wrap(. ~ k) +
  theme_minimal()


# By Department (Wave 1) Grid
df_we_ki %>% 
  filter(we_k < 1e4, k == 1) %>%  
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    trans = "log"
    # limits = c(0.027,3)
  ) +
  facet_wrap(. ~ dpt_cdc) +
  theme_minimal()


# By Department (Wave 1)
gg_we_ki_1 <- config$pred$depts %>% 
  map(
    ~ df_we_ki %>% 
      filter(we_k < 3, k == 1, dpt_cdc == .x) %>%  
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = "log",
        limits = c(0.027,3)
      ) +
      labs(title = paste0(.x, " - Wave 1")) +
      theme_minimal()
  )

gg_we_ki_1 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_ki/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
    ))

# By Department (Wave 2) Grid
df_we_ki %>% 
  filter(we_k < 3, k == 2) %>%  # Had some weird outliers when high thetas, very few
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = "log",
    limits = c(0.027,3)
  ) +
  facet_wrap(. ~ dpt_cdc) +
  theme_minimal()

# By Department (Wave 2)
gg_we_ki_2 <- config$pred$depts %>% 
  map(
    ~ df_we_ki %>% 
      filter(we_k < 3, k == 2, dpt_cdc == .x) %>%  
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = "log",
        limits = c(0.027,3)
      ) +
      labs(title = paste0(.x, " - Wave 2")) +
      theme_minimal()
  )

gg_we_ki_2 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_ki/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))

# By Province (Wave 1) Grid
df_we_kij %>% 
  filter(we_k < 2e4, k == 1) %>%
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = "log"
    # limits = c(0.0000001, 1.5)
  ) +
  facet_wrap(. ~ prov_cdc) +
  theme_minimal()


# By Province by Department (Wave 1)
gg_we_kij_1 <- config$pred$depts %>% 
  map(
    ~ df_we_kij %>% 
      filter(we_k < 1.5, k == 1, dpt_cdc == .x) %>%  
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = "log",
        limits = c(0.0000001, 1.5)
      ) +
      facet_wrap(. ~ prov_cdc) +
      labs(title = paste0(.x, " - Wave 1")) +
      theme_minimal()
  )

gg_we_kij_1 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_kij/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))

# By Province (Wave 2) Grid
df_we_kij %>% 
  filter(we_k < 1.5, k == 2) %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = "log",
    limits = c(0.0000001, 1.5)
  ) +
  facet_wrap(. ~ prov_cdc) +
  theme_minimal()


# By Province by Department (Wave 2)
gg_we_kij_2 <- config$pred$depts %>% 
  map(
    ~ df_we_kij %>% 
      filter(we_k < 1.5, k == 2, dpt_cdc == .x) %>%  
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = "log",
        limits = c(0.0000001, 1.5)
      ) +
      facet_wrap(. ~ prov_cdc) +
      labs(title = paste0(.x, " - Wave 2")) +
      theme_minimal()
  )

gg_we_kij_2 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_kij/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))



# Lab -----------------------------------------------------------------------------------------


# By Province (Wave 1) Grid
df_we_kij_avg %>% 
  filter(k == 1) %>%
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k_avg)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    limits = c(0, 1)
  ) +
  facet_wrap(. ~ prov_cdc) +
  theme_minimal()


