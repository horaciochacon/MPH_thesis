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
library(viridis)
library(viridisLite)
library(stringr)
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
waves <- waves[!prov_cdc == "LIMA"]
waves <- waves[, w_k := ifelse(
  k == 1, y_obs / sum(waves$y_obs[waves[,k == 1]]),
  y_obs / sum(waves$y_obs[waves[,k == 2]]))
  ]
waves

# Predicted 
data_sens <- copy(data)

data_sens <- (
  data_sens
  [ !is.na(y_obs)]
  [, k := ifelse(x1 < 43, 1, 2)]
  [waves, on = .(prov_cdc, dpt_cdc, k), nomatch = NA]
  [, e := (exp(y_obs) * 1e6 - exp(y_hat)*1e6)^2]
  [, we := e * w_k]
)


# Saving the Predicted theta-specific Weighted Errors -----------------------------------------

df_we_k   <- data_sens[, .(we_k = sum(we[is.finite(we)])), by = .(theta_dpt, theta_prov, k)]
df_we_l   <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov,l = x1)]
df_we_ki  <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc)]
df_we_kij <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc, prov_cdc)]


# data <- df_we_kij %>% 
#   group_by(prov_cdc, k) %>% 
#   mutate(we_k = ifelse(we_k >= 4e4, NA, we_k)) %>% 
#   summarise(med = median(we_k, na.rm = TRUE))
# 
# df_we_kij_avg <-  df_we_kij %>% 
#   left_join(data) %>% 
#   mutate(we_k_avg = we_k / med)

fwrite(waves, paste0(config$sbatch$wd, "sensitivity/waves.csv")) 
fwrite(df_we_k, paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_k.csv")) 
fwrite(df_we_l, paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_l.csv"))
fwrite(df_we_ki, paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_ki.csv"))
fwrite(df_we_kij,  paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_kij.csv"))

config$pred$provs <- config$pred$provs[-which(config$pred$provs == "LIMA")]

# Heatmaps ---------------------------------------------------------------------------------------


# Wave 1
df_we_k %>% 
  filter(k == 1) %>%
  mutate(
    we_k = ifelse(
      we_k > quantile(.$we_k, probs = 0.8),
      quantile(.$we_k, probs = 0.8),
      we_k
    )
  ) %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno", 
    direction = -1,
    trans = "log"
    ) +
  theme_minimal()

# Wave 2
df_we_k %>% 
  filter(k == 2) %>%
  mutate(
    we_k = ifelse(
      we_k > quantile(.$we_k, probs = 0.8),
      quantile(.$we_k, probs = 0.8),
      we_k
    )
  ) %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno", 
    direction = -1,
    trans = "log"
  ) +
  theme_minimal()


# By Department (Wave 1)
gg_we_ki_1 <- config$pred$depts %>% 
  map(
    ~ df_we_ki %>% 
      filter(k == 1, dpt_cdc == .x) %>%  
      mutate(
        we_k = ifelse(
          we_k > quantile(.$we_k, probs = 0.8),
          quantile(.$we_k, probs = 0.8),
          we_k
        )
      ) %>% 
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = "log"
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

# By Department (Wave 2)
gg_we_ki_2 <- config$pred$depts %>% 
  map(
    ~ df_we_ki %>% 
      filter(k == 2, dpt_cdc == .x) %>%  
      mutate(
        we_k = ifelse(
          we_k > quantile(.$we_k, probs = 0.8),
          quantile(.$we_k, probs = 0.8),
          we_k
        )
      ) %>% 
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = "log"
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


# By Province (Wave 1)
gg_we_kij_1 <- config$pred$provs %>% 
  map(
    ~ df_we_kij %>% 
      filter(k == 1, prov_cdc == .x) %>%  
      mutate(
        we_k = ifelse(
          we_k > quantile(.$we_k, probs = 0.8),
          quantile(.$we_k, probs = 0.8),
          we_k
          )
        )%>% 
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = log_trans()
        # limits = c(0.0000001, 1.5)
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

# By Province (Wave 2)
gg_we_kij_2 <- config$pred$provs %>% 
  map(
    ~ df_we_kij %>% 
      filter(k == 2, prov_cdc == .x) %>%  
      mutate(
        we_k = ifelse(
          we_k > quantile(.$we_k, probs = 0.8),
          quantile(.$we_k, probs = 0.8),
          we_k
        )
      )%>% 
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = log_trans()
        # limits = c(0.0000001, 1.5)
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

# By week 
gg_we_l <- 3:85 %>% 
  map(
    ~ df_we_l %>% 
      filter(l == .x) %>%  
      mutate(
        we_k = ifelse(
          we_k > quantile(.$we_k, probs = 0.8),
          quantile(.$we_k, probs = 0.8),
          we_k
        )
      )%>% 
      ggplot(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
      geom_tile() +
      scale_fill_viridis(
        option = "inferno",
        direction = -1, 
        trans = log_trans()
      ) +
      labs(title = paste0("Week ", .x)) +
      theme_minimal()
  )

gg_we_l %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_l/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))


# Additional Exploration ----------------------------------------------------------------------

df_we_l %>% 
  group_by(l) %>% 
  summarise(median_we = median(we_k)) %>% 
  ggplot(aes(x = l, y = median_we)) +
  geom_line(linewidth = 1) +
  labs(x = "Week", y = "Weighted Error") +
  theme_bw()


# 5 -10% Cluster identification (Province) --------------------------------------------------------

# Per Province per Wave 1
prov_min_5_df_1 <- df_we_kij %>% 
  filter(k == 1) %>% 
  group_by(prov_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

prov_min_5_agg_1 <- prov_min_5_df_1 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
prov_min_5_agg_1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()

# Per Province per Wave 2
prov_min_5_df_2 <- df_we_kij %>% 
  filter(k == 2) %>% 
  group_by(prov_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

prov_min_5_agg_2 <- prov_min_5_df_2 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
prov_min_5_agg_2 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()


# Plotting the Clusters  ---------------------------------------------------------------------------

generate_combined_plot <- function(prov_cdc_value, cluster_df, wave) {
  # Filter and prepare the first dataset
  data1 <- df_we_kij %>%
    filter(k == wave, prov_cdc == prov_cdc_value) %>%
    mutate(
      we_k = ifelse(
        we_k > quantile(we_k, probs = 0.8),
        quantile(we_k, probs = 0.8),
        we_k
      )
    )
  
  # Filter the second dataset
  data2 <- cluster_df %>%
    filter(prov_cdc == prov_cdc_value)
  
  # Combine plots
  ggplot(data = data1) +
    geom_tile(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
    scale_fill_viridis(option = "inferno", direction = -1, trans = "log") +
    geom_tile(
      data = data2,
      aes(x = theta_prov, y = theta_dpt), 
      fill = "transparent", color = "black", size = 1
      ) +
    labs(title = paste0(unique(data1$dpt_cdc), " - ", prov_cdc_value, " - Wave ", wave)) +
    theme_minimal() +
    lims(x = c(1, 50), y = c(1, 50))
}

# Clusters overlay-ed into Heat map at province-level (Wave 1)
combined_plots_1 <- unique(df_we_kij$prov_cdc) %>%
  map(~generate_combined_plot(.x, prov_min_5_df_1, 1))

combined_plots_1 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_cluster_we_ijk/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))

# Clusters overlay-ed into Heat map at province-level (Wave 2)
combined_plots_2 <- unique(df_we_kij$prov_cdc) %>%
  map(~generate_combined_plot(.x, prov_min_5_df_2, 2))

combined_plots_2 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_cluster_we_ijk/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))

# Alternative plots 

# Combined by Waves
# Function to extract "Department - Province" from the plot title
extract_dept_prov <- function(plot) {
  full_title <- plot$labels$title
  parts <- strsplit(full_title, " - ")[[1]]
  return(paste(parts[1], parts[2], sep = " - "))
}

# Combine the plots and extract titles
wave_combined <- map2(
  combined_plots_1, 
  combined_plots_2, 
  ~ {
    combined_plot <- plot_grid(.x, .y, ncol = 2)
    title <- extract_dept_prov(.x)  # Assuming the title is the same for both .x and .y
    list(plot = combined_plot, title = title)
  }
)

# Save the combined plots with extracted titles
map(wave_combined, ~ggsave(
  filename = paste0("sensitivity/Plots/we_cluster_we_ijk/Wave Combined/", .x$title, ".pdf"), 
  plot = .x$plot,
  scale = 1,
  height = 6,
  width = 10
))

# Combined by Department (Wave 1)
# Extract titles
titles <- map_vec(combined_plots_1, ~ .x$labels$title) %>% 
  str_split(" - ", simplify = T) %>% .[,1]

# Extract department names and group the plots
grouping_parts <- map_chr(titles, ~ strsplit(.x, " - ")[[1]][1])
grouped_data <- tibble(plot = combined_plots_1, group = grouping_parts)
grouped_plots <- grouped_data %>% group_by(group) %>% group_split()

# Combine the plots in each group
plot_grids <- map(grouped_plots, ~plot_grid(plotlist = .x$plot, align = 'v'))

# Save the combined plots
map2(plot_grids, config$pred$depts, ~ggsave(
  filename = paste0(
    "sensitivity/Plots/we_cluster_we_ijk/Department Combined/",
    .y,
    " - Wave 1",
    ".pdf"),
  plot = .x,
  scale = 3,
  height = 7,
  width = 10
))

# Combined by Department (Wave 2)
# Extract titles
titles <- map_vec(combined_plots_2, ~ .x$labels$title) %>% 
  str_split(" - ", simplify = T) %>% .[,1]

# Extract department names and group the plots
grouping_parts <- map_chr(titles, ~ strsplit(.x, " - ")[[1]][1])
grouped_data <- tibble(plot = combined_plots_2, group = grouping_parts)
grouped_plots <- grouped_data %>% group_by(group) %>% group_split()

# Combine the plots in each group
plot_grids <- map(grouped_plots, ~plot_grid(plotlist = .x$plot, align = 'v'))

# Save the combined plots
map2(plot_grids, config$pred$depts, ~ggsave(
  filename = paste0(
    "sensitivity/Plots/we_cluster_we_ijk/Department Combined/",
    .y,
    " - Wave 2",
    ".pdf"),
  plot = .x,
  scale = 3,
  height = 7,
  width = 10
))

# 5 -10% Cluster identification (Week) --------------------------------------------------------

# Week
week_min_5_df <- df_we_l %>% 
  group_by(l) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

week_min_5_agg <-week_min_5_df %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination 
week_min_5_agg %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()

# Week (Wave 1)
week_min_5_df_1 <- df_we_l %>%
  filter(l < 43) %>% 
  group_by(l) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

week_min_5_agg_1 <-week_min_5_df_1 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
week_min_5_agg_1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()

# Week (Wave 2)
week_min_5_df_2 <- df_we_l %>%
  filter(l >= 43) %>% 
  group_by(l) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

week_min_5_agg_2 <-week_min_5_df_2 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 2)
week_min_5_agg_2 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()



# Plotting the Clusters Week------------------------------------------------------------------------

generate_combined_plot <- function(weeks, cluster_df) {
  # Filter and prepare the first dataset
  data1 <- df_we_l %>%
    filter(l == weeks) %>%
    mutate(
      we_k = ifelse(
        we_k > quantile(we_k, probs = 0.8),
        quantile(we_k, probs = 0.8),
        we_k
      )
    )
  
  # Filter the second dataset
  data2 <- cluster_df %>%
    filter(l == weeks)
  
  # Combine plots
  ggplot(data = data1) +
    geom_tile(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
    scale_fill_viridis(option = "inferno", direction = -1, trans = "log") +
    geom_tile(
      data = data2,
      aes(x = theta_prov, y = theta_dpt), 
      fill = "transparent", color = "black", size = 1
    ) +
    labs(title = paste0("Week - ", weeks)) +
    theme_minimal() +
    lims(x = c(1, 50), y = c(1, 50))
}

# Clusters overlay-ed into Heat map at province-level (Wave 1)
weeks_combined <- 3:85 %>%
  map(~generate_combined_plot(.x, week_min_5_df))

# Saving the plots
weeks_combined %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_cluster_we_l/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))

# 5 -10% Cluster identification (Department) ------------------------------------------------------

# Per Department per Wave 1
dpt_min_5_df_1 <- df_we_ki %>% 
  filter(k == 1) %>% 
  group_by(dpt_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

dpt_min_5_agg_1 <- dpt_min_5_df_1 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
dpt_min_5_agg_1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()

# Per Department per Wave 2
dpt_min_5_df_2 <- df_we_ki %>% 
  filter(k == 2) %>% 
  group_by(dpt_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  )

dpt_min_5_agg_2 <- dpt_min_5_df_2 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
dpt_min_5_agg_2 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  theme_bw()

# Plotting the Clusters  ---------------------------------------------------------------------------

generate_combined_plot <- function(dpt_cdc_value, cluster_df, wave) {
  # Filter and prepare the first dataset
  data1 <- df_we_ki %>%
    filter(k == wave, dpt_cdc == dpt_cdc_value) %>%
    mutate(
      we_k = ifelse(
        we_k > quantile(we_k, probs = 0.8),
        quantile(we_k, probs = 0.8),
        we_k
      )
    )
  
  # Filter the second dataset
  data2 <- cluster_df %>%
    filter(dpt_cdc == dpt_cdc_value)
  
  # Combine plots
  ggplot(data = data1) +
    geom_tile(aes(x = theta_prov, y = theta_dpt, fill = we_k)) +
    scale_fill_viridis(option = "inferno", direction = -1, trans = "log") +
    geom_tile(
      data = data2,
      aes(x = theta_prov, y = theta_dpt), 
      fill = "transparent", color = "black", size = 1
    ) +
    labs(title = paste0(unique(data1$dpt_cdc), " - Wave ", wave)) +
    theme_minimal() +
    lims(x = c(1, 50), y = c(1, 50))
}

# Clusters overlay-ed into Heat map at department-level (Wave 1)
combined_plots_1 <- unique(df_we_ki$dpt_cdc) %>%
  map(~generate_combined_plot(.x, dpt_min_5_df_1, 1))

combined_plots_1 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_cluster_we_ik/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))

# Clusters overlay-ed into Heat map at department-level (Wave 2)
combined_plots_2 <- unique(df_we_ki$dpt_cdc) %>%
  map(~generate_combined_plot(.x, dpt_min_5_df_2, 2))

combined_plots_2 %>% 
  map(~ggsave(
    filename = paste0("sensitivity/Plots/we_cluster_we_ik/", .x$labels$title, ".pdf"), 
    plot = .x,
    scale = 1,
    height = 10,
    width = 10
  ))



# Error Size Exploration - Weighted Error categories ------------------------------------------

we_kij_min_1 <- df_we_kij %>% 
  filter(k == 1) %>% 
  group_by(dpt_cdc, prov_cdc, k) %>% 
  summarise(min = quantile(we_k, 0.05)) %>% 
  ungroup() %>%
  mutate(cat = cut_number(min, 6)) %>% 
  select(prov_cdc, k, cat)
  

# Per Province per Wave 1
prov_min_5_df_1 <- df_we_kij %>% 
  filter(k == 1) %>% 
  group_by(prov_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  ) %>% 
  left_join(we_kij_min_1)

prov_min_5_agg_1 <- prov_min_5_df_1 %>% 
  group_by(theta_dpt, theta_prov, cat) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
prov_min_5_agg_1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  facet_wrap(. ~ cat) +
  theme_bw()


# Wave 2
we_kij_min_2 <- df_we_kij %>% 
  filter(k == 2) %>% 
  group_by(dpt_cdc, prov_cdc, k) %>% 
  summarise(min = quantile(we_k, 0.05)) %>% 
  ungroup() %>%
  mutate(cat = cut_number(min, 6)) %>% 
  select(prov_cdc, k, cat)


# Per Province per Wave 2
prov_min_5_df_2 <- df_we_kij %>% 
  filter(k == 2) %>% 
  group_by(prov_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  ) %>% 
  left_join(we_kij_min_2)

prov_min_5_agg_2 <- prov_min_5_df_2 %>% 
  group_by(theta_dpt, theta_prov, cat) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
prov_min_5_agg_2 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  facet_wrap(. ~ cat) +
  theme_bw()

# Error Size Exploration - Weight categories ------------------------------------------


# Wave 1
weight_cat <- waves %>% 
  filter(k == 1) %>% 
  mutate(cat = cut_number(w_k, 6)) %>% 
  select(prov_cdc, k, cat)
  
  
# Per Province per Wave 1
prov_min_5_df_1 <- df_we_kij %>% 
  filter(k == 1) %>% 
  group_by(prov_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  ) %>% 
  left_join(weight_cat)

prov_min_5_agg_1 <- prov_min_5_df_1 %>% 
  group_by(theta_dpt, theta_prov, cat) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
prov_min_5_agg_1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  facet_wrap(. ~ cat) +
  theme_bw() 

# Wave 2
weight_cat <- waves %>% 
  filter(k == 2) %>% 
  mutate(cat = cut_number(w_k, 6)) %>% 
  select(prov_cdc, k, cat)


# Per Province per Wave 1
prov_min_5_df_2 <- df_we_kij %>% 
  filter(k == 2) %>% 
  group_by(prov_cdc) %>% 
  group_split() %>% 
  map_df(
    ~ .x %>% 
      filter(we_k < quantile(.$we_k, probs = 0.05))
  ) %>% 
  left_join(weight_cat)

prov_min_5_agg_2 <- prov_min_5_df_2 %>% 
  group_by(theta_dpt, theta_prov, cat) %>% 
  summarise(sum = n())

# Plot count heat map of count of lowest 5% we by theta combination (Wave 1)
prov_min_5_agg_2 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = sum)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1, 
    trans = log_trans()
  ) +
  facet_wrap(. ~ cat) +
  theme_bw() 
