####################################################################################################
## Author:      Horacio Chacon Torrico
##
## Description: This script identifies the best theta parameter combinations for the MR-BRT 
##              provincial-level sensitivity analysis. It loads the aggregated weighted error data,
##              applies conditions to filter the best theta combinations, and saves the results. 
##              The script also generates heatmaps for visualizing the best theta combinations.
##
## Passed args: output_dir [character] -- directory to save outputs
##              config_loc [character] -- path to the configuration file
##
## Requires:    aggregated weighted errors by wave, department, and province 
##              ("estimates/predicted_we/df_we_kij.csv")
##              waves data ("estimates/waves.csv")
##
## Outputs:     best theta combinations and summaries:
##                - best theta summary for wave 1 ("estimates/best_theta/best_k1_summary.csv")
##                - best theta data for wave 1 ("estimates/best_theta/best_k1.csv")
##                - best theta counts for wave 1 ("estimates/best_theta/best_k1_count.csv")
##                - best theta parameters for wave 1 ("estimates/best_theta/best_k1_theta.csv")
##                - best theta summary for wave 2 ("estimates/best_theta/best_k2_summary.csv")
##                - best theta data for wave 2 ("estimates/best_theta/best_k2.csv")
##                - best theta counts for wave 2 ("estimates/best_theta/best_k2_count.csv")
##                - best theta parameters for wave 2 ("estimates/best_theta/best_k2_theta.csv")
##                - lowest theta error combination per province and wave for wave 1 
##                  ("estimates/best_theta/lowest_k1_we_kij.csv")
##                - lowest theta error combination per province and wave for wave 2 
##                  ("estimates/best_theta/lowest_k2_we_kij.csv")
##                - integrated best theta with algorithm and condition and lowest theta for wave 1 
##                  ("estimates/best_theta/merged_k1.csv")
##                - integrated best theta with algorithm and condition and lowest theta for wave 2 
##                  ("estimates/best_theta/merged_k2.csv")
##                - summary of best theta parameters ("estimates/best_theta/summary_best_theta.csv")
##                - heatmap for wave 1 best theta ("plots/heatmaps/wave1_best.pdf")
##                - heatmap for wave 2 best theta ("plots/heatmaps/wave2_best.pdf")
##
####################################################################################################

library(data.table)
library(tidyverse)
library(viridis)

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
  (config_loc <- as.character(commandArgs(TRUE)[[2]]))
}


config <- config::get(file = config_loc, config = "sensitivity")
source(paste0(config$sbatch$wd, "R/sens_functions.R"))

data <- fread(paste0(output_dir, "/estimates/predicted_we/df_we_kij.csv"))
waves <- fread(paste0(output_dir, "/estimates/waves.csv"))

# Setting the minimal weighted error threshold algorithm --------------------------------------
con1 <- config$sensitivity$condition1
con2 <- config$sensitivity$condition2


provs <- data %>% 
  select(dpt_cdc, prov_cdc) %>% 
  unique() %>% 
  group_by(dpt_cdc) %>% 
  summarize(n_provs = n())

best <- data %>% 
  filter(k == 1) %>% 
  group_by(theta, dpt_cdc) %>% 
  summarize(we_k = sum(we_k)) %>% 
  arrange(dpt_cdc) %>% 
  group_by(dpt_cdc) %>% 
  slice_min(we_k, with_ties = FALSE)


mins_k1 <- data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 1) %>% 
  group_by(prov_cdc) %>% 
  summarize(min = min(we_k)) %>% 
  mutate(condition1 = min * con1) %>% 
  left_join(waves %>% filter(k == 1) %>% select(prov_cdc,k, w_k)) %>% 
  mutate(condition2 = min + ((con2 * 43)^2 * w_k)) %>% 
  mutate(
    mort_error_min = sqrt(min/w_k),
    mort_error_cond1 = sqrt((min * con1)/w_k),
    mort_error_cond2 = sqrt((min + ((con2 * 43)^2 * w_k))/w_k)
  )

mins_k2 <- data %>% 
  filter(k == 2) %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  group_by(prov_cdc) %>% 
  summarize(min = min(we_k)) %>% 
  mutate(condition1 = min * con1) %>% 
  left_join(waves %>% filter(k == 2) %>% select(prov_cdc,k, w_k)) %>% 
  mutate(condition2 = min + (con2 * 42)^2 * w_k) %>% 
  mutate(
    mort_error_min = sqrt(min/w_k),
    mort_error_cond1 = sqrt((min * con1)/w_k),
    mort_error_cond2 = sqrt((min + (con2 * 42)^2 * w_k)/w_k)
  )

# Filtering by the conditions -----------------------------------------------------------------

## Wave 1
best_k1_summary <-  data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 1) %>% 
  left_join(mins_k1) %>% 
  filter(we_k < condition1 | we_k < condition2) %>% 
  group_by(prov_cdc) %>% 
  summarize(n = n(), min = min(we_k))

best_k1 <-  data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 1) %>% 
  left_join(mins_k1) %>% 
  filter(we_k < condition1 | we_k < condition2)

best_k1_count <- best_k1 %>% 
  group_by(dpt_cdc, theta) %>% 
  count() %>% 
  left_join(provs)

best_k1_count_list <- best_k1_count %>% group_by(dpt_cdc) %>% group_split()

best_k1_theta <- best_k1_count_list %>%
  map_df(
    ~
      {
        max <- max(.x$n)
        min_theta <- .x %>% 
          filter(n == max & theta >= 1) %>% 
          pull(theta) %>% 
          min()
        .x %>% 
          filter(theta == min_theta)
      }
  )

fwrite(best_k1_summary, paste0(output_dir, "/estimates/best_theta/best_k1_summary.csv")) 
fwrite(best_k1, paste0(output_dir, "/estimates/best_theta/best_k1.csv")) 
fwrite(best_k1_count, paste0(output_dir, "/estimates/best_theta/best_k1_count.csv"))
fwrite(best_k1_theta, paste0(output_dir, "/estimates/best_theta/best_k1_theta.csv"))

## Wave 2
best_k2_summary <-  data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 2) %>% 
  left_join(mins_k2) %>% 
  filter(we_k < condition1 | we_k < condition2) %>% 
  group_by(prov_cdc) %>% 
  summarize(n = n(), min = min(we_k))

best_k2 <-  data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 2) %>% 
  left_join(mins_k2) %>% 
  filter(we_k < condition1 | we_k < condition2)

best_k2_count <- best_k2 %>% 
  group_by(dpt_cdc, theta) %>% 
  count() %>% 
  left_join(provs)

best_k2_count_list <- best_k2_count %>% group_by(dpt_cdc) %>% group_split()

best_k2_theta <- best_k2_count_list %>%
  map_df(
    ~
      {
        max <- max(.x$n)
        min_theta <- .x %>% 
          filter(n == max & theta >= 1) %>% 
          pull(theta) %>% 
          min()
        .x %>% 
          filter(theta == min_theta)
      }
  )

fwrite(best_k2_summary, paste0(output_dir, "/estimates/best_theta/best_k2_summary.csv")) 
fwrite(best_k2, paste0(output_dir, "/estimates/best_theta/best_k2.csv")) 
fwrite(best_k2_count, paste0(output_dir, "/estimates/best_theta/best_k2_count.csv"))
fwrite(best_k2_theta, paste0(output_dir, "/estimates/best_theta/best_k2_theta.csv"))

# Lowest theta error combination per Province and wave ----------------------------------------

lowest_k1_we_kij <- data %>%
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 1, !is.infinite(we_k)) %>%
  group_by(prov_cdc) %>%
  slice_min(we_k) %>%
  select(prov_cdc, theta_min = theta, we_k_min = we_k) 

fwrite(lowest_k1_we_kij, paste0(output_dir, "/estimates/best_theta/lowest_k1_we_kij.csv"))

lowest_k2_we_kij <- data %>%
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 2, !is.infinite(we_k)) %>%
  group_by(prov_cdc) %>%
  slice_min(we_k) %>%
  select(prov_cdc, theta_min = theta, we_k_min = we_k) 

fwrite(lowest_k2_we_kij, paste0(output_dir, "/estimates/best_theta/lowest_k2_we_kij.csv"))

# Integrating Best Theta with algorithm and condition and Lowest Theta ------------------------

merged_k1 <- data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 1) %>% 
  left_join(best_k1_theta) %>% 
  na.omit() %>% 
  select(dpt_cdc, prov_cdc, best_theta = theta, we_k_best = we_k, -n, -n_provs) %>% 
  left_join(lowest_k1_we_kij) %>% 
  mutate(
    k = 1,
    abs_diff = abs(we_k_min - we_k_best),
    rel_diff = abs(we_k_min - we_k_best)/we_k_min
  )

fwrite(merged_k1, paste0(output_dir, "/estimates/best_theta/merged_k1.csv"))

merged_k2 <- data %>% 
  mutate(we_k = as.numeric(we_k)) %>% 
  filter(k == 2) %>% 
  left_join(best_k2_theta) %>% 
  na.omit() %>% 
  select(dpt_cdc, prov_cdc, best_theta = theta, we_k_best = we_k, -n, -n_provs) %>% 
  left_join(lowest_k2_we_kij) %>% 
  mutate(
    k = 2,
    abs_diff = abs(we_k_min - we_k_best),
    rel_diff = abs(we_k_min - we_k_best)/we_k_min
  )

fwrite(merged_k2, paste0(output_dir, "/estimates/best_theta/merged_k2.csv"))

summary_best_theta <- merged_k1 %>% bind_rows(merged_k2)

fwrite(
  summary_best_theta, 
  paste0(output_dir, "/estimates/best_theta/summary_best_theta.csv")
)


# Heat Map plots ------------------------------------------------------------------------------

heatmap_k1 <- best_k1_count %>%
  filter(theta_dpt>=1, theta_prov>=1) %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = n)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(150, max(best_k1_count$n))
    ) +
  add_config_caption(config) +
  theme_minimal()

ggsave(
  filename = paste0(output_dir, "/plots/heatmaps/wave1_best.pdf"),
  plot = heatmap_k1, scale = 2.5
  )

heatmap_k2 <- best_k2_count  %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = n)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(180, max(best_k2_count $n))
    ) +
  add_config_caption(config) +
  theme_minimal()

ggsave(
  filename = paste0(output_dir, "/plots/heatmaps/wave2_best.pdf"),
  plot = heatmap_k2, scale = 2.5
)


# Saving best run Preductions -----------------------------------------------------------------

# Set path to folder
path <- paste0(output_dir, "/MR_BRT/output/")

# List all files in the folder
file_list <- list.files(path, pattern = "\\.gz$", full.names = TRUE)

# Read all files in the folder

data_pred <- rbindlist(
  lapply(file_list, 
         function(file) {
           df <- fread(file)
           temp <- str_split(file, "//",simplify = TRUE)[,2]
           df$theta_prov <- as.numeric(str_split_i(str_split_i(temp, "-",1), "_", 2))
           df$theta_dpt <- as.numeric(str_split_i(str_split_i(temp, "-",2), "[_|.]", 2))
           return(df)
         }
  )
)

data_pred_best_k1 <- data_pred[
  theta_dpt == best_k1_theta$theta_dpt & theta_prov == best_k1_theta$theta_prov
  ]

data_pred_best_k2 <- data_pred[
  theta_dpt == best_k2_theta$theta_dpt & theta_prov == best_k2_theta$theta_prov
]

fwrite(data_pred_best_k1, paste0(output_dir, "/estimates/best_theta/data_pred_best_k1.csv"))
fwrite(data_pred_best_k2, paste0(output_dir, "/estimates/best_theta/data_pred_best_k2.csv"))


print("Script Completed!")
