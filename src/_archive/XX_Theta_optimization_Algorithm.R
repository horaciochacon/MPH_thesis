library(data.table)
library(tidyverse)
config <- config::get(config = "sensitivity")

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
}

data <- fread(paste0(output_dir, "/sensitivity/predicted_we/df_we_kij.csv"))
waves <- fread(paste0(output_dir, "/sensitivity/waves.csv"))

# Setting the minimal weighted error threshold algorithm --------------------------------------
con1 <- config$sensitivity$condition1
con2 <- config$sensitivity$condition1

mins_k1 <- data %>% 
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
  group_by(prov_cdc) %>% 
  summarize(min = min(we_k)) %>% 
  mutate(condition1 = min * con1) %>% 
  left_join(waves %>% filter(k == 2) %>% select(prov_cdc,k, w_k)) %>% 
  mutate(condition2 = min + (con2 * 42^2 * w_k)) %>% 
  mutate(
    mort_error_min = sqrt(min/w_k),
    mort_error_cond1 = sqrt((min * con1)/w_k),
    mort_error_cond2 = sqrt((min + (con2 * 42^2 * w_k))/w_k)
  )

# Filtering by the conditions -----------------------------------------------------------------

## Wave 1
best_k1_summary <-  data %>% 
  filter(k == 1) %>% 
  left_join(mins_k1) %>% 
  filter(we_k < condition1 | we_k < condition2) %>% 
  group_by(prov_cdc) %>% 
  summarize(n = n(), min = min(we_k))

best_k1 <-  data %>% 
  filter(k == 1) %>% 
  left_join(mins_k1) %>% 
  filter(we_k < condition1 | we_k < condition2)

best_k1_count <- best_k1 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  count()

best_k1_theta <- best_k1_count[which.max(best_k1_count$n), 1:2]

fwrite(best_k1_summary, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k1_summary.csv")) 
fwrite(best_k1, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k1.csv")) 
fwrite(best_k1_count, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k1_count.csv"))
fwrite(best_k1_theta, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k1_theta.csv"))

## Wave 2
best_k2_summary <-  data %>% 
  filter(k == 2) %>% 
  left_join(mins_k2) %>% 
  filter(we_k < condition1 | we_k < condition2) %>% 
  group_by(prov_cdc) %>% 
  summarize(n = n(), min = min(we_k))

best_k2 <-  data %>% 
  filter(k == 2) %>% 
  left_join(mins_k2) %>% 
  filter(we_k < condition1 | we_k < condition2)

best_k2_count <- best_k2 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  count()

best_k2_theta <- best_k2_count[which.max(best_k2_count$n), 1:2]

fwrite(best_k2_summary, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k2_summary.csv")) 
fwrite(best_k2, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k2.csv")) 
fwrite(best_k2_count, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k2_count.csv"))
fwrite(best_k2_theta, paste0(config$sbatch$wd, "sensitivity/best_theta/best_k2_theta.csv"))

# Plotting the heat maps ----------------------------------------------------------------------

best_k1_count %>%
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = n)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(50, max(k1$n))
  ) +
  theme_minimal()

best_k2_count  %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = n)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(100, max(k2$n))
  ) +
  theme_minimal()


# Lowest theta error combination per Province and wave ----------------------------------------

lowest_k1_we_kij <- data %>%
  filter(k == 1, !is.infinite(we_k)) %>%
  group_by(prov_cdc) %>%
  slice_min(we_k) %>%
  select(prov_cdc, theta_dpt_min = theta_dpt, theta_prov_min = theta_prov, we_k_min = we_k) 

fwrite(lowest_k1_we_kij, paste0(config$sbatch$wd, "sensitivity/best_theta/lowest_k1_we_kij.csv"))

lowest_k2_we_kij <- data %>%
  filter(k == 2, !is.infinite(we_k)) %>%
  group_by(prov_cdc) %>%
  slice_min(we_k) %>%
  select(prov_cdc, theta_dpt_min = theta_dpt, theta_prov_min = theta_prov, we_k_min = we_k) 

fwrite(lowest_k2_we_kij, paste0(config$sbatch$wd, "sensitivity/best_theta/lowest_k2_we_kij.csv"))

# Integrating Best Theta with algorithm and condition and Lowest Theta ------------------------

merged_k1 <- data %>% 
  filter(k == 1, theta_dpt == best_k1_theta$theta_dpt, theta_prov == best_k1_theta$theta_prov) %>% 
  select(prov_cdc, we_k_best = we_k) %>% 
  left_join(lowest_k1_we_kij) %>% 
  mutate(
    k = 1,
    abs_diff = abs(we_k_min - we_k_best),
    rel_diff = abs(we_k_min - we_k_best)/we_k_min
  )

fwrite(merged_k1, paste0(config$sbatch$wd, "sensitivity/best_theta/merged_k1.csv"))

merged_k2 <- data %>% 
  filter(k == 2, theta_dpt == best_k2_theta$theta_dpt, theta_prov == best_k2_theta$theta_prov) %>% 
  select(prov_cdc, we_k_best = we_k) %>% 
  left_join(lowest_k2_we_kij) %>% 
  mutate(
    k = 2,
    abs_diff = abs(we_k_min - we_k_best),
    rel_diff = abs(we_k_min - we_k_best)/we_k_min
  )

fwrite(merged_k2, paste0(config$sbatch$wd, "sensitivity/best_theta/merged_k2.csv"))
  
summary_best_theta <- merged_k1 %>% bind_rows(merged_k2)
  
fwrite(
  summary_best_theta, 
  paste0(config$sbatch$wd, "sensitivity/best_theta/summary_best_theta.csv")
  )
  
  
  
