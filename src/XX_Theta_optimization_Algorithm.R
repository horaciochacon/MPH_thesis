library(data.table)
library(tidyverse)
config <- config::get(config = "sensitivity")

data <- fread(paste0(config$sbatch$wd, "sensitivity/predicted_we/df_we_kij.csv"))
waves <- fread(paste0(config$sbatch$wd, "sensitivity/waves.csv"))


# Setting the minimal weighted error threshold algorithm --------------------------------------
con1 <- 1.2
con2 <- 1.5

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

best_k1_count <-  data %>% 
  filter(k == 1) %>% 
  left_join(mins_k1) %>% 
  filter(we_k < condition1 | we_k < condition2) %>% 
  group_by(prov_cdc) %>% 
  summarize(n = n(), min = min(we_k))

best_k1 <-  data %>% 
  filter(k == 1) %>% 
  left_join(mins_k1) %>% 
  filter(we_k < condition1 | we_k < condition2)

best_k2_count <-  data %>% 
  filter(k == 2) %>% 
  left_join(mins_k2) %>% 
  filter(we_k < condition1 | we_k < condition2) %>% 
  group_by(prov_cdc) %>% 
  summarize(n = n(), min = min(we_k))

best_k2 <-  data %>% 
  filter(k == 2) %>% 
  left_join(mins_k2) %>% 
  filter(we_k < condition1 | we_k < condition2)


# Plotting the heatmaps -----------------------------------------------------------------------

k1 <- best_k1 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  count()

k2 <-  best_k2 %>% 
  group_by(theta_dpt, theta_prov) %>% 
  count()


k1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = n)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(50, max(k1$n))
  ) +
  theme_minimal()

k2 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt, fill = n)) +
  geom_tile() +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(100, max(k2$n))
  ) +
  theme_minimal()

# Finding Clusters ----------------------------------------------------------------------------

clust_k1 <- kmeans(best_k1 %>% select(theta_dpt, theta_prov), centers = 10, nstart = 20)

k1 %>% 
  ggplot(aes(x = theta_prov, y = theta_dpt)) +
  geom_tile(aes(fill = n)) +
  geom_point(color = "white", data = clust_k1$centers %>% as_tibble) +
  scale_fill_viridis(
    option = "inferno",
    direction = -1,
    limits = c(50, max(k1$n))
  ) +
  theme_minimal()

