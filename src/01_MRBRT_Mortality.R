# Load Packages -----------------------------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)
library(reticulate)
use_condaenv("mrtool-0.0.1")
library(mrtoolr)
library(ggpubr)
library(cowplot)

# Loading and Formatting the data -----------------------------------------

# Death count per day dataset
death_count_day <- read_csv("data/pre_processed/death_count_prov_day.csv")

# Province level population
poblacion_prov <- read_csv("data/pre_processed/poblacion_prov.csv")

# Offset of 0.00001
# Province level death counts
data_prov <- death_count_day %>% 
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
    )
  ) %>% 
  group_by(fecha_fallecimiento, dpt_cdc, prov_cdc) %>%
  summarise(n = sum(n)) %>% 
  left_join(poblacion_prov, by = "prov_cdc") %>% 
  ungroup() %>% 
  mutate(
    y1 = n / pob,
    ylog = log(y1 + min(y1, na.rm = TRUE)/2),
    x1 = (as.numeric(fecha_fallecimiento) - 18323) / 7,
    sd = sqrt(
      (1 - (y1 + min(y1, na.rm = TRUE)/2)) / 
        ((y1 + min(y1, na.rm = TRUE)/2) * pob)
    ),
    y_low = ylog - (1.96 * sd),
    y_upp = ylog + (1.96 * sd)
  ) %>% 
  na.omit() %>% 
  select(-dpt_cdc.y, dpt_cdc = dpt_cdc.x, -fecha_fallecimiento)

# Baseline Province level data y1 = 0
province_baseline <- purrr:::map(
  .x = list(-3,-2,-1),
  .f = ~ data_prov %>%
    group_by(dpt_cdc, prov_cdc, pob) %>%
    summarize(x1 = min(x1)) %>%
    mutate(
      x1 = 0 + .x,
      y1 = 0,
      ylog = log(0 + min(data_prov$y1)/2),
      n = 0,
      sd = 1,
      y_low = ylog - (1.96 * sd),
      y_upp = ylog + (1.96 * sd)
      )
  ) %>%
  bind_rows()

# Final provincial dataset with baseline dummy data
data_prov <- data_prov %>%
  bind_rows(province_baseline) %>%
  arrange(dpt_cdc, prov_cdc, pob, x1) %>%
  mutate(
    id = row_number(),
    x1 = x1 + 3
  )

# MR-BRT Model ---------------------------------------------------------------

# Load data in MRData object
prov_mrbrt <- MRData()
prov_mrbrt$load_df(
  data = data_prov,  
  col_obs = "ylog", 
  col_obs_se = "sd",
  col_covs = list("x1"), 
  col_study_id = "id" 
  )

# Create MRBRT model and configure covariates
mod_prov <- MRBRT(
  data = prov_mrbrt,
  cov_models = list(
    LinearCovModel("intercept", use_re = TRUE),
    LinearCovModel(
      alt_cov = "x1",
      use_spline = TRUE,
      spline_knots = array(c(seq(0, 1, by = 0.1))),
      spline_degree = 3L,
      spline_knots_type = 'frequency',
      prior_spline_maxder_gaussian = array(c(0, 0.03))
      )
    )
  )

# Fit MRBRT model
mod_prov$fit_model(
  inner_print_level = 5L, 
  inner_max_iter = 10000L
  )

# Create predicted MRData object
df_pred <- data.frame(x1 = seq(0, 85, by = 0.01))
dat_pred_prov <- MRData()
dat_pred_prov$load_df(
  data = df_pred, 
  col_covs = list('x1')
  )

# Obtain predicted values
pred_prov <- mod_prov$predict(data = dat_pred_prov)

# Plot national level mortality estimates
ggplot(data_prov) +
  geom_point(aes(x = x1, y = y1 * 100000), alpha = 0.3, size = 0.3) +
  geom_line(data = df_pred, aes(x = x1, y = exp(pred_prov) * 100000)) +
  labs(x = "Week", y = "Mortality per 100,000") +
  ylim(c(0,50)) +
  theme_bw()

# Cascade spline at department level --------------------------------------

# Create and fit MRBRT department-level cascade spline model
mod_spline_dpto <- run_spline_cascade(
  stage1_model_object = mod_prov,
  df = data_prov,
  col_obs = "ylog",
  col_obs_se = "sd",
  col_study_id = "id",
  stage_id_vars = "dpt_cdc",
  thetas = 10,
  output_dir = "output/",
  model_label = "mbrt_cascade_dpto_peru",
  overwrite_previous = TRUE
  )

# Obtain predicted values from the department-level cascade spline model
pred_cascade_dpto <- predict_spline_cascade(
  fit = mod_spline_dpto,
  newdata = expand.grid(
    x1 = seq(0, 85, by = 0.01),
    dpt_cdc = unique(data_prov$dpt_cdc)
    ) 
  )

# Plot department level mortality estimates
ggplot() +
  geom_line(aes(x = x1, y = exp(pred) * 100000), pred_cascade_dpto, ) +
  geom_point(aes(x = x1, y = y1 * 100000), data_prov,  size = 0.1, alpha = 0.5) +
  facet_wrap( . ~ dpt_cdc) +
  ylim(c(0, 80)) +
  labs(x = "Week", y = "Mortality per 100,000") +
  theme_bw()

# Cascade splines Prov --------------------------------------------------------

# Create and fit MRBRT province-level cascade spline model
mod_spline_prov <- run_spline_cascade(
  stage1_model_object = mod_prov,
  df = data_prov,
  col_obs = "ylog",
  col_obs_se = "sd",
  col_study_id = "id",
  stage_id_vars = c("dpt_cdc", "prov_cdc"),
  thetas = c(10, 10),
  output_dir = "output/",
  model_label = "mbrt_cascade_peru_prov",
  overwrite_previous = TRUE
  )

# Create prediction dataframe to fill predicted values
df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 85, by = 0.01),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>%
  left_join(poblacion_prov %>% select(-pob))

# Obtain predicted values from the provincial-level cascade spline model
pred_cascade_prov <- predict_spline_cascade(
  fit = mod_spline_prov,
  newdata = df_pred
  ) 

# depts <- unique(data_prov$dpt_cdc)
depts <- "ANCASH"

for (i in depts) {
  
  graph_dpto <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_errorbar(
      aes(
        x = x1,
        y = y1 * 1e5,
        ymin = exp(y_low) * 1e5,
        ymax = exp(y_upp) * 1e5
      ),
      size = 0.2
      ) +
    geom_point(
      aes(x = x1, y = y1 * 100000),
      size = 0.7,
      alpha = 0.4
    ) +
    geom_line(
      data = pred_cascade_dpto %>%  filter(dpt_cdc == i),
      aes(x = x1, y = exp(pred) * 100000), col = "red"
      ) +
    facet_wrap( . ~ dpt_cdc) +
    labs(x = NULL, y = "Mortality per 100,000") +
    theme_bw()
  
  graph_prov <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_point(aes(x = x1, y = y1 * 100000), size = 1, alpha = 0.5) +
    geom_errorbar(
      aes(
        x = x1,
        y = y1 * 1e5,
        ymin = exp(y_low) * 1e5,
        ymax = exp(y_upp) * 1e5
      ),
      size = 0.2) +
    geom_line(
      data = pred_cascade_prov %>%  filter(dpt_cdc == i),
      aes(x = x1, y = exp(pred) * 100000), col = "blue"
      ) +
    facet_wrap(.~prov_cdc) +
    coord_cartesian(
      ylim = c(
        0,
        max(
          data_prov %>% filter(dpt_cdc == i) %>% .$y1,
          na.rm = TRUE
        ) * 1e5 + 5
      )
    ) +
    labs(x = "Week", y = "Mortality per 100,000") +
    theme_bw()
  
  graph <-  ggdraw() +
    draw_plot(graph_dpto, x = 0.25, y = .7, width = .5, height = .3) +
    draw_plot(graph_prov, x = 0, y = 0, width = 1, height = 0.7)
  
  graph_dpto_log <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_errorbar(
      aes(
        x = x1,
        y = ylog,
        ymin = y_low,
        ymax = y_upp
      ),
      size = 0.2
      ) +
    geom_point(
      aes(x = x1, y = ylog),
      size = 0.7,
      alpha = 0.4
    ) +
    geom_line(
      data = pred_cascade_dpto %>%  filter(dpt_cdc == i),
      aes(x = x1, y = pred), col = "red"
      ) +
    facet_wrap(.~dpt_cdc) +
    labs(x = NULL, y = "Mortality per 100,000 (exp)") +
    theme_bw()

  graph_prov_log <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_point(aes(x = x1, y = ylog), size = 1, alpha = 0.5) +
    geom_errorbar(
      aes(
        x = x1,
        y = ylog,
        ymin = y_low,
        ymax = y_upp
      ),
      size = 0.2) +
    geom_line(
      data = pred_cascade_prov %>%  filter(dpt_cdc == i),
      aes(x = x1, y = pred), col = "blue"
      ) +
    facet_wrap(.~prov_cdc) +
    labs(x = "Week", y = "Mortality per 100,000 (exp)") +
    theme_bw()

  graph_log <-  ggdraw() +
    draw_plot(graph_dpto_log, x = 0.25, y = .7, width = .5, height = .3) +
    draw_plot(graph_prov_log, x = 0, y = 0, width = 1, height = 0.7)

  print(graph_log)
  print(graph)
  
  # ggsave(
  #   plot = graph_log, 
  #   filename =  paste0("plots/final/log-",i, ".png"), 
  #   scale = 2
  #   )
  # ggsave(
  #   plot = graph,
  #   filename =  paste0("plots/final/exp-",i, ".png"),
  #   scale = 2
    # )
}

prov_time_series <- predict_spline_cascade(
  fit = mod_spline_prov,
  newdata = df_pred
  ) %>%
  mutate(mortality = exp(pred)) %>% 
  write.csv("data/pred_prov_time_series.csv")
