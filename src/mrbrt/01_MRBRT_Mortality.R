# Load Packages -----------------------------------------------------------

library(lubridate)
library(ggplot2)
library(reticulate)
library(ggpubr)
library(cowplot)
library(data.table)
library(tidyr)
library(dplyr)
reticulate::use_python("/ihme/code/mscm/miniconda3/envs/mrtool_0.0.1/bin/python")
cw <- reticulate::import("crosswalk")
mr <- reticulate::import("mrtool")
config <- config::get()
library(mrbrt003, lib.loc = "/ihme/code/mscm/Rv4/dev_packages/")

# Loading and Formatting the data -----------------------------------------

# Death count per day dataset
death_count_day <- fread("data/pre_processed/death_count_prov_day.csv")

# Province level population
poblacion_prov <- fread("data/pre_processed/poblacion_prov.csv")

# Province level death counts
data_prov <- (
  death_count_day
  [, fecha_fallecimiento := floor_date(fecha_fallecimiento, "weeks", week_start = 1) ]
  [, .(n = sum(n)), by = .(fecha_fallecimiento, dpt_cdc, prov_cdc) ]
  [ poblacion_prov[, 2:5], on = .(prov_cdc) ]
  [, y1 := n / pob ]
  [, x1 := (as.numeric(fecha_fallecimiento) - 18323) / 7 ]
  [, sd := sqrt((y1 * (1 - y1)) / pob) ]
  [, ylog := cw$utils$linear_to_log(mean = array(y1), sd = array(sd))[[1]]]
  [, sdlog := cw$utils$linear_to_log(mean = array(y1), sd = array(sd))[[2]]]
  [, !"fecha_fallecimiento"]
)

# Baseline Province level data y1 = 0
province_baseline <- purrr:::map(
  .x = list(-3,-2,-1),
  .f = ~ data_prov %>%
    group_by(dpt_cdc, prov_cdc, pob) %>%
    summarize(x1 = min(x1)) %>%
    mutate(
      x1 = 0 + .x,
      y1 = 0,
      ylog = log(min(data_prov$y1)/2),
      n = 0,
      sd = 1,
      ylog = cw$utils$linear_to_log(mean = array(0 + min(data_prov$y1)/2), sd = array(1))[[1]],
      sdlog = 0.01
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
  ) %>% 
  fill(id_dpt, .direction = "up")

# MR-BRT Model ---------------------------------------------------------------

# Load data in MRData object
nat_mrbrt <- mr$MRData()
nat_mrbrt$load_df(
  data = data_prov,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_covs = list("x1"),
  col_study_id = "id"
  )

# Create MRBRT model and configure covariates
mod_nat <- mr$MRBRT(
  data = nat_mrbrt,
  cov_models = list(
    mr$LinearCovModel("intercept", use_re = FALSE),
    mr$LinearCovModel(
      alt_cov = "x1",
      use_spline = TRUE,
      spline_knots = config$mrbrt$spline_knots,
      spline_degree = config$mrbrt$spline_degree,
      spline_knots_type = config$mrbrt$spline_knots_type,
      prior_spline_maxder_gaussian = config$mrbrt$prior_spline_maxder_gaussian
      )
    )
  )

# Fit MRBRT model
mod_nat$fit_model(inner_print_level = 5L, inner_max_iter = 1000L)

# Create predicted MRData object
df_pred <- data.frame(x1 = seq(0, 80, by = config$pred$res))
dat_pred_nat <- mr$MRData()
dat_pred_nat$load_df(
  data = df_pred, 
  col_covs = list('x1')
  )

# Obtain predicted values
pred_nat <- mod_nat$predict(data = dat_pred_nat)

# Plot national level mortality estimates
ggplot(data_prov) +
  geom_point(aes(x = x1, y = y1 * 100000), alpha = 0.3, size = 0.3) +
  geom_line(data = df_pred, aes(x = x1, y = exp(pred_nat) * 100000)) +
  labs(x = "Week", y = "Mortality per 100,000") +
  ylim(c(0,50)) +
  theme_bw()

# Cascade spline at department level --------------------------------------

# Create and fit MRBRT department-level cascade spline model
mod_cascade_dpt <- run_spline_cascade(
  stage1_model_object = mod_nat,
  gaussian_prior = TRUE,
  df = data_prov,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_study_id = "id",
  stage_id_vars = "dpt_cdc",
  thetas = config$cascade$theta_dpt,
  output_dir = config$cascade$output,
  model_label = config$cascade$dpt_label,
  overwrite_previous = TRUE
  )

# Obtain predicted values from the department-level cascade spline model
pred_cascade_dpt <- predict_spline_cascade(
  fit = mod_cascade_dpt,
  newdata = expand.grid(
    x1 = seq(0, 85, by = config$pred$res),
    dpt_cdc = unique(data_prov$dpt_cdc)
    ) 
  )

# Plot department level mortality estimates
ggplot() +
  geom_line(aes(x = x1, y = exp(pred) * 100000), pred_cascade_dpt) +
  geom_point(aes(x = x1, y = y1 * 100000), data_prov,  size = 0.1, alpha = 0.5) +
  facet_wrap( . ~ dpt_cdc) +
  ylim(c(0, 80)) +
  labs(x = "Week", y = "Mortality per 100,000") +
  theme_bw()

# Cascade splines Prov --------------------------------------------------------

# Create and fit MRBRT province-level cascade spline model
mod_cascade_prov <- run_spline_cascade(
  stage1_model_object = mod_nat,
  df = data_prov,
  gaussian_prior = TRUE,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_study_id = "id",
  stage_id_vars = c("dpt_cdc", "prov_cdc"),
  thetas = c(config$cascade$theta_dpt, config$cascade$theta_prov),
  output_dir = config$cascade$output,
  model_label = config$cascade$prov_label,
  overwrite_previous = TRUE
  )

# Create prediction data frame to fill predicted values
df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 85, by = config$pred$res),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>%
  left_join(poblacion_prov %>% select(-pob))

# Obtain predicted values from the provincial-level cascade spline model
pred_cascade_prov <- predict_spline_cascade(
  fit = mod_cascade_prov,
  newdata = df_pred
  ) 

for (i in config$pred$depts) {
  
  graph_dpto <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_point(
      aes(x = x1, y = y1 * 100000),
      size = 0.7,
      alpha = 0.4
    ) +
    geom_line(
      data = pred_cascade_dpt %>%  filter(dpt_cdc == i),
      aes(x = x1, y = exp(pred) * 100000), col = "red"
      ) +
    facet_wrap( . ~ dpt_cdc) +
    labs(x = NULL, y = "Mortality per 100,000") +
    theme_bw()
  
  graph_prov <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_point(aes(x = x1, y = y1 * 100000), size = 1, alpha = 0.5) +
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
    geom_point(
      aes(x = x1, y = ylog),
      size = 0.7,
      alpha = 0.4
    ) +
    geom_line(
      data = pred_cascade_dpt %>%  filter(dpt_cdc == i),
      aes(x = x1, y = pred), col = "red"
      ) +
    facet_wrap(.~dpt_cdc) +
    labs(x = NULL, y = "Mortality per 100,000 (exp)") +
    theme_bw()

  graph_prov_log <- data_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_point(aes(x = x1, y = ylog), size = 1, alpha = 0.5) +
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
  
  if (config$save$plots == TRUE) {
    ggsave(
      plot = graph_log,
      filename =  paste0(
        config$save$plots_dir,
        "log-theta_dept",
        config$cascade$theta_dpt,
        "_theta_prov",
        config$cascade$theta_prov,
        "_",
        i,
        config$save$plots_format
      ),
      scale = 2
    )
    ggsave(
      plot = graph,
      filename =  paste0(
        config$save$plots_dir,
        "exp-theta_dept",
        config$cascade$theta_dpt,
        "_theta_prov",
        config$cascade$theta_prov,
        "_",
        i,
        config$save$plots_format
      ),
      scale = 2
    )
  }
}

if (config$save$prediction) {
  prov_time_series <- predict_spline_cascade(
    fit = mod_cascade_prov,
    newdata = df_pred
  ) %>%
    mutate(mortality = exp(pred)) %>%
    write.csv(config$save$prediction_dir)
}

