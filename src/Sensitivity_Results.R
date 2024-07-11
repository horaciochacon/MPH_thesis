####################################################################################################
## Author:      Horacio Chacon Torrico
##
## Description: This script creates grid plots for different MR-BRT sensitivity runs, displaying 
##              the best theta combinations for wave 1 (k1) and wave 2 (k2). It extracts MR-BRT 
##              model parameters from each run's configuration file and includes this information 
##              in the plots.
##
## Passed args: None
##
## Requires:    Directory containing sensitivity run outputs (specified in config.yml)
##              Configuration file for each run to extract MR-BRT model parameters
##              Sensitivity analysis predictions and best theta combinations for each run
##
## Outputs:     Grid plots for each sensitivity run, saved in the specified output directory
##
####################################################################################################

rm(list = ls())
library(ggplot2)
library(data.table)
library(dplyr)
library(purrr)
library(lubridate)
library(cowplot)
library(stringr)
library(config)

# Functions -----------------------------------------------------------------------------------

# Load configuration
config <- config::get(config = "sensitivity")
runs_dir <- str_remove(config$sbatch$output_dir, "/$")
source("R/sens_functions.R")

# Function to extract MR-BRT parameters from config file
extract_mrbrt_params <- function(config_path) {
  config <- config::get(file = paste0(config_path,"/config.yml"), config = "sensitivity")
  list(
    spline_degree = config$mrbrt$spline_degree,
    spline_knots_type = config$mrbrt$spline_knots_type,
    prior_spline_maxder_gaussian = paste(config$mrbrt$prior_spline_maxder_gaussian, collapse = ", "),
    spline_knots = length(config$mrbrt$spline_knots),
    run = str_split_i(config_path, "/", 8)
  )
}

# Function to read best theta combinations from each run
read_best_theta <- function(output_dir, wave) {
  best_theta <- fread(file.path(output_dir, paste0("estimates/best_theta/best_k", wave, "_theta.csv")))
  best_theta <- best_theta[, run := str_split_i(output_dir, "/", 8)]
  best_theta
}

# Function to read best theta predictions
read_best_theta_pred <- function(output_dir, wave) {
  best_theta <- fread(file.path(output_dir, paste0(
    "estimates/best_theta/data_pred_best_k", wave, ".csv"
    )))
  best_theta <- best_theta[, run := str_split_i(output_dir, "/", 8)]
  best_theta
}

# Function to read best department predictions
read_best_dpt <- function(output_dir) {
  best_dpt <- fread(file.path(output_dir, paste0(
    "estimates/best_theta/best_dpt.csv"
  )))
  best_dpt <- best_dpt[, run := str_split_i(output_dir, "/", 8)]
  best_dpt
}


# Function to read best theta summary
read_best_theta_summary <- function(output_dir) {
  best_theta_summary <- fread(
    file.path(output_dir, paste0("estimates/best_theta/summary_best_theta.csv"))
    )
  best_theta_summary <- best_theta_summary[, run := str_split_i(output_dir, "/", 8)]
}

# Function to read and format prediction data
# read_prediction_data <- function(output_dir) {
#   path <- file.path(output_dir, "MR_BRT/output/")
#   file_list <- list.files(path, pattern = "\\.gz$", full.names = TRUE)
#   data_pred <- rbindlist(
#     lapply(file_list, 
#            function(file) {
#              df <- fread(file)
#              temp <- str_split(file, "//",simplify = TRUE)[,2]
#              df$theta_prov <- as.numeric(str_split_i(str_split_i(temp, "-",1), "_", 2))
#              df$theta_dpt <- as.numeric(str_split_i(str_split_i(temp, "-",2), "[_|.]", 2))
#              df$run <- basename(output_dir)
#              return(df)
#            }
#     )
#   )
# }

# Function to create plot for a single province
create_province_plot <- function(data, prov_id, params, title) {
  p <- ggplot(data[data$prov_cdc == prov_id], aes(x = x1, y = exp(y_hat) * 1e5, color = run)) +
    geom_line() +
    labs(
      title = title,
      subtitle = paste("Spline Degree:", params$spline_degree,
                       "Knots Type:", params$spline_knots_type,
                       "Max Deriv Gaussian:", params$prior_spline_maxder_gaussian,
                       "Knots:", params$spline_knots),
      x = "Week",
      y = "Mortality per 100,000"
    ) +
    theme_bw()
  
  p
}


# Aggregation ---------------------------------------------------------------------------------

# List all run directories
run_dirs <- list.dirs(runs_dir, recursive = FALSE)[1:14]
prov_names <- fread(paste0(config$sbatch$wd, "data/pre_processed/poblacion_prov.csv")) %>% 
  select(dpt_cdc, prov_cdc, id_prov = id, id_dpt)


# Extract params
param_conf <- run_dirs %>% map_df(~ extract_mrbrt_params(config_path = .x))

# Read best 
pred_k1 <- run_dirs %>% 
  map_df(read_best_theta_pred, 1) %>% 
  left_join(prov_names) %>% 
  left_join(param_conf) %>% 
  mutate(
    conf = paste0("Spline:", spline_degree," | knots:", spline_knots)
  )


# Death count per day dataset
death_count_day <- fread(paste0(config$sbatch$wd, "data/pre_processed/death_count_prov_day.csv"))

# Province level population
poblacion_prov <- fread(paste0(config$sbatch$wd,"data/pre_processed/poblacion_prov.csv")) %>% 
  rename(id_prov = id)
poblacion_dpt <- poblacion_prov[, .(pob = sum(pob)), by = dpt_cdc]

# Province level death counts
data_prov <- (
  death_count_day
  [, fecha_fallecimiento := floor_date(fecha_fallecimiento, "weeks", week_start = 1) ]
  [, .(n = sum(n)), by = .(fecha_fallecimiento, dpt_cdc, prov_cdc) ]
  [ poblacion_prov[, 2:5], on = .(prov_cdc) ]
  [, y1  := n / pob * 100000]
  [, x1 := ((as.numeric(fecha_fallecimiento) - 18323) / 7) + 3 ]
  [, !"fecha_fallecimiento"]
)

data_dpt <- (
  data_prov
  [, .(x1,dpt_cdc, prov_cdc, deaths = (y1* pob ) / 1e5)]
  [, .(deaths = sum(deaths)), by = .(dpt_cdc, x1)]
  [poblacion_dpt, on = .(dpt_cdc)]
  [, .(dpt_cdc, x1, deaths, pob, y1 = deaths / pob * 1e5)]
)


# Plot ----------------------------------------------------------------------------------------

plot_dir <- "/ihme/scratch/users/hchacont/mrbrtcovid/plots/"
max_dpt <- max_y_hat_dept2(pred_k1, data_prov)
max_dpt[dpt_cdc == "HUANUCO"]$max_y_hat <- 30
max_dpt[dpt_cdc == "APURIMAC"]$max_y_hat <- 50
max_dpt[dpt_cdc == "AREQUIPA"]$max_y_hat <- 45
max_dpt[dpt_cdc == "AYACUCHO"]$max_y_hat <- 35
max_dpt[dpt_cdc == "CUSCO"]$max_y_hat <- 30
max_dpt[dpt_cdc == "HUANCAVELICA"]$max_y_hat <- 40
max_dpt[dpt_cdc == "ICA"]$max_y_hat <- 50
max_dpt[dpt_cdc == "LAMBAYEQUE"]$max_y_hat <- 40
max_dpt[dpt_cdc == "LIMA"]$max_y_hat <- 100
max_dpt[dpt_cdc == "SAN MARTIN"]$max_y_hat <- 55
max_dpt[dpt_cdc == "UCAYALI"]$max_y_hat <- 50
max_dpt <- max_dpt[prov_names[,1:2], on = "dpt_cdc"]

## All plots
plot_all <- config$pred$prov %>% 
  map(plot_agg_sens, data_pred = pred_k1, data_obs = data_prov, max_dpt = max_dpt)

# plot_all %>% 
#   map(~ggsave(filename = paste0(plot_dir, "all/", .x$labels$title,".pdf"),
#               plot = .x, scale = 3))

## By Knots
plot_knots <- config$pred$prov %>% 
  map(plot_agg_sens_facet, data_pred = pred_k1, data_obs = data_prov)

# plot_knots %>% 
#   map(~ggsave(filename = paste0(plot_dir, "by_knots/", .x$labels$title,".pdf"),
#               plot = .x, scale = 3))

## By Degree
plot_degrees <- config$pred$prov %>% 
  map(plot_agg_sens_facet, data_pred = pred_k1, data_obs = data_prov, facet = "degree")

# plot_degrees %>% 
#   map(~ggsave(filename = paste0(plot_dir, "by_degree/", .x$labels$title,".pdf"),
#               plot = .x, scale = 3))


# Grid Plots
dpt_list <- map_vec(plot_all, ~ str_split(.x$labels$title," -",simplify = TRUE)[1,1])
dpt_count <- tibble(dpt = names(table(dpt_list)), count = table(dpt_list))
max_dpt2 <- unique(max_dpt[,1:2])

best_theta <- map_df(run_dirs, read_best_theta, wave = 1)
best_dpt <- map_df(run_dirs, read_best_dpt) %>% 
  left_join(param_conf) %>% 
  mutate(
    conf = paste0("Spline:", spline_degree," | knots:", spline_knots)
  )

dpt_plots <- unique(dpt_list) %>% 
  map(
    ~ ggplot(aes(x = x1), data = best_dpt[dpt_cdc == .x]) +
      geom_line(aes(y = mort, color = conf), linewidth = 2) +
      geom_point(
        aes(y = y1), data = data_dpt[dpt_cdc == .x],
        alpha = 0.8, size = 2.5, col = "red"
      ) +
      geom_point(
        aes(y = y1), data = data_prov[dpt_cdc == .x],
        alpha = 0.5, size = 0.8
      ) +
      ylim(c(0, max_dpt2[dpt_cdc == .x]$max_y_hat)) +
      labs(
        x = "Week", 
        y = "Mortality per 100,000"
      ) +
      theme_bw() 
  )

plot_all <- map(plot_all, ~ .x + theme(legend.position = "none"))
split_plots <- split(plot_all, dpt_list)

grid_plots <- split(dpt_count, seq(nrow(dpt_count))) %>% 
  map2(
    dpt_plots,
    ~ ggdraw() +
      draw_plot(
        .y, x = 0.25, y = .7, width = .5, height = .2
      ) +
      draw_plot(
        plot_grid(plotlist = split_plots[[.x$dpt]], ncol = 3),
        x = 0, y = 0.05, width = 1, height = .65
      )
  )

grid_plots %>% 
  map2(
    split(dpt_count, seq(nrow(dpt_count))),
    ~ ggsave( 
      filename = paste0(
        plot_dir,"grid/",.y$dpt,".png"
      ),
      plot = .x,
      dpi = 500,
      scale = 2,
      limitsize = FALSE,
      width = 12, 
      height = 4 + 2.5 * ceiling(.y$count / 3)
    )
  )


# Summary table -------------------------------------------------------------------------------

summary_thetas <- run_dirs %>% 
  map_df(read_best_theta_summary) %>% 
  left_join(param_conf)

best_theta <- run_dirs %>% map_df(read_best_theta,  1) %>% 
  left_join(param_conf)



summary_thetas


