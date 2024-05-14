library(ggplot2)
library(data.table)
library(dplyr)
library(purrr)
library(lubridate)
library(cowplot)

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
  (config_loc <- as.character(commandArgs(TRUE)[[2]]))
}

config <- config::get(file = config_loc, config = "sensitivity")
source(paste0(config$sbatch$wd, "R/sens_functions.R"))

# Death count per day dataset
death_count_day <- fread(paste0(config$sbatch$wd, "data/pre_processed/death_count_prov_day.csv"))

# Province level population
poblacion_prov <- fread(paste0(config$sbatch$wd,"data/pre_processed/poblacion_prov.csv"))
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

# Set path to folder
path <- paste0(output_dir, "/sensitivity/output/")

# List all files in the folder
file_list <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
data_pred <- rbindlist(lapply(file_list, fread))

# Get Best Theta combination information
merged_k1 <- fread(paste0(output_dir,"/sensitivity/best_theta/merged_k1.csv"))
merged_k2 <- fread(paste0(output_dir,"/sensitivity/best_theta/merged_k2.csv"))
summary_best_theta <- fread(paste0(output_dir,"/sensitivity/best_theta/summary_best_theta.csv"))
lowest_k1_we_kij <- fread(paste0(output_dir,"/sensitivity/best_theta/lowest_k1_we_kij.csv"))
lowest_k2_we_kij <- fread(paste0(output_dir,"/sensitivity/best_theta/lowest_k1_we_kij.csv"))
best_k1_theta <- fread(paste0(output_dir,"/sensitivity/best_theta/best_k1_theta.csv"))
best_k2_theta <- fread(paste0(output_dir,"/sensitivity/best_theta/best_k2_theta.csv"))
waves <- fread(paste0(output_dir,"/sensitivity/waves.csv"))

# Predictions

best_min <- best_min_pred(
  data_pred, best_k1_theta, lowest_k1_we_kij, best_k2_theta, lowest_k2_we_kij
)

best_dpt <- best_dpt_pred(data_pred, best_k1_theta, poblacion_dpt)

# Plots ----------------------------------------------------------------------------------------

## Wave 1
plots_k1 <- config$pred$provs %>% 
  map(
    ~plot_prov_data_k1(
      prov_id = .x, data_prov, best_min$prov_pred_best_k1, 
      best_min$prov_pred_min_k1, merged_k1, waves, best_k1_theta, config
      ) +
      add_config_caption(config)
  )

plots_k1 %>% 
  map(
    ~ggsave(
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/wave1/", .x$labels$title, ".pdf"),
      plot = .x,
      scale = 2.5
    )
  )

## Wave 2
plots_k2 <- config$pred$provs %>% 
  map(
    ~ plot_prov_data_k2(
      prov_id = .x, data_prov, best_min$prov_pred_best_k2, 
      best_min$prov_pred_min_k2, merged_k2, waves, best_k2_theta
      ) +
      add_config_caption(config)
  )

plots_k2 %>% 
  map(
    ~ggsave(
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/wave2/", .x$labels$title, ".pdf"),
      plot = .x,
      scale = 2.5
    )
  )

## Combined

combined_plots <- config$pred$provs %>% 
  map(
    ~ plot_prov_data_combined(
      prov_id = .x, data_prov, best_min$prov_pred_best_k1, 
      best_min$prov_pred_min_k1, merged_k1,  best_k1_theta,
      best_min$prov_pred_best_k2,best_min$prov_pred_min_k2, merged_k2, best_k2_theta, waves
      ) +
      add_config_caption(config)
  )

combined_plots %>% 
  map(
    ~ggsave(
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/combined/", .x$labels$title, ".pdf"),
      plot = .x,
      scale = 2.5
    )
  )

## All based on Wave one selection

combined_plot_k1 <- config$pred$provs %>% 
  map(
    ~plot_prov_data_combined_k1(
      prov_id = .x, data_prov, best_min$prov_pred_best_all_k1, 
      best_min$prov_pred_min_all_k1, merged_k1, waves, best_k1_theta
      ) +
      add_config_caption(config)
  )

combined_plot_k1 %>% 
  map(
    ~ggsave(
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/combined_k1/", .x$labels$title, ".pdf"),
      plot = .x,
      scale = 3
    )
  )

## All based on Wave twp selection

combined_plot_k2 <- config$pred$provs %>% 
  map(
    ~plot_prov_data_combined_k2(
      prov_id = .x, data_prov, best_min$prov_pred_best_all_k2, 
      best_min$prov_pred_min_all_k2, merged_k2, waves, best_k2_theta
    ) +
      add_config_caption(config)
  )

combined_plot_k2 %>% 
  map(
    ~ggsave(
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/combined_k2/", .x$labels$title, ".pdf"),
      plot = .x,
      scale = 3
    )
  )

# Grid plot -----------------------------------------------------------------------------------

dpt_list <- map_vec(combined_plots, ~ str_split(.x$labels$title," -",simplify = TRUE)[1,1])

dpt_count <- tibble(dpt = names(table(dpt_list)), count = table(dpt_list))

dpt_plots <- unique(dpt_list) %>% 
  map(
    ~ ggplot(aes(x = x1), data = best_dpt[dpt_cdc == .x]) +
      geom_line(aes(y = mort), linewidth = 2, color = "blue") +
      geom_point(
        aes(y = y1), data = data_dpt[dpt_cdc == .x],
        alpha = 0.8, size = 2.5, col = "red"
      ) +
      geom_point(
        aes(y = y1), data = data_prov[dpt_cdc == .x],
        alpha = 0.5, size = 0.8
        ) +
      ylim(c(0, quantile(best_dpt[dpt_cdc == .x]$mort, 0.975) * 1.2)) +
      labs(
        x = "Week", 
        y = "Mortality per 100,000"
      ) +
      theme_bw() 
  )

combined_plots <- map(combined_plots, ~ .x + labs(caption = NULL))
split_plots <- split(combined_plots, dpt_list)

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
        ) +
      add_config_caption_ggdraw(config)
  )

grid_plots %>% 
  map2(
    split(dpt_count, seq(nrow(dpt_count))),
    ~ ggsave( 
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/grid_plots/", .y$dpt , ".pdf"),
      plot = .x,
      dpi = 500,
      scale = 2,
      limitsize = FALSE,
      width = 12, 
      height = 4 + 2.5 * ceiling(.y$count / 3)
    )
  )

combined_plot_k1 <- map(combined_plot_k1, ~ .x + labs(caption = NULL))
split_plots_k1 <- split(combined_plot_k1, dpt_list)

grid_plots_k1 <- unique(dpt_list) %>% 
  map2(
    dpt_plots,
    ~ ggdraw() +
      draw_plot(.y, x = 0.25, y = .7, width = .5, height = .2) +
      draw_plot(
        plot_grid(plotlist = split_plots_k1[[.x]],ncol = 3),
        x = 0, y = 0, width = 1, height = 0.65 
      ) +
      add_config_caption_ggdraw(config)
  )

grid_plots_k1 %>% 
  map2(
    split(dpt_count, seq(nrow(dpt_count))),
    ~ ggsave( 
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/grid_plots_k1/", .y$dpt , ".pdf"),
      plot = .x,
      dpi = 500,
      scale = 2,
      limitsize = FALSE,
      width = 12, 
      height = 4 + 2 * ceiling(.y$count / 3)
    )
  )

combined_plot_k2 <- map(combined_plot_k2, ~ .x + labs(caption = NULL))
split_plots_k2 <- split(combined_plot_k2, dpt_list)

grid_plots_k2 <- unique(dpt_list) %>% 
  map2(
    dpt_plots,
    ~ ggdraw() +
      draw_plot(.y, x = 0.25, y = .7, width = .5, height = .2) +
      draw_plot(
        plot_grid(plotlist = split_plots_k2[[.x]],ncol = 3),
        x = 0, y = 0, width = 1, height = 0.65 
      ) +
      add_config_caption_ggdraw(config)
  )

grid_plots_k2 %>% 
  map2(
    split(dpt_count, seq(nrow(dpt_count))),
    ~ ggsave( 
      filename = paste0(
        output_dir,
        "/sensitivity/Plots/epidemic_curves/grid_plots_k2/", .y$dpt , ".pdf"),
      plot = .x,
      dpi = 500,
      scale = 2,
      limitsize = FALSE,
      width = 12, 
      height = 4 + 2 * ceiling(.y$count / 3)
    )
  )


print("Script Completed!")