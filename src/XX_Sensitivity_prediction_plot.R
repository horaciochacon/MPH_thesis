library(ggplot2)
library(data.table)
library(dplyr)
library(purrr)
library(lubridate)
source("R/sens_functions.R")

config <- config::get(config = "sensitivity")


# Death count per day dataset
death_count_day <- fread("data/pre_processed/death_count_prov_day.csv")

# Province level population
poblacion_prov <- fread("data/pre_processed/poblacion_prov.csv")
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
path <- "sensitivity/output/"

# List all files in the folder
file_list <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
data_pred <- rbindlist(lapply(file_list, fread))

# Get Best Theta combination information
merged_k1 <- fread("sensitivity/best_theta/merged_k1.csv")
merged_k2 <- fread("sensitivity/best_theta/merged_k2.csv")
summary_best_theta <- fread("sensitivity/best_theta/summary_best_theta.csv")
lowest_k1_we_kij <- fread("sensitivity/best_theta/lowest_k1_we_kij.csv")
lowest_k2_we_kij <- fread("sensitivity/best_theta/lowest_k1_we_kij.csv")
best_k1_theta <- fread("sensitivity/best_theta/best_k1_theta.csv")
best_k2_theta <- fread("sensitivity/best_theta/best_k2_theta.csv")
waves <- fread("sensitivity/waves.csv")

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
      best_min$prov_pred_min_k1, merged_k1, waves, best_k1_theta
      )
    )

plots_k1 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/wave1/", .x$labels$title, ".pdf"),
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
      )
    )

plots_k2 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/wave2/", .x$labels$title, ".pdf"),
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
    )
  )

combined_plots %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/combined/", .x$labels$title, ".pdf"),
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
    )
  )

plots_k1 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/wave1/", .x$labels$title, ".pdf"),
      plot = .x,
      scale = 2.5
    )
  )


combined_plot_k1 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/combined_k1/", .x$labels$title, ".png"),
      plot = .x,
      scale = 3
    )
  )

combined_plot_k2 <- config$pred$provs %>% 
  map(
    ~ ggplot() +
      geom_point(aes(x = x1, y = y1), data = data_prov[prov_cdc == .x]) +
      geom_line(
        data = prov_pred_best_all_k2[prov_cdc == .x],
        aes(
          x = x1, 
          y = exp(y_hat) * 100000, 
          color = paste0("Best k2 (we = ", round(merged_k2[prov_cdc == .x]$we_k_best, 6),")")
        )
      ) +
      geom_line(
        data = prov_pred_min_all_k2[prov_cdc == .x],
        aes(
          x = x1, 
          y = exp(y_hat) * 100000,
          color =  paste0("Min k2 (we = ", round(merged_k2[prov_cdc == .x]$we_k_min, 6),")")
        )
      ) +
      labs(
        title = paste(
          unique(prov_pred_best_k2[prov_cdc == .x]$dpt_cdc), "-", .x
        ),
        subtitle = bquote(
          atop(
            "Absolute difference =" ~ .(round(merged_k2[prov_cdc == .x]$abs_diff, 6)) ~
              "|" ~"Relative difference  =" ~ .(round(merged_k2[prov_cdc == .x]$rel_diff, 6)),
            "Best common" ~ theta*": Dpt =" ~  .(best_k2_theta$theta_dpt) ~ "," ~
              "Prov =" ~ .(best_k2_theta$theta_prov) ~ "|" ~
              "Minimum" ~ theta*": Dpt =" ~  .(merged_k2[prov_cdc == .x]$theta_dpt_min) ~ "," ~
              "Prov =" ~ .(merged_k2[prov_cdc == .x]$theta_prov_min)
          )
        ),
        x = "Week", 
        y = "Mortality per 100,000",
        color = "Model"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  )

combined_plot_k2 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/combined_k2/", .x$labels$title, ".png"),
      plot = .x,
      scale = 3
    )
  )

combined_plot_all <- config$pred$provs %>% 
  map(
    ~ ggplot() +
      geom_point(aes(x = x1, y = y1), data = data_prov[prov_cdc == .x]) +
      geom_line(
        data = prov_pred_best_all_k1[prov_cdc == .x],
        aes(
          x = x1, 
          y = exp(y_hat) * 100000, 
          color = paste0("Best k1 (we = ", round(merged_k1[prov_cdc == .x]$we_k_best, 6),")")
        )
      ) +
      geom_line(
        data = prov_pred_min_all_k1[prov_cdc == .x],
        aes(
          x = x1, 
          y = exp(y_hat) * 100000,
          color =  paste0("Min k1 (we = ", round(merged_k1[prov_cdc == .x]$we_k_min, 6),")")
        )
      ) +
      geom_line(
        data = prov_pred_best_all_k2[prov_cdc == .x],
        aes(
          x = x1, 
          y = exp(y_hat) * 100000, 
          color = paste0("Best k2 (we = ", round(merged_k2[prov_cdc == .x]$we_k_best, 6),")")
        )
      ) +
      geom_line(
        data = prov_pred_min_all_k2[prov_cdc == .x],
        aes(
          x = x1, 
          y = exp(y_hat) * 100000,
          color =  paste0("Min k2 (we = ", round(merged_k2[prov_cdc == .x]$we_k_min, 6),")")
        )
      ) +
      labs(
        title = paste(
          unique(prov_pred_best_k1[prov_cdc == .x]$dpt_cdc), "-", .x
        ),
        x = "Week", 
        y = "Mortality per 100,000",
        color = "Model"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  )

combined_plot_all %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/combined_all/", .x$labels$title, ".png"),
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
        x = 0, y = 0, width = 1, height = .65
        )
  )

grid_plots %>% 
  map2(
    split(dpt_count, seq(nrow(dpt_count))),
    ~ ggsave( 
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/grid_plots/", .y$dpt , ".pdf"),
      plot = .x,
      dpi = 500,
      scale = 2,
      limitsize = FALSE,
      width = 12, 
      height = 4 + 2.5 * ceiling(.y$count / 3)
    )
  )

split_plots_k1 <- split(combined_plot_k1, dpt_list)

grid_plots_k1 <- unique(dpt_list) %>% 
  map2(
    dpt_plots,
    ~ ggdraw() +
      draw_plot(.y, x = 0.25, y = .7, width = .5, height = .2) +
      draw_plot(
        plot_grid(plotlist = split_plots_k1[[.x]],ncol = 3),
        x = 0, y = 0, width = 1, height = 0.65
      )
  )

grid_plots_k1 %>% 
  map2(
    split(dpt_count, seq(nrow(dpt_count))),
    ~ ggsave( 
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/grid_plots_k1/", .y$dpt , ".pdf"),
      plot = .x,
      dpi = 500,
      scale = 2,
      limitsize = FALSE,
      width = 12, 
      height = 4 + 2 * ceiling(.y$count / 3)
    )
  )




