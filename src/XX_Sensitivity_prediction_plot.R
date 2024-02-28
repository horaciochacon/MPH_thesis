library(ggplot2)
library(data.table)
library(dplyr)
library(purrr)

config <- config::get(config = "sensitivity")


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
  [, y1  := n / pob * 100000]
  [, x1 := ((as.numeric(fecha_fallecimiento) - 18323) / 7) + 3 ]
  [, !"fecha_fallecimiento"]
)

# Set path to folder
path <- "sensitivity/output/"

# List all files in the folder
file_list <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
data_pred <- rbindlist(lapply(file_list, fread))
prov_pred1 <- copy(data_pred)
prov_pred2 <- copy(data_pred)
prov_pred_best <- copy(data_pred)

# Get ranking of biggest difference
diff_ranking_k1 <- read.csv("temp/wave1_prov_rankingdiff.csv") %>% as.data.table()
diff_ranking_k2 <- read.csv("temp/wave1_prov_rankingdiff.csv") %>% as.data.table()


prov_pred_k1 <- (
  prov_pred1
  [x1 < 43]
  [diff_ranking_k1, on = .(prov_cdc)]
  [, min := (theta_dpt == theta_dpt_min & theta_prov == theta_prov_min)]
  [, .SD ]
  [, max := (theta_dpt == theta_dpt_max & theta_prov == theta_prov_max)]
  [, .SD ]
  [min == TRUE | max == TRUE]
  [, type := ifelse(min, "Min Error", "Max Error")]
  [, .SD ]
)

prov_pred_k2 <- (
  prov_pred2 
  [x1 >= 43]
  [diff_ranking_k2, on = .(prov_cdc)]
  [, min := (theta_dpt == theta_dpt_min & theta_prov == theta_prov_min)]
  [, .SD ]
  [, max := (theta_dpt == theta_dpt_max & theta_prov == theta_prov_max)]
  [, .SD ]
  [min == TRUE | max == TRUE]
  [, type := ifelse(min, "Min Error", "Max Error")]
  [, .SD ]
)


clust1_k1 <-  prov_pred_best[theta_dpt == 45 & theta_prov == 49 & x1 < 43]
clust2_k1 <-  prov_pred_best[theta_dpt == 38 & theta_prov == 8 & x1 < 43]
clust3_k1 <-  prov_pred_best[theta_dpt == 5 & theta_prov == 43 & x1 < 43]
clust4_k1 <-  prov_pred_best[theta_dpt == 6 & theta_prov == 13 & x1 < 43]


clust1_k2 <-  prov_pred_best[theta_dpt == 45 & theta_prov == 49 & x1 >= 43]
clust2_k2 <-  prov_pred_best[theta_dpt == 10 & theta_prov == 45 & x1 >= 43]
clust3_k2 <-  prov_pred_best[theta_dpt == 12 & theta_prov == 12 & x1 >= 43]
clust4_k2 <-  prov_pred_best[theta_dpt == 46 & theta_prov == 10 & x1 >= 43]

best <- prov_pred_best[theta_dpt == 4 & theta_prov == 30 & x1 < 43]


# Plot ----------------------------------------------------------------------------------------


plots_k1 <- config$pred$provs %>% 
  map(
    ~ ggplot() +
      geom_point(aes(x = x1, y = y1), data = data_prov[prov_cdc == .x & x1 < 43]) +
      geom_line(
        data = prov_pred_k1[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = type)
      ) +
      # geom_line(
      #   data = clust1_k1[prov_cdc == .x],
      #   aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 1")
      # ) +
      # geom_line(
      #   data = clust2_k1[prov_cdc == .x],
      #   aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 2")
      # ) +
      # geom_line(
      #   data = clust3_k1[prov_cdc == .x],
      #   aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 3")
      # ) +
      # geom_line(
      #   data = clust4_k1[prov_cdc == .x],
      #   aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 4")
      # ) +
      geom_line(
        data = best[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = "Best")
      ) +
      labs(
        title = paste0(.x, " - Wave 1"),
        x = "Week", 
        y = "Mortality per 100,000"
        ) +
      theme_bw()
      )

plots_k1 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/", .x$labels$title, ".png"),
      plot = .x,
      scale = 2
      )
    )

plots_k2 <- config$pred$provs %>% 
  map(
    ~ ggplot() +
      geom_point(aes(x = x1, y = y1), data = data_prov[prov_cdc == .x & x1 >= 43]) +
      geom_line(
        data = prov_pred_k2[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = type)
      ) +
      geom_line(
        data = clust1_k2[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 1")
      ) +
      geom_line(
        data = clust2_k2[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 2")
      ) +
      geom_line(
        data = clust3_k2[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 3")
      ) +
      geom_line(
        data = clust4_k2[prov_cdc == .x],
        aes(x = x1, y = exp(y_hat) * 100000, color = "Cluster 4")
      ) +
      labs(
        title = paste0(.x, " - Wave 2"),
        x = "Week", 
        y = "Mortality per 100,000"
      ) +
      theme_bw()
  )

plots_k2 %>% 
  map(
    ~ggsave(
      filename = paste0(
        "sensitivity/Plots/epidemic_curves/", .x$labels$title, ".png"),
      plot = .x,
      scale = 2
    )
  )
