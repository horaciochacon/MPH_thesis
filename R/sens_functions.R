library(data.table)
library(tidyverse)


# Sensitivity Prediction Plots ----------------------------------------------------------------

add_config_caption <- function(config) {
  labs(
    caption = paste0(
      "MR-BRT Configuration\n", "spline_degree: ", config$mrbrt$spline_degree,
      " | spline_knots_type: ", config$mrbrt$spline_knots_type,
      " | prior_spline_maxder_gaussian: ", config$mrbrt$prior_spline_maxder_gaussian,
      "\nspline_knots: ", paste(config$mrbrt$spline_knots, collapse = ", ")
    )
  )
}

add_config_caption_ggdraw <- function(config) {
    # Adding a caption at the bottom
    draw_label(
      label = paste0(
        "MR-BRT Configuration\n", "spline_degree: ", config$mrbrt$spline_degree,
        " | spline_knots_type: ", config$mrbrt$spline_knots_type,
        " | prior_spline_maxder_gaussian: ", config$mrbrt$prior_spline_maxder_gaussian,
        "\nspline_knots: ", paste(config$mrbrt$spline_knots, collapse = ", ")
      ), # Customize your caption text
      x = 0.01, y = 0.01, # Position at the bottom center of the plot area
      hjust = 0, # Horizontal alignment (0.5 for center)
      vjust = 0, # Vertical alignment (0 for bottom)
      size = 10 # Font size, adjust as needed
    )
}

plot_prov_data_k1 <- function(prov_id, data_prov, prov_pred_best_k1, prov_pred_min_k1, 
                           merged_k1, waves, best_k1_theta, config) {

  ggplot() +
    geom_point(aes(x = x1, y = y1), data = data_prov[data_prov$prov_cdc == prov_id & x1 < 43]) +
    geom_line(
      data = prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000, 
        color = paste0(
          "Best (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_best, 6),")"
        )
      ), linewidth = 1.5
    ) +
    geom_line(
      data = prov_pred_min_k1[prov_pred_min_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000,
        color =  paste0(
          "Min (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_min, 6),")"
          )
      ), linewidth = 1.5,  linetype = 2
    ) +

    labs(
      title = paste(
        unique(
          prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id]$dpt_cdc),
        "-", prov_id, "- Wave 1",
        "(w =",  round(waves[waves$prov_cdc == prov_id & waves$k == 1]$w_k, 6),")"
      ),
      subtitle = bquote(
        atop(
          "Absolute difference =" ~ .(round(merged_k1[merged_k1$prov_cdc == prov_id]$abs_diff, 6)) ~
            "|" ~"Relative difference  =" ~ .(
              round(merged_k1[merged_k1$prov_cdc == prov_id]$rel_diff, 6)
              ),
          "Best common" ~ theta*": Dpt =" ~  .(best_k1_theta$theta_dpt) ~ "," ~
            "Prov =" ~ .(best_k1_theta$theta_prov) ~ "|" ~
            "Minimum" ~ theta*": Dpt =" ~  .(
              merged_k1[merged_k1$prov_cdc == prov_id]$theta_dpt_min
              ) ~ "," ~
            "Prov =" ~ .(merged_k1[merged_k1$prov_cdc == prov_id]$theta_prov_min)
        )
      ),
      x = "Week", 
      y = "Mortality per 100,000",
      color = "Model"
    ) +
    ylim(c(0, quantile(data_prov[dpt_cdc == data_prov[prov_cdc == prov_id][1,1]]$y1, 0.975)) *1.5) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_prov_data_k2 <- function(prov_id, data_prov, prov_pred_best_k1, prov_pred_min_k1, 
                              merged_k1, waves, best_k1_theta) {
  
  ggplot() +
    geom_point(aes(x = x1, y = y1), data = data_prov[data_prov$prov_cdc == prov_id & x1 >= 43]) +
    geom_line(
      data = prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000, 
        color = paste0(
          "Best (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_best, 6),")"
        )
      ), linewidth = 1.5
    ) +
    geom_line(
      data = prov_pred_min_k1[prov_pred_min_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000,
        color =  paste0(
          "Min (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_min, 6),")"
        )
      ), linewidth = 1.5, linetype = 2
    ) +
    labs(
      title = paste(
        unique(
          prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id]$dpt_cdc),
        "-", prov_id, "- Wave 2",
        "(w =",  round(waves[waves$prov_cdc == prov_id & waves$k == 2]$w_k, 6),")"
      ),
      subtitle = bquote(
        atop(
          "Absolute difference =" ~ .(round(merged_k1[merged_k1$prov_cdc == prov_id]$abs_diff, 6)) ~
            "|" ~"Relative difference  =" ~ .(
              round(merged_k1[merged_k1$prov_cdc == prov_id]$rel_diff, 6)
            ),
          "Best common" ~ theta*": Dpt =" ~  .(best_k1_theta$theta_dpt) ~ "," ~
            "Prov =" ~ .(best_k1_theta$theta_prov) ~ "|" ~
            "Minimum" ~ theta*": Dpt =" ~  .(
              merged_k1[merged_k1$prov_cdc == prov_id]$theta_dpt_min
            ) ~ "," ~
            "Prov =" ~ .(merged_k1[merged_k1$prov_cdc == prov_id]$theta_prov_min)
        )
      ),
      x = "Week", 
      y = "Mortality per 100,000",
      color = "Model"
    ) +
    ylim(c(0, quantile(data_prov[dpt_cdc == data_prov[prov_cdc == prov_id][1,1]]$y1, 0.975)) *1.5) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_prov_data_combined <- function(prov_id, data_prov, prov_pred_best_k1, prov_pred_min_k1, 
                              merged_k1, best_k1_theta, prov_pred_best_k2, prov_pred_min_k2, 
                              merged_k2, best_k2_theta, waves) {
  
  ggplot() +
    geom_point(aes(x = x1, y = y1), data = data_prov[data_prov$prov_cdc == prov_id]) +
    geom_line(
      data = prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000, 
        color = paste0(
          "k1 Best (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_best, 6),")"
        )
      ), linewidth = 1.5
    ) +
    geom_line(
      data = prov_pred_min_k1[prov_pred_min_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000,
        color =  paste0(
          "k1 Min (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_min, 6),")"
        )
      ), linewidth = 1.5, linetype = 2
    ) +
    geom_line(
      data = prov_pred_best_k2[prov_pred_best_k2$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000, 
        color = paste0(
          "k2 Best (we = ", round(merged_k2[merged_k2$prov_cdc == prov_id]$we_k_best, 6),")"
        )
      ), linewidth = 1.5
    ) +
    geom_line(
      data = prov_pred_min_k2[prov_pred_min_k2$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000,
        color =  paste0(
          "k2 Min (we = ", round(merged_k2[merged_k2$prov_cdc == prov_id]$we_k_min, 6),")"
        )
      ), linewidth = 1.5, linetype = 2
    ) +
    labs(
      title = paste(
        unique(
          prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id]$dpt_cdc),
        "-", prov_id,
        "(w1 = ",  round(waves[waves$prov_cdc == prov_id & waves$k == 1]$w_k, 6),",", 
        "w2 = ",  round(waves[waves$prov_cdc == prov_id & waves$k == 2]$w_k, 6),")"
      ),
      subtitle = bquote(
        atop(
          "Absolute difference =" ~ .(round(merged_k1[merged_k1$prov_cdc == prov_id]$abs_diff, 6)) ~
            "|" ~"Relative difference  =" ~ .(
              round(merged_k1[merged_k1$prov_cdc == prov_id]$rel_diff, 6)
            ),
          "k1 Best" ~ theta*": Dpt =" ~  .(best_k1_theta$theta_dpt) ~ "," ~
            "Prov =" ~ .(best_k1_theta$theta_prov) ~ "|" ~
            "k1 Min" ~ theta*": Dpt =" ~  .(
              merged_k1[merged_k1$prov_cdc == prov_id]$theta_dpt_min
            ) ~ "," ~
            "Prov =" ~ .(merged_k1[merged_k1$prov_cdc == prov_id]$theta_prov_min) ~
          "| k2 Best" ~ theta*": Dpt =" ~  .(best_k2_theta$theta_dpt) ~ "," ~
            "Prov =" ~ .(best_k2_theta$theta_prov) ~ "|" ~
            "k2 Min" ~ theta*": Dpt =" ~  .(
              merged_k2[merged_k2$prov_cdc == prov_id]$theta_dpt_min
            ) ~ "," ~
            "Prov =" ~ .(merged_k2[merged_k1$prov_cdc == prov_id]$theta_prov_min)
        )
      ),
      x = "Week", 
      y = "Mortality per 100,000",
      color = "Model"
    ) +
    ylim(c(0, quantile(data_prov[dpt_cdc == data_prov[prov_cdc == prov_id][1,1]]$y1, 0.975)) *1.5) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_prov_data_combined_k1 <- function(prov_id, data_prov, prov_pred_best_k1, prov_pred_min_k1, 
                              merged_k1, waves, best_k1_theta) {
  
  ggplot() +
    geom_point(aes(x = x1, y = y1), data = data_prov[data_prov$prov_cdc == prov_id]) +
    geom_line(
      data = prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000, 
        color = paste0(
          "k1 Best (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_best, 5),")"
        )
      ), linewidth = 1.5
    ) +
    geom_line(
      data = prov_pred_min_k1[prov_pred_min_k1$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000,
        color =  paste0(
          "k1 Min (we = ", round(merged_k1[merged_k1$prov_cdc == prov_id]$we_k_min, 5),")"
        )
      ), linewidth = 1.5, linetype = 2
    ) +
    labs(
      title = paste(
        unique(
          prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id]$dpt_cdc),
        "-", prov_id,
        "(w1 =",  round(waves[waves$prov_cdc == prov_id & waves$k == 1]$w_k, 6),
        "w2 =",  round(waves[waves$prov_cdc == prov_id & waves$k == 2]$w_k, 6),
        ")"
      ),
      subtitle = bquote(
        atop(
          "Absolute difference =" ~ .(round(merged_k1[merged_k1$prov_cdc == prov_id]$abs_diff, 6)) ~
            "|" ~"Relative difference  =" ~ .(
              round(merged_k1[merged_k1$prov_cdc == prov_id]$rel_diff, 6)
            ),
          "Best common" ~ theta*": Dpt =" ~  .(best_k1_theta$theta_dpt) ~ "," ~
            "Prov =" ~ .(best_k1_theta$theta_prov) ~ "|" ~
            "Minimum" ~ theta*": Dpt =" ~  .(
              merged_k1[merged_k1$prov_cdc == prov_id]$theta_dpt_min
            ) ~ "," ~
            "Prov =" ~ .(merged_k1[merged_k1$prov_cdc == prov_id]$theta_prov_min)
        )
      ),
      x = "Week", 
      y = "Mortality per 100,000",
      color = NULL
    ) +
    ylim(c(0, quantile(data_prov[dpt_cdc == data_prov[prov_cdc == prov_id][1,1]]$y1, 0.975)) *1.5) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_prov_data_combined_k2 <- function(prov_id, data_prov, prov_pred_best_k2, prov_pred_min_k2, 
                                       merged_k2, waves, best_k2_theta) {
  
  ggplot() +
    geom_point(aes(x = x1, y = y1), data = data_prov[data_prov$prov_cdc == prov_id]) +
    geom_line(
      data = prov_pred_best_k2[prov_pred_best_k2$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000, 
        color = paste0(
          "k2 Best (we = ", round(merged_k2[merged_k2$prov_cdc == prov_id]$we_k_best, 5),")"
        )
      ), linewidth = 1.5
    ) +
    geom_line(
      data = prov_pred_min_k2[prov_pred_min_k2$prov_cdc == prov_id],
      aes(
        x = x1, 
        y = exp(y_hat) * 100000,
        color =  paste0(
          "k2 Min (we = ", round(merged_k2[merged_k2$prov_cdc == prov_id]$we_k_min, 5),")"
        )
      ), linewidth = 1.5, linetype = 2
    ) +
    labs(
      title = paste(
        unique(
          prov_pred_best_k2[prov_pred_best_k2$prov_cdc == prov_id]$dpt_cdc),
        "-", prov_id,
        "(w1 =",  round(waves[waves$prov_cdc == prov_id & waves$k == 1]$w_k, 6),
        "w2 =",  round(waves[waves$prov_cdc == prov_id & waves$k == 2]$w_k, 6),
        ")"
      ),
      subtitle = bquote(
        atop(
          "Absolute difference =" ~ .(round(merged_k2[merged_k1$prov_cdc == prov_id]$abs_diff, 6)) ~
            "|" ~"Relative difference  =" ~ .(
              round(merged_k1[merged_k2$prov_cdc == prov_id]$rel_diff, 6)
            ),
          "Best common" ~ theta*": Dpt =" ~  .(best_k2_theta$theta_dpt) ~ "," ~
            "Prov =" ~ .(best_k2_theta$theta_prov) ~ "|" ~
            "Minimum" ~ theta*": Dpt =" ~  .(
              merged_k2[merged_k2$prov_cdc == prov_id]$theta_dpt_min
            ) ~ "," ~
            "Prov =" ~ .(merged_k2[merged_k1$prov_cdc == prov_id]$theta_prov_min)
        )
      ),
      x = "Week", 
      y = "Mortality per 100,000",
      color = NULL
    ) +
    ylim(c(0, quantile(data_prov[dpt_cdc == data_prov[prov_cdc == prov_id][1,1]]$y1, 0.975)) *1.5) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

# List of Best and Minimum Predictions -----------------------------------------------------------

best_min_pred <- function(data, best_k1_theta, lowest_k1_we_kij, best_k2_theta, lowest_k2_we_kij){
  
  prov_pred_best_k1 <- (
    data_pred
    [x1 < 43 & theta_dpt == best_k1_theta$theta_dpt & theta_prov == best_k1_theta$theta_prov]
  )
  
  prov_pred_min_k1 <- (
    data_pred
    [x1 < 43]
    [lowest_k1_we_kij, on = .(prov_cdc)]
    [theta_dpt == theta_dpt_min & theta_prov == theta_prov_min]
  )
  
  prov_pred_best_k2 <- (
    data_pred
    [x1 >= 43 & theta_dpt == best_k2_theta$theta_dpt & theta_prov == best_k2_theta$theta_prov]
  )
  
  prov_pred_min_k2 <- (
    data_pred
    [x1 >= 43]
    [lowest_k2_we_kij, on = .(prov_cdc)]
    [theta_dpt == theta_dpt_min & theta_prov == theta_prov_min]
  )
  
  prov_pred_best_all_k1 <- (
    data_pred
    [theta_dpt == best_k1_theta$theta_dpt & theta_prov == best_k1_theta$theta_prov]
  )
  
  prov_pred_min_all_k1 <- (
    data_pred
    [lowest_k1_we_kij, on = .(prov_cdc)]
    [theta_dpt == theta_dpt_min & theta_prov == theta_prov_min]
  )
  
  prov_pred_best_all_k2 <- (
    data_pred
    [theta_dpt == best_k2_theta$theta_dpt & theta_prov == best_k2_theta$theta_prov]
  )
  
  prov_pred_min_all_k2 <- (
    data_pred
    [lowest_k2_we_kij, on = .(prov_cdc)]
    [theta_dpt == theta_dpt_min & theta_prov == theta_prov_min]
  )
  
  list(
    prov_pred_best_k1 = prov_pred_best_k1, 
    prov_pred_min_k1 = prov_pred_min_k1, 
    prov_pred_best_k2 = prov_pred_best_k2, 
    prov_pred_min_k2 = prov_pred_min_k2,
    prov_pred_best_all_k1 = prov_pred_best_all_k1,
    prov_pred_min_all_k1 = prov_pred_min_all_k1, 
    prov_pred_best_all_k2 = prov_pred_best_all_k2, 
    prov_pred_min_all_k2 = prov_pred_min_all_k2  
  )
  
}


best_dpt_pred <- function(data, best_theta, pob_dpt){
  
  (
    data
    [theta_dpt == best_theta$theta_dpt & theta_prov == best_theta$theta_prov]
    [, .(
      dpt_cdc, prov_cdc, pob,x1, 
      y1, deaths_obs = y1 * pob,
      y_hat, deaths_hat = (exp(y_hat) * pob)
      )]
    [, .(deaths = sum(deaths_hat)), by = .(dpt_cdc, x1)]
    [pob_dpt, on = .(dpt_cdc)]
    [,.(dpt_cdc, x1, pob, deaths, mort = deaths/ pob * 1e5)]
  )
  
  
}

make_time_stamp <- function() {
  run_date <- gsub("-", "_", Sys.time())
  run_date <- gsub(":", "_", run_date)
  run_date <- gsub(" ", "_", run_date)
  
  return(run_date)
}
