library(data.table)
library(tidyverse)


# Create Run directory structure --------------------------------------------------------------

create_directories <- function(output_dir) {
  message(output_dir)
  dir.create(output_dir, showWarnings = FALSE)
  
  # Define the directory structure
  dirs <- c(
    "logs", "MR_BRT/models", "MR_BRT/models/cascade",
    "MR_BRT/output", "estimates/predicted_we",
    "estimates/best_theta", "plots/heatmaps",
    "plots/mrbrt_predictions",
    "plots/epidemic_curves/wave1", "plots/epidemic_curves/wave2",
    "plots/epidemic_curves/combined", "plots/epidemic_curves/combined_k1",
    "plots/epidemic_curves/combined_k2", "plots/epidemic_curves/grid_plots",
    "plots/epidemic_curves/grid_plots_k1", "plots/epidemic_curves/grid_plots_k2"
  )
  
  # Create directories iteratively
  for (dir in dirs) {
    dir.create(file.path(output_dir, dir), showWarnings = FALSE, recursive = TRUE)
  }
}

# Sensitivity Prediction Plots ----------------------------------------------------------------

# Calculate the maximum y_hat for each department
max_y_hat_dept <- function(prov_pred_best_k1, prov_pred_best_k2) {
  rbind(
    prov_pred_best_k1[, .(max_y_hat = max(exp(y_hat) * 100000, na.rm = TRUE)), by = dpt_cdc],
    prov_pred_best_k2[, .(max_y_hat = max(exp(y_hat) * 100000, na.rm = TRUE)), by = dpt_cdc]
  )[, .(max_y_hat = max(max_y_hat, na.rm = TRUE)), by = dpt_cdc]
}


max_y_hat_dept2 <- function(data_pred, data_obs) {
  rbind(
    data_pred[, .(max_y_hat = max(exp(y_hat) * 1e5, na.rm = TRUE)), by = dpt_cdc],
    data_obs[, .(max_y_hat = max(y1)), by = dpt_cdc]
  )[, .(max_y_hat = max(max_y_hat, na.rm = TRUE)), by = dpt_cdc]
}



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
                              merged_k2, best_k2_theta, waves, max_dpt) {
  
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
    ylim(
      c(
        0, 
        max_dpt[dpt_cdc == unique(
          prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id]$dpt_cdc
          )]$max_y_hat
        )
      ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_prov_data_combined_k1 <- function(prov_id, data_prov, prov_pred_best_k1, prov_pred_min_k1, 
                              merged_k1, waves, best_k1_theta, max_dpt) {
  
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
    ylim(
      c(
        0, 
        max_dpt[dpt_cdc == unique(
          prov_pred_best_k1[prov_pred_best_k1$prov_cdc == prov_id]$dpt_cdc
        )]$max_y_hat
      )
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_prov_data_combined_k2 <- function(prov_id, data_prov, prov_pred_best_k2, prov_pred_min_k2, 
                                       merged_k2, waves, best_k2_theta, max_dpt) {
  
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
    ylim(
      c(
        0, 
        max_dpt[dpt_cdc == unique(
          prov_pred_best_k2[prov_pred_best_k2$prov_cdc == prov_id]$dpt_cdc
        )]$max_y_hat
      )
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}


plot_agg_sens <- function(prov, data_pred, data_obs, max_dpt) {
  ggplot(data = data_pred[prov_cdc == prov,]) +
    geom_point(
      aes(x = x1, y = y1), data = data_obs[prov_cdc == prov,],
      size = 1, alpha = 0.65
    ) +
    geom_line(aes(x = x1, y = exp(y_hat) * 1e5, col = conf), linewidth = 1, alpha = 0.9) +
    labs(
      title = paste(
        unique(data_pred[data_pred$prov_cdc == prov]$dpt_cdc),
        "-",
        prov
        ),
      x = "Week", 
      y = "Mortality per 100,000",
      col = NULL
    ) +
    theme_bw() +
    ylim(c(0, max_dpt[prov_cdc == prov,]$max_y_hat)) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_agg_sens_facet <- function(prov, data_pred, data_obs, facet = "knots") {
  
  if (facet == "knots") {
    ggplot(data = data_pred[prov_cdc == prov,]) +
      geom_point(
        aes(x = x1, y = y1), data = data_obs[prov_cdc == prov,],
        size = 1, alpha = 0.65
      ) +
      geom_line(
        aes(x = x1, y = exp(y_hat) * 1e5, col = paste0(spline_degree)), 
        linewidth = 1, alpha = 0.9
      ) +
      labs(
        title = prov,
        x = "Week", 
        y = "Mortality per 100,000",
        col = "Spline Degree"
      ) +
      facet_wrap(.~paste0("Knots:", spline_knots)) +
      theme_bw() +
      ylim(c(0, ifelse(
        max(data_obs[prov_cdc == prov,]$y1, na.rm = TRUE) < 
          (exp(max(data_pred[prov_cdc == prov,]$y_hat, na.rm = TRUE)) * 1e5),
        max(data_obs[prov_cdc == prov,]$y1, na.rm = TRUE) * 1.2,
        (exp(max(data_pred[prov_cdc == prov,]$y_hat, na.rm = TRUE)) * 1e5) * 1.2
      )
      )
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  } else if (facet == "degree") {
    ggplot(data = data_pred[prov_cdc == prov,]) +
      geom_point(
        aes(x = x1, y = y1), data = data_obs[prov_cdc == prov,],
        size = 1, alpha = 0.65
      ) +
      geom_line(
        aes(x = x1, y = exp(y_hat) * 1e5, col = paste0(spline_knots)), 
        linewidth = 1, alpha = 0.9
      ) +
      labs(
        title = prov,
        x = "Week", 
        y = "Mortality per 100,000",
        col = "Spline Knots"
      ) +
      ylim(c(0, ifelse(
        max(data_obs[prov_cdc == prov,]$y1, na.rm = TRUE) < 
          (exp(max(data_pred[prov_cdc == prov,]$y_hat, na.rm = TRUE)) * 1e5),
        max(data_obs[prov_cdc == prov,]$y1, na.rm = TRUE) * 1.2,
        (exp(max(data_pred[prov_cdc == prov,]$y_hat, na.rm = TRUE)) * 1e5) * 1.2
      )
      )
      ) +
      facet_wrap(.~paste0("Spline Degree:", spline_degree)) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }
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


# Timestamp -----------------------------------------------------------------------------------

make_time_stamp <- function() {
  run_date <- gsub("-", "_", Sys.time())
  run_date <- gsub(":", "_", run_date)
  run_date <- gsub(" ", "_", run_date)
  
  return(run_date)
}


# sbatch --------------------------------------------------------------------------------------


send_job <- function(config, jobname, output_dir, config_loc, rshell, rscript_rel_path, args,
                     job_ids = NULL) {
  # Construct the path to the R script
  rscript <- paste0(config$sbatch$wd, rscript_rel_path)
  
  # Set the arguments
  args <- c(output_dir, config_loc)
  
  # Create the system submission command
  sys.sub <- paste0(
    "sbatch --parsable -A proj_lsae ",
    " -J ", jobname,
    " -o ", paste0(output_dir, "/logs/", "%x.o%j", ".txt"),
    " -c ", config$sbatch$threads,
    " --mem ", config$sbatch$memory_agg,
    " -t ", config$sbatch$time,
    " -p all.q"
  )
  
  # Add dependency if job_ids are provided
  if (!is.null(job_ids) && length(job_ids) > 0) {
    sys.sub <- paste0(sys.sub, " --dependency=afterok:", paste(job_ids, collapse = ":"))
  }
  
  # Construct the full command
  command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
  
  # Execute the command and capture the job ID
  job_id_agg <- system(command, intern = TRUE)
  
  # Return the job ID
  return(job_id_agg)
}

