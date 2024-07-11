

# Load Packages -----------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(dplyr)
library(cowplot)

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
  (config_loc <- as.character(commandArgs(TRUE)[[2]]))
}

config <- config::get(file = config_loc, config = "analysis")


# Read Data -----------------------------------------------------------------------------------

data_prov <- fread(paste0(output_dir,"/data_prov.csv"))
pred_cascade_prov <- fread(paste0(output_dir,"/MR_BRT/output/pred_cascade_prov.csv"))
pred_cascade_dpt <- fread(paste0(output_dir,"/MR_BRT/output/pred_cascade_dpt.csv"))

# Plots ---------------------------------------------------------------------------------------

plots <- config$pred$depts %>% 
  map(~{
    graph_dpto <- data_prov %>%
      filter(dpt_cdc == .x) %>%
      ggplot() +
      geom_point(
        aes(x = x1, y = y1 * 100000),
        size = 0.7,
        alpha = 0.4
      ) +
      geom_line(
        data = pred_cascade_dpt %>%  filter(dpt_cdc == .x),
        aes(x = x1, y = exp(pred) * 100000), col = "red"
      ) +
      facet_wrap( . ~ dpt_cdc) +
      labs(x = NULL, y = "Mortality per 100,000") +
      theme_bw()
    
    graph_prov <- data_prov %>%
      filter(dpt_cdc == .x) %>%
      ggplot() +
      geom_point(aes(x = x1, y = y1 * 100000), size = 1, alpha = 0.5) +
      geom_line(
        data = pred_cascade_prov %>%  filter(dpt_cdc == .x),
        aes(x = x1, y = exp(pred) * 100000), col = "blue"
      ) +
      facet_wrap(.~prov_cdc) +
      coord_cartesian(
        ylim = c(
          0,
          max(
            data_prov %>% filter(dpt_cdc == .x) %>% .$y1,
            na.rm = TRUE
          ) * 1e5 + 5
        )
      ) +
      labs(x = "Week", y = "Mortality per 100,000") +
      theme_bw()
    
    graph <-  ggdraw() +
      draw_plot(graph_dpto, x = 0.25, y = .7, width = .5, height = .3) +
      draw_plot(graph_prov, x = 0, y = 0, width = 1, height = 0.7)
    
    graph
  })

plots %>% 
  map2(config$pred$depts,
    ~ ggsave( 
      filename = paste0(
        output_dir,"/",config$save$plot_mrbrt,"/", .y,
        config$save$plots_format),
      plot = .x,
      units = "in",
      width = 16.7,
      height = 10.8
    )
  )
