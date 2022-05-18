source("R/functions.R")
library(stringr)
library(ggpubr)
library(viridis)

# MR-BRT Plot -------------------------------------------------------------

depts <- "Junin"
pred_cascade_dpto <- pred_cascade_dpto %>%
  mutate(dpt_cdc = str_to_title(dpt_cdc))
preds_cascade_prov <- preds_cascade_prov %>% 
  mutate(prov_cdc = str_to_title(prov_cdc), dpt_cdc = str_to_title(dpt_cdc))
data_prov <-  data_prov %>% 
  mutate(dpt_cdc = str_to_title(dpt_cdc))


for (i in depts) {
  
  graph_national <-  ggplot(data_prov) +
    geom_errorbar(
      aes(
        x = x1,
        y = y1 * 1e5,
        ymin = exp(y_low) * 1e5,
        ymax = exp(y_upp) * 1e5
      ),
      color = "grey",
      alpha = 0.3,
      size = 0.2
    ) +
    geom_point(
      aes(x = x1, y = y1 * 100000), 
      alpha = 0.1, size = 0.3
      ) +
    geom_line(
      data = df_pred_nat, 
      aes(x = x1, y = exp(pred_prov) * 100000),
      size = 1.5,
      alpha = 0.8,
      col = "#f032e6"
      ) +
    labs(
      x = NULL, 
      y = "Weekly mortality rate / 100,000",
      title = "Country"
      ) +
    ylim(
      c(0, 40)
    ) +
    goldenScatterCAtheme

  
  
  graph_dpto <- pred_cascade_dpto %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_errorbar(
      aes(
        x = x1,
        y = y1 * 1e5,
        ymin = exp(y_low) * 1e5,
        ymax = exp(y_upp) * 1e5
      ),
      color = "grey",
      alpha = 0.95,
      size = 0.2
      ) +
    geom_point(
      data = data_prov  %>% filter(dpt_cdc == i),
      aes(x = x1, y = y1 * 100000),
      size = 0.7,
      alpha = 0.4
    ) +
    geom_line(
      aes(x = x1, y = exp(pred) * 100000), 
      size = 1.5,
      alpha = 0.8,
      col = "#e6194b") +
    coord_cartesian(
      ylim = c(0,40)
    ) +
    labs(
      x = NULL, 
      y = NULL,
      title = paste0("Department - ", i)) +
    goldenScatterCAtheme
  
  graph_prov <- preds_cascade_prov %>%
    filter(dpt_cdc == i) %>%
    ggplot() +
    geom_point(aes(x = x1, y = y1 * 100000), size = 0.7, alpha = 0.4) +
    geom_errorbar(
      aes(
        x = x1,
        y = y1 * 1e5,
        ymin = exp(y_low) * 1e5,
        ymax = exp(y_upp) * 1e5
      ),
      color = "grey",
      alpha = 0.95,
      size = 0.2) +
    geom_line(
      aes(x = x1, y = exp(pred) * 100000), 
      size = 1.5,
      alpha = 0.8,
      col = "#4363d8") +
    facet_wrap(.~prov_cdc) +
    coord_cartesian(
      ylim = c(
        0,
        max(
          preds_cascade_prov %>% filter(dpt_cdc == i) %>% .$y1,
          na.rm = TRUE
        ) * 1e5 + 5
      )
    ) +
    labs(
      x = "Week", 
      y = "Weekly mortality rate / 100,000",
      title = "Province"
      ) +
    goldenScatterCAtheme
  
  graph <-  ggdraw() +
    draw_plot(graph_national, x = 0.125, y = .7, width = .51, height = .3) +
    draw_plot(graph_dpto, x = 0.355, y = .7, width = .55, height = .3) +
    draw_plot(graph_prov, x = 0, y = 0, width = 1, height = 0.7)
  
  
  # print(graph_log)
  print(graph)
  
  ggsave(
    plot = graph,
    filename =  paste0("plots/Presentation/Methods/MR-BRT2.png"),
    scale = 2
    )

}


# Feature extraction ------------------------------------------------------

# Peaks

provs <- "TAMBOPATA"
for (i in provs) {
  peak_graph <- ggplot() +
    geom_line(
      aes(x = day, y = mortality * 100000),
      size = 1.5,
      alpha = 0.8,
      col = "#4363d8",
      data = prov_preds_peaks[[i]], 
    ) +
    labs(
      x = "Day",
      y = "Weekly mortality rate / 100,000"
    ) +
    goldenScatterCAtheme +
    theme(aspect.ratio = 9/25)
  
  if (sum(prov_preds_peaks[[i]]$peak) > 0) {
    peak_graph <-  peak_graph +
      geom_label_repel(
        aes(x = day_first_peak, y = mort_first_peak * 100000, label = "peak"),
        nudge_y = -5, nudge_x = 50,
        data =  first_peak %>%
          filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  
  if (second_peak %>%
      filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)) %>% nrow() > 0) {
    peak_graph <- peak_graph +
      geom_label_repel(
        aes(x = day_second_peak, y = mort_second_peak * 100000, label = "peak"),
        nudge_y = 0, nudge_x = -50,
        data =  second_peak %>%
          filter(prov_cdc == unique(prov_preds_peaks[[i]]$prov_cdc)),
        arrow = arrow(length = unit(0.015, "npc"))
      ) 
  }
  print(peak_graph)
}

# RT

rt_graph <- map2(
  rt_list[provs], 
  rt_peak[provs],
  .f = 
    ~estimate_R_plots(.x, what = "R") + 
    geom_line(color = "#e6194b") +
    geom_hline(yintercept = 1) +
    labs(title = str_to_title(prov_preds_peaks[[i]]$prov_cdc[1]), x = NULL) +
    geom_vline(xintercept = .y$t_end) +
    coord_cartesian(
      ylim = c(0,4)
    ) +
    goldenScatterCAtheme +
    theme(aspect.ratio = 9/25)
    
)


graph_features <- plot_grid(
  rt_graph[[1]], 
  peak_graph, 
  nrow = 2, 
  align = "v"
  ) 


# print(graph_log)
print(graph_features)

save_plot(
  plot = graph_features,
  filename =  paste0("plots/Presentation/Methods/Features.png"),
  base_asp = 2,
  scale = 2
)

# Maps features -----------------------------------------------------------
map_prov <- readRDS("data/pre_processed/map_prov.RDS") %>% 
  select(prov_cdc)
provinces <- read.csv("output/provinces_final_covariates.csv") %>% 
  left_join(map_prov) %>% 
  as_tibble()


# Peaks

map_day_peak1 <- provinces %>% 
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = day_first_peak, geometry = geometry), 
    size = 0.05, 
    color = "grey40", 
    alpha = 1
  ) +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = 1,
    name = "Day of first peak",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      reverse = TRUE,
      title.position = 'top',
      title.hjust = 0.5
      )
    )

map_mort_peak1 <- provinces %>% 
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = log_mort1, geometry = geometry), 
    size = 0.05, 
    color = "grey40", 
    alpha = 1
  ) +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = "Log-Mortality rate first peak",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  )

map_day_peak2 <- provinces %>% 
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = day_second_peak, geometry = geometry), 
    size = 0.05, 
    color = "grey40", 
    alpha = 1
  ) +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = 1,
    name = "Day of second peak",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      reverse = TRUE,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  )

map_mort_peak2 <- provinces %>% 
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = log_mort2, geometry = geometry), 
    size = 0.05, 
    color = "grey40", 
    alpha = 1
  ) +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    name = "Log-Mortality rate second peak",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  )

save_plot(
  plot = map_day_peak1,
  filename =  paste0("plots/Presentation/Results/map_day_peak1.png"),
  scale = 1.5
)

save_plot(
  plot = map_mort_peak1,
  filename =  paste0("plots/Presentation/Results/map_mort_peak1.png"),
  scale = 1.5
)

save_plot(
  plot = map_day_peak2,
  filename =  paste0("plots/Presentation/Results/map_day_peak2.png"),
  scale = 1.5
)

save_plot(
  plot = map_mort_peak2,
  filename =  paste0("plots/Presentation/Results/map_mort_peak2.png"),
  scale = 1.5
)

# RT

map_day_peak1 <- provinces %>% 
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "white", size = 0.05, color = "grey40"
  ) +
  geom_sf(
    aes(fill = day_first_peak, geometry = geometry), 
    size = 0.05, 
    color = "grey40", 
    alpha = 1
  ) +
  theme_map() +
  theme(legend.position = "top") +
  scale_fill_viridis(
    option = "mako", 
    direction = 1,
    name = "Day of first peak",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      reverse = TRUE,
      title.position = 'top',
      title.hjust = 0.5
    )
  )

