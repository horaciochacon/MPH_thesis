source("R/functions.R")
library(stringr)
library(ggpubr)
library(viridis)
library(scales)

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



# Principal component analysis --------------------------------------------

vars <- c(
  "Human development index" = "HDI", 
  "Years of education" = "education_years", 
  "Prop. EsSalud (log)" = "log_essalud", 
  "Incoming migration (log)" = "mig_in_perc", 
  "Government density index" = "ide",
  "Prop. female population" = "perc_fem", 
  "Prop. 65 or older" = "perc_65_plus", 
  "Population density (log)" = "log_pop_dens", 
  "Med-pop ratio (log)" = "log_med_ratio",
  "Altitude" = "altitud"
  )
mort_breaks <- c(5, 10, 20, 50)

# First Peak
provinces.cov.1 <- provinces %>% 
  select(all_of(vars))

prov.cov.pca.1 <- prcomp(provinces.cov.1, center = TRUE, scale = TRUE)
  
provinces.cov.1 <- provinces %>% 
  select(all_of(vars), mort_first_peak, HDI_low) %>% 
  mutate(pc.1 = prov.cov.pca.1$x[,1], pc.2 = prov.cov.pca.1$x[,2])

graph_pca_mort1 <- ggbiplot(prov.cov.pca.1, obs.scale = 1, varname.size = 4, alpha = 0) +
  geom_point(
    aes(x = pc.1, y = pc.2, size = mort_first_peak * 1e5),
    alpha = 0.55,
    color = "#4363d8",
    data = provinces.cov.1
    ) +
  scale_size(
    trans = "log",
    breaks = mort_breaks
  ) +
  labs(
    size = "Mortality rate\nfirst peak",
    color = "Human\ndevelopment\nindex"
  ) +
  goldenScatter

save_plot(
  plot = graph_pca_mort1,
  filename =  paste0("plots/Presentation/Results/graph_pca_mort1.png"),
  scale = 2
)

# Second Peak
provinces.cov.2 <- provinces %>% 
  filter(!is.na(log_mort2)) %>% 
  select(all_of(vars)) 

prov.cov.pca.2 <- prcomp(provinces.cov.2, center = TRUE, scale = TRUE)
provinces.cov.2 <- provinces %>% 
  select(all_of(vars), mort_second_peak, HDI_low) %>% 
  na.omit() %>% 
  mutate(
    pc.1  = prov.cov.pca.2$x[,1],
    pc.2  = prov.cov.pca.2$x[,2]
  )

graph_pca_mort2 <- ggbiplot(prov.cov.pca.2, obs.scale = 1, varname.size = 4, alpha = 0) +
  geom_point(
    aes(x = pc.1, y = pc.2, size = mort_second_peak * 1e5),
    alpha = 0.55,
    color = "#4363d8",
    data = provinces.cov.2
  ) +
  scale_size(
    trans = "log",
    breaks = mort_breaks
  ) +
  labs(
    size = "Mortality rate\nsecond peak",
    color = "Human\ndevelopment\nindex"
  ) +
  goldenScatter

save_plot(
  plot = graph_pca_mort2,
  filename =  paste0("plots/Presentation/Results/graph_pca_mort2.png"),
  scale = 2
)

# RT
provinces.cov.3 <- provinces %>% 
  filter(!is.na(rt)) %>% 
  select(all_of(vars))

prov.cov.pca.3 <- prcomp(provinces.cov.3, center = TRUE, scale. = TRUE)
provinces.cov.3 <- provinces %>% 
  select(all_of(vars), HDI_low, rt) %>% 
  filter(!is.na(rt)) %>% 
  mutate(
    pc.1 = prov.cov.pca.3$x[,1],
    pc.2 = prov.cov.pca.3$x[,2]
  )

graph_pca_rt <- ggbiplot(prov.cov.pca.3, obs.scale = 1, varname.size = 4, alpha = 0) +
  geom_point(
    aes(x = pc.1, y = pc.2, size = rt),
    alpha = 0.55,
    color = "#4363d8",
    data = provinces.cov.3
  ) +
  scale_size(
    trans = "log"
  ) +
  labs(
    size = "Rt",
    color = "Human\ndevelopment\nindex"
  ) +
  goldenScatter

save_plot(
  plot = graph_pca_rt,
  filename =  paste0("plots/Presentation/Results/graph_pca_rt.png"),
  scale = 2
)

# Correlation plots -------------------------------------------------------
xbreaks <- c(5,10, 30, 100)
popBreaks <- c(1, 10, 100, 1000) * 1000
labels <- c("1,000", "10,000", "100,000", "1,000,000")

# Mortality 1 vs Mortality 2
scatter_mortality <- provinces %>% 
  ggplot(aes(x = mort_first_peak * 1e5, y = mort_second_peak *1e5)) +
  geom_point(aes(size = pop), alpha = 0.55, color = "#e6194b") +
  scale_x_log10(breaks = xbreaks) +
  scale_y_log10(breaks = xbreaks) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
    ) +
  geom_smooth(method = "lm", color = "black") +
  labs(
    x = "Mortality rate first peak",
    y = "Mortality rate second peak",
    size = "Population\nsize"
  ) +
  goldenScatter

save_plot(
  plot = scatter_mortality ,
  filename =  paste0("plots/Presentation/Results/scatter_mortality.png"),
  scale = 1.5
)

# Migration vs Mortality 1
scatter_migration1 <- provinces %>% 
  ggplot(
    aes(x = mig_in_perc * 1e4, y = mort_first_peak *1e5, color = HDI_low)
    ) +
  geom_point(aes(size = pop), alpha = 0.55) +
  scale_y_log10(breaks = xbreaks) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Incoming internal migration per 10,000 inhabitants",
    y = "Mortality rate first peak",
    size = "Population\nsize",
    color = "Human\ndevelopment\nindex"
  ) +
  goldenScatter

save_plot(
  plot = scatter_migration1,
  filename =  paste0("plots/Presentation/Results/scatter_migration1.png"),
  scale = 1.5
)

# Migration vs Mortality 2
scatter_migration2 <- provinces %>% 
  ggplot(
    aes(x = mig_in_perc * 1e4, y = mort_second_peak *1e5, color = HDI_low)
  ) +
  geom_point(aes(size = pop), alpha = 0.55) +
  scale_y_log10(breaks = xbreaks) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Incoming internal migration per 10,000 inhabitants",
    y = "Mortality rate second peak",
    size = "Population\nsize",
    color = "Human\ndevelopment\nindex"
  ) +
  goldenScatter

save_plot(
  plot = scatter_migration2,
  filename =  paste0("plots/Presentation/Results/scatter_migration2.png"),
  scale = 1.5
)

# Migration vs Mortality cum
scatter_migration_cum <- provinces %>% 
  ggplot(
    aes(x = mig_in_perc * 1e4, y = mort_cum *1e5, color = HDI_low)
  ) +
  geom_point(aes(size = pop), alpha = 0.55) +
  scale_y_log10(breaks = xbreaks) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Incoming internal migration per 10,000 inhabitants",
    y = "Cumulative mortality rate",
    size = "Population\nsize",
    color = "Human\ndevelopment\nindex"
  ) +
  goldenScatter

save_plot(
  plot = scatter_migration_cum ,
  filename =  paste0("plots/Presentation/Results/scatter_migration_cum.png"),
  scale = 1.5
)

# 65 vs Mortality 1
scatter_65years_mort1 <- provinces %>% 
  ggplot(
    aes(x = perc_65_plus, y = mort_first_peak *1e5)
  ) +
  geom_point(aes(size = pop), alpha = 0.55, color = "#e6194b") +
  scale_y_log10(breaks = xbreaks) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Population 65 year or older (%)",
    y = "Mortality rate first peak",
    size = "Population\nsize"
  ) +
  goldenScatter

save_plot(
  plot = scatter_65years_mort1,
  filename =  paste0("plots/Presentation/Results/scatter_65years_mort1.png"),
  scale = 1.5
)

# 65 vs Mortality 2
scatter_65years_mort2 <- provinces %>% 
  ggplot(
    aes(x = perc_65_plus, y = mort_second_peak *1e5)
  ) +
  geom_point(aes(size = pop), alpha = 0.55, color = "#e6194b") +
  scale_y_log10(breaks = xbreaks) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Population 65 year or older (%)",
    y = "Mortality rate second peak",
    size = "Population\nsize"
  ) +
  goldenScatter

save_plot(
  plot = scatter_65years_mort2,
  filename =  paste0("plots/Presentation/Results/scatter_65years_mort2.png"),
  scale = 1.5
)

# HDI vs Mortality 1
scatter_hdi_mort1 <- provinces %>% 
  ggplot(
    aes(x = HDI, y = mort_first_peak *1e5)
  ) +
  geom_point(aes(size = pop), alpha = 0.55, color = "#e6194b") +
  scale_y_log10(breaks = xbreaks) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Human development index",
    y = "Mortality rate first peak",
    size = "Population\nsize"
  ) +
  goldenScatter

save_plot(
  plot = scatter_hdi_mort1 ,
  filename =  paste0("plots/Presentation/Results/scatter_hdi_mort1.png"),
  scale = 1.5
)

# HDI vs Mortality 2
scatter_hdi_mort2 <- provinces %>% 
  ggplot(
    aes(x = HDI, y = mort_second_peak *1e5)
  ) +
  geom_point(aes(size = pop), alpha = 0.55, color = "#e6194b") +
  scale_y_log10(breaks = xbreaks) +
  scale_x_continuous(labels = scales::percent) +
  scale_size_area(
    max_size = 10,
    breaks = popBreaks, 
    labels = labels,
    limits = c(0, 3e6)
  ) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Human development index",
    y = "Mortality rate second peak",
    size = "Population\nsize"
  ) +
  goldenScatter

save_plot(
  plot = scatter_hdi_mort2 ,
  filename =  paste0("plots/Presentation/Results/scatter_hdi_mort2.png"),
  scale = 1.5
)
