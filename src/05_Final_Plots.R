source("R/functions.R")
library(stringr)

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


