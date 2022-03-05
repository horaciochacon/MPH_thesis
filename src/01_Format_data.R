library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(ggplot2)
library(reticulate)
use_condaenv("mrtool-0.0.1")
library(mrtoolr)
library(ggpubr)


# Loading and Formatting the data -----------------------------------------


# Death count per day dataset
death_count_day <- read.csv("data/death_count_prov_day.csv",
  colClasses = c(
    "Date","character", "character","character","character","numeric"
    )
  ) %>% 
  tibble()

# Province level population
poblacion_prov <- read.csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(dpt_cdc = DEPARTAMENTO,prov_cdc = PROVINCIA) %>% 
  summarise(pob = sum(POBLACION))

# Department level population
poblacion_dpt <- read.csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(dpt_cdc = DEPARTAMENTO) %>% 
  summarize(pob = sum(POBLACION))

# National Death count
death_count_ntl <- death_count_day %>% 
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
    )
  ) %>% 
  group_by(fecha_fallecimiento) %>% 
  summarise(n = sum(n)) %>% 
  mutate(
    week = (as.numeric(fecha_fallecimiento) - 18323) / 7
  )

# Department level death counts
data_dpt <- death_count_day %>% 
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
    )
  ) %>% 
  group_by(fecha_fallecimiento, dpt_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_dpt, by = "dpt_cdc") %>% 
  ungroup() %>% 
  mutate(
    y1 = n / pob,
    ylog = log(y1 + 0.0000001),
    x1 = (as.numeric(fecha_fallecimiento) - 18323) / 7,
    sd = sqrt(1 / y1),
    id = row_number()
  ) %>% 
  na.omit() 

# Offset of 0.00001
# Province level death counts
data_prov <- death_count_day %>% 
  mutate(
    fecha_fallecimiento = floor_date(
      fecha_fallecimiento, "weeks", week_start = 1
    )
  ) %>% 
  group_by(fecha_fallecimiento, dpt_cdc, prov_cdc) %>% 
  summarise(n = sum(n)) %>% 
  left_join(poblacion_prov, by = "prov_cdc") %>% 
  ungroup() %>% 
  mutate(
    y1 = n / pob,
    ylog = log(y1 + 0.0000001),
    x1 = (as.numeric(fecha_fallecimiento) - 18323) / 7,
    sd = sqrt(1 / y1),
    id = row_number()
  ) %>% 
  na.omit() %>% 
  select(-dpt_cdc.y, dpt_cdc = dpt_cdc.x)
  

# Splines DPTO ---------------------------------------------------------------

dpto_mrbrt <- MRData() 
dpto_mrbrt$load_df(
  data = data_prov,  
  col_obs = "ylog", 
  col_obs_se = "sd",
  col_covs = list("x1"), 
  col_study_id = "id" 
  )


mod_dpto <- MRBRT(
  data = dpto_mrbrt,
  cov_models = list(
    LinearCovModel("intercept", use_re = TRUE),
    LinearCovModel(
      alt_cov = "x1",
      use_spline = TRUE,
      # spline_knots = array(c(0, 0.25, 0.5, 0.75, 1)),
      spline_knots = array(seq(0, 1, by = 0.05)),
      spline_degree = 3L,
      spline_knots_type = 'frequency',
      # spline_r_linear = FALSE,
      spline_l_linear = TRUE
      # prior_spline_monotonicity = 'increasing',
      # prior_spline_convexity = "convex"
      # prior_spline_maxder_gaussian = array(c(0, 0.01))
      # prior_spline_maxder_gaussian = rbind(c(0,0,0,0,-1), c(Inf,Inf,Inf,Inf,0.0001))
    )
  )
)

mod_dpto$fit_model(inner_print_level = 5L, inner_max_iter = 10000L)

df_pred <- data.frame(x1 = seq(0, 80, by = 0.1))

dat_pred_dpto <- MRData()
dat_pred_dpto$load_df(
  data = df_pred, 
  col_covs = list('x1')
)

pred_dpto <- mod_dpto$predict(data = dat_pred_dpto)

ggplot(data_dpt) +
  geom_point(aes(x = x1, y = y1 * 100000), alpha = 0.3, size = 0.3) +
  geom_line(data = df_pred, aes(x = x1, y = exp(pred_dpto) * 100000)) +
  ylim(c(0, 30)) +
  labs(x = "Week", y = "Mortality per 100,000") +
  theme_bw()


# Cascade splines DPTO --------------------------------------------------------

mod_spline_dpto <- run_spline_cascade(
  stage1_model_object = mod_dpto,
  df = data_prov,
  col_obs = "ylog",
  col_obs_se = "sd",
  col_study_id = "id",
  stage_id_vars = "dpt_cdc",
  thetas = 2,
  output_dir = "output/",
  model_label = "mbrt_cascade_dpto_peru",
  overwrite_previous = TRUE
)

df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 80, by = 0.1),
  dpt_cdc = unique(data$dpt_cdc)
  ) %>%
  mutate(data_id = 1:nrow(.)) 

pred_cascade_dpto <- predict_spline_cascade(
  fit = mod_spline_dpto,
  newdata = df_pred
  ) %>% 
  left_join(data_prov)

ggplot(data = pred_cascade_dpto) +
  geom_line(aes(x = x1, y = exp(pred) * 100000)) +
  geom_point(aes(x = x1, y = y1 * 100000), size = 0.1, alpha = 0.5) +
  facet_wrap(.~dpt_cdc) +
  labs(x = "Week", y = "Mortality per 100,000") +
  ylim(c(0,50)) +
  theme_bw()

ggsave("plots/departments.png", scale = 3)

# Splines Prov ---------------------------------------------------------------

prov_mrbrt <- MRData()
prov_mrbrt$load_df(
  data = data,  
  col_obs = "ylog", 
  col_obs_se = "sd",
  col_covs = list("x1"), 
  col_study_id = "id" 
  )


mod_prov <- MRBRT(
  data = prov_mrbrt,
  cov_models = list(
    LinearCovModel("intercept", use_re = TRUE),
    LinearCovModel(
      alt_cov = "x1",
      use_spline = TRUE,
      # spline_knots = array(c(0, 0.25, 0.5, 0.75, 1)),
      spline_knots = array(seq(0, 1, by = 0.05)),
      spline_degree = 3L,
      spline_knots_type = 'frequency',
      # spline_r_linear = FALSE,
      spline_l_linear = TRUE
      # prior_spline_monotonicity = 'increasing',
      # prior_spline_convexity = "convex"
      # prior_spline_maxder_gaussian = array(c(0, 0.01))
    )
  )
)

mod_prov$fit_model(inner_print_level = 5L, inner_max_iter = 10000L)

df_pred <- data.frame(x1 = seq(0, 80, by = 0.1))

dat_pred_prov <- MRData()
dat_pred_prov$load_df(
  data = df_pred, 
  col_covs = list('x1')
)

pred_prov <- mod_prov$predict(data = dat_pred_prov)

ggplot(data_prov) +
  geom_point(aes(x = x1, y = y1 * 100000), alpha = 0.3, size = 0.3) +
  geom_line(data = df_pred, aes(x = x1, y = exp(pred_prov) * 100000)) +
  ylim(c(0, 30)) +
  labs(x = "Week", y = "Mortality per 100,000") +
  theme_bw()


# Cascade splines Prov --------------------------------------------------------

mod_spline_prov <- run_spline_cascade(
  stage1_model_object = mod_prov,
  df = data_prov,
  col_obs = "ylog",
  col_obs_se = "sd",
  col_study_id = "id",
  stage_id_vars = c("dpt_cdc", "prov_cdc"),
  thetas = c(2,8),
  output_dir = "output/",
  model_label = "mbrt_cascade_peru_prov",
  overwrite_previous = TRUE
)

# Prediction Provinces
df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 80, by = 0.1),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>%
  mutate(
    data_id = 1:nrow(.)
  ) %>% 
  left_join(poblacion_prov %>% select(-pob))

preds_cascade_prov <- predict_spline_cascade(
  fit = mod_spline_prov,
  newdata = df_pred
  ) %>% 
  left_join(data_prov) 

depts <- unique(data$dpt_cdc)

for (i in depts) {
  
  graph_dpto <- pred_cascade_dpto %>% 
    filter(dpt_cdc == i) %>% 
    ggplot() +
    geom_line(aes(x = x1, y = exp(pred) * 100000)) +
    geom_point(
      data = data_prov  %>% filter(dpt_cdc == i), 
      aes(x = x1, y = y1 * 100000), 
      size = 1, 
      alpha = 0.5
      ) +
    facet_wrap(.~dpt_cdc) +
    labs(x = "Week", y = "Mortality per 100,000") +
    theme_bw()
  
  graph_prov <- preds_cascade_prov %>% 
    filter(dpt_cdc == i) %>% 
    ggplot() +
    geom_line(aes(x = x1, y = exp(pred) * 100000)) +
    geom_point(aes(x = x1, y = y1 * 100000), size = 1, alpha = 0.5) +
    facet_wrap(.~prov_cdc) +
    labs(x = "Week", y = "Mortality per 100,000") +
    theme_bw() 
  
  graph <- ggarrange(
    graph_dpto,
    graph_prov, 
    nrow = 2,
    heights = c(2, 5), 
    widths = c(0.5,5)
    )
  
  print(graph)
  
  ggsave(plot = graph, filename =  paste0("plots/", i, ".png"), scale = 3)
}

