library(purrr)

dt <- tibble()

# Create prediction data frame to fill predicted values
df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 85, by = 1),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>%
  left_join(poblacion_prov %>% select(-pob))

obs <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 85, by = 1),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>% 
  tibble() %>% 
  left_join(data_prov %>% select(obs_y1 = y1, x1, prov_cdc, dpt_cdc, deaths = n)) %>% 
  fill(dpt_cdc, .direction = "down") %>% 
  mutate(
    obs_y1_imp = ifelse(is.na(obs_y1), 0, obs_y1),
    deaths_imp = ifelse(is.na(deaths), 0, deaths),
    wave_l = ifelse(x1 < 43, 1, 2)
    )
  

for (j in 5:40) {
  for (k in 5:40) {
    # Create and fit MRBRT province-level cascade spline model
    mod_spline_prov <- run_spline_cascade(
      stage1_model_object = mod_prov,
      df = data_prov,
      gaussian_prior = TRUE,
      col_obs = "ylog",
      col_obs_se = "sdlog",
      col_study_id = "id",
      stage_id_vars = c("dpt_cdc", "prov_cdc"),
      thetas = c(j, k),
      output_dir = "output/",
      model_label = "mbrt_cascade_peru_prov",
      overwrite_previous = TRUE
    )
    
    pred <- predict_spline_cascade(
      fit = mod_spline_prov,
      newdata = df_pred
      ) %>%
      tibble() %>% 
      mutate(
        id = paste0("dpt_",j,"_prov_",k),
        theta_dpt = j,
        theta_prov = k,
        mortality = exp(pred)
        ) %>% 
      select(theta_dpt, theta_prov, x1, dpt_cdc, prov_cdc, mortality)
    
    dt <- rbind(dt, pred)
  }
}

# dt_list <- dt %>% split(.$id)
# a <- dt_list %>% 
#   map(
#     . %>% 
#       left_join(obs) %>% 
#       mutate(
#         w_ijkl = deaths_imp / sum(obs$deaths_imp, na.rm = TRUE),
#         error = (mortality - obs_y1_imp)^2,
#         error_weighted = error * w_ijkl
#       )
#   )


write.csv(dt, "output/prediction_thetas.csv")



# Generate province level errors ----------------------------------------------------------------------------------

waves <- obs %>% 
  filter(!(deaths == 0 & x1 %in% 0:2), !is.na(deaths)) %>% 
  mutate(
    wave_k = ifelse(x1 < 43, 1, 2)
  ) %>% 
  group_by(dpt_cdc,prov_cdc, wave_k) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) 

waves <- waves %>% 
  mutate(
    w = ifelse(wave_k == 1, deaths / sum(waves$deaths[wave_k ==1]), deaths / sum(waves$deaths[wave_k ==2]))
    ) %>% 
  select(
          dpt_cdc_i = dpt_cdc,
          prov_cdc_j = prov_cdc,
          wave_k,
          w_ijk = w
        ) %>% as.data.table() 
  
  

obs <- obs %>%
  # filter(!(deaths == 0 & x1 %in% 0:2), !is.na(deaths)) %>%
  # mutate(
  #   wave_k = ifelse(x1 < 43, 1, 2)
  # ) %>%
  select(
    dpt_cdc_i = dpt_cdc,
    prov_cdc_j = prov_cdc,
    x1_l = x1,
    deaths,
    obs_y1_imp
  ) %>% 
  as.data.table()

dt <- as.data.table(dt)
setnames(
  dt,
  c("dpt_cdc", "prov_cdc","x1", "theta_dpt", "theta_prov", "mortality"),
  c("dpt_cdc_i", "prov_cdc_j","x1_l", "theta_dpt", "theta_prov", "mortality"),
)

dt_1 <- (
  dt
  [, id := paste0("dpt_", theta_dpt, "_prov_", theta_prov)]
  [, wave_k := ifelse(x1_l < 43, 1, 2)]
  [waves, on = c("dpt_cdc_i", "prov_cdc_j", "wave_k")]
  [obs, on = c("dpt_cdc_i", "prov_cdc_j", "x1_l")]
  [, e_ijkl := (mortality - obs_y1_imp)^2]
  [, we_ijkl := e_ijkl * w_ijk]
)


summary_model <- dt_1[, .(we = sum(we_ijkl), w =  sum(w_ijk)), by = id]
summary_wave  <- dt_1[, .(we = sum(we_ijkl), w =  sum(w_ijk)), by = c("id", "wave_k")]
summary_adm2  <- dt_1[, .(we = sum(we_ijkl), w =  sum(w_ijk)), by = c("id", "prov_cdc_j")]
summary_adm1  <- dt_1[, .(we = sum(we_ijkl), w =  sum(w_ijk)), by = c("id", "dpt_cdc_i")]
summary_wave_adm2  <- dt_1[, .(we = sum(we_ijkl), w =  sum(w_ijk)), by = c("id", "wave_k", "prov_cdc_j")]


best <- summary_wave_adm2 %>% filter(id == "dpt_12_prov_15")


plot(summary_wave$wave_k, summary_wave$we)

plot(best$wave_k, best$we)


