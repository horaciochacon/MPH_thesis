library(purrr)

dt <- tibble()

# Create prediction dataframe to fill predicted values
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






