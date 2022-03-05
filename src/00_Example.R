library(reticulate)
use_condaenv("mrtool-0.0.1")
library(mrtoolr)


# Prepare simulated data --------------------------------------------------

set.seed(1)
k_studies <- 10 # Number of studies
n_per_study <- 5 # Number of individuals? per study
# k_studies <- 40
# n_per_study <- 40
tau_1 <- 4 # Between study heterogeneity, variation in true effects, also gamma
sigma_1 <- 1 # uncertainty in the dependent variable (measurement error)
tau_2 <- 0.6
sigma_2 <- 0.2

df_sim_study <- data.frame(study_id = as.factor(1:k_studies)) %>%
  mutate(
    study_effect1 = rnorm(n = k_studies, mean = 0, sd = tau_1), # Uj
    study_effect2 = rnorm(n = k_studies, mean = 0, sd = tau_2)
    # study_colors = brewer.pal(n = k_studies, "Spectral")
  )

df_sim1 <- do.call("rbind", lapply(1:nrow(df_sim_study), function(i) {
  df_sim_study[rep(i, n_per_study), ] })) %>%
  mutate(
    x1 = runif(n = nrow(.), min = 0, max = 10),
    y1 = 0.9*x1 + study_effect1 + rnorm(nrow(.), mean = 0, sd = sigma_1),
    y1_se = sigma_1,
    y2_true = 2 * sin(0.43*x1-2.9),
    y2 = y2_true - min(y2_true) + study_effect2 + 
      rnorm(nrow(.), mean = 0, sd = sigma_2),
    y2_se = sigma_2,
    is_outlier = FALSE) %>%
  arrange(x1)

df_sim2 <- df_sim1 %>%
  rbind(., .[(nrow(.)-7):(nrow(.)-4), ] %>% 
          mutate(y1=y1-18, y2=y2-4, is_outlier = TRUE))

# function for plotting uncertainty intervals
add_ui <- function(
  dat, x_var, lo_var, hi_var, color = "darkblue", opacity = 0.2
  ) {
  polygon(
    x = c(dat[, x_var], rev(dat[, x_var])),
    y = c(dat[, lo_var], rev(dat[, hi_var])),
    col = adjustcolor(col = color, alpha.f = opacity), border = FALSE
  )
}

# Fitting a standard mixed effects model ----------------------------------
dat1 <- MRData()
dat1$load_df(
  data = df_sim1,  col_obs = "y1", col_obs_se = "y1_se",
  col_covs = list("x1"), col_study_id = "study_id" )

mod1 <- MRBRT(
  data = dat1,
  cov_models = list(
    LinearCovModel("intercept", use_re = TRUE),
    LinearCovModel("x1"),
    ) )

mod1$fit_model(inner_print_level = 5L, inner_max_iter = 1000L)

# df_pred1 <- data.frame(x1 = seq(0, 10, by = 2), study_id = "4")
df_pred1 <- data.frame(x1 = seq(0, 10, by = 0.1))

dat_pred1 <- MRData()

dat_pred1$load_df(
  data = df_pred1, 
  col_covs=list('x1')
)

# Point prediction --------------------------------------------------------
pred1 <- mod1$predict(data = dat_pred1)
df_pred1$pred1 <- pred1
with(df_sim1, plot(x1, y1))
with(df_pred1, lines(x1, pred1))


# Removing effects of outliers with trimming ------------------------------

dat2 <- MRData()
dat2$load_df(
  data = df_sim2,  col_obs = "y1", col_obs_se = "y1_se",
  col_covs = list("x1"), col_study_id = "study_id" )

df_pred2 <- data.frame(x1 = seq(0, 10, by = 2))
dat_pred2 <- MRData()

dat_pred2$load_df(
  data = df_pred2,
  col_covs=list('x1')
)

mod2 <- MRBRT(
  data = dat2,
  cov_models = list(
    LinearCovModel("intercept", use_re = TRUE),
    LinearCovModel("x1") ),
  inlier_pct = 0.8)

mod2$fit_model(inner_print_level = 5L, inner_max_iter = 1000L)
pred4 <- mod2$predict(data = dat_pred2)
df_pred2$pred4 <- pred4

# get a data frame with estimated weights
df_mod2 <- cbind(mod2$data$to_df(), data.frame(w = mod2$w_soln))

with(df_mod2, plot(
  x = x1, y = obs,  
  col = ifelse(w == 1, "black", "red"),  
  pch = ifelse(w == 1, 1, 16)
))
with(df_pred2, lines(x1, pred4))


# Splines -----------------------------------------------------------------

dat3 <- MRData()
dat3$load_df(
  data = df_sim1, col_obs = "y2", col_obs_se = "y2_se",
  col_covs = list("x1"), col_study_id = "study_id" )

mod5 <- MRBRT(
  data = dat3,
  cov_models = list(
    LinearCovModel("intercept", use_re = TRUE),
    LinearCovModel(
      alt_cov = "x1",
      use_spline = TRUE,
      # spline_knots = array(c(0, 0.25, 0.5, 0.75, 1)),
      spline_knots = array(seq(0, 1, by = 0.2)),
      spline_degree = 3L,
      spline_knots_type = 'frequency',
      spline_r_linear = TRUE,
      spline_l_linear = FALSE
      # prior_spline_monotonicity = 'increasing'
      # prior_spline_convexity = "convex"
      # prior_spline_maxder_gaussian = array(c(0, 0.01))
      # prior_spline_maxder_gaussian = rbind(c(0,0,0,0,-1), c(Inf,Inf,Inf,Inf,0.0001))
    )
  )
)

mod5$fit_model(inner_print_level = 5L, inner_max_iter = 1000L)

# df_pred1 <- data.frame(x1 = seq(0, 10, by = 2), study_id = "4")
df_pred3 <- data.frame(x1 = seq(0, 10, by = 0.1))

dat_pred3 <- MRData()

dat_pred3$load_df(
  data = df_pred3, 
  col_covs=list('x1')
)

pred5 <- mod5$predict(data = dat_pred3)

df_pred3$pred5 <- pred5
with(df_sim1, plot(x1, y2))
with(df_pred3, lines(x1, pred5))
