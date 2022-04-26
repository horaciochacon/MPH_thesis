library(dplyr)
source("R/functions.R")
library(purrr)
library(ggrepel)
library(EpiEstim)
library(incidence)
library(lubridate)


# Province level population
poblacion_prov <- read.csv("data/poblacion_provincial_peru.csv") %>% 
  group_by(prov_cdc = PROVINCIA) %>% 
  summarise(pob = sum(POBLACION)) 

# Read predicted mortality time series ------------------------------------
prov_preds <- read.csv("data/pred_prov_time_series.csv") %>% 
  tibble() %>% 
  left_join(poblacion_prov) %>% 
  mutate(
    days = lubridate::as_date(x1 * 7,),
    D = mortality * pob / 7,
    I = D / 0.007
    ) %>% 
  select(days, I, prov_cdc) %>% 
  split(.$prov_cdc)

d <- prov_preds[["LORETO"]] %>% 
  select(I, days) 

inc <- as.incidence(x = d$I, interval = 7, dates = d$days)

plot(inc)



# Estimating Rt -----------------------------------------------------------

res_parametric_si <- estimate_R(
  incid = d,
  method = "parametric_si",
  config = make_config(
    list(
      mean_si = 5.4,
      std_si = 0.11
    )
  )
)

plot(res_parametric_si, legend = FALSE)                                

g <- res_parametric_si$R %>% 
  ggplot(aes(x = t_start, y = `Median(R)`)) +
  geom_ribbon(
    aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)` ),
    fill = "lightgray"
    ) +
  geom_line() +
  ylim(c(0,2.5)) +
  geom_hline(yintercept = 1,lty = 2)

h <- d %>% 
  ggplot(aes(x = days, y = I)) +
  geom_line()

ggarrange(g,h,nrow = 2)

View(res_parametric_si$R)


# Estimating Rt -----------------------------------------------------------

rts <- prov_preds[1:2] %>% 
  purrr::map(
     . %>% 
      select(I, days) %>% 
      estimate_R(
         incid = .,
         method = "parametric_si",
         config = make_config(
           list(
             mean_si = 5.4,
             std_si = 0.11
           )
         )
       )
  )


for (i in 1:2) {
  g <- rts[[i]]$R %>% 
    ggplot(aes(x = t_start, y = `Median(R)`)) +
    geom_ribbon(
      aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)` ),
      fill = "lightgray"
    ) +
    geom_line() +
    geom_hline(yintercept = 1,lty = 2) +
    ylim(c(0,5))
  
  print(g)
}
