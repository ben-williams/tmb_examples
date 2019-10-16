# load ----
library(TMB)
library(tidyverse)
library(FNGr)
theme_set(theme_sleek())

setwd(here::here("dub_logistic"))

# data ----

# note if the lengths are not rounded there are occasionaly more precise estimates
# these will not allow the model to converge

read_csv("br_bio.csv") %>% 
  rename_all(tolower) %>% 
  dplyr::select(year, Area = g_management_area_code, 
                length = length_millimeters, age) %>% 
  filter(Area=="CSEO") %>% 
  mutate(length = round(length / 10)) -> brf



# functions ----

range01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

clean_up <- function(data, variable){
  
  data %>% 
    dplyr::select(X = {{variable}}) %>% 
    group_by(X) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(prop = n / sum(n),
           prop = range01(prop)) %>% 
    dplyr::select(-n) %>% 
    drop_na()
  
}

# TMB model ----

compile("dub_logistic.cpp")
dyn.load(dynlib("dub_logistic"))

# starting parameters 
params <- list(inf = 9, inf2 = 15, 
               slope = 0.7, slope2 = 0.3, 
               max_sel = .2, min_sel = 0.05, 
               log_sigma = 0.01)
dat <- clean_up(brf, age)

# check that the data are in a good form for estimating

dat %>% 
  ggplot(aes(X, prop)) + 
  geom_point()

# add parameter bounds 
map = list()
L = list(inf = 1, inf2 = 10, 
         slope = 0.1, slope2 = 0.001, 
         max_sel = 0.2, min_sel = 0.001, 
         sigma = 0.0001)
U = list(inf = 14.9, inf2 = 20, 
         slope = 1, slope2 = 1, 
         max_sel = 1, min_sel = 1, 
         sigma = 10)

# build model
model <- MakeADFun(data = dat, 
                   parameters = params, 
                   DLL="dub_logistic",
                   map = map)

# optimize the model
fit <- nlminb(model$par, 
              model$fn, 
              model$gr,
              lower = L,
              upper = U)

best <- model$env$last.par.best
rep <- sdreport(model)

best
rep

inf <- model$report()$inf
inf2 <- model$report()$inf2
slope <- model$report()$slope
slope2 <- model$report()$slope2
max_sel <- model$report()$max_sel
min_sel <- model$report()$min_sel

dat %>% 
  ggplot(aes(X, prop)) + 
  geom_point() +
  geom_line(aes(y = (max_sel / (1 + exp(-slope * (X - inf)))) *
                  (1 - min_sel / (1 + exp(-slope2 * (X - inf2))))), color = 2)




