# Basic vonBertalanffy growth curve optimized via TMB
# ben.williams@alaska.gov
# 2019-10

# load ----
library(TMB)
library(dplyr)
library(ggplot2)
library(FNGr)
theme_set(theme_sleek())

setwd("von_b")

# data ----

# vonB parameter inputs
Linf <- 55
kappa <- 0.18
t0 <- -0.5

# create "true" data

set.seed(900)
data.frame(age = 1:50) %>% 
  mutate(length = Linf * (1 - exp(-kappa * (age - t0)))) -> true


true %>% 
  ggplot(aes(age, length)) + 
  geom_line()

# add variability to true data
true %>% 
  sample_n(1000, replace = TRUE) %>% 
  mutate(length = length + rnorm(1000, 0, 7)) -> sim 
  
sim %>% 
  ggplot(aes(age, length)) + 
  geom_point() + 
  geom_line(data = true)

# TMB model ----
compile("von_b.cpp")
dyn.load(dynlib("von_b"))

# starting parameters 
params <- list(Linf = 50, kappa = 0.12, t0 = 1.0, log_sigma = 0)

# build model
model <- MakeADFun(data = sim, 
                   parameters = params, 
                   DLL="von_b")

# optimize the model
fit <- nlminb(model$par, 
              model$fn, 
              model$gr)

best <- model$env$last.par.best
rep <- sdreport(model)

best
rep

Linf1 <- model$report()$Linf
kappa1 <- model$report()$kappa
t01 <- model$report()$t0

sim %>% 
  ggplot(aes(age, length)) + 
  geom_point(alpha = 0.3) + 
  geom_line(data = true) +
  geom_line(aes(y = Linf1 * (1 - exp(-kappa1 * (age - t01)))), color = 2)



