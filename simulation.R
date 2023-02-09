library(lme4)
library(tidyverse)
library(furrr)


simulation <- function(m = 1000, sd_t_c = 1, sd_c = 2, sd_e = 3) { 

  df <- crossing(tempo = c("A", "B", "C", "D"), 
                 cluster = c("c1", "c2"), 
                 id = 1:m) %>% 
    left_join(tibble(cluster = c("c1", "c2"), 
                     alpha = rnorm(2, 0, sd_c)), by = "cluster") %>% 
    left_join(crossing(tempo = c("A", "B", "C", "D"), 
                       cluster = c("c1", "c2")) %>% 
                mutate(gamma = rnorm(nrow(.), 0, sd_t_c)), by = c("tempo", "cluster")) %>% 
    mutate(y = 5 + alpha + gamma + rnorm(nrow(.), 0, sd_e))

  
  fit <- lmer(y ~ (1|cluster/tempo), df)
  
  as.data.frame(VarCorr(fit)) %>% 
    select(grp, sdcor)
  
}


m <- 1000   # sample size per period*cluster
sd_t_c <- 1 # sd of random effect for period*cluster
sd_c   <- 2 # sd of random effect for cluster
sd_e   <- 3 # sd of error
B <- 1000    # B datasets and fitted models


results <- tibble(b = 1:B) %>% 
  mutate(table = future_map(b, ~simulation(m, dp_t_c, dp_c, dp_e), .options = furrr_options(seed = 123))) %>% 
  unnest(table) 
  

results %>% 
  ggplot(aes(sdcor)) + 
    geom_histogram(color = "black", fill = "grey") +
    facet_wrap(~ grp, ncol = 1) +
    geom_vline(data = filter(results, grp == "tempo:cluster"), aes(xintercept = sd_t_c), colour = "red") + 
    geom_vline(data = filter(results, grp == "cluster"), aes(xintercept = sd_c), colour = "red") + 
    geom_vline(data = filter(results, grp == "Residual"), aes(xintercept = sd_e), colour = "red") + 
    theme_bw()
