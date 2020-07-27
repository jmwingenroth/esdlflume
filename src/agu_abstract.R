#So, here's the script to make estimates for the AGU abstract. This will probably evolve into the script where we produce figures for the paper, meaning that along with the data it references, it will be the only code that needs to go in the paper repo. However, other code that generates important info but not figures (nothing coming to mind in particular) might be good to include also if we end up making the repo publicly available.

library(tidyverse)

library(R.matlab)



## flow velocity profiles

vecpaths <- list.files("data/TurbulenceDataCleaned/", recursive = T, pattern = ".mat")

vecdata <- lapply(paste0("data/TurbulenceDataCleaned/",vecpaths), readMat)

names(vecdata) <- vecpaths

vec_v <- lapply(vecdata, function(X) {X$v})

vec_t <- lapply(vecdata, function(X) {X$t})

vec_tv <- list()

for (i in 1:length(vec_v)) {
  
  vec_tv[[i]] <- list()
  
  for (j in 1:length(vec_v[[i]])) {
    
    vec_tv[[i]][[j]] <- cbind(names(vec_t)[[i]], j, vec_t[[i]][[j]][[1]], vec_v[[i]][[j]][[1]])
    colnames(vec_tv[[i]][[j]]) <- c("run", "ht", "t", "x","y","z1","z2")
  }
}

vec_tv2 <- lapply(vec_tv, function(X) {rbind(X[[1]],X[[2]],X[[3]])})

vec_tv_table <- as.tibble(do.call(rbind, vec_tv2)) %>%
  mutate(run = str_extract(run, "/.*")) %>%
  mutate_at(vars(t,x,y,z1,z2), as.numeric)

vec_tv_table %>%
  mutate(density = str_sub(run, 2, 4),
         freq = str_sub(run,8,9),
         )
  
  
  # filter(str_detect(run, "Contr")) %>%
  # group_by(run) %>%
  # mutate(t_rel = t - min(t), 
  #        bottomdist = str_extract(run, "Hz.*"),
  #        freq = str_extract(run, ".*Hz")) %>%
  # ggplot(aes(x = t_rel, y = x)) +
  # geom_point(alpha = .1) +
  # facet_grid(rows = vars(bottomdist), cols = vars(freq))
