lms <- lmList(log(mvc)~t | run, data = tidy_data)

lapply(lms, coef) %>% 
  do.call(rbind, .) %>%
  cbind(names(lms), .) %>%
  as.tibble() %>%
  mutate_at(vars(`(Intercept)`, t), as.numeric) %>%
  mutate(starting_conc = exp(`(Intercept)`)) %>%
  transmute(date = str_sub(V1,,4), k_t = t, starting_conc)
