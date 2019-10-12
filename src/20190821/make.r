source("20190821/libs.r")
source("20190821/funs.r")

d1 <- load_pump_data(path = "../data/raw/peristaltic pumps/")

d2 <- remove_blanks(remove_blanks(d1)) #two timesteps worth

d3 <- d2 %>%
  filter(timepoint < 20, mvc < 100, mvc > 0) %>%
  mutate(t = timepoint*300) %>%
  mutate(run = str_sub(run,,6)) %>%
  
  filter(! run %in% c(180920, 180928, 181005, 190308, 190815, 181019, 190802))

s1 <- lmtable(d3) %>%
  left_join(r2table(d3), by = "date") %>%
  mutate(r2 = as.numeric(r2))

p1 <- d3 %>%
  left_join(s1, by = c("run" = "date")) %>%
  ggplot(aes(x = t, y = log(mvc), color = loc, lty = as.character(height))) +
  geom_point(alpha = .2, size = 1) +
  facet_wrap(~run) +
  geom_line(stat = "smooth", method = "lm", se = FALSE, alpha = .8) +
  geom_text(x = 500, y = 1.1, color = 'black', hjust = "left", 
            aes(label = paste0("R2 = ",round(r2, digits = 4))), alpha = .05, size = 3) +
  geom_text(x = 500, y = 1.6, color = 'black', hjust = "left", 
            aes(label = paste0("K_t = ",round(k_t, digits = 6))), alpha = .05, size = 3)

#height and us/ds effects

lms <- lmList(log(mvc)~t | run, data = d3)

d3$lmres <- resid(lms)
d3$lmfit <- fitted(lms)

d3 <- d3 %>%
  mutate(mvcresid = mvc - exp(lmfit)) %>%
  group_by(run) %>%
  mutate(scaledresid = mvcresid/sd(mvcresid)) %>%
  ungroup()
attr(d3$mvcresid, "label") <- "Residuals"

lm(scaledresid ~ loc + as.character(height), data = d3) %>% summary()

p2 <- d3 %>%
  ggplot(aes(x = factor(height), y = scaledresid)) +
  geom_violin(draw_quantiles = .5)

p3 <- d3 %>%
  ggplot(aes(x = loc, y = scaledresid)) +
  geom_violin(draw_quantiles = .5)

meta <- read_csv("../data/run_metadata.csv")
meta$date <- as.character(meta$date)
s1 <- left_join(s1, meta, by = "date")

sed1 <- load_trap_data() %>%
  mutate(run = str_sub(run,,6)) %>%
  filter(!is.na(sed))

lm(sed~station, data = sed1) %>%
  anova() #station matters!

s2 <- sed1 %>%
  group_by(date = run) %>%
  summarise(m_s = mean(sed)/1000, dm_s = 1.96*sd(sed)/1000) %>%
  right_join(s1, by = "date") %>%
  select(-run)

s3 <- s2 %>%
  mutate(
    dk_t = 1.96*dk_t,
    m.0 = 200,
    dm.0 = 1,
    tm = 6000,
    dtm= 180,
    A.sec = 1.2*0.6,
    dA.sec = 0.01,
    a.trap =  pi*.0127^2, #area of a sed trap
    da.trap =  pi*.001^2,  #lab precision estimate
    d.c =  .003175,
    dd.c  =  .02*0.003175,
    k_t.nd = -0.0002025636,
    dk_t.nd = 8.075173e-06,
    m_s.nd = if_else(u>0.06,.012224444444,
                     if_else(u>0.04, .01422,
                             .016228888888)),
    dm_s.nd = if_else(u>0.06,.06061877,
                      if_else(u>0.04,.075,
                              .091638)),
    I.c = dowel_density,
    dI.c = ddowel_density,
    Re.c = u*d.c/9.144e-7
    
  ) %>%
  arrange(u, dowel_density) %>%
  mutate_with_error(c.nd ~ 1 - exp(k_t.nd*tm)) %>%
  mutate_with_error(c.d ~ 1 - exp(k_t*tm)) %>%
  mutate_with_error(k.s.nd ~ m_s.nd*k_t.nd*A.sec/a.trap/m.0/c.nd) %>%
  mutate_with_error(k.s.d ~ m_s*k_t*A.sec/a.trap/m.0/c.d) %>%
  mutate_with_error(k.bg ~ k_t.nd - k.s.nd) %>%
  mutate_with_error(k.c ~ k_t - k.s.d - k.bg) %>%
  mutate_with_error(effective.collector.efficiency ~ -k.c/u/d.c/I.c)
  
s4 <- s3 %>%
  select(date, I.c, Re.c, growth_days, effective.collector.efficiency, CI_error_95 = deffective.collector.efficiency)
  
s4 %>%
  filter(effective.collector.efficiency<1, growth_days==0) %>%
  ggplot(aes(x = Re.c, y = effective.collector.efficiency, color = factor(I.c))) +
  geom_point()

write_csv(s4, "collector_efficiency_output.csv")

# TODO
# change plot names to dowel density
# table to calculate vars of interest with error propogation
### [x] velocity and dowel density from metadata
### [ ] sediment trap data
##### lm on position (ABC;DEF;GHI)
##### edit filenames to correspond to pump data

