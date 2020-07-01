source("20190821/libs.r")
source("20190821/funs.r")

d1 <- load_pump_data(path = "../data/raw/peristaltic pumps/")

d2 <- remove_blanks(remove_blanks(d1)) #two timesteps worth

d3 <- d2 %>%
  filter(timepoint < 20, mvc < 100, mvc > 0) %>%
  mutate(t = timepoint*300) %>%
  mutate(run = str_sub(run,,6)) %>%
  
  filter(! run %in% c(180920, 180928, 181005, 190308, 190815, 181019, 190802, 191126))

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

lm(mvcresid ~ as.character(height), data = d3) %>% anova()

p2 <- d3 %>%
  ggplot(aes(x = factor(height), y = mvcresid)) +
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

sed1 %>% 
  group_by(station) %>% 
  summarise(sed = mean(sed)) %>% 
  ungroup() %>% 
  group_by((row_number()-1) %/% 3) %>% 
  summarise(mean(sed)) # position likely explains why ^!

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

write_csv(s4, "collector_efficiency_output.csv")

# TODO
# change plot names to dowel density
# table to calculate vars of interest with error propogation
### [x] velocity and dowel density from metadata
### [ ] sediment trap data
##### lm on position (ABC;DEF;GHI)
##### edit filenames to correspond to pump data

#poster plots

p3 <- d3 %>% 
  group_by(height) %>%  
  summarise(m = mean(mvcresid), 
            s = sd(mvcresid)/sqrt(n()),
            ct = n()) %>% 
  ggplot(aes(y = 40 - height, x = m, xmin = m-1.96*s, xmax = m+1.96*s)) +
  geom_point() + geom_errorbarh() + 
  theme_classic(base_size = 30) + 
  scale_y_reverse() + 
  geom_hline(yintercept = 0, color = "blue") + 
  geom_hline(yintercept = 40, color = "tan") + 
  geom_text(aes(x = -.25, y = 2, label = "Water surface"), color = "blue", size = 8, hjust = "left") + 
  geom_text(aes(x = -.25, y = 42, label = "Channel bottom"), color = "tan", size = 8, hjust = "left") + 
  geom_text(aes(y = 40 - height, x = m + 1.96*s +.2, label = paste0("n = ",ct)), size = 8) +
  labs(x = "Concentration residual [mg/L]", y = "Depth [cm]") +
  ggtitle("Depth profile of suspended \nsediment concentration") +
  scale_x_continuous(limits = c(-.25, 1.5))

ggsave(p3, filename = "p3.png", width = 8, height = 6, dpi = 300)



p1 <- d3 %>% 
  filter(run == 190926) %>% 
  ggplot(aes(x = t, group = t, y = mvc)) + 
  geom_point(size = 2) + 
  theme_classic(base_size = 30) + 
  stat_function(fun = function(t) exp(lms$`190926`$coefficients[1]+t*lms$`190926`$coefficients[2]), 
                size = 1, color = "red") +
  ggtitle("2019-09-26, Re = 152.8, \nlow collector-density, no biofilm") +
  labs(x = "Time [s]", y = "Concentration [mg/L]") +
  geom_text(aes(x = 3500, y = 50), label = expression(paste(R^2," = 0.89")),
            hjust = "left", size = 8) +
  geom_text(aes(x = 3500, y = 40), label = expression(paste(k[tot]," = 2.131e-4 ", s^-1)),
            hjust = "left", size = 8)

ggsave(p1, filename = "p1.png", width = 9, height = 6, dpi = 300)



p2 <- d3 %>%
  ggplot(aes(x = lmres)) +
  theme_classic(base_size = 30) +
  geom_histogram(color = "black", fill = "lightsteelblue") +
  geom_vline(xintercept = mean(d3$lmres), lty = 2, size = 1) +
  labs(x = "Residual [log(mg/L)]", y = "Frequency") +
  ggtitle("Distribution of residuals from\nlinear (log-transform) model")

ggsave(p2, filename = "p2.png", width = 9, height = 6, dpi = 300)

p4 <- s4 %>%
  filter(effective.collector.efficiency<1, Re.c>220) %>%
  ggplot(aes(x = growth_days, y = effective.collector.efficiency*100, 
             color = factor(I.c))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic(base_size = 30) +
  labs(y = "%ECE", x = "Biofilm growth days", color = "Collector\ndensity") +
  ggtitle("Effective capture efficiency\nresponse to biofilm growth") +
  theme(legend.background = element_rect(colour = "black"))
p4  
ggsave(p4, filename = "p4.png", width = 10, height = 8, dpi = 300)

p5 <- s4 %>%
  filter(effective.collector.efficiency<1, I.c<300, I.c > 250, growth_days==0) %>%
  ggplot(aes(x = Re.c, y = effective.collector.efficiency*100)) +
  geom_point(aes(color = 'red'), size = 2, show.legend = FALSE) +
  theme_classic(base_size = 30) +
  ggtitle("Effective capture efficiency\nat different Reynolds numbers") +
  labs(y = "%ECE", x = "Reynolds number") 
p5
