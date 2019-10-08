source("20190821/libs.r")
source("20190821/funs.r")

d1 <- load_pump_data(path = "../data/raw/peristaltic pumps/")

d2 <- remove_blanks(remove_blanks(d1)) #two timesteps worth

d3 <- d2 %>%
  filter(timepoint < 20, mvc < 100, mvc > 0) %>%
  mutate(t = timepoint*300) %>%
  mutate(run = str_sub(run,,6)) %>%
  
  filter(! run %in% c(180920, 180928, 181005, 190308))

s1 <- lmtable(d3) %>%
  left_join(r2table(d3), by = "date") %>%
  mutate(r2 = as.numeric(r2))

p1 <- d3 %>%
  left_join(s1, by = c("run" = "date")) %>%
  ggplot(aes(x = t, y = log(mvc), color = loc, lty = as.character(height))) +
  geom_point(alpha = .2, size = 1) +
  facet_wrap(~run) +
  geom_line(stat = "smooth", method = "lm", se = FALSE, alpha = .8) +
  geom_text(x = 500, y = 1.1, color = 'black', size = 3, hjust = "left", 
            aes(label = paste0("R2 = ",round(r2, digits = 4)))) +
  geom_text(x = 500, y = 1.6, color = 'black', size = 3, hjust = "left", 
            aes(label = paste0("K_t = ",round(k_t, digits = 6))))

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
s1 <- left_join(meta, s1, by = "date")

sed1 <- load_trap_data() %>%
  mutate(run = str_sub(run,,6)) %>%
  filter(!is.na(sed))

lm(sed~station, data = sed1) %>%
  anova() #station matters!

s2 <- sed1 %>%
  group_by(date = run) %>%
  summarise(m_s = mean(sed), dm_s = sd(sed)) %>%
  right_join(s1, by = "date") %>%
  select(-run)

# TODO
# change plot names to dowel density
# table to calculate vars of interest with error propogation
### [x] velocity and dowel density from metadata
### [ ] sediment trap data
##### lm on position (ABC;DEF;GHI)
##### edit filenames to correspond to pump data

