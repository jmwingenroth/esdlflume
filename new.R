library(dplyr)

mutate_with_error = function(.data, f) {
  exprs = list(
    # expression to compute new variable values
    deparse(f[[3]]),
    
    # expression to compute new variable errors
    sapply(all.vars(f[[3]]), function(v) {
      dfdp = deparse(D(f[[3]], v))
      sprintf('(d%s*(%s))^2', v, dfdp)
    }) %>%
      paste(collapse='+') %>%
      sprintf('sqrt(%s)', .)
  )
  names(exprs) = c(
    deparse(f[[2]]),
    sprintf('d%s', deparse(f[[2]]))
  )
  
  .data %>%
    # the standard evaluation alternative of mutate()
    mutate_(.dots=exprs)
}

variables = function(model,modelname,filename) {
  summ = summary(model)
  coefstd = summ$coefficients[[4]]
  labels=names(k_t)
  index=which(labels==modelname)
  kt=-k_t[[index]]
  dkt=1.96*coefstd
  fullfile=paste0("../data/raw/sed trap data/",filename)
  seddata = read_csv(fullfile)
  sedmass=seddata$`sediment mass (mg)`
  sedmass=as.numeric(sedmass)
  sedmass=sedmass[!is.na(sedmass)]
  sedmass=sedmass[!sedmass<2]*(10^-3)
  avg=mean(sedmass)
  std=sd(sedmass)
  if (grepl("high",modelname)==TRUE) {
    dowdens= 1450
    ddowdens= 50
  } else if (grepl("mid",modelname)==TRUE) {
    dowdens=960
    ddowdens=5
  } else if (grepl("low",modelname)==TRUE) {
    dowdens=278
    ddowdens=5
  } else if (grepl("control",modelname)==TRUE) {
    dowdens=0
    ddowdens=0
  }
  return(list(modelname,avg,std,dowdens,ddowdens,kt,dkt))
}
   
  
#get input variables
inpvars = variables(modellow3,"modellow3 (10 Hz)","0808sedtraps.csv")
names(inpvars) = c("X1","mass_avg","mass_sd","dow_dens","ddow_dens","kt","dkt")
  
# Solving for collection rate
tibble(
  
  m.0        =  200, #inital mass of sediment in g
  dm.0       =  1,  # d: error; lab precision estimate
  
  m.s.nd     =  12.22e-3, #mass in g of sediment settled in trap as calculated from sed. trap data; no dowels
  dm.s.nd    =  6.06e-3,  #1.96*sd of samples
  
  m.s.d      =  inpvars[[2]], #mass in g of sediment settled in test section as calculated from sed. trap data; yes dowels
  dm.s.d     =  inpvars[[3]], #1.96*sd of samples
  
  k.t.nd     =  1.62e-4, #total exponential decay rate, from peristaltic pump model; nd: no dowels 
  dk.t.nd    =  5.9e-6,  #1.96*linear model std error
  
  k.t.d      =  inpvars[[6]], #total exponential decay rate, from peristaltic pump model; d: yes dowels 
  dk.t.d     =  inpvars[[7]],   #1.96*linear model std error
  
  T          =  6000, #time for which sediment traps collected sediment in seconds
  dT         =  180,  #estimated from experiments conducted
  
  A.sec      =  1.2*.6, #area of test section
  dA.sec     =  .01,    #lab precision estimate
  
  a.trap     =  pi*.0127^2, #area of a sed trap
  da.trap    =  pi*.001^2,  #lab precision estimate
  
  u          =  0.066, #flow velocity, m/s
  du         =  0.003, #1.96*sd
  
  d.c        =  .003175, #collector diameter, m
  dd.c       =  .02*d.c, #conservative estimate (1/8 inch known from dowel specs, confirmed by calipers)
  
  I.c        =  inpvars[[4]], # dowels per m^2 (equal to h.c*N.c/V)
  dI.c       =  inpvars[[5]]    # estimated from experiments conducted
  
) %>%
  
  mutate_with_error(c.nd ~ 1-exp(-k.t.nd*T)) %>%
  
  mutate_with_error(k.s.nd ~ m.s.nd*k.t.nd*A.sec/a.trap/m.0/c.nd) %>%
  
  mutate_with_error(c.d ~ 1 - exp(-k.t.d*T)) %>%
  
  mutate_with_error(k.s.d ~ m.s.d*k.t.d*A.sec/a.trap/m.0/c.d) %>%
  
  mutate_with_error(k.bg ~ k.t.nd - k.s.nd) %>%
  
  mutate_with_error(k.c ~ k.t.d - k.s.d - k.bg) %>%
  
  mutate_with_error(effective.collector.efficiency ~ k.c/u/d.c/I.c) -> output

output <- t(output)

alldata = c(inpvars,output[29:36,0:1])

