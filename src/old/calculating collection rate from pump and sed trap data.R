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

# Solving for collection rate
tibble(
  
m.0        =  200, #inital mass of sediment in g
dm.0       =  1,  # d: error; lab precision estimate

#m.s.nd     =  12.22e-3, #mass in g of sediment settled in trap as calculated from sed. trap data; no dowels
#dm.s.nd    =  6.06e-3,  #1.96*sd of samples

m.s.d      =  6.80e-3, #mass in g of sediment settled in test section as calculated from sed. trap data; yes dowels
dm.s.d     =  3.80e-3, #1.96*sd of samples

#k.t.nd     =  1.62e-4, #total exponential decay rate, from peristaltic pump model; nd: no dowels 
#dk.t.nd    =  5.9e-6,  #1.96*linear model std error

k.t.d      =  2.229e-4, #total exponential decay rate, from peristaltic pump model; d: yes dowels 
dk.t.d     =  9.1e-6,   #1.96*linear model std error

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

I.c        =  1450, # dowels per m^2 (equal to h.c*N.c/V)
dI.c       =  50    # estimated from experiments conducted

) %>%

mutate_with_error(c.nd ~ 1-exp(-k.t.nd*T)) %>%

mutate_with_error(k.s.nd ~ m.s.nd*k.t.nd*A.sec/a.trap/m.0/c.nd) %>%
  
mutate_with_error(c.d ~ 1 - exp(-k.t.d*T)) %>%

mutate_with_error(k.s.d ~ m.s.d*k.t.d*A.sec/a.trap/m.0/c.d) %>%
  
mutate_with_error(k.bg ~ k.t.nd - k.s.nd) %>%
  
mutate_with_error(k.c ~ k.t.d - k.s.d - k.bg) %>%
  
mutate_with_error(effective.collector.efficiency ~ k.c/u/d.c/I.c) -> output

output <- t(output)
