data = function(model)
  source("dowelcomparison7-11.R")
  summ = summary(model)
  coefstd = summ$coefficients[[4]]
  val=c(10.78,11.63,10.97,6.44,6.57,4.19)*(10^-3)
  m.s.d=mean(val)
  dm.s.d=sd(val)*1.96
  k.t.d=k_t[[9]]
  dk.t.d=1.96*coefstd
  k.t.nd     =  1.62e-4
  dk.t.nd    =  5.9e-6
  m.s.nd     =  12.22e-3
  dm.s.nd    =  6.06e-3
  source("calculating collection rate from pump and sed trap data.R")
  k_s_d = output[25]
  dk_s_d = output[26]
  k_bg = output[27]
  dk_bg = output[28]
  k_c = output[29]
  dk_c = output[30]
  effective_collector_efficiency = output[31]
  deffective_collector_efficiency = output[32]
  tbl= data.frame(k_t)
  tbl$k_s_d=k_s_d
  tbl$dk_s_d=dk_s_d
  tbl$k_bg=k_bg
  tbl$dk_bg= dk_bg
  tbl$k_c= k_c
  tbl$dk_c= dk_c
  tbl$effective_collector_efficiency =effective_collector_efficiency 
  tbl$deffective_collector_efficiency = deffective_collector_efficiency
  return(tbl)
  
  