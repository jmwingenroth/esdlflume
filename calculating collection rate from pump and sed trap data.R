# Solving for collection rate (k_c_d, i.e., k of (c)ollection with (d)owels)

m_0     <-  200 #inital mass of sediment in g
m_s_nd  <-  .00854*1.2/(pi*(.0127^2)) #mass in g of sediment settled in test section as calculated from sed. trap data; nd: no dowels
m_s_d   <-  .00680*1.2/(pi*(.0127^2)) #mass in g of sediment settled in test section as calculated from sed. trap data; d: yes dowels
k_total_nd  <-  1.6e-4 #total exponential decay rate, from peristaltic pump model; nd: no dowels (ROUGH ESTIMATE)
k_total_d   <-  2.2e-4 #total exponential decay rate, from peristaltic pump model; d: yes dowels (ROUGH ESTIMATE)
t_final <-  6000 #time for which sediment traps collected sediment in seconds (ROUGH ESTIMATE)

k_s <- function(m_s, k_total, m_0, t_final) { #function solved by integrating settling decay: 
                                              #settled mass = integral{0, t_final}(suspended mass * settling rate (i.e., k_s) * dt)  
  return(
    
    m_s/(m_0/k_total*(1 - exp(-k_total*t_final)))
    
  )
  
}

k_s_nd <- k_s(m_s_nd, k_total_nd, m_0, t_final)

k_f <- k_total_nd - k_s_nd

k_s_d <- k_s(m_s_d, k_total_d, m_0, t_final)

k_c_d <- k_total_d - k_s_d - k_f

# Solving for effective collector efficiency:

u <- .06 #water velocity in m/s (ROUGH ESTIMATE)
d_c <- .005 #collector diameter in m (ROUGH ESTIMATE)
h_c <- .4 #collector height in m (equal to water depth)
n_c <- 1750 #n of collectors
vol <- 1.2*.4 #volume of test section in m^3
I_c <- n_c*h_c/vol

nu_prime <- k_c_d/u/d_c/I_c