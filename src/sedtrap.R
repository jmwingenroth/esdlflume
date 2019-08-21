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

sedtrap_var = function(model,modelname,filename) {
  input_var = data.frame()
  for (i in 1:length(model)) {
    summ = summary(model[[i]])
    coefstd = summ$coefficients[[4]]
    labels=names(k_t)
    index=which(labels==modelname[[i]])
    kt=-k_t[[index]]
    dkt=1.96*coefstd
    fullfile=paste0("../data/raw/sed trap data/",filename[[i]])
    seddata = read_csv(fullfile)
    sedmass=seddata$`sediment mass (mg)`
    sedmass=as.numeric(sedmass)
    sedmass=sedmass[!is.na(sedmass)]
    sedmass=sedmass[!sedmass<3]*(10^-3)
    avg=mean(sedmass)
    std=sd(sedmass)
    if (grepl("high",modelname[[i]])==TRUE) {
      dowdens= 1450
      ddowdens= 50
    } else if (grepl("mid",modelname[[i]])==TRUE) {
      dowdens=800
      ddowdens=5
    } else if (grepl("low",modelname[[i]])==TRUE) {
      dowdens=278
      ddowdens=5
    } else if (grepl("control",modelname[[i]])==TRUE) {
      dowdens=0
      ddowdens=0
    }
    if (grepl("10Hz",modelname[[i]])==TRUE) {
      k.t.nd = 0.000212
      dk.t.nd = 1.39e-5
      u = 0.022 #flow velocity, m/s
      du = 0.001 #1.96*sd
      m.s.nd = 17.3475e-3
      dm.s.nd = 6.9e-3
    } else if (grepl("20Hz",modelname[[i]])==TRUE) {
      k.t.nd = 0.000229
      dk.t.nd = 1.483e-5
      u = 0.044 #flow velocity, m/s
      du = 0.002 #1.96*sd
      m.s.nd = 9.946e-3
      dm.s.nd = 6.77e-3
    } else {
      k.t.nd = 0.0002026
      dk.t.nd = 8.075e-6
      u = 0.066 #flow velocity, m/s
      du = 0.003 #1.96*sd
      m.s.nd = 12.22e-3
      dm.s.nd = 6.06e-3
    }
    input_var[i,1:13] = data.frame(modelname[[i]],avg,std,dowdens,ddowdens, m.s.nd, dm.s.nd, u, du, k.t.nd, dk.t.nd, kt, dkt)
  }
  output_var = data.frame()
  for (j in 1:nrow(input_var)) {
    variables = tibble(
      run.name = input_var[[j,1]],
      m.0 = 200,
      dm.0 = 1,
      m.s.nd = input_var[[j,6]],
      dm.s.nd = input_var[[j,7]],
      m.s.d = input_var[[j,2]],
      dm.s.d = input_var[[j,3]],
      k.t.nd = input_var[[j,10]],
      dk.t.nd = input_var[[j,11]],
      k.t.d = input_var[[j,12]],
      dk.t.d = input_var[[j,13]],
      tm = 6000,
      dtm= 180,
      A.sec = 1.2*0.6,
      dA.sec = 0.01,
      a.trap =  pi*.0127^2, #area of a sed trap
      da.trap =  pi*.001^2,  #lab precision estimate
      u = input_var[[j,8]],
      du = input_var[[j,9]],
      d.c =  .003175,
      dd.c  =  .02*0.003175,
      I.c = input_var[[j,4]],
      dI.c = input_var[[j,5]]
    ) %>%
      mutate_with_error(c.nd ~ 1 - exp(-k.t.nd*tm)) %>%
      
      mutate_with_error(k.s.nd ~ m.s.nd*k.t.nd*A.sec/a.trap/m.0/c.nd) %>%
      
      mutate_with_error(c.d ~ 1 - exp(-k.t.d*tm)) %>%
      
      mutate_with_error(k.s.d ~ m.s.d*k.t.d*A.sec/a.trap/m.0/c.d) %>%
      
      mutate_with_error(k.bg ~ k.t.nd - k.s.nd) %>%
      
      mutate_with_error(k.c ~ k.t.d - k.s.d - k.bg) %>%
      
      mutate_with_error(effective.collector.efficiency ~ k.c/u/d.c/I.c) -> output
    output_var[j,1:37] = output
  } 
  return(output_var)
}

