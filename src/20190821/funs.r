load_pump_data <- function(path = "../data/raw/peristaltic pumps/") {
  
  data_path <- path
  
  files <- list.files(data_path, "pump")
  
  raw_data <- lapply(paste0(data_path, files), read_csv)
  
  names(raw_data) <- str_extract(files, ".*(?=\\.)")
  
  # compile a table of variables of interest (tidy_data)
  
  tidy_data <- lapply(raw_data, function(X) select(X, 
                                                   timepoint = contains("series"),
                                                   mvc = contains("mass volume"),
                                                   loc = contains("Locat"),
                                                   height = contains("height")))
  
  tidy_data <- 
    Map(function(X,Y) mutate(X, run = Y), X = tidy_data, Y = names(tidy_data))
  
  tidy_data <- do.call(rbind, tidy_data)
  
  tidy_data$mvc <- as.numeric(tidy_data$mvc)
  
  tidy_data <- filter(tidy_data, !is.na(mvc))
  
  tidy_data
  
}

remove_blanks <- function(indata, threshold = 10) {
  
  blanks <- indata %>%
    filter(timepoint == 1) %>%
    group_by(run) %>%
    summarise(avg = mean(mvc)) %>%
    filter(avg < threshold)
  
  indata %>%
    mutate(timepoint = if_else(run %in% blanks$run, timepoint-1, as.numeric(timepoint))) %>%
    filter(timepoint > 0)
  
}

lmtable <- function(indata) {
  
  lms <- lmList(log(mvc)~t | run, data = indata)
  
  summary(lms)$coefficients[,,'t'][,1:2] %>%
    cbind(names(lms), .) %>%
    as.tibble() %>%
    transmute(date = str_sub(V1,,6), k_t = as.numeric(Estimate), dk_t = as.numeric(`Std. Error`))
  
}

r2table <- function(indata) {
  
  lms <- lmList(log(mvc)~t | run, data = indata)
  
  lapply(lms, function(x) summary(x)[8][[1]]) %>% 
    do.call(rbind, .) %>%
    cbind(names(lms), .) %>%
    as.tibble() %>%
    transmute(date = str_sub(V1,,6), r2 = V2)
  
}

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