# load raw_data

data_path <- "../data/raw/"

files <- raw_data <- list.files(data_path, "pump")

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

# remove blanks

tidy_data <- tidy_data %>%
  mutate(mvc = as.numeric(mvc)) %>%
  filter(!(timepoint == 1 & mvc < 3), !is.na(mvc), timepoint<22)

tidy_data <- tidy_data %>%
  mutate(t = if_else(run %in% c('0612pumpdata',
                                '0802pumpdata',
                                '1019pumpdata'),
                     300*(timepoint-1), 300*timepoint))
