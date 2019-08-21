data_path <- "../data/raw/"

files <- raw_data <- list.files(data_path, "pump")

raw_data <- lapply(paste0(data_path, files), read_csv)

names(raw_data) <- str_extract(files, ".*(?=\\.)")

tidy_data <- lapply(raw_data, function(X) select(X, 
                                    timepoint = contains("series"),
                                    mvc = contains("mass volume"),
                                    loc = contains("Locat"),
                                    height = contains("height")))

tidy_data <- 
  Map(function(X,Y) mutate(X, run = Y), X = tidy_data, Y = names(tidy_data))

tidy_data <- do.call(rbind, tidy_data)

tidy_data <- tidy_data %>%
  mutate(mvc = as.numeric(mvc)) %>%
  filter(!(timepoint == 1 & mvc < 3))

tidy_data %>%
  ggplot(aes(x = timepoint, y = mvc, color = run)) +
  geom_smooth()
