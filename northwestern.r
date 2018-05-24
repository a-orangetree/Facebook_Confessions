unique(us_data$university)

northwestern_data <- read_csv('data/us.csv') %>% filter(university == 'Northwestern Univeristy')
uchicago_data <- read_csv('data/us.csv') %>% filter(university == 'University of Chicago')
