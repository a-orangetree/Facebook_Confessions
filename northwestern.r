unique(us_data$university)

northwestern_data <- us_data %>% filter(university == 'Northwestern Univeristy')
uchicago_data <- us_data %>% filter(university == 'University of Chicago')
