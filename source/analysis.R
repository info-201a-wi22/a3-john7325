library("tidyverse")

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

washington_data <- data %>%
  filter(state == "WA", 
         year == max(year))


# Calculate the average proportion of the population being Asian American/
# Pacific Islander across 39 Washington state counties
average_aapi_prop <- function(data) {
  temp <- data %>% 
    summarize(asian_prop = aapi_pop_15to64 / total_pop_15to64, 
              year = year)
  temp$asian_prop %>%
    mean() %>%
    round(4)
}

# Calculate the average proportion of the population being Black across 39 
# Washington state counties
average_black_prop <- function(data) {
  temp <- data %>%
    summarize(black_prop = black_pop_15to64 / total_pop_15to64,
              year = year)
  temp$black_prop %>% 
    mean() %>%
    round(4)
}

# Calculate average proportion of jail population being Asian American/Pacific
# Islander across 39 counties of Washington State
average_aapi_jail_prop <- function(data) {
  temp <- data %>% 
    summarize(asian_jail_prop = aapi_jail_pop / total_jail_pop,
              year = year)
  temp$asian_jail_prop %>%
    mean() %>%
    round(4)
}

# Calculate average proportion of jail population being Black across 39 counties
# of Washington State
average_black_jail_prop <- function(data) {
  temp <- data %>%
    summarize(black_jail_prop = black_jail_pop / total_jail_pop,
              year = year) 
  temp$black_jail_prop %>%
    mean() %>%
    round(4)
}

# Calculate the max proportion of jail population being Black in Washington
highest_black_jail_prop <- function(data) {
  data %>%
    summarize(black_jail_prop = black_jail_pop / total_jail_pop,
              county = county_name) %>%
    filter(black_jail_prop == max(black_jail_prop)) %>%
    pull(black_jail_prop) %>% 
    round(4)
}

# Calculate the max proportion of jail population being Asian American/Pacific
# Islander in Washington
highest_aapi_jail_prop <- function(data) {
  data %>%
    summarize(aapi_jail_prop = aapi_jail_pop / total_jail_pop,
              county = county_name) %>%
    filter(aapi_jail_prop == max(aapi_jail_prop)) %>%
    pull(aapi_jail_prop) %>% 
    round(4)
}

