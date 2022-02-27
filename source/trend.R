# library("tidyverse")

jail_pop_data <- function(data) {
  temp <- data %>%
    filter(state == "WA") %>%
    filter(!is.na(black_jail_pop)) %>%
    filter(!is.na(aapi_jail_pop)) %>% 
    filter(county_name == "King County") %>%
    select(year, 
           black_jail_pop, 
           aapi_jail_pop)
  temp <- gather(temp, Race, population, -1)
  temp <- temp %>% mutate_at("Race", str_replace, "black_jail_pop", "Black")
  temp %>% mutate_at("Race", str_replace, "aapi_jail_pop", "Asian/Pacific Islander")
}


generate_plot_black_pop <- function(data) {
  ggplot(data, aes(x = year, 
                   y = population,
                   color = Race)) + 
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 2)) +
    scale_y_continuous(breaks = seq(0, 1200, by = 50)) + 
    theme(axis.text.x = element_text(angle = 90)) + 
    geom_point() + 
    geom_smooth(method = lm, se = FALSE, fullrange = TRUE) + 
    labs(title = "Black and Asian Jail Population of King County", 
         x = "Year",
         y = "Sub Jail Population")
}
