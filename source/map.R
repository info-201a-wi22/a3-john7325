library("maps")
library("tidyverse")

generate_map <- function(data) {
  data <- na.omit(data)
  data %>% 
    filter(year == max(year))
  data$state <- state.name[match(data$state, state.abb)]
  data$state <- tolower(data$state)
  
  states <- map_data("state") %>%
    mutate(subregion = NULL) %>%
    rename(state = region) %>%
    left_join(data, by = "state") %>%
    group_by(state) %>%
    summarize(black_jail_prop = sum(black_jail_pop)/sum(total_jail_pop),
              long = long,
              lat = lat)
  
  
  ggplot(states) + 
  geom_polygon(mapping = aes(x = long, 
                             y = lat, 
                             group = state,
                             fill = black_jail_prop), 
               color = "white", 
               size = .1) + 
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Red") + 
    labs(fill = "Black Jail Proportion",
         title = "Black Jail Proportion Across The United States", 
         x = "longitude", 
         y = "latitude")
    
    
}




