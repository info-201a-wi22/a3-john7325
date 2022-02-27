library("tidyverse")

comparison <- function(data) {
  data %>%
    filter(state == "WA") %>%
    filter(!is.na(black_jail_pop),
           !is.na(aapi_jail_pop),
           !is.na(aapi_pop_15to64), 
           !is.na(black_pop_15to64)) %>%
    group_by(year) %>%
    summarize(Black_Jail_Proportion = sum(black_jail_pop)/sum(black_pop_15to64), 
              Black_Population = sum(black_pop_15to64)) 
}

generate_comparison_plot <- function(data) {
  ggplot(data, mapping = aes(x = Black_Population, 
                             y = Black_Jail_Proportion)) + 
    geom_point() + 
    geom_smooth(method = lm, se = FALSE, fullrange = TRUE) + 
    labs(title = "Black Population vs Black Jail Proportion in King County",
         x = "Black Population", 
         y = "Black Jail Proportion")
}
