library(tidyverse)
library(fmsb)

## Load data ====
load("fixtures_goals.Rdata")

## Calculate metrics ====
radar_df <- fixtures_goals %>% 
  group_by(teamName) %>% 
  summarise(
    goals = n(), 
    avg_goal_time = mean(elapsed), 
    earliest_goal = min(elapsed), 
    latest_goal = max(elapsed), 
    assisted_goals = sum(!is.na(assist)), 
    first_half_goals = sum(elapsed <= 45), 
    second_half_goals = sum(elapsed > 45)
  ) %>% 
  slice(4, 15) %>% 
  select(-teamName) 

## Indicate min and max for each metric ====
## It requires row 1 to be max and row 2 to be min
radar_df_mm <- bind_rows(
  tibble(goals = c(63, 0), 
         avg_goal_time = c(90, 0), 
         earliest_goal = c(90, 0), 
         latest_goal = c(90, 0), 
         assisted_goals = c(47, 0),  
         first_half_goals = c(32, 0), 
         second_half_goals = c(31, 0)
  ), 
  radar_df
)  

## Set color codes for each team ====
barca_col <- c(col2rgb("darkblue", alpha = FALSE))
barca_col <- c(rgb(barca_col[1], barca_col[2], barca_col[3], 128, 
                   maxColorValue = 255))
madrid_col <- c(col2rgb("snow3", alpha = FALSE))
madrid_col <- c(rgb(madrid_col[1], madrid_col[2], madrid_col[3], 128, 
                   maxColorValue = 255))

## Visualize radar chart ====
radarchart(radar_df_mm, 
           pcol = c(barca_col, madrid_col), 
           pfcol = c(barca_col, madrid_col), plty = 1, plwd = 10)

## Add legend
legend(x = 1.3, y = 1.5, legend = c("Barcelona", "Real Madrid"), 
       bty = "n", pch = 19, col = c(barca_col, madrid_col), 
       pt.cex = 3, x.intersp = .3, y.intersp = .3)


