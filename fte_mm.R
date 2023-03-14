library(tidyverse)

data_raw <- read.csv("fte_mm_2023.csv")%>%
  view()
data <- data_raw %>%
  filter(gender == "mens") %>%
  select(team_id,team_name,rd2_win,team_rating,team_seed,team_region)%>%
  mutate(implied_seed = rep(1:16, each = 4)) %>%
  mutate(team_seed =as.integer(team_seed)) %>%
  mutate(seed_dif = implied_seed - team_seed)%>%
  view()

# matchup combinations
matchup_comb <- combn(data$team_name,2)
matchups <- data.frame(matchup_comb) %>%
  view()

# rename matchups team1, team2
matchups2 <- as.data.frame(t(matchups))%>%
  rename(team1 = V1, team2 = V2) %>%
  view()


result <- left_join(matchups2, data, by = c("team1" = "team_name"))%>%
  select(team1, team_seed, team2, team_rating, team_region)%>%
  view()

result2 <- left_join(result, data, by = c("team2" = "team_name"))%>%
  select(team1, team_seed.x, team_rating.x, team_region,
         team2, team_seed.y, team_rating.y, team_region)%>%
  mutate(pr_dif = team_rating.x-team_rating.y)%>%
  mutate(win_p = win_pct(pr_dif))%>%
  mutate(seed_dif = team_seed.x -team_seed.y)%>%
  view()

assign_index <- function(team_region, team_seed) {
  region_seeds <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
  region_names <- c("South", "East", "MidWest", "West")
  
  region_index <- match(team_region, region_names)
  seed_index <- match(team_seed, region_seeds)
  
  if (is.na(region_index) || is.na(seed_index)) {
    return(NA)
  }
  
  index <- 16 * (region_index - 1) + seed_index
  
  return(index)
}

 
  
win_pct_2 <- function(pr_dif,seed_dif){
  if(seed_dif > 4.0){
    pr_dif <- pr_dif + 2
    return(win_pct(pr_dif))
  }
  else if(seed_dif < -6.0){
    pr_dif <- pr_dif -2
    return(win_pct(pr_dif))
  }
  else{
    return(win_pct(pr_dif))
  }
}
  
win_pct <- function(pr_dif){
  return(1.0/(1.0 + 10^(-pr_dif * 30.464 / 400)))
}
