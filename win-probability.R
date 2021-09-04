#***********************************
# Import Packages
#***********************************
library(animation)
library(cfbfastR)
library(glue)
library(tidyverse)
library(zoo)

#***********************************
# Fetch Game Info
#***********************************
team <- "Ohio State"
week <- 1
year <- 2021

interp_TimeSecsRem <- function(pbp) {
  temp <- pbp %>% 
    mutate(TimeSecsRem = ifelse(TimeSecsRem == lag(TimeSecsRem), NA, TimeSecsRem)) %>%
    select(TimeSecsRem)
  ind <- which(temp$TimeSecsRem == 1800)
  temp$TimeSecsRem[1] <- 1800
  temp$TimeSecsRem[nrow(temp)] <- 0
  temp$TimeSecsRem[ind-1] <- 0
  pbp<- pbp %>% 
    mutate(TimeSecsRem = round(zoo::na.approx(temp$TimeSecsRem))) %>% 
    mutate(clock.minutes = floor(TimeSecsRem/60),clock.seconds = TimeSecsRem %% 60)
  return(pbp)
}
game_pbp <- cfbfastR::cfbd_pbp_data(year, team = team, week = week, epa_wpa = TRUE) %>% 
  filter(down > 0) %>%
  interp_TimeSecsRem() 

game <- cfbfastR::cfbd_game_info(year, team = team, week = week)

#***********************************
# Retrieve Team Info
#***********************************

team_info <- cfbfastR::cfbd_team_info()
team_logos <- team_info %>%
  select(school, color, alt_color, logos, alt_name2) %>%
  mutate(logo = map(logos, magrittr::extract2, 1),
         logo = as.character(logo)) %>% select(-logos)
game <- game %>% 
  inner_join(team_logos, by = c("away_team" = "school")) %>% 
  rename(away_logo = logo, away_color = color, away_alt_color = alt_color, away_abr = alt_name2) %>% 
  inner_join(team_logos, by = c("home_team" = "school")) %>% 
  rename(home_logo = logo, home_color = color, home_alt_color = alt_color, home_abr = alt_name2) %>%
  mutate(result = home_points - away_points) %>%
  rename(home_score = home_points, away_score = away_points)

#***********************************
# Transform Play-by-Play Data
#***********************************
game_pbp <- game_pbp %>%
  rename(qtr = period, wp = wp_before, posteam = pos_team, defteam = def_pos_team,
         away_team = away, home_team = home, play_id = game_play_number,
         posteam_score = pos_team_score, defteam_score = def_pos_team_score) %>%
  mutate(game_seconds_remaining = ifelse(half == 1, TimeSecsRem + 1800, TimeSecsRem),
         result = game$result,
         minlabel = ifelse(clock.minutes >= 15,
                           ifelse(clock.minutes == 15 & clock.seconds == 0, 15, clock.minutes - 15),
                           clock.minutes),
         minlabel = ifelse(minlabel < 10, paste0("0", minlabel), minlabel),
         seclabel = ifelse(clock.seconds < 10, paste0("0", clock.seconds), clock.seconds),
         time = paste0(minlabel, ":", seclabel)) 

#***********************************
# Clean Data
#***********************************

# filter plays that don't have win percentage
base_wp_data <- game_pbp %>% 
  filter(!is.na(wp)) %>% 
  mutate(s = game_seconds_remaining,
         wp = ifelse(posteam == away_team, wp, 1 - wp))
         