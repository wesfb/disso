
# setup -------------------------------------------------------------------

library(dplyr)
library(readr)
library(stringr)
library(fuzzyjoin)

matches <- read_csv(
  "https://raw.githubusercontent.com/wesfb/disso/main/match_data_new%202.csv"
)

odds <- read_csv(
  "https://raw.githubusercontent.com/wesfb/disso/main/matches_and_odds.csv"
)

odds$Date <- as.Date(odds$Date)

# names -------------------------------------------------------------------

matches_names <- tibble(match_name = unique(c(matches$player_i, matches$player_j)))
odds_names <- tibble(odds_name = unique(c(odds$Winner, odds$Loser)))

# fuzzy match
matched_names <- stringdist_left_join(
  matches_names, odds_names, by = c("match_name"="odds_name"),
  method = "jw", distance_col = "dist"
  )

best_matches <- matched_names %>% 
  group_by(match_name) %>% 
  arrange(match_name,dist)

# join --------------------------------------------------------------------
