
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
matches_names$match_name_rev <- sapply(
  strsplit(matches_names$match_name, "\\s+"), function(x) paste(rev(x), collapse=" ")
  )

odds_names <- tibble(odds_name = unique(c(odds$Winner, odds$Loser)))

# fuzzy match
matched_names <- stringdist_left_join(
  matches_names, odds_names, by = c("match_name_rev"="odds_name"),
  method = "jw", distance_col = "dist"
  )

best_matches <- matched_names %>% 
  group_by(match_name) %>% 
  dplyr::filter(dist == min(dist))

# join --------------------------------------------------------------------

matches_odds <- matches %>% 
  # join name lookup
  left_join(
    select(best_matches, match_name, odds_name),
    by = c("winner_name"="match_name")
    ) %>% 
  left_join(
    select(best_matches, match_name, odds_name),
    by = c("loser_name"="match_name")
  ) %>% 
  # join odds data
  left_join(
    odds, by = c(
      "tourney_date"="Date",
      "odds_name.x"="Winner",
      "odds_name.y"="Loser"
    )
  )

