#Helper functions

min_max_norm <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

#apply Min-Max normalization to first four columns in iris dataset

foo <- full_players_data_ %>% 
  mutate(normalised_PctPointWonOnServe = min_max_norm(pct_point_won_on_serve),
         normalised_PctPointWonOnReturn = min_max_norm(pct_point_won_on_return))
