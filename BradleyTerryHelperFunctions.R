### Functions for Bradley Terry Model

find_players_and_scores_from_model <- function(btm_model_results)
  
{
  
  bt_abilties <- BradleyTerry2::BTabilities(btm_model_results)
  player_names <- as_tibble(rownames(bt_abilties))
  bt_abilties_with_names <- cbind(bt_abilties, player_names)
  
  return(bt_abilties_with_names)
  
}
