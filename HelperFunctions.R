#Helper functions

min_max_norm <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

#apply Min-Max normalization to first four columns in iris dataset
# 
# foo <- full_players_data_ %>% 
#   mutate(normalised_PctPointWonOnServe = min_max_norm(pct_point_won_on_serve),
#          normalised_PctPointWonOnReturn = min_max_norm(pct_point_won_on_return))
# 

find_match_head_to_head_records <- function(all_players, players_record_needed, by_surface)
  
{
  
  # winner_head_to_head <- all_players %>% 
  #   #group_by(winner_name, loser_name) %>% 
  #   group_by_at({{winner_group_vars}}) %>% 
  #   tally(name = "player_i_wins") %>% 
  #   rename("player_i" = "winner_name",
  #          "player_j" = "loser_name") %>% 
  #   ungroup() %>% 
  #   filter(player_i %in% players_record_needed) 
  # 
  # 
  # looser_head_to_head <- all_players %>% 
  #   #group_by(loser_name, winner_name) %>% 
  #   group_by_at({{loser_group_vars}}) %>% 
  #   tally(name = "player_j_wins") %>% 
  #   rename("player_j" = "winner_name",
  #          "player_i" = "loser_name") %>% 
  #   ungroup() %>% 
  #   filter(player_j %in% players_record_needed)
  
  player_names_one <- rbind(as_tibble(unique(all_players$winner_name)), 
                            as_tibble(unique(all_players$loser_name))) %>% 
    rename("player_i" = "value") %>% 
    unique()
  
  player_names_two <- rbind(as_tibble(unique(all_players$winner_name)), 
                            as_tibble(unique(all_players$loser_name))) %>% 
    rename("player_j" = "value") %>% 
    unique()
  
  all_potential_players <- tidyr::crossing(player_names_one, player_names_two) %>% 
    filter(!player_i == player_j) %>% 
    group_by(grp = paste(pmax(player_i, player_j),
                         pmin(player_i, player_j), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-grp) 
  
  
  if (by_surface == T)
    
  {
    
    all_potential_players <- all_potential_players %>% 
      slice(rep(1:n(), each = length(surfaces))) %>% 
      group_by(player_i, player_j) %>% 
      mutate(surface = rep(surfaces, times = 1)) %>% # 2x3 = 6 %>% 
      ungroup()
    
    n_times_won_on_surface <- all_players %>% 
      group_by(winner_name, loser_name, surface) %>% 
      tally()
    
    when_player_i_wins <- all_potential_players %>%
      left_join(select(n_times_won_on_surface, winner_name, loser_name, surface, n),
                by = c("player_i" = "winner_name", 
                       "player_j" = "loser_name",
                       "surface" = "surface"))
    
    when_player_j_wins <- when_player_i_wins %>% 
      left_join(select(n_times_won_on_surface, winner_name, loser_name, surface, n),
                by = c("player_j" = "winner_name", 
                       "player_i" = "loser_name",
                       "surface" = "surface")) %>% 
      rename("player_i_wins" = "n.x",
             "player_j_wins" = "n.y") %>% 
      filter_at(vars(
        player_i_wins, player_j_wins), any_vars(!is.na(.)))
    
    head_to_head_records <- when_player_j_wins
    
    head_to_head_records$player1 <- factor(head_to_head_records$player_i, 
                                           levels=unique(c(head_to_head_records$player_i, 
                                                           head_to_head_records$player_j)))  
    head_to_head_records$player2 <- factor(head_to_head_records$player_j, 
                                           levels=unique(c(head_to_head_records$player_i, 
                                                           head_to_head_records$player_j)))  
    
    return(head_to_head_records)
    
  }
  
}


find_game_head_to_head_record <- function(all_players, players_record_needed, by_surface)
  
{
  
  player_names_one <- rbind(as_tibble(unique(all_players$player_i)), 
                            as_tibble(unique(all_players$player_j))) %>% 
    rename("player_i" = "value") %>% 
    unique()
  
  player_names_two <- rbind(as_tibble(unique(all_players$player_i)), 
                            as_tibble(unique(all_players$player_j))) %>% 
    rename("player_j" = "value") %>% 
    unique()
  
  all_potential_players <- tidyr::crossing(player_names_one, player_names_two) %>% 
    mutate(player_i = pmax(player_i, player_j),
           player_j = pmin(player_i, player_j)) %>% 
    filter(!player_i == player_j) %>% 
    group_by(grp = paste(pmax(player_i, player_j),
                         pmin(player_i, player_j), sep = "_")) %>%
    slice(1) %>%
    ungroup() 
  
  if(by_surface == T)
    
  {   
    all_potential_players <- all_potential_players %>% 
      slice(rep(1:n(), each = length(surfaces))) %>% 
      group_by(grp) %>% 
      mutate(surface = rep(surfaces, times = 1)) %>% # 2x3 = 6 %>% 
      ungroup()
    
    
    player_i_games_won_against_opponents <- all_players %>% 
      group_by(player_i, player_j, surface) %>% 
      summarise(player_i_games_won = 
                  sum(player_i_games_won, 
                      na.rm = T)) %>% 
      ungroup()
    
    player_j_games_won_against_opponents <- all_players %>% 
      group_by(player_j, player_i, surface) %>% 
      summarise(player_j_games_won = 
                  sum(player_j_games_won, 
                      na.rm = T)) %>% 
      ungroup()
    
    player_and_games_won_against_oppents <- all_potential_players %>% 
      left_join(player_i_games_won_against_opponents, by = c("player_i", "player_j", "surface")) %>% 
      left_join(player_j_games_won_against_opponents, by = c("player_i", "player_j", "surface")) %>% 
      filter_at(vars(
        player_i_games_won, player_j_games_won), any_vars(!is.na(.))) %>% 
      mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), surface, sep = "_")) 
    
    
    return(player_and_games_won_against_oppents)
    
  } else
    
    
    player_i_games_won_against_opponents <- all_players %>% 
    group_by(player_i, player_j) %>% 
    summarise(player_i_games_won = 
                sum(player_i_games_won, 
                    na.rm = T)) %>% 
    ungroup()
  
  player_j_games_won_against_opponents <- all_players %>% 
    group_by(player_j, player_i) %>% 
    summarise(player_j_games_won = 
                sum(player_j_games_won, 
                    na.rm = T)) %>% 
    ungroup()
  
  player_and_games_won_against_oppents <- all_potential_players %>% 
    left_join(player_i_games_won_against_opponents, by = c("player_i", "player_j")) %>% 
    left_join(player_j_games_won_against_opponents, by = c("player_i", "player_j")) %>% 
    filter_at(vars(
      player_i_games_won, player_j_games_won), any_vars(!is.na(.))) %>% 
    mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) 
  
  
  return(player_and_games_won_against_oppents)
  
}


find_point_head_to_head_record <- function(all_players, players_record_needed, by_surface)
  
{
  
  player_names_one <- rbind(as_tibble(unique(all_players$player_i)), 
                            as_tibble(unique(all_players$player_j))) %>% 
    rename("player_i" = "value") %>% 
    unique()
  
  player_names_two <- rbind(as_tibble(unique(all_players$player_i)), 
                            as_tibble(unique(all_players$player_j))) %>% 
    rename("player_j" = "value") %>% 
    unique()
  
  all_potential_players <- tidyr::crossing(player_names_one, player_names_two) %>% 
    mutate(player_i = pmax(player_i, player_j),
           player_j = pmin(player_i, player_j)) %>% 
    filter(!player_i == player_j) %>% 
    group_by(grp = paste(pmax(player_i, player_j),
                         pmin(player_i, player_j), sep = "_")) %>%
    slice(1) %>%
    ungroup() 
  
  if(by_surface == T)
    
  {   
    all_potential_players <- all_potential_players %>% 
      slice(rep(1:n(), each = length(surfaces))) %>% 
      group_by(grp) %>% 
      mutate(surface = rep(surfaces, times = 1)) %>% # 2x3 = 6 %>% 
      ungroup()
    
    
    player_i_points_won_against_opponents <- all_players %>% 
      group_by(player_i, player_j, surface) %>% 
      summarise(player_i_points_won = 
                  sum(player_i_points_won, 
                      na.rm = T)) %>% 
      ungroup()
    
    player_j_points_won_against_opponents <- all_players %>% 
      group_by(player_j, player_i, surface) %>% 
      summarise(player_j_points_won = 
                  sum(player_j_points_won, 
                      na.rm = T)) %>% 
      ungroup()
    
    player_and_points_won_against_oppents <- all_potential_players %>% 
      left_join(player_i_points_won_against_opponents, by = c("player_i", "player_j", "surface")) %>% 
      left_join(player_j_points_won_against_opponents, by = c("player_i", "player_j", "surface")) %>% 
      filter_at(vars(
        player_i_points_won, player_j_points_won), any_vars(!is.na(.))) %>% 
      mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), surface, sep = "_")) 
    
    
    return(player_and_games_won_against_oppents)
    
  } else
    
    
    player_i_points_won_against_opponents <- all_players %>% 
    group_by(player_i, player_j) %>% 
    summarise(player_i_points_won = 
                sum(player_i_points_won, 
                    na.rm = T)) %>% 
    ungroup()
  
  player_j_points_won_against_opponents <- all_players %>% 
    group_by(player_j, player_i) %>% 
    summarise(player_j_points_won = 
                sum(player_j_points_won, 
                    na.rm = T)) %>% 
    ungroup()
  
  player_and_points_won_against_oppents <- all_potential_players %>% 
    left_join(player_i_points_won_against_opponents, by = c("player_i", "player_j")) %>% 
    left_join(player_j_points_won_against_opponents, by = c("player_i", "player_j")) %>% 
    filter_at(vars(
      player_i_points_won, player_j_points_won), any_vars(!is.na(.))) %>% 
    mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) 
  
  
  return(player_and_points_won_against_oppents)
  
}

# 
# players_and_games_scores_test <- players_and_games_scores %>% 
#   rename("player_i" = "winner_name",
#          "player_j" = "loser_name",
#          "player_i_games_won" = "winner_total_games_won",
#          "player_j_games_won" = "loser_total_games_won")
# 
# all_potential_players <- tidyr::crossing(player_names_one, player_names_two) %>% 
#   filter(!player_i == player_j) %>% 
#   group_by(grp = paste(pmax(player_i, player_j),
#                        pmin(player_i, player_j), sep = "_")) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(-grp) 
# 
# games_won_against_opponents <- players_and_games_scores_test %>% 
#   mutate(grp  = paste(pmax(player_i, player_j),
#                           pmin(player_i, player_j), sep = "_")) %>% 
#   group_by(grp, surface) %>% 
#   summarise(player_i_games_won = sum(player_i_games_won, na.rm = T),
#             player_j_games_won = sum(player_j_games_won, na.rm = T))
#             
# 
# loser_games_won_against_opponents <- players_and_games_scores %>% 
#   group_by(loser_name, winner_name, surface) %>% 
#   summarise(loser_games_won = sum(loser_total_games_won, na.rm = T))

# 
# player_and_games_won_against_oppents <- all_potential_players %>% 
#   left_join(games_won_against_opponents), by = c("player_i" = "winner_name",
#                                                        "player_j" = "loser_name")) %>% 
#   left_join(loser_games_won_against_opponents, by = c("player_i" = "winner_name",
#                                                       "player_j" = "loser_name")) %>% 
#   filter_at(vars(
#     winner_games_won, loser_games_won), any_vars(!is.na(.))) %>% 
#   mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) %>%
#   group_by(grp) %>% 
#   arrange(grp)
# mutate(player_i_games_won = winner_games_won + lead(loser_game_won))


  