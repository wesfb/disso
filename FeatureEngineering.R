#### Feature selection for ML models


library(dplyr)
library(tidymodels)
tidy_atp_matches %>% names()

tidy_atp_matches <- readr::read_csv("tidy_atp_matches.csv")
  
na_count <-sapply(x, function(y) sum(length(which(is.na(y)))))


tidy_atp_matches %>% select(loser_entry) %>% 
  sum(length(which(is.na(.))))

colSums(is.na(tidy_atp_matches)) %>% as.data.frame()


missing <- tidy_atp_matches %>% filter(is.na(w_return_points))




colSums(is.na(tidy_atp_matches_slim)) %>% as.data.frame()

rank_missing <- tidy_atp_matches_slim %>% 
  filter_at(vars(c("winner_rank", "loser_rank")), any_vars(is.na(.))) %>% 
  select(winner_name, loser_name, winner_rank, loser_rank, tourney_id)
  

players_highest_lowest_rank <- all_ranks %>% 
  group_by(name) %>% 
  summarise(
    max = max(rank, na.rm = T),
    min = min(rank, na.rm = T)
  ) %>% 
  mutate(diff = max - min)

ranks_with_estimates <- rank_missing %>% 
  left_join(players_highest_lowest_rank, by = c("winner_name" = "name")) %>% 
  rename("winner_max" = "max",
         "winner_min" = "min",
         "winner_diff" = "diff") %>% 
  left_join(players_highest_lowest_rank, by = c("loser_name" = "name")) %>% 
  rename("loser_max" = "max",
         "loser_min" = "min",
         "loser_diff" = "diff")
        

find_mean_loser_rank_per_tournament <- tidy_atp_matches %>% 
  group_by(tourney_id, round) %>% 
  summarise(mean_loser_rank = mean(loser_rank, na.rm = T))

find_mean_loser_rank_per_tournament <- tidy_atp_matches %>% 
  group_by(tourney_id, round) %>% 
  summarise(mean_winner_rank = mean(winner_rank, na.rm = T))

find_players_lowest_rank <- function(data, type)
  
  all_players <- rbind(as_tibble(unique(data$player_i)), 
        as_tibble(unique(data$player_j)))

losers <- data %>% 
  select(loser_name, loser_rank, tourney_date) %>% 
  rename("name" = "loser_name",
         "rank" = "loser_rank")

winners <- data %>% 
  select(winner_name, winner_rank, tourney_date) %>% 
  rename("name" = "winner_name",
         "rank" = "winner_rank")


all_ranks <- rbind(losers, winners) %>% 
  arrange(name, tourney_date) %>% 
  group_by(name) 

all_missing_ranks <- all_ranks %>% 
  group_by(name, tourney_date) %>% 
  unique() %>% 
  filter(is.na(rank))
  

ranks_found <- readr::read_csv("ranking_found.csv") %>% 
  #filter(str_detect(name, "Haas")) %>% 
  mutate(name = str_trim(name,"both")) 


ranks_found <- all_missing_ranks %>% ungroup() %>% 
  #filter(str_detect(name, "Haas")) %>% 
  left_join(select(ranks_found, name, tourney_date, rank_found), by =  c("name", "tourney_date")) %>% 
  mutate(rank = if_else(is.na(rank), rank_found, rank))

still_missing_ranks <- ranks_found %>% 
  group_by(name, tourney_date) %>% 
  filter(is.na(rank)) %>% 
  unique()

tidy_atp_matches_cleaned <- readr::read_csv("tidy_atp_matches.csv") %>% 
  filter_at(vars(variable_names$new_name), any_vars(!is.na(.))) %>% 
  left_join(ranks_to_fill, by = c("winner_name" ="name", "tourney_date")) %>% 
  rename("winner_rank_if_missing" = "rank") %>% 
  left_join(ranks_to_fill, by = c("loser_name" ="name", "tourney_date")) %>% 
  rename("loser_rank_if_missing" = "rank") %>% 
  mutate(winner_rank = if_else(is.na(winner_rank), winner_rank_if_missing, winner_rank),
         winner_rank = if_else(is.na(loser_rank), loser_rank_if_missing, loser_rank))

colSums(is.na(tidy_atp_matches_cleaned)) %>% as.data.frame()



is_missing_rank <- all_ranks %>% 
  filter(is.na(rank))

has_one_rank <- all_ranks %>% 
  group_by(name) %>% 
  filter(!is.na(rank))

ranks_to_fill <- all_ranks %>% 
  filter(name %in% is_missing_rank$name) %>% 
  filter(name %in% has_one_rank$name) %>% 
  group_by(name, tourney_date) %>% 
  unique()

ranks_found_ <- ranks_to_fill %>% ungroup() %>% 
  left_join(select(ranks_found, name, tourney_date, rank_found), by =  c("name", "tourney_date")) %>% 
  mutate(rank = if_else(is.na(rank), rank_found, rank))
  

still_missing_ranks <- ranks_found_ %>% 
  group_by(name, tourney_date) %>% 
  filter(is.na(rank)) %>% 
  unique()


ranks_to_fill_ <-zoo::na.locf(zoo::na.locf(ranks_to_fill), fromLast = TRUE) %>% 
  unique()



break_points <- tidy_atp_matches_slim %>% 
  filter_at(vars(c("w_bp_saved_pct", "l_bp_saved_pct")), any_vars(is.na(.))) %>% 
  select(tourney_id, winner_name, loser_name, w_no_bp, l_no_bp, w_bp_converted, 
         l_bp_converted, w_bp_converted_pct, l_bp_converted_pct, w_bp_saved_pct,
         l_bp_saved_pct)
  
  
w_no_bp  = l_no_bp_faced,
l_no_bp = w_no_bp_faced,
w_bp_converted = l_no_bp_faced - l_no_bp_saved,
l_bp_converted = w_no_bp_faced - w_no_bp_saved,
w_bp_converted_pct = w_bp_converted / l_no_bp_faced,
l_bp_converted_pct = l_bp_converted / l_no_bp_faced,
w_bp_saved_pct = w_no_bp_saved / w_no_bp_faced,
l_bp_saved_pct = l_no_bp_saved / l_no_bp_faced,


tidy_atp_matches_long <- tidy_atp_matches_slim

find_mean_length <- tidy_atp_matches_slim %>% 
  group_by(tourney_id) %>% 
  summarise(mean_length = mean(minutes, na.rm = T))
  

missing_aces <- tidy_atp_matches_slim %>% 
  filter_at(vars(c("w_no_ace", "l_no_ace")), any_vars(is.na(.))) %>% 
  
has_no_rank_ever <- all_ranks %>% 
  group_by(name) %>% 
  filter(is.na(rank)) %>% 
  group_by(name) %>% 
  arrange(tourney_date) %>% 
  slice(1)



ranks_to_fill_check <- ranks_to_fill %>% 
  left_join(ranks_found, by =  c("name", "tourney_date"))

if (type == "lowest") 
  
  { 
  
  players_highest_lowest_rank <- all_ranks %>% 
    group_by(name) %>% 
    summarise(
      max = max(rank, na.rm = T),
      min = min(rank, na.rm = T)
    ) %>% 
    mutate(diff = max - min)
  
  noin_missing_start_end <- function(rank_col, date, df){
    start <- df[[date]][min(which(!is.na(df[[rank_col]])))]
    end <- df[[date]][max(which(!is.na(df[[rank_col]])))]
    data.frame(rank_col = rank_col, start=start, end=end)
    
  }
    

  x <-  lapply(names(all_ranks), noin_missing_start_end, date = 'tourney_date', df=all_ranks)
  do.call(rbind,x)
    
    
  
}
  
  
## Find match wins


## Create itterative h2h recrod


foo <- tidy_atp_matches %>% 
  group_by()


tidy_atp_matches_differenes <- tidy_atp_matches %>% 
  mutate(player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name)) %>% 
  mutate(grp = paste(pmax(player_i, player_j),
                       pmin(player_i, player_j), sep = "_"),
         grp_two = paste(grp, tourney_id, round, sep = "_")) %>% 
  group_by(grp) %>% 
  arrange(tourney_date) %>% 
  mutate(match_num = row_number()) %>% 
  mutate(player_i_wins = if_else(player_i == winner_name, 1, 0),
         player_j_wins = if_else(player_j == winner_name, 1, 0),
         cum_player_i_win = cumsum(player_i_wins),
         cum_player_j_win = cumsum(player_j_wins),
         h2h_record = lag(cum_player_i_win) - lag(cum_player_j_win),
         h2h_record = if_else(is.na(h2h_record), 0, h2h_record)) %>% 
  select(grp, grp_two, tourney_id, winner_name, loser_name, player_i, player_j, player_i_wins, player_j_wins,
         cum_player_i_win, cum_player_j_win, h2h_record, match_num) %>% 
  ungroup()
  #filter(grp == "Roger Federer_Rafael Nadal") 
  
  
tidy_atp_matches_with_h2h <- tidy_atp_matches %>% 
  mutate(player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name)) %>% 
  mutate(grp = paste(pmax(player_i, player_j),
                     pmin(player_i, player_j), sep = "_")) %>% 
  #filter(grp == "Roger Federer_Rafael Nadal") %>% 
  select(grp, tourney_id, winner_name, loser_name) %>% 
  left_join(select(
    tidy_atp_matches_differenes, player_i, player_j, cum_player_i_win, cum_player_j_win, match_num, tourney_id, grp_two), by = c("grp", "tourney_id")) %>% 
  mutate(w_h2h = if_else(winner_name == player_i, cum_player_i_win, cum_player_j_win),
         l_h2h = if_else(loser_name == player_i, cum_player_i_win, cum_player_j_win),
         h2h_record = lag(w_h2h) - lag(l_h2h),
         h2h_record = if_else(is.na(h2h_record), 0, h2h_record)) %>% 
  ungroup()
         

## Construct player i and player j remove info about winner




atp_winners <- tidy_atp_matches %>% 
  mutate(result = 1,
         player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name)) %>% 
  select(player_i, player_j, tourney_id, match_id, tourney_name, surface, draw_size, tourney_level, tourney_date, 
         match_num, contains("winner_"), score, best_of, round, minutes, contains("w_"), match_total_points) %>% 
  rename_with(~ gsub("w_", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("winner_", "", .x, fixed = T)) %>% 
  rename("player_id" = "id",
         "draw_size" = "drasize") %>% 
  mutate(grp = paste(pmax(player_i, player_j),
                     pmin(player_i, player_j), sep = "_"),
         grp_two = paste(grp, tourney_id, round, sep = "_"))

atp_loosers <- tidy_atp_matches %>% 
  mutate(result = 0,
         player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name)) %>% 
  select(player_i, player_j, tourney_id, match_id, tourney_name, surface, draw_size, tourney_level, tourney_date, 
         match_num, contains("loser_"), score, best_of, round, minutes, contains("l_"), match_total_points) %>% 
  rename_with(~ gsub("l_", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("loser_", "", .x, fixed = T)) %>% 
  rename("player_id" = "id",
         "match_total_points" = "match_totapoints") %>% 
  mutate(grp = paste(pmax(player_i, player_j),
                     pmin(player_i, player_j), sep = "_"),
         grp_two = paste(grp, tourney_id, round, sep = "_"))


tidy_atp_matches_long <- rbind(atp_winners, atp_loosers) %>% 
  arrange(match_id)

tidy_atp_matches_differenes_edit <- tidy_atp_matches %>% 
  mutate(player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name)) %>% 
  mutate(grp = paste(pmax(player_i, player_j),
                     pmin(player_i, player_j), sep = "_"),
         grp_two = paste(grp, tourney_id, round, sep = "_"))  %>% 
  #filter(grp == "Roger Federer_Rafael Nadal") %>% 
  select(player_i, player_j, grp, grp_two, winner_name, loser_name, surface) %>% 
  mutate(result = if_else(player_i == winner_name, 1, 0)) #is player i winner

match_features <- readr::read_csv("match_features.csv")

match_data <- purrr::map_df(.x = tidy_atp_matches_differenes$grp_two,
                     .f = function(.x)
                     {
                       
                       match_result <- tidy_atp_matches_differenes_edit %>% 
                       filter(grp_two == .x) 
                       
                       is_player_i_winner <- match_result$result == 1
                       
                       if (is_player_i_winner) {
                         
                         when_player_i_wins_var <- atp_winners %>% 
                           filter(grp_two == .x) %>% 
                           select_at(vars(match_features$feature))
                         
                         colnames(when_player_i_wins_var) <- 
                           paste0("player_i_", colnames(when_player_i_wins_var))  # Add prefix
                         
                         when_player_i_wins_var$player_i <- match_result$player_i[1]
                         
                           when_player_j_loses_var <- atp_loosers %>% 
                          filter(grp_two == .x) %>% 
                          select(match_features$feature)
                           
                           colnames(when_player_j_loses_var) <- 
                             paste0("player_j_", colnames(when_player_j_loses_var))  # Add prefix
                          
                           when_player_j_loses_var$player_j <- match_result$player_j[1]
                           
                           match_result <- match_result %>% 
                             left_join(when_player_i_wins_var, by = c("player_i" = "player_i")) %>% 
                             left_join(when_player_j_loses_var, by = c("player_j" = "player_j"))
                           

                         
                       } else  {
                         when_player_j_wins_var <- atp_winners %>% 
                         filter(grp_two == .x) %>% 
                         select(match_features$feature)
                       
                       colnames(when_player_j_wins_var) <- 
                         paste0("player_j_", colnames(when_player_j_wins_var))
                       
                       when_player_j_wins_var$player_j <- match_result$player_j[1]
              
                       
                       when_player_i_loses_var <- atp_loosers %>% 
                         filter(grp_two == .x) %>% 
                         select(match_features$feature)
                       
                       colnames(when_player_i_loses_var) <- 
                         paste0("player_i_", colnames(when_player_i_loses_var))  # Add prefix
                       
                       when_player_i_loses_var$player_i <- match_result$player_i[1]
                       
                       match_result <- match_result %>% 
                         left_join(when_player_j_wins_var, by = c("player_j" = "player_j")) %>% 
                         left_join(when_player_i_loses_var, by = c("player_i" = "player_i"))
                                
                       } 
                                     
                      return(match_result)
                     }
)

readr::write_csv(match_data, "match_data.csv")     

winners_height <- atp_winners %>% 
  select(name, ht, ioc)

losers_height <- atp_loosers %>% 
  select(name, ht, ioc)


mean_height_for_country <- rbind(winners_height, losers_height) %>% 
  unique() %>% 
  group_by(ioc) %>% 
  summarise(mean_ht = mean(ht, na.rm = T)) 
winners_height <- atp_winners %>% 
  select(name, ht, ioc)

losers_height <- atp_loosers %>% 
  select(name, ht, ioc)


mean_height_for_country <- rbind(winners_height, losers_height) %>% 
  unique() %>% 
  group_by(ioc) %>% 
  summarise(mean_ht = mean(ht, na.rm = T)) 

full_data_with_features <- match_data  %>% 
  left_join(mean_height_for_country, by = c("player_i_ioc" = "ioc")) %>% 
  rename("player_i_mean_ht" = "mean_ht") %>% 
  left_join(mean_height_for_country, by = c("player_j_ioc" = "ioc")) %>% 
  rename("player_j_mean_ht" = "mean_ht") %>% 
  mutate(player_i_ht = if_else(is.na(player_i_ht) | is.nan(player_i_ht), player_i_mean_ht, player_i_ht),
         player_j_ht = if_else(is.na(player_j_ht) | is.nan(player_j_ht), player_j_mean_ht, player_j_ht)) %>% 
  left_join(select(tidy_atp_matches_differenes, grp_two, h2h_record, match_num),
                   by = c("grp_two")) %>% 
  mutate(player_i_aces_per_game_pct = player_i_no_ace / player_i_no_service_games,
         player_j_aces_per_game_pct = player_j_no_ace / player_j_no_service_games,
         player_i_df_per_game_pct = player_i_df / player_i_no_service_games,
         player_j_df_per_game_pct = player_j_df / player_j_no_service_games,
         player_i_bp_converted_pct = if_else(is.nan(player_i_bp_converted_pct), 0, player_i_bp_converted_pct),
         player_j_bp_converted_pct = if_else(is.nan(player_j_bp_converted_pct), 0, player_j_bp_converted_pct),
         player_i_bp_saved_pct = if_else(is.nan(player_i_bp_saved_pct), 0, player_i_bp_saved_pct),
         player_j_bp_saved_pct = if_else(is.nan(player_j_bp_saved_pct), 0, player_j_bp_saved_pct),
         age_diff = player_i_age - player_j_age,
         height_diff = player_i_ht - player_j_ht,
         rank_diff = player_i_rank - player_j_rank,
         first_serve_pct_diff = player_i_first_serve_pct - player_j_first_serve_pct,
         first_serve_win_pct_diff = player_i_first_serve_win_pct - player_j_first_serve_win_pct,
         second_serve_win_pct_diff = player_i_second_serve_win_pct - player_j_second_serve_win_pct,
         first_serve_return_win_pct_diff = player_i_first_serve_return_win_pct - player_j_first_serve_return_win_pct,
         second_serve_return_win_pct_diff = player_i_second_serve_return_win_pct - player_j_second_serve_return_win_pct,
         bp_converted_pct_diff = player_i_bp_converted_pct - player_j_bp_converted_pct,
         bp_saved_pct_diff = player_i_bp_saved_pct - player_j_bp_saved_pct,
         aces_per_game_pct_diff = player_i_aces_per_game_pct - player_j_aces_per_game_pct,
         df_per_game_pct_diff = player_i_df_per_game_pct - player_j_df_per_game_pct) %>% 
  select(player_i, player_j, contains("serve")) %>% 
  filter(str_detect(player_i, "Nadal"))
  #select(player_i, player_j, surface, player_i_hand, player_j_hand, contains("diff")) %>% 
  na.omit()
         
readr::write_csv(full_data_with_features, "full_data_with_features.csv")     
         
colSums(is.na(full_data_with_features)) %>% as.data.frame()


library(corrr)

corrr_foo <- full_data_with_features %>% 
  select(contains("diff")) %>% 
  correlate() %>% 
  rearrange() %>% 
  shave() %>% 
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan")) %>% 
  theme_plex


  
missing_bp <- full_feature_set %>% 
  filter(is.na(player_i_bp_saved_pct)) %>%
  select(player_i, player_j, winner_name, loser_name, player_i_no_bp, player_j_no_bp,
         player_i_bp_saved_pct, player_j_bp_saved_pct, player_i_bp_converted,
         player_j_bp_converted, grp_two)

         
bp_tidy_atp_matches <- tidy_atp_matches %>% 
  mutate(player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name),
         grp = paste(pmax(player_i, player_j),
                     pmin(player_i, player_j), sep = "_"),
         grp_two = paste(grp, tourney_id, round, sep = "_")) %>% 
  select(player_i, player_j, winner_name, loser_name, tourney_id, tourney_name, grp, grp_two, contains("bp"), score) %>% 
  filter_at(vars(contains("bp")), any_vars(is.na(.)))
  


  

plyaer_i_ht_missing <- tidy_atp_matches %>% 
  mutate(player_i = pmax(winner_name, loser_name),
         player_j = pmin(winner_name, loser_name)) %>% 
  filter_at(vars(contains("ht")), any_vars(is.na(.))) %>% 
  select(player_i, player_j, contains("ht")) %>% 
  pivot_longer(cols = c("player_i", "player_j"))


full_feature_set %>% 
  left_join(mean_height_for_country, by = "ioc")
  