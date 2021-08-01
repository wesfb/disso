#### Feature selection for ML models


library(dplyr)
library(tidymodels)
library(stringr)

tidy_atp_matches <- readr::read_csv("tidy_atp_matches.csv")


colSums(is.na(tidy_atp_matches)) %>% as.data.frame()






colSums(is.na(tidy_atp_matches)) %>% as.data.frame()

rank_missing <- tidy_atp_matches %>% 
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

all_players <- rbind(as_tibble(unique(data$player_i)), 
        as_tibble(unique(data$player_j)))

losers <- tidy_atp_matches %>% 
  select(loser_name, loser_rank, tourney_date) %>% 
  rename("name" = "loser_name",
         "rank" = "loser_rank")

winners <- tidy_atp_matches %>% 
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
  mutate(rank = if_else(is.na(rank), rank_found, rank)) %>% unique()

still_missing_ranks <- ranks_found %>% 
  group_by(name, tourney_date) %>% 
  filter(is.na(rank)) %>% 
  unique()

still_missing_ranks

tidy_atp_matches <- tidy_atp_matches %>% 
  mutate(grp = paste(pmax(player_i, player_j),
                     pmin(player_i, player_j), sep = "_"),
         grp_two = paste(grp, tourney_id, round, sep = "_")) %>% 
  filter_at(vars(variable_names$new_name), any_vars(!is.na(.))) %>%  
  left_join(select(ranks_found, -rank_found), by = c("winner_name" ="name", "tourney_date")) %>% 
  dplyr::rename("winner_rank_if_missing" = "rank") %>%  
  left_join(select(ranks_found, -rank_found), by = c("loser_name" ="name", "tourney_date")) %>% 
  rename("loser_rank_if_missing" = "rank") %>% 
  mutate(winner_rank = if_else(is.na(winner_rank), winner_rank_if_missing, winner_rank),
         loser_rank = if_else(is.na(loser_rank), loser_rank_if_missing, loser_rank)) 

colSums(is.na(tidy_atp_matches)) %>% as.data.frame()

# 
# 
# is_missing_rank <- all_ranks %>% 
#   filter(is.na(rank))
# 
# has_one_rank <- all_ranks %>% 
#   group_by(name) %>% 
#   filter(!is.na(rank))
# 
# ranks_to_fill <- all_ranks %>% 
#   filter(name %in% is_missing_rank$name) %>% 
#   filter(name %in% has_one_rank$name) %>% 
#   group_by(name, tourney_date) %>% 
#   unique()
# 
# ranks_found_<- ranks_to_fill %>% ungroup() %>% 
#   left_join(select(ranks_found, name, tourney_date, rank_found), by =  c("name", "tourney_date")) %>% 
#   mutate(rank = if_else(is.na(rank), rank_found, rank))
#   
# 
# still_missing_ranks <- ranks_found_ %>% 
#   group_by(name, tourney_date) %>% 
#   filter(is.na(rank)) %>% 
#   unique()
# 
# 
# ranks_to_fill_ <-zoo::na.locf(zoo::na.locf(ranks_to_fill), fromLast = TRUE) %>% 
#   unique()
# 
# 
# 
# break_points <- tidy_atp_matches %>% 
#   filter_at(vars(c("w_bp_saved_pct", "l_bp_saved_pct")), any_vars(is.na(.))) %>% 
#   select(tourney_id, winner_name, loser_name, w_no_bp, l_no_bp, w_bp_converted, 
#          l_bp_converted, w_bp_converted_pct, l_bp_converted_pct, w_bp_saved_pct,
#          l_bp_saved_pct)
#   
#   
# 
# 
# tidy_atp_matches_long <- tidy_atp_matches_slim
# 
# find_mean_length <- tidy_atp_matches_slim %>% 
#   group_by(tourney_id) %>% 
#   summarise(mean_length = mean(minutes, na.rm = T))
#   
# 
# missing_aces <- tidy_atp_matches_slim %>% 
#   filter_at(vars(c("w_no_ace", "l_no_ace")), any_vars(is.na(.))) %>% 
#   
# has_no_rank_ever <- all_ranks %>% 
#   group_by(name) %>% 
#   filter(is.na(rank)) %>% 
#   group_by(name) %>% 
#   arrange(tourney_date) %>% 
#   slice(1)
# 
# 
# 
# ranks_to_fill_check <- ranks_to_fill_ %>% 
#   left_join(ranks_found, by =  c("name", "tourney_date"))
# 
# 
# ## Find match wins
# 
# 
# ## Create itterative h2h recrod
# 
# 
# foo <- tidy_atp_matches %>% 
#   group_by()


tidy_atp_matches_with_match_record1 <- tidy_atp_matches %>% 
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
         cum_player_i_win, cum_player_j_win, h2h_record, match_num, surface) %>% 
  ungroup() %>% 
  group_by(player_i, winner_name) %>% 
  mutate(total_player_i_wins = row_number()) %>% 
  ungroup() %>% 
  group_by(player_j_wins, winner_name) %>% 
  mutate(total_player_j_wins = row_number()) 
  
  #filter(grp == "Roger Federer_Rafael Nadal") 
  
#   
# tidy_atp_matches_with_h2h <- tidy_atp_matches %>% 
#   mutate(player_i = pmax(winner_name, loser_name),
#          player_j = pmin(winner_name, loser_name)) %>% 
#   mutate(grp = paste(pmax(player_i, player_j),
#                      pmin(player_i, player_j), sep = "_")) %>% 
#   #filter(grp == "Roger Federer_Rafael Nadal") %>% 
#   select(grp, tourney_id, winner_name, loser_name) %>% 
#   left_join(select(
#     tidy_atp_matches_differenes, player_i, player_j, cum_player_i_win, cum_player_j_win, match_num, tourney_id, grp_two), by = c("grp", "tourney_id")) %>% 
#   mutate(w_h2h = if_else(winner_name == player_i, cum_player_i_win, cum_player_j_win),
#          l_h2h = if_else(loser_name == player_i, cum_player_i_win, cum_player_j_win),
#          h2h_record = lag(w_h2h) - lag(l_h2h),
#          h2h_record = if_else(is.na(h2h_record), 0, h2h_record)) %>% 
#   ungroup()
#          

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



tidy_atp_matches_results <- tidy_atp_matches_with_match_record %>% 
  select(player_i, player_j, grp, grp_two, winner_name, loser_name, surface) %>% 
  mutate(result = if_else(player_i == winner_name, 1, 0)) %>%  #is player i winner
  left_join(select(tidy_atp_matches, grp_two, tourney_date))



test_long <- tidy_atp_matches_results %>% 
  select(player_i, player_j, result, grp, grp_two) %>% 
  tidyr::pivot_longer(cols = c(result), names_to = "player",
                      values_to = "name") %>% 
  group_by(grp_two) %>% 
  mutate(grp_check = n()) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarise(wins = sum(result == 1))

player_i_wins_loss_total <- purrr::map_df(.x = unique(c(tidy_atp_matches_results$player_i,
                                                     tidy_atp_matches_results$player_j)),
                                         .f = function(.x)
                                         {
                                           
                                           total_wins_loses_matches <- 
                                             tidy_atp_matches_results %>% 
                                             filter_at(vars(c(winner_name, 
                                                              loser_name)), 
                                                       any_vars(. ==.x)) %>% 
                                             mutate(player_to_grp = .x) %>% 
                                             group_by(player_to_grp) %>% 
                                             arrange(tourney_date) %>% 
                                             mutate(
                                               total_wins = lag(
                                               cumsum(if_else(winner_name == .x, 1, 0))),
                                               total_loses = lag(
                                                 cumsum(if_else(loser_name == .x, 1, 0))),
                                                 total_matches = lag(row_number()),
                                                 check = (total_loses +  total_wins) == total_matches,
                                               total_matches = if_else(is.na(total_matches), 0, as.numeric(total_matches))) %>% 
                                             ungroup() %>% 
                                             select(player_i, player_j, player_to_grp, grp, grp_two, tourney_date, winner_name, 
                                                    loser_name, total_matches, 
                                                    total_wins, total_loses, check)
                                           
                                           player_i_data <- total_wins_loses_matches %>% 
                                             filter(player_i == .x) %>% 
                                             select(player_i, grp, grp_two, winner_name, 
                                                    loser_name, total_matches, 
                                                    total_wins, total_loses, check) %>% 
                                             rename("player_i_total_matches" = "total_matches",
                                                    "player_i_wins" = "total_wins",
                                                    "player_i_loses" = "total_loses") %>% 
                                             mutate(player_i_wins = if_else(player_i_total_matches == 0, 0, player_i_wins),
                                                    player_i_loses = if_else(player_i_total_matches == 0, 0, player_i_loses))
                                           
                                           total_wins_loses_matches_player_i <- total_wins_loses_matches %>% 
                                             left_join(select(
                                               player_i_data, grp, grp_two, contains("player_i")),
                                               by = c("player_i", "grp", "grp_two")) %>% 
                                             filter_at(vars(c(player_i_total_matches, 
                                                              player_i_wins,
                                                              player_i_loses)), 
                                                            all_vars(!is.na(.))) 
                                           
                                           
                                    
                                           
                                           return(total_wins_loses_matches_player_i)
                                           
                                           # test = tidy_atp_matches_results %>% 
                                           #   left_join(dplyr::select(
                                           #     total_wins_loses_matches_player_i,
                                           #     player_i, grp, grp_two, contains("player_i")),
                                           #     by = c("player_i", "grp", "grp_two")) %>% 
                                           #   left_join(dplyr::select(
                                           #     total_wins_loses_matches_player_j,
                                           #     player_j, grp, grp_two, contains("player_j")),
                                           #     by = c("player_j", "grp", "grp_two")) %>% unique()
                                           # 
                                           #return(total_wins_list)
                                   
                                         }
                                         
)
                                                     
player_j_wins_loss_total <- purrr::map_df(.x = unique(c(tidy_atp_matches_results$player_i,
                                                        tidy_atp_matches_results$player_j)),
                                          .f = function(.x)
                                          {
                                            
                                            total_wins_loses_matches <- 
                                              tidy_atp_matches_results %>% 
                                              filter_at(vars(c(winner_name, 
                                                               loser_name)), 
                                                        any_vars(. ==.x)) %>% 
                                              mutate(player_to_grp = .x) %>% 
                                              group_by(player_to_grp) %>% 
                                              arrange(tourney_date) %>% 
                                              mutate(
                                                total_wins = lag(
                                                  cumsum(if_else(winner_name == .x, 1, 0))),
                                                total_loses = lag(
                                                  cumsum(if_else(loser_name == .x, 1, 0))),
                                                total_matches = lag(row_number()),
                                                check = (total_loses +  total_wins) == total_matches,
                                                total_matches = if_else(is.na(total_matches), 0, as.numeric(total_matches))) %>% 
                                              ungroup() %>% 
                                              select(player_i, player_j, player_to_grp, grp, grp_two,tourney_date, winner_name, 
                                                     loser_name, total_matches, 
                                                     total_wins, total_loses, check)
                                            
                                            player_j_data <- total_wins_loses_matches %>% 
                                              filter(player_j == .x) %>% 
                                              select(player_j, grp, grp_two, winner_name, 
                                                     loser_name, total_matches, 
                                                     total_wins, total_loses, check) %>% 
                                              rename("player_j_total_matches" = "total_matches",
                                                     "player_j_wins" = "total_wins",
                                                     "player_j_loses" = "total_loses") %>% 
                                              mutate(player_j_wins = if_else(player_j_total_matches == 0, 0, player_j_wins),
                                                     player_j_loses = if_else(player_j_total_matches == 0, 0, player_j_loses))
                                            

                                            total_wins_loses_matches_player_j <- total_wins_loses_matches %>% 
                                              left_join(select(
                                                player_j_data, grp, grp_two, contains("player_j")),
                                                by = c("player_j", "grp", "grp_two")) %>% 
                                              filter_at(vars(c(player_j_total_matches, 
                                                               player_j_wins,
                                                               player_j_loses)), 
                                                        all_vars(!is.na(.))) 
                                            
                                            return(total_wins_loses_matches_player_j)
                                            
                                            # test = tidy_atp_matches_results %>% 
                                            #   left_join(dplyr::select(
                                            #     total_wins_loses_matches_player_i,
                                            #     player_i, grp, grp_two, contains("player_i")),
                                            #     by = c("player_i", "grp", "grp_two")) %>% 
                                            #   left_join(dplyr::select(
                                            #     total_wins_loses_matches_player_j,
                                            #     player_j, grp, grp_two, contains("player_j")),
                                            #     by = c("player_j", "grp", "grp_two")) %>% unique()
                                            # 
                                            #return(total_wins_list)
                                            
                                          }
                                          
)

test <- tidy_atp_matches_results %>% 
  left_join(select(player_i_wins_loss_total, grp, grp_two, tourney_date, contains("player_i")),
            by = c("player_i", "grp", "grp_two", "tourney_date")) %>% 
  left_join(select(player_j_wins_loss_total, grp, grp_two, tourney_date, contains("player_j")),
            by = c("player_j", "grp", "grp_two", "tourney_date"))
  
                                         

player_wins <- tidy_atp_matches_results %>% 
  filter(result == 1) %>% 
  group_by(player_i) %>% 
  tally()
  
match_features <- readr::read_csv("match_features.csv")

match_data <- purrr::map_df(.x = tidy_atp_matches_results$grp_two,
                     .f = function(.x)
                     {
                       
                       match_result <- tidy_atp_matches_results %>% 
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

readr::write_csv(match_data, "match_data_new.csv")     
match_data_old <- readr::read_csv("match_data.csv")

match_data <- readr::read_csv("match_data_new.csv")



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

test_join_record <- match_data %>% 
  left_join(select(players_wins_loss_total, player_i, grp, 
                   grp_two, total_matches, total_wins, total_loses), 
                   by = c("player_i", "grp", "grp_two"))

full_data_with_features <- match_data  %>% ungroup() %>% 
  filter_at(vars(c(player_i_no_service_games, player_j_no_service_games)), any_vars(. !=0)) %>% 
  filter(!grp_two == "Lukas Dlouhy_Andy Roddick_2008-580_R128") %>%
   left_join(select(player_i_wins_loss_total, grp, grp_two, contains("player_i")),
             by = c("player_i", "grp", "grp_two")) %>% 
   left_join(select(player_j_wins_loss_total, grp, grp_two, contains("player_j")),
             by = c("player_j", "grp", "grp_two")) %>% 
   left_join(mean_height_for_country, by = c("player_i_ioc" = "ioc")) %>% 
  rename("player_i_mean_ht" = "mean_ht") %>% 
  left_join(mean_height_for_country, by = c("player_j_ioc" = "ioc")) %>% 
  rename("player_j_mean_ht" = "mean_ht") %>% 
  mutate(player_i_ht = if_else(is.na(player_i_ht) | is.nan(player_i_ht), player_i_mean_ht, player_i_ht),
         player_j_ht = if_else(is.na(player_j_ht) | is.nan(player_j_ht), player_j_mean_ht, player_j_ht)) %>% 
  left_join(select(tidy_atp_matches_with_match_record, grp_two, h2h_record, match_num),
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
         pct_serve_points_won_diff = player_i_pct_serve_points_won - player_j_pct_serve_points_won,
         pct_return_points_won_diff = player_i_pct_return_points_won - player_j_pct_return_points_won,
         pct_point_won_on_serve_diff = player_i_pct_point_won_on_serve - player_j_pct_point_won_on_serve,
         bp_converted_pct_diff = player_i_bp_converted_pct - player_j_bp_converted_pct,
         bp_saved_pct_diff = player_i_bp_saved_pct - player_j_bp_saved_pct,
         aces_per_game_pct_diff = player_i_aces_per_game_pct - player_j_aces_per_game_pct,
         df_per_game_pct_diff = player_i_df_per_game_pct - player_j_df_per_game_pct,
         total_matches_played_diff = player_i_total_matches - player_j_total_matches,
         total_match_wins_diff = player_i_wins - player_j_wins,
         total_match_loses_diff = player_i_loses - player_j_loses) %>% 
  #select((c(grp_two, player_i, player_j,player_i_no_ace,player_i_no_service_games,player_i_aces_per_game_pct, player_j_aces_per_game_pct,
   #         player_j_no_ace,player_j_no_service_games, player_i_df_per_game_pct, player_j_df_per_game_pct))) %>% 
  #select(player_i, player_j, contains("serve")) %>% 
  #filter(str_detect(player_i, "Nadal"))
  #select(player_i, player_j, surface, player_i_hand, player_j_hand, contains("diff")) %>% 
  select(grp, grp_two, player_i, player_j, surface, player_i_hand, player_j_hand, 
         contains("diff"), h2h_record, result) %>% 
  na.omit() 

names(full_data_with_features)
## Rational was to use, stats of the game that got them the win, with prior winning and loses up to that match i.e confidence.

# %>% 
#   filter(grp_two == "Sam Querrey_Andy Roddick_2010-424_SF") %>% 
#   mutate(player_bp_check = player_i_bp_converted / player_i_no_bp)
#          
readr::write_csv(full_data_with_features, "full_data_with_features_new.csv")     
full_data_with_features_old <- readr::read_csv("full_data_with_features.csv")
colSums(is.na(full_data_with_features)) %>% as.data.frame()

test_fdwf = readr::read_csv("full_data_with_features_new.csv")

library(dplyr)
library(readr)
library(tidyr)
library(corrr)
library(corrpl)

full_data_with_features <- readr::read_csv("full_data_with_features.csv")


corrr_foo <- full_data_with_features %>% 
  select(contains("diff"), -pct_return_points_won_diff, -first_serve_return_win_pct_diff, -second_serve_return_win_pct_diff) %>% 
  correlate() %>% 
  rearrange() %>% 
  shave() %>% 
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan")) 

full_data_with_features %>% 
  select(contains("diff"), -pct_return_points_won_diff, -first_serve_return_win_pct_diff, -second_serve_return_win_pct_diff) %>%
  cor(method = "kendall") %>% 
  ggcorrplot::ggcorrplot(., method = 'square', type = 'lower')


### Some Box plots of data
full_data_with_features_long <- full_data_with_features %>% 
  select(-where(is.character)) %>% 
  pivot_longer(cols = c(contains("diff"), h2h_record), names_to = "variable")

full_data_with_features_long %>% 
ggplot(aes(x = result, y = value),fill=result) +
  geom_boxplot() +
  facet_wrap(.~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Anova TEsts

library(AICcmodavg)
one_way <- aov(result ~ age_diff + height_diff + rank_diff + first_serve_pct_diff +
                 first_serve_win_pct_diff  + second_serve_win_pct_diff +
                 pct_serve_points_won_diff + pct_point_won_on_serve_diff + bp_converted_pct_diff +
                 bp_saved_pct_diff + aces_per_game_pct_diff + df_per_game_pct_diff + 
                 total_matches_played_diff + total_match_wins_diff + 
                 total_match_loses_diff + h2h_record , data = full_data_with_features)
summary(one_way)

## Use recipies for PCA analysis

## To start here next
library(tidymodels)
foo <- recipes::recipe(result ~ ., data = full_data_with_features) %>% 
  recipes::update_role(grp:player_j_hand, new_role = "id") %>% 
  recipes::step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

foo1 <- prep(foo)
foo2 = tidy(foo1)

foo2 = tidy(foo1, 2)

foo2 %>% 
  mutate(component = forcats::fct_inorder(component)) %>% 
  ggplot(aes(value, terms, fill = terms )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component) +
  labs(y = NULL)


foo2 %>% 
  filter(component %in% c("PC1", "PC2")) %>% 
  group_by(component) %>% 
  top_n(6, abs(value)) %>% 
  ungroup() %>% 
  mutate(terms = tidytext::reord)
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() + 
  facet_wrap(~component, scales = "free_y")

## Check values
  
pct_serve_return_points_won_diff <- full_data_with_features %>% 
  select(player_i, player_j, player_i_pct_serve_points_won, player_j_pct_serve_points_won, pct_serve_points_won_diff,
         player_i_pct_return_points_won, player_j_pct_return_points_won, pct_return_points_won_diff)

first_serve_win_pct_diff_first_serve_return_win_pct_diff <- 
  select(player_i, player_j, first_serve_win_pct_diff,  first_serve_return_win_pct_diff)

  

#3 Find nan and inf
aces_nan <- match_data %>% 
  select((c(player_i_no_ace,player_i_no_service_games,
                   player_j_no_ace,player_j_no_service_games)))

inf_ace <- full_data_with_features %>% filter(is.infinite(player_i_aces_per_game_pct))

zero_serve_games <- tidy_atp_matches %>% 
  filter_at(vars(l_no_service_games, w_no_service_games), any_vars(.==0)) %>% 
  select(winner_name, loser_name, w_no_ace, w_df, l_no_ace, l_df, score,
         w_no_service_games, l_no_service_games)

inf_obs <- full_data_with_features %>% 
  filter_at(vars(contains("bp")), any_vars(is.infinite(.))) %>% 
  select(player_i, player_j, contains("bp"))
 
inf_obs <- tidy_atp_matches %>% 
  filter_at(vars(contains("ace")), any_vars(is.infinite(.))) %>% 
  select(player_i, player_j, contains("ace"))


pearson_num = cov(full_data_with_features$player_i_aces_per_game_pct, 
                  full_data_with_features$player_j_aces_per_game_pct, use="complete.obs")

cov(i,j)/[stdev(i)*stdev(j)]

  
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
  