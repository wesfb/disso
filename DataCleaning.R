

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(data.table)

atp_matches <-
  list.files(pattern = "*atp_matches") %>% 
  purrr::map_df(~readr::read_csv(.))
  
player_rankings <-  list.files(pattern = "*atp_rankings") %>% 
  purrr::map_df(~readr::read_csv(.)) %>% 
  dplyr::mutate(ranking_date = ymd(ranking_date))

end_twenty_eighteen_rankings<- player_rankings %>% 
  dplyr::filter(ranking_date == "2018-12-31")
  
  
variable_names <- readr::read_csv("variable_names.csv")

tidy_atp_matches <- atp_matches %>% 
  dplyr::rename_at(vars(variable_names$old_name), ~variable_names$new_name) %>% 
  dplyr::mutate(match_id = paste0(tourney_id, winner_id, loser_id),
         tourney_date = ymd(tourney_date),
         year = year(tourney_date),
         w_second_serve_in = w_serve_points - w_first_serve_in - w_df,
         l_second_serve_in = l_serve_points - l_first_serve_in - l_df,
         w_first_serve_pct = w_first_serve_in / w_serve_points,
         l_first_serve_pct = l_first_serve_in / l_serve_points,
         w_first_serve_win_pct = w_first_serve_points_won / w_first_serve_in,
         l_first_serve_win_pct = l_first_serve_points_won / l_first_serve_in,
         w_second_serve_win_pct = w_second_serve_points_won / w_second_serve_in,
         l_second_serve_win_pct = l_second_serve_points_won / l_second_serve_in,
         w_first_serve_return_win = l_first_serve_in - l_first_serve_points_won,
         l_first_serve_return_win = w_first_serve_in - w_first_serve_points_won,
         w_first_serve_return_win_pct = w_first_serve_return_win / l_first_serve_in,
         l_first_serve_return_win_pct = l_first_serve_return_win / w_first_serve_in,
         w_second_serve_return_win = l_second_serve_in - l_second_serve_points_won,
         l_second_serve_return_win = w_second_serve_in - w_second_serve_points_won,
         w_second_serve_return_win_pct = w_second_serve_return_win / l_second_serve_in,
         l_second_serve_return_win_pct = l_second_serve_return_win / w_second_serve_in,
         w_no_bp  = l_no_bp_faced,
         l_no_bp = w_no_bp_faced,
         w_bp_converted = l_no_bp_faced - l_no_bp_saved,
         l_bp_converted = w_no_bp_faced - w_no_bp_saved,
         w_bp_converted_pct = w_bp_converted / l_no_bp_faced,
         l_bp_converted_pct = l_bp_converted / l_no_bp_faced,
         w_bp_saved_pct = w_no_bp_saved / w_no_bp_faced,
         l_bp_saved_pct = l_no_bp_saved / l_no_bp_faced,
         w_serve_point_check = w_df + w_first_serve_in + w_second_serve_in == w_serve_points,
         l_serve_point_check = l_df + l_first_serve_in + l_second_serve_in == l_serve_points,
         w_pct_point_won_on_serve = (w_first_serve_pct*w_first_serve_win_pct + 
                                       ((1 - w_first_serve_pct)*w_second_serve_win_pct)),
         #w_PctPointWonOnReturn = (w_1stSvPct*w_1stSvWinPCt + ((1 - w_1stSvPct)*w_1stSvPct)),
         l_pct_point_won_on_serve = (l_first_serve_pct*l_first_serve_win_pct + 
                                   ((1 - l_first_serve_pct)*l_second_serve_win_pct)),
         #l_PctPointWonOnReturn = (l_1stSvPct*l_1stSvWinPCt + ((1 - l_1stSvPct)*l_1stSvPct)),
         w_serve_points_won = w_first_serve_points_won + w_second_serve_points_won,
         l_serve_points_won = l_first_serve_points_won + l_second_serve_points_won,
         w_return_points_won = w_first_serve_return_win + w_second_serve_return_win,
         l_return_points_won = l_first_serve_return_win + l_second_serve_return_win,
         match_total_points = w_serve_points +  l_serve_points,
         w_points_won = w_serve_points_won + w_return_points_won + l_df,
         l_points_won = l_serve_points_won + l_return_points_won  + w_df
             ) %>% 
  filter(!str_detect(tourney_name, "Davis Cup"))



atp_winners <- tidy_atp_matches %>% 
  select(tourney_id, match_id, tourney_name, surface, draw_size, tourney_level, tourney_date, 
         match_num, contains("winner_"), score, best_of, round, minutes, contains("w_"), match_total_points) %>% 
  rename_with(~ gsub("w_", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("winner_", "", .x, fixed = T)) %>% 
  rename("player_id" = "id",
         "draw_size" = "drasize") 

atp_loosers <- tidy_atp_matches %>% 
  select(tourney_id, match_id, tourney_name, surface, draw_size, tourney_level, tourney_date, 
         match_num, contains("loser_"), score, best_of, round, minutes, contains("l_"), match_total_points) %>% 
  rename_with(~ gsub("l_", "", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("loser_", "", .x, fixed = T)) %>% 
  rename("player_id" = "id",
         "match_total_points" = "match_totapoints",
  )

tour_average_variables <- c("first_serve_pct" , "pct_point_won_on_serve")

tour_averages_by_year <- rbind(atp_winners, atp_loosers) %>%
  group_by(year(tourney_date)) %>%
  summarise_at(tour_average_variables, mean, na.rm = T) %>% 
  rename("year" = "year(tourney_date)",
         "tour_average_first_serve_pct" = "first_serve_pct",
         "tour_average_pct_point_won_on_serve" = "pct_point_won_on_serve")


tournament_averages_by_year <- rbind(atp_winners, atp_loosers) %>%
  group_by(tourney_date, tourney_name) %>%
  summarise_at(tour_average_variables, mean, na.rm = T) %>% 
  mutate(year_to_match_with = year(ymd(tourney_date) + years(1))) %>% 
  rename("tournament_average_first_serve_pct" = "first_serve_pct",
         "tournament_average_pct_point_won_on_serve" = "pct_point_won_on_serve") %>% 
  ungroup() %>% 
  select(-tourney_date)


full_atp_data <- rbind(atp_winners, atp_loosers) %>% 
  arrange(tourney_date, match_num, match_id) %>% 
  mutate(year = year(tourney_date)) %>% 
  left_join(tour_averages_by_year, by = "year") %>%
  left_join(tournament_averages_by_year, by = c("year" =  "year_to_match_with", "tourney_name")) %>% 
  mutate(pct_point_won_on_return =  (tour_average_first_serve_pct * first_serve_return_win_pct) + 
           (1 - tour_average_first_serve_pct) * second_serve_return_win_pct) %>% 
  ungroup()


tour_average_pct_point_won_on_return_by_year <- full_atp_data %>% 
  group_by(year) %>% 
  summarise(tour_average_pct_point_won_on_return = 
              mean(pct_point_won_on_return, na.rm = T)) %>% 
  ungroup()

five_year_tour_average_serve_return <- full_atp_data %>% 
  filter(between(year, 2014, 2018)) %>% 
  summarise_at(c("pct_point_won_on_serve", "pct_point_won_on_return"), mean, na.rm = TRUE)

full_atp_data <- full_atp_data %>% 
  left_join(tour_average_pct_point_won_on_return_by_year, by = "year")



markov_model_variables <- c("first_serve_pct", "first_serve_win_pct", "second_serve_win_pct",
                            "first_serve_return_win_pct", "second_serve_return_win_pct")


full_atp_data_markov <- full_atp_data %>% 
  select(tourney_id, match_id, tourney_name, tourney_date, year, surface, name, age, rank, score, round, all_of(markov_model_variables)) %>% 
  filter(between(year, 2014, 2018)) %>% 
  mutate(year_to_match_with = year(ymd(tourney_date) + years(1))) %>% 
  group_by(name, year, year_to_match_with) %>% 
  summarise_at(markov_model_variables, mean, na.rm = T) %>% 
  rename("five_year_average_first_serve_pct" = "first_serve_pct",
         "five_year_average_first_serve_win_pct" = "first_serve_win_pct",
         "five_year_average_second_serve_win_pct" = "second_serve_win_pct",
         "five_year_average_first_serve_return_win_pct" = "first_serve_return_win_pct",
         "five_year_average_second_serve_return_win_pct" = "second_serve_return_win_pct") %>% 
  ungroup() %>% 
  mutate(five_year_average_pct_point_won_on_serve = 
           (five_year_average_first_serve_pct * five_year_average_first_serve_win_pct + 
              ((1 - five_year_average_first_serve_pct) * five_year_average_second_serve_win_pct)),
         five_year_average_pct_point_won_on_return = 
           five_year_tour_average_serve_return$pct_point_won_on_return * five_year_average_first_serve_return_win_pct + 
           (1 - five_year_tour_average_serve_return$pct_point_won_on_return) * five_year_average_second_serve_return_win_pct)


twenty_nineteen_slams <- tidy_atp_matches %>% 
  filter(year(tourney_date) == 2019,
         best_of == 5 ) %>% 
  mutate(round_number = str_extract(round, "[[:digit:]]+")) %>% 
  select(tourney_id, match_id, tourney_name, tourney_date, year, surface, match_num, round, round_number, winner_id, 
         winner_name, loser_id, loser_name) %>% 
  arrange(tourney_name, tourney_id, round_number) %>% 
  rename("player_i" = "winner_name",
         "player_i_id" = "winner_id",
         "player_j" = "loser_name",
         "player_j_id" = "loser_id") %>% 
  left_join(tournament_averages_by_year,
                   by = c("tourney_name", "year" = "year_to_match_with")) %>% 
  left_join(select(full_atp_data_markov, name, five_year_average_pct_point_won_on_serve, 
                   five_year_average_pct_point_won_on_return, year_to_match_with),
            by = c("player_i" = "name", "year" = "year_to_match_with")) %>% 
  rename("player_i_five_year_average_pct_point_won_on_serve" = "five_year_average_pct_point_won_on_serve",
         "player_i_five_year_average_pct_point_won_on_return" = "five_year_average_pct_point_won_on_return") %>% 
  left_join(select(full_atp_data_markov, name, five_year_average_pct_point_won_on_serve, 
                   five_year_average_pct_point_won_on_return, year_to_match_with),
            by = c("player_j" = "name", "year" = "year_to_match_with")) %>% 
  rename("player_j_five_year_average_pct_point_won_on_serve" = "five_year_average_pct_point_won_on_serve",
         "player_j_five_year_average_pct_point_won_on_return" = "five_year_average_pct_point_won_on_return") %>% 
  mutate(tour_average_pct_point_won_on_serve = 
           five_year_tour_average_serve_return$pct_point_won_on_serve,
         tour_average_pct_point_won_on_return = 
           five_year_tour_average_serve_return$pct_point_won_on_return) %>% 
  mutate(prob_player_i_wins_point_on_serve = 
           tournament_average_pct_point_won_on_serve + 
           (player_i_five_year_average_pct_point_won_on_serve - tour_average_pct_point_won_on_serve) - 
           (player_j_five_year_average_pct_point_won_on_return - tour_average_pct_point_won_on_return),
         prob_player_j_wins_point_on_serve = 
           tournament_average_pct_point_won_on_serve + 
           (player_j_five_year_average_pct_point_won_on_serve - tour_average_pct_point_won_on_serve) - 
           (player_i_five_year_average_pct_point_won_on_return - tour_average_pct_point_won_on_return),
         prob_player_i_wins_point_on_return = 1 - prob_player_j_wins_point_on_serve,
         prob_player_j_wins_point_on_return = 1- prob_player_i_wins_point_on_serve
         )


grand_slam_rounds <- unique(twenty_nineteen_slams$round)

foo <- purrr::map_df(x = grand_slam_rounds, .f = function(x)
  
  { foo_ <- twenty_nineteen_slams %>% 
    filter(round == x) %>% 
    left_join(select(full_atp_data_markov, name, five_year_average_pct_point_won_on_serve, 
                   five_year_average_pct_point_won_on_return, year_to_match_with),
            by = c("player_i" = "name", "year" = "year_to_match_with")) %>% 
    rename("player_i_five_year_average_pct_point_won_on_serve" = "five_year_average_pct_point_won_on_serve",
         "player_i_five_year_average_pct_point_won_on_return" = "five_year_average_pct_point_won_on_return") %>% 
    left_join(select(full_atp_data_markov, name, five_year_average_pct_point_won_on_serve, 
                   five_year_average_pct_point_won_on_return, year_to_match_with),
            by = c("player_j" = "name", "year" = "year_to_match_with")) %>% 
    rename("player_j_five_year_average_pct_point_won_on_serve" = "five_year_average_pct_point_won_on_serve",
         "player_j_five_year_average_pct_point_won_on_return" = "five_year_average_pct_point_won_on_return") 
  
  return(foo_)
}
)
  
  
  
  

fij = ft + (fi − fav) − (gj − gav)
#Next we need to add averages and tounrmaent year before to this data from and 
#3then work out the prob of winning a point on serve agaisnt player j.

foo <- tidy_atp_matches %>% group_by(winner_name, loser_name) %>% 
  summarise(n=n())
  
all_twenty_nineteen_slams_players <- unique(c(twenty_nineteen_slams$player_i,
                                              twenty_nineteen_slams$player_j)) %>% 
  as_tibble() %>% 
  left_join(select(full_atp_data, name, player_id), by = c("value" = "name")) %>% 
  unique()

winner_head_to_head <- tidy_atp_matches %>% 
  group_by(winner_name, loser_name) %>% 
  tally(name = "player_i_wins") %>% 
  rename("player_i" = "winner_name",
         "player_j" = "loser_name") %>% 
  ungroup() %>% 
  filter(player_i %in% all_twenty_nineteen_slams_players) 


looser_head_to_head <- tidy_atp_matches %>% 
  group_by(loser_name, winner_name) %>% 
  tally(name = "player_j_wins") %>% 
  rename("player_j" = "winner_name",
         "player_i" = "loser_name") %>% 
  ungroup() %>% 
  filter(player_j %in% all_twenty_nineteen_slams_players)

players_and_games_scores <- tidy_atp_matches %>% 
  select(winner_name, loser_name, score) %>% 
  mutate(score_without_tie = str_replace(score, "\\((\\d+)\\)", "")) %>% 
  filter(!str_detect(score, "RET")) %>% 
  separate(score_without_tie, 
           c("set_one", "set_two", "set_three", "set_four", "set_five"), " ", 
           remove = FALSE) %>% 
  separate(set_one, 
           c("set_one_games_winner", "set_one_games_loser"), "-", remove = FALSE) %>% 
  separate(set_two, 
           c("set_two_games_winner", "set_two_games_loser"), "-", remove = FALSE) %>% 
  separate(set_three, 
           c("set_three_games_winner", "set_three_games_loser"), "-", remove = FALSE) %>% 
  separate(set_four, 
           c("set_four_games_winner", "set_four_games_loser"), "-", remove = FALSE) %>% 
  separate(set_five, 
           c("set_five_games_winner", "set_five_games_loser"), "-", remove = FALSE) %>% 
  mutate(across(contains(c("games_winner", "games_loser")), as.numeric)) %>% 
  mutate(winner_total_games_won = 
           rowSums(across(contains("games_winner")), na.rm = T)) %>% 
  mutate(loser_total_games_won = 
           rowSums(across(contains("games_loser")), na.rm = T))  
  
  
winner_games_won_against_opponents <- players_and_games_scores %>% 
  group_by(winner_name, loser_name) %>% 
  summarise(winner_games_won = sum(winner_total_games_won, na.rm = T))

loser_games_won_against_opponents <- players_and_games_scores %>% 
  group_by(loser_name, winner_name) %>% 
  summarise(loser_games_won = sum(loser_total_games_won, na.rm = T))




player_and_games_won_against_oppents <- all_potential_players %>% 
  left_join(winner_games_won_against_opponents, by = c("player_i" = "winner_name",
                                        "player_j" = "loser_name")) %>% 
  left_join(loser_games_won_against_opponents, by = c("player_i" = "winner_name",
                                        "player_j" = "loser_name")) %>% 
  filter_at(vars(
    winner_games_won, loser_games_won), any_vars(!is.na(.))) %>% 
  mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) %>%
  group_by(grp) %>% 
  arrange(grp)
  mutate(player_i_games_won = winner_games_won + lead(loser_game_won))
  
  
  
game_base_model <- BTm(outcome = cbind(player_i, player_j_wins), player1 = player1, 
                       player2 = player2, data = head_to_head_results) 



find_head_to_head_records <- function(all_players, players_record_needed)
  
  {
  
  winner_head_to_head <- all_players %>% 
    group_by(winner_name, loser_name) %>% 
    tally(name = "player_i_wins") %>% 
    rename("player_i" = "winner_name",
           "player_j" = "loser_name") %>% 
    ungroup() %>% 
    filter(player_i %in% players_record_needed) 
  
  
  looser_head_to_head <- all_players %>% 
    group_by(loser_name, winner_name) %>% 
    tally(name = "player_j_wins") %>% 
    rename("player_j" = "winner_name",
           "player_i" = "loser_name") %>% 
    ungroup() %>% 
    filter(player_j %in% players_record_needed)
  
  player_names_one <- rbind(as_tibble(unique(all_players$winner_name)), 
                            as_tibble(unique(all_players$loser_name))) %>% 
    rename("player_i" = "value") %>% 
    unique()
  
  player_names_two <- rbind(as_tibble(unique(all_players$winner_name)), 
                            as_tibble(unique(all_players$loser_name))) %>% 
    rename("player_j" = "value") %>% 
    unique()
  
  all_potential_players <- tidyr::crossing(player_names_one, player_names_two) %>% 
    filter(!player_i == player_j)
  
  all_potential_head_to_head <- all_potential_players %>% 
    left_join(winner_head_to_head, by = c("player_i" = "player_i",
                                          "player_j" = "player_j")) %>% 
    left_join(looser_head_to_head, by = c("player_i" = "player_i",
                                          "player_j" = "player_j"))
  
  head_to_head_results <- all_potential_head_to_head %>% 
    group_by(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-grp)
  
  head_to_head_results <- head_to_head_results %>% 
    filter_at(vars(
      player_i_wins, player_j_wins), any_vars(!is.na(.)))
  
  
  head_to_head_results$player1 <- factor(head_to_head_results$player_i, 
                                         levels=unique(c(head_to_head_results$player_i, 
                                                         head_to_head_results$player_j)))  
  head_to_head_results$player2 <- factor(head_to_head_results$player_j, 
                                         levels=unique(c(head_to_head_results$player_i, 
                                                         head_to_head_results$player_j)))  
  
  
  
  return(head_to_head_results)
  
}

last_five_years_matches <- tidy_atp_matches %>% 
  dplyr::filter(year(tourney_date) >2014)


find_players_and_scores_from_model <- function(btm_model_results)

{

  bt_abilties <- BradleyTerry2::BTabilities(btm_model_results)
  player_names <- as_tibble(rownames(bt_abilties))
  bt_abilties_with_names <- cbind(bt_abilties, player_names)
  
  return(bt_abilties_with_names)
  
}

five_year_head_to_head_record <- 
  find_head_to_head_records(last_five_years_matches, all_twenty_nineteen_slams_players$value)


#Five year Model 
five_year_model <- BTm(outcome = cbind(player_i_wins, player_j_wins), player1 = player1, 
                   player2 = player2, data = five_year_head_to_head_record, refcat = "Zhe Li")


five_year_model_results <- find_players_and_scores_from_model(five_year_model) %>% 
  dplyr::left_join(all_twenty_nineteen_slams_players) %>% 
  dplyr::left_join(select(end_twenty_eighteen_rankings, rank, player), 
                   by = c("player_id" = "player")) %>% 
  dplyr::arrange(desc(ability)) %>% 
  dplyr::mutate(abilty_rank = row_number()) 

player_names_one <- rbind(as_tibble(unique(tidy_atp_matches$winner_name)), 
                      as_tibble(unique(tidy_atp_matches$loser_name))) %>% 
  rename("player_i" = "value") %>% 
  unique()
player_names_two <- rbind(as_tibble(unique(tidy_atp_matches$winner_name)), 
                          as_tibble(unique(tidy_atp_matches$loser_name))) %>% 
  rename("player_j" = "value") %>% 
  unique()

all_potential_players <- tidyr::crossing(player_names_one, player_names_two) %>% 
  filter(!player_i == player_j)

all_potential_head_to_head <- all_potential_players %>% 
  left_join(winner_head_to_head, by = c("player_i" = "player_i",
                                        "player_j" = "player_j")) %>% 
  left_join(looser_head_to_head, by = c("player_i" = "player_i",
                                        "player_j" = "player_j"))

head_to_head_results <- all_potential_head_to_head %>% 
  group_by(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-grp)
 
head_to_head_results <- head_to_head_results %>% 
  filter_at(vars(
    player_i_wins, player_j_wins), any_vars(!is.na(.)))
  

head_to_head_results$player1 <- factor(head_to_head_results$player_i, levels=unique(c(head_to_head_results$player_i, head_to_head_results$player_j)))  
head_to_head_results$player2 <- factor(head_to_head_results$player_j, levels=unique(c(head_to_head_results$player_i, head_to_head_results$player_j)))  



base_model2 <- BTm(outcome = cbind(player_i_wins, player_j_wins), player1 = player1, 
                   player2 = player2, data = head_to_head_results, refcat = "Zhe Li")


bt_abilities2 <- BradleyTerry2::BTabilities(base_model2)
player_names2 <- as_tibble(rownames(bt_abilities2))
bt_abilities2 <- cbind(player_names2, bt_abilities2)

bt_abilities %>% 
  filter(is.na(ability)) %>% 
  count()

player_with_abilities2 <- 
all_twenty_nineteen_slams_players  %>% 
  as_tibble() %>% 
  left_join(bt_abilities2)
  
missing_abilities2 <-player_with_abilities2 %>% 
  filter(is.na(ability)) %>% 
  left_join(winner_head_to_head, by = c("value" = "player_i"))

head_to_head_results %>% 
  anti_join(as_tibble(unique(missing_abilities2$value)),
            by = c("player_i" = "value"))

still_missing2 <- as_tibble(unique(missing_abilities2$value)) %>% 
  anti_join(winner_head_to_head, by = c("value" = "player_i"))



bt_abilities2_and_rank <- bt_abilities2 %>% 
  dplyr::left_join(all_twenty_nineteen_slams_players) %>% 
  dplyr::left_join(select(end_twenty_eighteen_rankings, rank, player), 
            by = c("player_id" = "player"))

## 5 year model


last_five_years_head_to_head_results <- 
base_model2 <- BTm(outcome = cbind(player_i_wins, player_j_wins), player1 = player1, 
                   player2 = player2, data = head_to_head_results, refcat = "Zhe Li")


#The above shows the need for decay or receny weightrings

head_to_head_results_for_decay <- head_to_head_results %>% 
  renam

#btm using nfl
BTm(outcome = cbind(player_i_wins, player_j_wins), player1 = player1, 
    player2 = player2, data = head_to_head_results, refcat = "Zhe Li")

NFL2010$player1 <- factor(NFL2010$home.team, 
                            levels=unique(c(NFL2010$home.team, NFL2010$away.team)))  
NFL2010$player2 <- factor(NFL2010$away.team, levels=unique(c(NFL2010$home.team, NFL2010$away.team)))  


nfl_btm <- BradleyTerry2::BTm(cbind(home.win, away.win), player1= player1,
                                    player2 = player2, data = NFL2010, refcat = "Carolina Panthers")

nfl_ability <- BTabilities(nfl_btm) %>% 
  as_tibble()

nfl_names <- rownames(BTabilities(nfl_btm))

nfl_ability <- cbind(nfl_names, nfl_ability)  %>%   
  arrange(desc(ability)) %>% 
  mutate(pos = row_number())



x <- BTdataframe(NFL2010, home = F)

##Standard Bradley-Terry Model optimization
y <- BTdecay(x$dataframe, x$ability, decay.rate = 0, fixed = x$worstTeam)
summary(y)

decay_row_names <- rownames(summary(y))


decay_score <- as_tibble(summary(y)) 

decay_score <- cbind(decay_row_names, decay_score) %>% 
  arrange(desc(score)) %>% 
  mutate(pos = row_number())

compare_scores <- nfl_ability %>% 
  left_join(select(decay_score, decay_row_names, pos), 
            by = c("nfl_names" = "decay_row_names"))
  
  
data("NFL2010")

map_df(.x = base_model_names$value, .f = function(x)
  
  {
  foo <- base_model$coefficients[[x]]
  return(foo)
  
})



name_prob <- map_df(.x = base_model_names$value, .f = function(.x)
  
{ get_prob <-  as_tibble(base_model$coefficients[[.x]]) %>% 
  mutate(player = .x)
return(get_prob)
}
)
  
clean_name_prob <- name_prob %>% 
  mutate(player = str_remove(player, ".."))

missing_phi <- clean_name_prob %>% 
  filter(is.na(value)) %>% 
  left_join(select(head_to_head_results, player_i, player_i_wins), by = c("player" = "player_i")) %>% 
  rename("player_i_wins" = "value") %>% 
  
  

twenty_nineteen_slams <- twenty_nineteen_slams %>% 
  left_join(clean_name_prob, by = c("player_i" = "player")) %>% 
  rename("player_i_phi" = "value") %>% 
  left_join(clean_name_prob, by = c("player_j" = "player")) %>% 
  rename("player_j_phi" = "value")

bradley_results <- twenty_nineteen_slams %>% 
  rowwise %>% 
  mutate(player_i_pred = predict_from_bradley(player_i_phi  = player_i_phi,
                                             player_j_phi = player_j_phi),
         player_j_pred = predict_from_bradley(player_i_phi = player_j_phi, player_j_phi = player_i_phi)) %>% 
  select(tourney_id, match_id, year, surface, round, round_number, player_i, player_j,
         player_i_phi, player_j_phi, player_i_pred, player_j_pred)

predict_from_bradley <- function(data, player_i_phi, player_j_phi){
  
  result <- exp(player_i_phi) / (exp(player_i_phi) +  exp(player_j_phi))
    return(result)
}

  
BTm(cbind(home.wins, away.wins),home.team, away.team, data = baseball)

#Need to extract names and coefeicents to then calc proabitlioes

tidyr::crossing(unique(tidy_atp_matches$winner_name), unique(tidy_atp_matches$loser_name))

head_to_head <- player_names %>% 
  left_join(winner_head_to_head, by = c("player_name" = "player_i"))

head_to_head_games <- winner_head_to_head %>% 
  left_join(looser_head_to_head, by = c("player_i" = "player_j",
                                        "player_j" = "player_i")) %>% 
  mutate(total_games = rowSums(across(where(is.numeric)), na.rm = T)) %>% 
  

foo <- head_to_head_games %>% 
  





latest_atp_matches <- list.files(pattern = "*2019") %>% 
  purrr::map_df(~readr::read_csv(.))

year_average <- full_data_ %>% 
  group_by(player_id) %>% 
  summarise_at(c("first_serve_pct", "mass"), mean, na.rm = TRUE)
hist(full_players_data_$pct_point_won_on_serve)




library("ggplot2")

ggplot(data.frame(x=c(0, 1)), aes(x=x)) + 
  stat_function(fun=eq)

probability <- seq(0,1, 0.1)

foo <- purrr::map(probability, p_server_wins_game)

ggplot(data.frame(x = data.frame(x=seq(0, 1, by = 0.1)), aes(x=x)) + 
  stat_function(fun=p_server_wins_game))


#Set winnign probabilty

aker for a set? Well in that case…

TB(p,q)=\sum_{i=1}^{28}A[i,1]p^{A[i,2]}(1-p)^{A[i,3]}q^{A[i,4]}(1-q)^{A[i,5]}pq[1-{p(1-q)+(1-p)q}]^{-1}
Yeah… more scary



matrix_A_data <- read.csv("matrix_A.csv",header = F)
matrix_A <- as.matrix(matrix_A_data)

matrix_B_data <- read.csv("matrix_B.csv",header = F)
matrix_B <- as.matrix(matrix_B_data)

probability
tie_break_perm <- 1:28

prob_and_tie_break_perm <- purrr::cross2(tie_break_perm, )

p_wins_a_tie_break_ <- purrr::map_df(.x = 1:28, 
                                       ~p_wins_tie_break(outcome = .x,
                                                         A = matrix_A,
                                                         p_server_wins_point = p_,
                                                         p_wins_point_on_return = q_)) %>% 
  sum()



p_wins_set <- purrr::map_df(.x = 1:21, 
                            ~p_wins_a_set(outcome = .x, 
                                          B = matrix_B, 
                                          p_server_wins_point = p_, 
                                          p_wins_point_on_return = q_,
                                          p_wins_a_tie_break_)) %>% 
  sum()

foo <- purrr::map_df(.x = probability, function(.x)
{
  p_wins_a_match(p_server_wins_point = .x, p_wins_point_on_return = 1 - .x + 0.02,
               match_length = 3) %>% as_tibble() %>% 
    mutate(p_server_wins_point = .x)
}
)

foo %>% 
  ggplot(aes(p_server_wins_point, value)) +
  geom_point() +
  geom_line()
  


