## Stochastic MArkov

library(ggplot2)

atp_2007_matches <- tidy_atp_matches %>% 
  filter(year(tourney_date) == "2007") %>% 
  select(match_id, winner_name, loser_name, w_pct_point_won_on_serve, l_pct_point_won_on_serve,
         w_serve_points, w_serve_points_won, l_serve_points, l_serve_points_won,
         w_return_points_won, l_return_points_won, w_points_won, l_points_won, 
         match_total_points, w_df, l_df)

newton_winners <-   tidy_atp_matches %>% 
  select(match_id, surface, winner_name, w_points_won, w_serve_points, w_return_points,
         w_serve_points_won, w_return_points_won, match_total_points) %>% 
  rename_at(.vars = vars(starts_with("w_")), ~str_replace(., "w_", "")) %>% 
  rename("player_name" = "winner_name")


newton_losers <-   tidy_atp_matches %>% 
  select(match_id, surface, loser_name, l_points_won, l_serve_points, l_return_points,
         l_serve_points_won, l_return_points_won, match_total_points) %>% 
  rename_at(.vars = vars(starts_with("l_")), ~str_replace(., "l_", "")) %>% 
  rename("player_name" = "loser_name")


newton_players <- rbind(newton_winners, newton_losers) %>% 
  arrange(match_id) %>% 
  ungroup()


points_won_check <- newton_players %>% 
  mutate(points_played_check = (serve_points + return_points) == match_total_points,
    points_won_check = (serve_points_won + return_points_won) == points_won) %>% 
  filter_at(vars(c(points_won_check, points_played_check), any_vars(is.na(.))))
            
            %>% 
  filter(!(points_won_check))
points_won_check
filter_at(vars(C, D), all_vars(. == max(.)))

newton_player_surface_measures <- newton_players %>% 
  group_by(player_name, surface) %>% 
  summarise_at(vars(points_won, serve_points, return_points, serve_points_won, return_points_won),
               sum, na.rm = TRUE) %>% 
  mutate(pct_serve_points_won = serve_points_won / serve_points,
         pct_return_points_won = return_points_won / return_points) %>% 
  select(player_name, surface, pct_serve_points_won, pct_return_points_won)


### Field uses tpoints won!!!
newton_field_all_surface_measures <- newton_players %>% 
  group_by(player_name) %>% 
  summarise_at(vars(points_won, serve_points, return_points, serve_points_won, return_points_won),
               sum, na.rm = TRUE) %>% 
  mutate(pct_points_won_on_serve = serve_points_won / points_won,
         pct_points_won_on_return = return_points_won / points_won) %>% 
  select(player_name, pct_points_won_on_serve, pct_points_won_on_return)


newton_field_all_surface_measures_long <- newton_field_all_surface_measures %>% 
  tidyr::pivot_longer(starts_with("pct_"), names_to = "point_type")

mean_sd_of_all_surface_field <- newton_field_all_surface_measures %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

mean_sd_of_all_surface_field_long <- mean_sd_of_all_surface_field %>% 
  tidyr::pivot_longer(contains("mean"), names_to = "mean", values_to = "mean_value") %>% 
  tidyr::pivot_longer(contains("sd"), names_to = "sd", values_to = "sd_value")
  
newton_field_all_surface_measures_long %>%
  ggplot2::ggplot(aes(value, fill = point_type)) + geom_histogram() 

  
newton_field_by_surface_measures <- newton_players %>% 
  group_by(player_name, surface) %>% 
  summarise_at(vars(points_won, serve_points, return_points, serve_points_won, return_points_won),
               sum, na.rm = TRUE) %>% 
  mutate(pct_points_won_on_serve = serve_points_won / points_won,
         pct_points_won_on_return = return_points_won / points_won) %>% 
  select(player_name, surface, pct_points_won_on_serve, pct_points_won_on_return)

mean_sd_by_surface_field <- newton_field_by_surface_measures %>% 
  group_by(surface) %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

newton_field_by_surface_measures_long <- newton_field_by_surface_measures %>% 
  tidyr::pivot_longer(starts_with("pct_"), names_to = "point_type")

newton_field_by_surface_measures_long %>%
  ggplot2::ggplot(aes(value, fill = point_type)) + geom_histogram() +facet_wrap("surface")


newton_player_all_surface_measures <- newton_players %>% 
  #group_by(player_name) %>% 
  #summarise_at(vars(points_won, serve_points, return_points, serve_points_won, return_points_won),
  #            sum, na.rm = TRUE) %>% 
  mutate(pct_serve_points_won = serve_points_won / serve_points,
         pct_return_points_won = return_points_won / return_points) %>% 
  select(player_name, pct_serve_points_won, pct_return_points_won)

mean_sd_of_all_surface_by_player <- newton_player_all_surface_measures %>% 
  group_by(player_name) %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

big_three <- c("Rafael Nadal", "Novak Djokovic", "Roger Federer")
big_three_data <- newton_player_all_surface_measures %>% 
  filter(player_name %in% big_three) %>% 
  tidyr::pivot_longer(starts_with("pct_"), names_to = "point_type") 

big_three_data %>% 
  ggplot2::ggplot(aes(value, fill = point_type)) + geom_histogram() +facet_wrap("player_name")

newton_player_by_surface_measures <- newton_players %>% 
  group_by(surface) %>% 
  #summarise_at(vars(points_won, serve_points, return_points, serve_points_won, return_points_won),
  #            sum, na.rm = TRUE) %>% 
  mutate(pct_serve_points_won = serve_points_won / serve_points,
         pct_return_points_won = return_points_won / return_points) %>% 
  select(player_name, pct_serve_points_won, pct_return_points_won)

mean_sd_of_by_surface_by_player <- newton_player_by_surface_measures %>% 
  group_by(player_name, surface) %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

big_three_by_surface <- mean_sd_of_by_surface_by_player %>% 
  filter(player_name %in% big_three)
  
###### Create Field Adjsuted variables

players_field_all_surface_adjusted_variables <- mean_sd_of_all_surface_by_player %>% 
  mutate(player_adjusted_points_won_on_return = 
           pct_return_points_won_mean - mean_sd_of_all_surface_field$pct_points_won_on_return_mean)

players_field_by_surface_adjusted_variables <- mean_sd_of_by_surface_by_player %>% 
  left_join(mean_sd_by_surface_field, by = "surface") %>% 
  mutate(player_adjusted_points_won_on_return = 
           pct_return_points_won_mean - pct_points_won_on_return_mean)

         

twenty_nineteen_slams_newton  <- twenty_nineteen_slams %>% 
  select(match_id, tourney_name, tourney_date, year, surface, round, player_i, player_j) %>% 
  filter_at(vars(c(player_i, player_j)), any_vars(. %in% big_three)) %>% 
  left_join(select(players_field_all_surface_adjusted_variables, player_name, pct_serve_points_won_mean,
                   player_adjusted_points_won_on_return), by = 
              c("player_i" = "player_name")) %>% 
  rename("player_i_pct_points_won_on_serve_mean" = "pct_serve_points_won_mean",
         "player_i_adjusted_points_won_on_return" = "player_adjusted_points_won_on_return") %>% 
  left_join(select(players_field_all_surface_adjusted_variables, player_name, pct_serve_points_won_mean,
                   player_adjusted_points_won_on_return), by = 
              c("player_j" = "player_name")) %>% 
  rename("player_j_pct_points_won_on_serve_mean" = "pct_serve_points_won_mean",
         "player_j_adjusted_points_won_on_return" = "player_adjusted_points_won_on_return") %>% 
  mutate(player_i_adjusted_points_won_on_serve = 
           player_i_pct_points_won_on_serve_mean - player_j_adjusted_points_won_on_return,
         player_j_adjusted_points_won_on_serve = 
           player_j_pct_points_won_on_serve_mean - player_i_adjusted_points_won_on_return) %>% 
  left_join(select(players_field_all_surface_adjusted_variables, player_name,
                   pct_serve_points_won_sd), by = c("player_i" = "player_name")) %>% 
  rename("player_i_pct_serve_points_won_sd" = "pct_serve_points_won_sd") %>% 
  left_join(select(players_field_all_surface_adjusted_variables, player_name,
                   pct_serve_points_won_sd), by = c("player_j" = "player_name")) %>% 
  rename("player_j_pct_serve_points_won_sd" = "pct_serve_points_won_sd")

  

simulations <- 10
nadal_sample <- rnorm(10, nadal_adjusted, 0.083207)
fed_sample <- rnorm(10, fed_adjusted, 0.072314)


players_with_simulations_drawn <- 

players_with_simulations_drawn <- purrr::map_df(.x = 1:nrow(twenty_nineteen_slams_newton),
                     .f = function(.x)
                     {
                     
                      set.seed(123)
                       player_i_name <-  twenty_nineteen_slams_newton$player_i[.x]
                       player_i_adjusted_var <- twenty_nineteen_slams_newton$player_i_adjusted_points_won_on_serve[.x]
                       player_i_sd <- twenty_nineteen_slams_newton$player_i_pct_serve_points_won_sd
                       player_i_sample <- rnorm(simulations, player_i_adjusted_var, player_i_sd)
                       
                       player_j_name <-  twenty_nineteen_slams_newton$player_j[.x]
                       player_j_adjusted_var <- twenty_nineteen_slams_newton$player_j_adjusted_points_won_on_serve[.x]
                       player_j_sd <- twenty_nineteen_slams_newton$player_j_pct_serve_points_won_sd
                       player_j_sample <- rnorm(simulations, player_j_adjusted_var, player_j_sd)
                       
                       match_result_simulations <- 
                         map2_df(.x = player_i_sample,
                               .y = player_j_sample,
                               .f = function(.x, .y)
                               {
                                 tibble(
                                   player_i = player_i_name,
                                   prob_player_i_wins_match = p_wins_a_match(.x, 1-.y, 5),
                                   player_j = player_j_name,
                                   prob_player_j_wins_match =  p_wins_a_match(.y, 1-.x, 5)
                                 )

                               }
                       )
                       
                     }
)

summarised_player_and_prob <- foo %>% 
  mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) %>%
  group_by(grp) %>% 
  summarise_at(vars(starts_with("prob_")), list(mean = mean, sd = sd),  na.rm = T)

twenty_nineteen_slams_newton_results <- twenty_nineteen_slams_newton %>% 
  mutate(grp = paste(pmax(player_i, player_j), pmin(player_i, player_j), sep = "_")) %>%
  left_join(select(summarised_player_and_prob, grp, prob_player_i_wins_match_mean,
                   prob_player_j_wins_match_mean), 
            by = c("grp" = "grp")) %>% 
  mutate(winner = if_else(prob_player_i_wins_match_mean > prob_player_j_wins_match_mean,
         player_i, 
         if_else(prob_player_j_wins_match_mean > prob_player_i_wins_match_mean,
                 player_j, "TIE"))) 

correct <- twenty_nineteen_slams_newton_results %>% 
  filter(player_i == winner)

incorrect <- twenty_nineteen_slams_newton_results %>% 
  filter(!player_i == winner)

accuracy <- nrow(correct) / nrow(twenty_nineteen_slams_newton_results) * 100

#Looks harder to predict the later roudns i.e when players are tightly matched
#Wehreas all early rounds were fien. There are higer number of early round matches
# so this would skew the accuracy measur 





  mutate(winner = case_when(prob_player_i_wins_match > prob_player_j_wins_match ~
                              player_i_name, TRUE ~ as.character("TIE")))
                            prob_player_j_wins_match > prob_player_i_wins_match ~
                              player_j_name,
                            prob_player_i_wins_match > prob_player_j_wins_match ~
                              "TIE"))
                            
  mutate(winner = if_else(prob_player_i_wins_match > prob_player_j_wins_match,
                          player_i_name,
                          if_else(prob_player_j_wins_match > prob_player_i_wins_match,
                                  layer_j_name, "TIE")))
           
           
           
           if_else(prob_player_i_wins_match > prob_player_j_wins_match,
                          player_i_name,
                          if_else(prob_player_j_wins_match > prob_player_i_wins_match,
                                  player_j_name, "TIE")))


big_three_match_predictions <- 


colnames(mean_sd_of_field) <- paste("field",
                                colnames(mean_sd_of_field), sep = "_")

mean_sd_of_players <- newton_field %>% 
  group_by(player_name) %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

players_field_adjusted_variables <- mean_sd_of_players %>% 
  mutate(field_pct_points_won_on_serve_mean = mean_sd_of_field$field_pct_points_won_on_serve_mean,
         field_pct_points_won_on_return_mean = mean_sd_of_field$field_pct_points_won_on_return_mean,
         field_pct_points_won_on_serve_sd = mean_sd_of_field$field_pct_points_won_on_serve_sd,
         field_pct_points_won_on_return_sd= mean_sd_of_field$field_pct_points_won_on_return_sd,
         player_adjusted_points_won_on_serve = 
           pct_points_won_on_serve_mean - field_pct_points_won_on_serve_mean,
         player_adjusted_points_won_on_return = 
           pct_points_won_on_return_mean - field_pct_points_won_on_return_mean)
         
           
 
         
           
fedal <- players_field_adjusted_variables %>%
  filter_at(vars(starts_with("pct_")), any_vars(is.na(.))) %>% 
  
  filter(str_detect(player_name, paste(strings, collapse = "|"))) 

nadal_data <- fedal %>% 
  filter(str_detect(player_name, "Nadal")) %>% 
  mutate(pct_points_won_on_serve_mean = 0.6877,
         pct_points_won_on_return_mean = )
  

one_match_players <- tidy_atp_matches %>% 
  group_by(loser_name) %>% tally() %>% filter(n ==1)

players_na <- mean_sd_of_players %>% 
  filter_at(vars(starts_with("pct_")), any_vars(is.na(.))) %>% 
  left_join(one_match_players, by = c("player_name" = "loser_name")) %>% 
  filter(n >1)

nadal_mean_sd <- mean_sd_of_players %>% 
  filter(player_name == "Rafael Nadal")


runs = 300000

foo <- purrr::map_df(.x = 1:nrow(),
                     .f = function(.x)
                     {
                       results <- tibble(
                         fed = p_wins_a_match(fed_sample[.x], 1-nadal_sample[.x], 5),
                         nad = p_wins_a_match(nadal_sample[.x], 1-fed_sample[.x], 5),
                         check = abs(1 - (fed + nad)))
                       
                     }
)

p_wins_point_on_serve <- as_tibble(rnorm(runs, 
                                         mean = nadal_mean_sd$pct_points_won_on_serve_mean, 
                                         sd = nadal_mean_sd$pct_points_won_on_serve_sd)) %>% 
  rename("p_wins_point" = "value")

p_wins_point_on_serve %>%
  ggplot( aes(x=p_wins_point)) +
  geom_histogram() +
  ggtitle("Bin size = 3") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p
mc_sim_p_wins_game <- purrr::map_df(p_wins_point_on_serve$p_wins_point, 
                                    .f = ~as_tibble(p_server_wins_game(.x)))


mc_sim_p_wins_game %>%
  ggplot( aes(x=value)) +
  geom_histogram() +
  ggtitle("Bin size = 3") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )




p <- newton_field %>%
  ggplot( aes(x=pct_points_won_on_serve)) +
  geom_histogram() +
  ggtitle("Bin size = 3") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p
hist(newton_field$pct_points_won_on_serve,xlab = "Weight",col = "yellow",border = "blue")

field_measures <- newton_measures %>% 
  #group_by(surface) %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

  
player_measures <- newton_measures %>% 
  group_by(player_name) %>% 
  summarise_at(vars(starts_with("pct_")), list(mean = mean, sd = sd),  na.rm = T)

nadal <- newton_measures %>% 
  filter(player_name == "Rafael Nadal")

sd(nadal$pct_points_won_on_return)

newton_measures <- newton_players %>% 
  group_by(player_name) %>% 
  summarise_at(vars(serve_points:match_total_points),
               sum, na.rm = TRUE) %>% 
  mutate(pct_point_won_on_serve = serve_points_won / serve_points,
         pct_point_won_returning = return_points_won / )
  

all_players_2007 <- c(unique(atp_2007_matches$winner_name),
                      unique(atp_2007_matches$loser_name))


runs = 300000
p_wins_point_on_serve <- as_tibble(rnorm(runs, mean = 0.63316, sd = 0.094779)) %>% 
  rename("p_wins_point" = "value")
mc_sim_p_wins_game <- purrr::map_df(p_wins_point_on_serve$value, 
                                    .f = ~as_tibble(p_server_wins_game(.x)))



sim_test <- cbind(p_wins_point_on_serve, mc_sim_p_wins_game)

p <- sim_test %>%
  ggplot( aes(x=p_wins_point)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

p

hist(test_result$value, breaks=100, col="red", xlim=c(0,1), ylim=c(0,30000),
     main="Gaussian deviates :  mean=8, sigma=1.3", col.main="blue")
p <- ggplot(sim_test, aes(x=p_wins_point)) + 
  geom_histogram(bins = 50) 
p


nadal_adjusted_p_on_serve = 0.64455
nadal_adj_p_wins_point_on_serve <- as_tibble(rnorm(runs, nadal_adjusted_p_on_serve, sd = 0.0273))
  



fed_adjusted_p_on_serve =  0.64165

strings <- c("Nadal", "Federer")

fedal_2007 <- atp_2007_matches %>% 
  filter_at(vars(winner_name, loser_name), ~str_detect(.x,paste(strings, collapse = "|")))




atp_2007_matches %>% 
  filter_at(vars(
    winner_name, loser_name), any_vars(!is.na(.)))




## For now lets just use 2019 numbers
twenty_nineteen_slams_newton <- twenty_nineteen_slams %>%
  select(player_i, player_j, prob_player_i_wins_point_on_serve, 
         prob_player_j_wins_point_on_serve, prob_player_i_wins_point_on_return,
         prob_player_j_wins_point_on_return)

player_i_data <- twenty_nineteen_slams_newton %>% 
  select(contains("player_i")) %>% 
  rename_at(.vars = vars(contains("_player_i")), ~str_replace(., "_player_i", "")) %>% 
  rename("player_name" = "player_i")

  
player_j_data <- twenty_nineteen_slams_newton %>% 
  select(contains("player_j")) %>% 
  rename_at(.vars = vars(contains("_player_j")), ~str_replace(., "_player_j", "")) %>% 
  rename("player_name" = "player_j")

newton_player_data <- rbind(player_i_data, player_j_data)

newton_player_mean_sd <- newton_player_data %>% 
  group_by(player_name) %>% 
  summarise_at(vars(starts_with("prob")), list(mean = mean, sd = sd),  na.rm = T)

newton_player_data %>% 
  mutate(prob_wins_point_on_serve =as.numeric(prob_wins_point_on_serve)) %>% 
  filter(player_name == "Stefanos Tsitsipas") %>% 
  summarise(prob_wins_point_on_serve_mean = mean(prob_wins_point_on_serve, na.rm=T))
    

### Simulation Eg
nadal_adjusted = 0.64165
nadal_adjusted_return_sd <- 0.10075 - 0.094779
fed_adjusted = 0.64455  
fed_adjusted_reurn_sd <- 

nadal_sample <- rnorm(10, nadal_adjusted, 0.083207)
fed_sample <- rnorm(10, fed_adjusted, 0.072314)

for (i in 1:10) {
  
  result_list <- list()
  result <- p_wins_a_match(fed_sample[i], 1-nadal_sample[i], 5)
  result_list[[i]] <- result
}
foo <- purrr::map_df(.x = 1:10,
           .f = function(.x)
           {
             results <- tibble(
               fed = p_wins_a_match(fed_sample[.x], 1-nadal_sample[.x], 5),
               nad = p_wins_a_match(nadal_sample[.x], 1-fed_sample[.x], 5),
               check = abs(1 - (fed + nad)))
             
           }
)
            
             
