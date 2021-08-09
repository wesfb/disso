#### Script for model building


## Load packages

library(dplyr)
library(readr)
library(tidymodels)


### Load data and split into train and test

full_data_with_features <- readr::read_csv("full_data_with_features_new 2.csv") %>% 
  dplyr::mutate(result = as.factor(result),
                surface = as.factor(surface),
                player_i_hand = as.factor(player_i_hand),
                player_j_hand = as.factor(player_j_hand)) %>% 
  select(-pct_return_points_won_diff, -first_serve_return_win_pct_diff, -second_serve_return_win_pct_diff)



set.seed(123)


data_split <- initial_split(full_data_with_features, prop = 0.80, strata = result)

training_data <- training(data_split)

testing_data <- testing(data_split)

testing_data %>% 
  filter(result ==1) %>% 
  nrow()

## Creaete a receipe

recipe(result ~ ., data = training_data) %>%
  summary()

training_recipie_one <- recipes::recipe(result ~ ., data = training_data) %>%
  recipes::update_role(grp:player_j, new_role = "id") %>%
  #recipes::step_corr(all_numeric()) %>% 
  step_dummy(all_predictors(),-all_numeric()) %>% 
  recipes::step_normalize(all_numeric()) 
  #recipes::step_pca(all_predictors())
  #ecipes::prep()


### Logistic Regression

log_regression <- logistic_reg(mode = "classification") %>%
  set_engine(engine = "glm") 

summary(log_regression)

log_regression_wf <- workflow() %>%
  add_recipe(training_recipie_one)

log_regression_fit <- log_regression_wf %>% 
  add_model(log_regression) %>% 
  fit(data = training_data)

summary(log_regression_fit)

log_regression_fit_parameters <- log_regression_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

logistic_regression_results <- testing_data %>% 
  select(grp_two, result) %>% 
  bind_cols(predict(log_regression_fit, new_data = testing_data)) %>% 
  bind_cols(predict(log_regression_fit, testing_data, type = "prob")) %>% 
  as_tibble() %>% 
  mutate(log_prob = if_else(result == 0, log(.pred_0), log(.pred_1)))

average_probabily <- mean(logistic_regression_results$winning_probabilty)

conf_mat(logistic_regression_results, truth = result, estimate = .pred_class)
accuracy(logistic_regression_results, truth = result, estimate = .pred_class)
mcc(logistic_regression_results, truth = result, estimate = .pred_class)
f_meas(logistic_regression_results, truth = result, estimate = .pred_class)
mn_log_loss(logistic_regression_results, result, .pred_0)

## Tune penalsied logisitc regression 


set.seed(1234)
cross_val_samples <- mc_cv(training_data, times = 10)

penalised_log_regression <- logistic_reg(mode ="classification",  
                                penalty = tune::tune(), mixture = tune()) %>%
  set_engine("glmnet")

penalised_log_regression_param <- parameters(penalty(range = c(-5,1), trans = log10_trans()),
                       mixture())

penalised_log_regression_grid <- 
  grid_regular(penalised_log_regression_param, levels = c(7, 5))


ctrl <- control_resamples(save_pred = TRUE)
multi_metric <- metric_set(accuracy, mcc, f_meas, mn_log_loss)

penalised_log_regression_wf <- workflow() %>%
  add_recipe(training_recipie_one) %>% 
  add_model(penalised_log_regression) 

penalised_log_regression_tune <- 
  tune_grid(penalised_log_regression_wf, 
            grid = penalised_log_regression_grid,
            resamples = cross_val_samples,
            metrics = multi_metric,
            control = ctrl)

rlist::list.save(penalised_log_regression_tune, 'penalised_log_regression_tune.Rds')
#foo_tune <-  rlist::list.load("penalised_log_regression_tune.Rds")
#all.equal(foo_tune, penalised_log_regression_tune)


penalised_log_regression_metrics <- collect_metrics(penalised_log_regression_tune)

####  Naive Bayes model
#https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/

library(e1071)
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m

nb_spec <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")



nb_workflow <- workflow() %>%
  add_recipe(training_recipie_one) %>% 
  add_model(nb_spec) 
  

nb_fit <- nb_workflow %>%
  add_model(nb_spec) %>%
  fit(data = training_data)

nb_fit %>% summary()
nb_fit$fit




nb_results <- testing_data %>% 
  select(grp_two, result) %>% 
  bind_cols(predict(nb_fit, new_data = testing_data)) %>% 
  bind_cols(predict(nb_fit, testing_data, type = "prob")) %>% 
  as_tibble() %>% 
  mutate(log_prob = if_else(result == 0, log(.pred_0), log(.pred_1)))

conf_mat(nb_results, truth = result, estimate = .pred_class)
accuracy(nb_results, truth = result, estimate = .pred_class)
mcc(nb_results, truth = result, estimate = .pred_class)
f_meas(nb_results, truth = result, estimate = .pred_class)
mn_log_loss(nb_results, result, .pred_0)



fit(nb_spec, data = iris)

nb_mod <-
  naive_Bayes(smoothness = .8) %>%
  set_engine("klaR") %>%
  fit(class ~ ., data = parabolic)


parabolic_grid <-
  expand.grid(X1 = seq(-5, 5, length = 100),
              X2 = seq(-5, 5, length = 100))

prep_recipie_one <- prep(training_recipie_one)

training_data_outside_workflow <- bake(prep_recipie_one, new_data = training_data) %>% 
  select(-grp,  -grp_two, -player_i, -player_j)

nb <- naive_bayes(result ~ ., training_data_outside_workflow)

mean_sd_winners <- training_data_outside_workflow %>% 
  filter(result == 1) %>% 
  summarise_if(is.numeric, list(mean, sd))

mean_sd_losers <- training_data_outside_workflow %>% 
  filter(result == 0) %>% 
  summarise_if(is.numeric, list(mean, sd))

prior_prob_winner <- mean(training_data_outside_workflow$result == 1)
prior_prob_loser <- mean(training_data_outside_workflow$result == 0)

baked_testing_data_eg <- bake(prep_recipie_one, new_data = testing_data) %>% 
  select(where(is.numeric)) %>% 
  slice(1) %>% 
  pivot_longer(cols = where(is.numeric), names_to = "metric", values_to = "value")

winner_feature_means <- mean_sd_winners %>% 
  pivot_longer(contains("_fn1"), names_to = "metric", values_to = "winner_mean") %>%
  select(metric, winner_mean) %>% 
  mutate(metric = stringr::str_replace(metric, "_fn1", ""))

loser_feature_means <- mean_sd_losers %>% 
  pivot_longer(contains("_fn1"), names_to = "metric", values_to = "loser_mean") %>%
  select(metric, loser_mean) %>% 
  mutate(metric = stringr::str_replace(metric, "_fn1", ""))
 
winner_feature_sd <- mean_sd_winners %>% 
  pivot_longer(contains("_fn2"), names_to = "metric", values_to = "winner_sd") %>%
  select(metric, winner_sd) %>% 
  mutate(metric = stringr::str_replace(metric, "_fn2", ""))

loser_feature_sd <- mean_sd_losers %>% 
  pivot_longer(contains("_fn2"), names_to = "metric", values_to = "loser_sd") %>%
  select(metric, loser_sd) %>% 
  mutate(metric = stringr::str_replace(metric, "_fn2", ""))

features_mean_sds <- winner_feature_means %>% 
  left_join(winner_feature_sd) %>% 
  left_join(loser_feature_means) %>% 
  left_join(loser_feature_sd) %>% 
  left_join(baked_testing_data_eg) %>% 
  mutate(winner_norm = dnorm(value, mean = winner_mean, sd = winner_sd) / 22,
         loser_norm = dnorm(value, mean = loser_mean, loser_sd)/ 22),
         winner_prior_prob = prior_prob_winner,
         winning_prob = prior_prob_winner * winner_norm,
         loser_prior_prob = prior_prob_loser,
         loser_prob = prior_prob_loser * loser_norm)
  


tester <- features_mean_sds %>% select(metric, winner_norm) %>% 
  pivot_wider(names_from = metric, values_from = winner_norm) %>% 
  mutate(pct = age_diff * height_diff * rank_diff * first_serve_pct_diff * first_serve_win_pct_diff *
           second_serve_win_pct_diff * pct_serve_points_won_diff * pct_point_won_on_serve_diff * 
           bp_converted_pct_diff * bp_saved_pct_diff * aces_per_game_pct_diff * 
           df_per_game_pct_diff * total_matches_played_diff * total_match_wins_diff * 
           total_match_loses_diff * h2h_record * surface_Grass * surface_Hard * 
           player_i_hand_R * player_i_hand_U *  player_j_hand_R * player_j_hand_U * prior_prob_winner) %>% 
  select(pct)

tester2 <- features_mean_sds %>% select(metric, loser_norm) %>% 
  pivot_wider(names_from = metric, values_from = loser_norm) %>% 
  mutate(pct = age_diff * height_diff * rank_diff * first_serve_pct_diff * first_serve_win_pct_diff *
           second_serve_win_pct_diff * pct_serve_points_won_diff * pct_point_won_on_serve_diff * 
           bp_converted_pct_diff * bp_saved_pct_diff * aces_per_game_pct_diff * 
           df_per_game_pct_diff * total_matches_played_diff * total_match_wins_diff * 
           total_match_loses_diff * h2h_record * surface_Grass * surface_Hard * 
           player_i_hand_R * player_i_hand_U *  player_j_hand_R * player_j_hand_U * prior_prob_loser) %>% 
  select(pct)

split= initial_split(iris)
Trainingset= training(split)
Testset= testing(split)

#Finding Class Prior Probabilities of each species
PriorProb_Setosa= mean(Trainingset$Species=="setosa")
PriorProb_Virginica= mean(Trainingset$Species=="virginica")
PriorProb_versicolor= mean(Trainingset$Species=="versicolor")

all_three <- PriorProb_Setosa * PriorProb_Virginica * PriorProb_versicolor

setosa_var <- Trainingset %>% 
  filter(Species == "setosa") %>% 
  summarise(mean(Sepal.Length),mean(Sepal.Width),mean(Petal.Length),mean(Petal.Width),
            sd(Sepal.Length),sd(Sepal.Width),sd(Petal.Length),sd(Petal.Width))

virginica_var <- Trainingset %>% 
  filter(Species == "virginica") %>% 
  summarise(mean(Sepal.Length),mean(Sepal.Width),mean(Petal.Length),mean(Petal.Width),
            sd(Sepal.Length),sd(Sepal.Width),sd(Petal.Length),sd(Petal.Width))

versicolor_var <- Trainingset %>% 
  filter(Species == "versicolor") %>% 
  summarise(mean(Sepal.Length),mean(Sepal.Width),mean(Petal.Length),mean(Petal.Width),
            sd(Sepal.Length),sd(Sepal.Width),sd(Petal.Length),sd(Petal.Width))
sl=6.9
sw=3.1
pl=5.0
pw=2.3
newfeatures=data.frame(Sepal.Length=sl,Sepal.Width=sw,Petal.Length=pl,Petal.Width=pw)
Y_Pred=predict(classifier,newdata = newfeatures)
Y_Pred

# Set_sl=dnorm(sl,mean=setosa_var$`mean(Sepal.Length)`, sd=setosa_var$`sd(Sepal.Length)`) /setosa_var$`mean(Sepal.Length)`
# Set_sw=dnorm(sw,mean=setosa_var$`mean(Sepal.Width)` , sd=setosa_var$`sd(Sepal.Width)`) /setosa_var$`mean(Sepal.Width)`
# Set_pl=dnorm(pl,mean=setosa_var$`mean(Petal.Length)`, sd=setosa_var$`sd(Petal.Length)`) / setosa_var$`mean(Petal.Length)`
# Set_pw=dnorm(pw,mean=setosa_var$`mean(Petal.Width)` , sd=setosa_var$`sd(Petal.Width)`) / setosa_var$`mean(Petal.Width)`
# 
# ProbabilitytobeSetosa =Set_sl*Set_sw*Set_pl*Set_pw*PriorProb_Setosa
# Vir_sl=dnorm(sl,mean=virginica_var$`mean(Sepal.Length)`, sd=virginica_var$`sd(Sepal.Length)`) / virginica_var$`mean(Sepal.Length)`
# Vir_sw=dnorm(sw,mean=virginica_var$`mean(Sepal.Width)` , sd=virginica_var$`sd(Sepal.Width)`) / virginica_var$`mean(Sepal.Width)`
# Vir_pl=dnorm(pl,mean=virginica_var$`mean(Petal.Length)`, sd=virginica_var$`sd(Petal.Length)`) / virginica_var$`mean(Petal.Length)`
# Vir_pw=dnorm(pw,mean=virginica_var$`mean(Petal.Width)` , sd=virginica_var$`sd(Petal.Width)`) / virginica_var$`mean(Petal.Width)`
# 
# ProbabilitytobeVirginica =Vir_sl*Vir_sw*Vir_pl*Vir_pw*PriorProb_Virginica
# Ver_sl=dnorm(sl,mean=versicolor_var$`mean(Sepal.Length)`, sd=versicolor_var$`sd(Sepal.Length)`) /all_three
# Ver_sw=dnorm(sw,mean=versicolor_var$`mean(Sepal.Width)` , sd=versicolor_var$`sd(Sepal.Width)`) / all_three
# Ver_pl=dnorm(pl,mean=versicolor_var$`mean(Petal.Length)`, sd=versicolor_var$`sd(Petal.Length)`) / all_three
# Ver_pw=dnorm(pw,mean=versicolor_var$`mean(Petal.Width)` , sd=versicolor_var$`sd(Petal.Width)`) / all_three
#   
# ProbabilitytobeVersicolor=Ver_sl*Ver_sw*Ver_pl*Ver_pw*PriorProb_versicolor
# ProbabilitytobeSetosa
# ProbabilitytobeVirginica
# ProbabilitytobeVersicolor


classifier=naiveBayes(x = Trainingset[,-5],
                      y = Trainingset$Species)
classifier

Y_Pred=predict(classifier,newdata = newfeatures, type = "raw")

Y_Pred

nadal_fed <- baked_testing_data %>% 
  filter(grp_two == "Roger Federer_Rafael Nadal_2008-414_F") %>% 
  select(where(is.numeric))
  

nb_base <- naiveBayes(result ~ ., training_data_outside_workflow)


## This shows the mean differene in serving abilkites for losers is 0.18


pairs.panels(training_data_outside_workflow[-Rank])

###### Support Vector Machien Model


svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svn_wf <- workflow() %>% 
  add_recipe(training_recipie_one) %>% 
  add_model(svm_mod)


svm_param <- parameters(cost(range = c(-5,1), trans = log2_trans()),
                        rbf_sigma())

svm_grid <- 
  grid_regular(svm_param, levels = c(7, 5))


svm_tune <- 
  tune_grid(svn_wf, 
            resamples = cross_val_samples,
            metrics = multi_metric,
            control = ctrl)

rlist::list.save(svm_tune, 'svm_tune.rds')
#foo_svm_tune <- rlist::list.load("svm_tune.rds")


svm_metrics <- collect_metrics(svm_tune)
readr::write_rds(svm_metrics, "svm_metrics.Rdata")

best_log_loss <- show_best(svm_tune, metric = "mn_log_loss")
best_accuracy <- show_best(svm_tune, metric = "accuracy")
best_mcc <- show_best(svm_tune, metric = "mcc")
best_f_meas <- show_best(svm_tune, metric = "f_meas")

svm_predictions <- collect_predictions(svm_tune)

svm_best_param <- svm_tune %>% 
  tune::select_best(metric = "mn_log_loss")

#Refit using the entire training data

svm_tuned_with_optimal <-
  svn_wf %>%
  tune::finalize_workflow(svm_best_param) %>%
  parsnip::fit(data = training_data)

svm_results <- testing_data %>% 
  select(grp_two, result) %>% 
  bind_cols(predict(svm_tuned_with_optimal, new_data = testing_data)) %>% 
  bind_cols(predict(svm_tuned_with_optimal, testing_data, type = "prob")) %>% 
  as_tibble() %>% 
  mutate(log_prob = if_else(result == 0, log(.pred_0), log(.pred_1)))

average_probabily <- mean(logistic_regression_results$winning_probabilty)

conf_mat(svm_results, truth = result, estimate = .pred_class)
accuracy(svm_results, truth = result, estimate = .pred_class)
mcc(svm_results, truth = result, estimate = .pred_class)
f_meas(svm_results, truth = result, estimate = .pred_class)
mn_log_loss(svm_results, result, .pred_0)

svm_tune$.metrics


plot(iris)


full_data_with_features %>% 
  select(age_diff:result) %>% 
  plot()



# 
# lasso_grid <- tune_grid(
#   lasso_logit_workflow %>% 
#     add_model(lasso_tune_spec),
#   resamples = cross_val_samples,
#   grid = lambda_grid_to_search,
#   control = ctrl
# )
# 
# penalised_log_regression_wf_final <- 
#   penalised_log_regression_wf %>% 
#   finalize_workflow(penalised_log_regression_tune) %>% 
#   fit
# 
# 
# lasso_tune_spec <- logistic_reg(mode ="classification",  
#                                 penalty = tune::tune(), mixture = 1) %>%
#   set_engine("glm")
        